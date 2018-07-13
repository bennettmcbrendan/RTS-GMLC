
extract_interface_lines = function(){

    pacman::p_load(rplexos, RSQLite, magrittr, dplyr, lubridate, 
                   rmarkdown, scales, cowplot, data.table, fasttime,
                   Hmisc, gridExtra, rgdal, ggmap, Cairo, rgeos, maptools, 
                   lubridate, plyr, gdata, stringr, tidyr, dtplyr, knitr,grid)
    
    wd = "//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Analysis_scripts"
    setwd(wd)
    source("source_scripts/plot_parameters.R")
    source("source_scripts/ALL_price_flow_plots-fold.R")
    
    # ----------------------------------------------------------------------- |
    # Inputs ----
    # ----------------------------------------------------------------------- |
    
    xlim = c(-7000,7000)
    ylim = c(-100,100)
    
    # single-operator seams solution directory for price query
    solutions.dir <- "//nrelqnap02/PLEXOS CEII/Projects/Interconnections_Seam_Plexos/Continental/geodecomp_compare/SO/old results"
    
    # interface and area map
    SEAMS.interfaces <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                  "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/EI_interfaces.csv")
    
    SEAMS.regions <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                               "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/SEAMS_regions.csv")
    
    border.tx.buses.from <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                      "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/border_transformer_nodes_from.csv")
    
    border.tx.buses.to <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                    "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/border_transformer_nodes_to.csv")
    
    node.voltage <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                              "/RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/node_voltage.csv")
    
    node.key <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                             "/RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/node_region.csv")
    
    # ----------------------------------------------------------------------- |
    # Region region key ----
    # ----------------------------------------------------------------------- |
    
    ISO.Region.key = fread(SEAMS.regions)
    
    # ----------------------------------------------------------------------- |
    # Plot inputs ----
    # ----------------------------------------------------------------------- |
    
    SEAMS.interfaces = fread(SEAMS.interfaces)
    interfaces.to.plot = SEAMS.interfaces[,Interface]
    ISO.From = SEAMS.interfaces[,ISO.From]
    ISO.To = SEAMS.interfaces[,ISO.To]
    color.code = SEAMS.interfaces[,color]
    names(color.code) = interfaces.to.plot
    
    border.tx.buses.from = fread(border.tx.buses.from)[,Node.From:=tstrsplit(Node.From,'_')[[1]]]
    border.tx.buses.to = fread(border.tx.buses.to)[,Node.To:=tstrsplit(Node.To,'_')[[1]]]
    
    # N/A voltage nodes are all western interconnection
    node.voltage = fread(node.voltage)[!(Voltage == "#N/A"),.(Node,Voltage = as.numeric(Voltage))]
    node.voltage[,Node:=tstrsplit(Node,"_")[[1]]]
    
    node.key = fread(node.key)[,.(Node = `Parent Name`,Region = `Child Name`)]
    
    # ----------------------------------------------------------------------- |
    # Setup queries ----
    # ----------------------------------------------------------------------- |
    
    setwd(solutions.dir)
    
    solution.dbs = list.files(recursive = TRUE)[grepl('-rplexos.db',
                                                      list.files(recursive = TRUE))]
    solution.dbs = solution.dbs[grepl('so_',solution.dbs)]
    
    scenario.names = gsub(" Solution-rplexos.db","",solution.dbs)
    # scenario.names = tstrsplit(scenario.names,"_single_operator_")[[2]]
    # scenario.names = gsub(" Solution/Model 2024","",scenario.names)
    print(scenario.names)
    
    # ----------------------------------------------------------------------- |
    # Queries ----
    # ----------------------------------------------------------------------- |
    
    # mechanism to remove look-ahead from queries
    model.timesteps = data.table()
    for (i in 1:length(scenario.names)) {
        model.timesteps = rbind(model.timesteps, 
                                data.table(tbl(src_sqlite(solution.dbs[i]),
                                               sql("SELECT phase_id, min(time) start, max(time) end, count(time) count FROM 
                                                   time GROUP BY phase_id")) %>% 
                                               filter(phase_id == 4) %>% 
                                               select(start, end, count) %>% 
                                               collect(n = Inf) ))
    }
    
    # grab end dates for each partition - needed for regional DA queries
    model.starts = parse_date_time(model.timesteps$start, orders = "ymd H:M:S")
    model.ends = parse_date_time(model.timesteps$end, orders = "ymd H:M:S")
    model.starts = model.starts + 3600*24
    model.ends = model.ends - 3600*48 # chop off look-ahead and last day
    
    print(paste0('model starts - ',model.starts))
    print(paste0('model ends - ',model.ends))

   # 1. line names
    
    # assumes lines do not change between solution.dbs
    line.names <- data.table()
    for(i in 1:1){
    
      line.names <- unique(rbind(line.names,
                                  unique(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                  sql("SELECT name 
                                  FROM Line_Flow 
                                  WHERE collection IS 'Line' AND 
                                  property IS 'Flow' AND
                                  phase_id IS 4")) %>% 
                                  collect(n = Inf)))))
    }
    
    
    # ----------------------------------------------------------------------- |
    # Price ----
    # ----------------------------------------------------------------------- |
    
    line.names[,Node.From:=tstrsplit(name,'_')[[1]]]
    line.names[,Node.To:=tstrsplit(name,'_')[[2]]]
    
    line.names = merge(line.names,
                            node.voltage[,.(Node.From = Node,Voltage.From = Voltage)],
                            by = c('Node.From'))
    
    line.names = merge(line.names,
                            node.voltage[,.(Node.To = Node,Voltage.To = Voltage)],
                            by = c('Node.To'))
    
    # ASSUMPTION - node category names are equivalent to region names
    node.key = merge(node.key,ISO.Region.key,by = c('Region'))
    node.key[,Node.Name := Node]
    node.key[,Node:=tstrsplit(Node.Name,'_')[[1]]]
    
    interface.lines = merge(line.names,
                         node.key[,.(Node.From = Node,Node.Name.From = Node.Name,ISO.From = ISO)],
                         by = c('Node.From'))
    
    interface.lines = merge(interface.lines,
                         node.key[,.(Node.To = Node,Node.Name.To = Node.Name,ISO.To = ISO)],
                         by = c('Node.To'))
    
    interface.lines[,Node.From := Node.Name.From]
    interface.lines[,Node.To := Node.Name.To]
    interface.lines[,c('Node.Name.From','Node.Name.To'):=NULL]
    
    interface.lines = interface.lines[!(ISO.From == ISO.To)]
    
    interface.lines = merge(interface.lines,SEAMS.interfaces,by = c('ISO.From','ISO.To'),
                         all.x = TRUE)
    
    interface.lines[is.na(Interface),c('ISO.From','ISO.To'):=.(ISO.To,ISO.From)]
    interface.lines[is.na(Interface),c('Node.From','Node.To'):=.(Node.To,Node.From)]
    interface.lines[is.na(Interface),c('Voltage.From','Voltage.To'):=.(Voltage.To,Voltage.From)]
    interface.lines[,c("Interface","color"):=NULL]
    interface.lines = merge(interface.lines,SEAMS.interfaces,by = c('ISO.From','ISO.To'),
                         all.x = TRUE)
    
    interface.lines[,color:=NULL]
    setnames(interface.lines,'name','Line')
    
    return(list(interface.lines = interface.lines,
                node.names = c(unique(interface.lines[,Node.From]),unique(interface.lines[,Node.To])),
                line.names = unique(interface.lines[,Line])))
    
}