
SINGLE_OPT_LINE_price_flow_plots = function(voltage.threshold = 0){

    pacman::p_load(rplexos, RSQLite, magrittr, dplyr, lubridate, 
                   rmarkdown, scales, cowplot, data.table, fasttime,
                   Hmisc, gridExtra, rgdal, ggmap, Cairo, rgeos, maptools, 
                   lubridate, plyr, gdata, stringr, tidyr, dtplyr, knitr,grid)
    
    wd = "//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Analysis_scripts"
    setwd(wd)
    source("source_scripts/plot_parameters.R")
    source("source_scripts/ALL_price_flow_plots-fold.R")
    source("source_scripts/extract_interface_lines.R")
    
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
    
    # remember tfmr bus omission
    
    node.voltage <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                              "/RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/node_voltage.csv")
    
    # ----------------------------------------------------------------------- |
    # Region region key ----
    # ----------------------------------------------------------------------- |
    
    ISO.Region.key = fread(SEAMS.regions)
    
    interface.outputs = extract_interface_lines()
    interface.lines = interface.outputs[["interface.lines"]]
    node.names = interface.outputs[["node.names"]]
    line.names = interface.outputs[["line.names"]]
    
    # ----------------------------------------------------------------------- |
    # Plot inputs ----
    # ----------------------------------------------------------------------- |
    
    SEAMS.interfaces = fread(SEAMS.interfaces)
    interfaces.to.plot = SEAMS.interfaces[,Interface]
    ISO.From = SEAMS.interfaces[,ISO.From]
    ISO.To = SEAMS.interfaces[,ISO.To]
    color.code = SEAMS.interfaces[,color]
    names(color.code) = interfaces.to.plot
    
    # N/A voltage nodes are all western interconnection
    node.voltage = fread(node.voltage)[!(Voltage == "#N/A"),.(Node,Voltage = as.numeric(Voltage))]
    node.voltage[,Node:=tstrsplit(Node,"_")[[1]]]
    
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

    # 1. interval node price
    
    interval.node.price <- data.table()
    for(i in 1:length(scenario.names)){
        print(i)
        interval.node.price <- rbind(interval.node.price,
                                       expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                        sql("SELECT key, name, category, time_from, time_to, value, property
                                        FROM Node_Price 
                                        WHERE collection IS 'Node' AND 
                                        property IS 'Price' AND
                                        phase_id IS 4")) %>% 
                                        dplyr::mutate(scenario = scenario.names[i]) %>%
                                        filter(name %in% node.names) %>%
                                        collect(n = Inf)),
                          solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i] & time >= model.starts[i]])
    }
    
    int.node.price = copy(interval.node.price)

    # 2. interval line flow
    
    interval.line.flow <- data.table()
    for(i in 1:length(scenario.names)){
      print(i)
      interval.line.flow <- rbind(interval.line.flow,
                                  expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                  sql("SELECT key, name, category, time_from, time_to, value, property
                                  FROM Line_Flow 
                                  WHERE collection IS 'Line' AND 
                                  property IS 'Flow' AND
                                  phase_id IS 4")) %>% 
                                  dplyr::mutate(scenario = scenario.names[i]) %>%
                                  filter(name %in% line.names) %>%
                                  collect(n = Inf)),
                                  solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i] & time >= model.starts[i]])
    }
    
    int.line.flow = copy(interval.line.flow)
    
    # ----------------------------------------------------------------------- |
    # Flow ----
    # ----------------------------------------------------------------------- |
    
    int.comparison = merge(int.line.flow[,.(Line = name,time,Interchange = value)],
                           interface.lines,
                           by = c('Line'))
    
    # filter voltage - all 220 kV+ lines are included (have node price in report)
    int.comparison = int.comparison[Voltage.From >= voltage.threshold & Voltage.To >= voltage.threshold]
    
    # ----------------------------------------------------------------------- |
    # Price ----
    # ----------------------------------------------------------------------- |
    
    int.comparison = merge(int.comparison,
                           int.node.price[,.(Node.From = name,time,Price.From = value)],
                           by = c('Node.From','time'))
    
    int.comparison = merge(int.comparison,
                           int.node.price[,.(Node.To = name,time,Price.To = value)],
                           by = c('Node.To','time'))
    
    int.comparison[,Price:=Price.From - Price.To]
    int.comparison[,pct:=abs(Price.From - Price.To)/(Price.To/2 + Price.From/2)]
    

    
    # ----------------------------------------------------------------------- |
    # Plot ----
    # ----------------------------------------------------------------------- |
    
    options(scipen = 9999)
    
    # A. outlier data
    outlier.data = int.comparison[Interchange < xlim[1] |
                                  Interchange > xlim[2] |
                                  Price < ylim[1] |
                                  Price > ylim[2]]
    
    # B. ME table
    ME.table = int.comparison[abs(Price)<ylim[2]]
    ME.table = ME.table[,lapply(.SD, function(x) mean(abs(x))),by = c('Interface'),
                        .SDcols = 'Price']
    
    ME.table.all = copy(int.comparison)
    ME.table.all = ME.table.all[,lapply(.SD, function(x) mean(abs(x))),by = c('Interface'),
                                .SDcols = 'Price']
    # C. price-flow plot
    plot.text = data.table(Interface = "PJM - MISO",
                           x = c(4000,-4000,-4000,4000),
                           y = c(70,70,-70,-70),
                           text = rep(c('Counter-intuitive','Under-utilized'),2))
    
    int.comparison$Interface = factor(int.comparison$Interface,levels = interfaces.to.plot)
    plot.text$Interface = factor(plot.text$Interface,levels = interfaces.to.plot)
    
    p1 <- ggplot() + stat_binhex(data = int.comparison,
                                 aes(x = Interchange,y = Price,color = ..density..*100,fill = ..density..*100),
                                 binwidth = c(200,3)) + 
        scale_fill_gradientn(name = "Pct.",colours = c('gray90','lightpink','darkred'),
                             guide = 'colourbar') + 
        scale_color_gradientn(name = "Pct.",colours = c('gray90','lightpink','darkred'),
                              guide = 'colourbar') + 
        geom_label(data = plot.text,aes(x = x,y = y,label = text),color = 'black',fill = 'lightblue',size = 2.4) + 
        geom_vline(xintercept = 0,size = 0.01,color = 'black') +
        geom_hline(yintercept = 0,size = 0.01,color = 'black') +
        facet_grid(Interface ~ .) + plot_theme + theme(legend.title = element_text()) + 
        labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
        coord_cartesian(xlim = xlim,ylim = ylim) 
    
    p2 <- folded_price_flow_plots(plot.data = copy(int.comparison),
                                  interfaces.to.plot = interfaces.to.plot)
    
    # D. quadrants
    quadrants = copy(int.comparison)
    
    quadrants[,c('quad.0','I','II','III','IV'):=0]
    quadrants[pct<0.01,quad.0:=1]
    quadrants[Price > 0 & Interchange > 0 & quad.0 == 0,I:=1]
    quadrants[Price >= 0 & Interchange <= 0 & quad.0 == 0,II:=1]
    quadrants[Price < 0 & Interchange < 0 & quad.0 == 0,III:=1]
    quadrants[Price <= 0 & Interchange >= 0 & quad.0 == 0,IV:=1]
    
    quadrants = quadrants[,lapply(.SD,mean),by = c('Interface'),
                          .SDcols = c('quad.0','I','II','III','IV')]
    
    quadrants[,Underutilized := II + IV]
    quadrants[,Counterintuitive := I + III]
    
    quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface'),
                          .SDcols = c('quad.0','I','II','III','IV','Underutilized','Counterintuitive')]
    
    # write quadrants csv and plot
    setwd(wd)
    # write.csv(quadrants,paste0('plots/EI_single-opt-border_efficiencies_',tag,'.csv'),row.names = FALSE)
    # ggsave(paste0('plots/EI_single-opt-border_hex_',tag,'.png'),p,height = 5.5,width = 3.5)
    # write.csv(outlier.data,paste0('plots/EI_single-opt-border_outliers',tag,'.csv'),row.names = FALSE)
    output.list = list(plot.data = int.comparison,p1 = p1,p2 = p2,outlier.data = outlier.data,quadrants = quadrants,
                       border.buses.from = border.buses.from,border.buses.to = border.buses.to,ME.table = ME.table,
                       ME.table.all = ME.table.all)
    return(output.list)
    
}