
SINGLE_OPT_BORDER_price_flow_plots = function(voltage.threshold = 220){

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
    ylim = c(-125,125)
    
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
                                        collect(n = Inf)),
                          solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i] & time >= model.starts[i]])
    }
    
    # copy table and get node number
    int.node.price = copy(interval.node.price)
    node.table = unique(int.node.price[,.(name)])
    node.table[,Node:=tstrsplit(name,'_')[[1]]]
    int.node.price = merge(int.node.price,node.table,by = c('name'))
    rm(node.table)
    
    # 2. interval Region.Region net interchange
    
    interval.region.NI <- data.table()
    for(i in 1:length(scenario.names)){
      print(i)
      interval.region.NI <- rbind(interval.region.NI,
                                  expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                  sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                  FROM RegionRegions_NetInterchange 
                                  WHERE collection IS 'Region.Regions' AND 
                                  property IS 'Net Interchange' AND
                                  phase_id IS 4")) %>% 
                                  dplyr::mutate(scenario = scenario.names[i]) %>% 
                                  collect(n = Inf)),
                          solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i] & time >= model.starts[i]])
    }
    
    # copy table
    int.region.NI = copy(interval.region.NI)
    
    # 3. line names
    
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
    
    # ASSUMPTION - node category names are equivalent to region names
    node.key = unique(int.node.price[,.(node = Node,Region = category)])
    node.key = merge(node.key,ISO.Region.key,by = c('Region'))
    
    border.buses = merge(line.names,
                         node.key[,.(Node.From = node,ISO.From = ISO)],
                         by = c('Node.From'))
    
    border.buses = merge(border.buses,
                         node.key[,.(Node.To = node,ISO.To = ISO)],
                         by = c('Node.To'))
    
    border.buses = border.buses[!(ISO.From == ISO.To)]
    
    border.buses = merge(border.buses,SEAMS.interfaces,by = c('ISO.From','ISO.To'),
                         all.x = TRUE)
    
    border.buses[is.na(Interface),c('ISO.From','ISO.To'):=.(ISO.To,ISO.From)]
    border.buses[is.na(Interface),c('Node.From','Node.To'):=.(Node.To,Node.From)]
    border.buses[,c("Interface","color"):=NULL]
    border.buses = merge(border.buses,SEAMS.interfaces,by = c('ISO.From','ISO.To'),
                         all.x = TRUE)
    
    border.buses.from = unique(border.buses[,.(ISO.From,Node.From,Interface)])
    border.buses.to = unique(border.buses[,.(ISO.To,Node.To,Interface)])
    border.buses.from = rbind(border.buses.from,border.tx.buses.from)
    border.buses.to = rbind(border.buses.to,border.tx.buses.to)
    
    border.buses.from = merge(border.buses.from,node.voltage[,.(Node.From = Node,Voltage)],
                              by = c('Node.From'))
    border.buses.to = merge(border.buses.to,node.voltage[,.(Node.To = Node,Voltage)],
                            by = c('Node.To'))
    border.buses.from = unique(border.buses.from[Voltage >= voltage.threshold])
    border.buses.to = unique(border.buses.to[Voltage >= voltage.threshold])
    
    int.region.from.price = merge(unique(border.buses.from),
                                  int.node.price[,.(Node.From = Node,time,Price.From = value)],
                                  by = c('Node.From'),
                                  allow.cartesian = TRUE)
    
    int.region.to.price = merge(unique(border.buses.to),
                                int.node.price[,.(Node.To = Node,time,Price.To = value)],
                                by = c('Node.To'))
    
    int.region.from.price = int.region.from.price[,lapply(.SD,mean),by = c('Interface','time'),
                                                  .SDcols = c('Price.From')]
    
    int.region.to.price = int.region.to.price[,lapply(.SD,mean),by = c('Interface','time'),
                                              .SDcols = c('Price.To')]
    
    int.region.price = merge(int.region.from.price,int.region.to.price,
                             by = c('Interface','time'))
    
    int.region.price[,value:=Price.To - Price.From]
    int.region.price[,pct:=abs(Price.To - Price.From)/(Price.To/2 + Price.From/2)]
    
    int.region.price = data.table(melt(int.region.price,id.vars = c('Interface','time'),
                           measure.vars = c('value','pct')))
    int.region.price[,Interface:=paste0(variable,'_',Interface)]
    int.region.price = data.table(dcast(int.region.price,time ~ Interface,value.var = c('value')))
 
    names(int.region.price) = gsub('value_','',names(int.region.price))
    
    # percentages
    int.region.price.pct = int.region.price[,.SD,.SDcols = c(paste0("pct_",interfaces.to.plot),'time')]
    names(int.region.price.pct) = sub("pct_","",names(int.region.price.pct))
    int.region.price.pct = data.table(melt(int.region.price.pct,id.vars = c('time'),
                                           measure.vars = interfaces.to.plot))
    setnames(int.region.price.pct,c('variable','value'),c('Interface','pct'))
    
    int.region.price = int.region.price[,.SD,.SDcols = c(interfaces.to.plot,'time')]
    int.region.price[,var:='Price']
    
    month.day.price = unique(int.region.price[,.(month = month(time),day = day(time))])
    # change from time to period
    time.key = data.table(time = sort(unique(int.region.price[,time])),
                          Period = seq(length(unique(int.region.price[,time]))))
    int.region.price = merge(int.region.price,time.key,by = c('time'))
    int.region.price[,time:=NULL]
    int.region.price.pct = merge(int.region.price.pct,time.key,by = c('time'))
    int.region.price.pct[,time:=NULL]
    
    # ----------------------------------------------------------------------- |
    # Flow ----
    # ----------------------------------------------------------------------- |
    
    setnames(int.region.NI,c('parent','name'),c('Region.From','Region.To'))
    int.region.NI = merge(int.region.NI,
                          ISO.Region.key[,.(Region.From = Region,ISO.From = ISO)],
                          by = 'Region.From')
    
    int.region.NI = merge(int.region.NI,
                          ISO.Region.key[,.(Region.To = Region,ISO.To = ISO)],
                          by = 'Region.To')
    
    int.region.NI = int.region.NI[,lapply(.SD,sum),by = c('ISO.From','ISO.To','time'),.SDcols = 'value']
    
    int.region.NI[,Interface:=paste0(ISO.From," - ",ISO.To)]
    int.region.NI = merge(int.region.NI,SEAMS.interfaces,by = c('ISO.From','ISO.To','Interface'))
    int.region.NI = data.table(dcast(int.region.NI,time ~ Interface,value.var = 'value'))
    int.region.NI[,var:= "Interchange"]
    
    month.day.NI = unique(int.region.NI[,.(month = month(time),day = day(time))])
    
    int.region.NI = merge(int.region.NI,time.key,by = c('time'))
    int.region.NI[,time:=NULL]
    
    int.region.NI = int.region.NI[,.SD,.SDcols = names(int.region.price)]
    
    # ----------------------------------------------------------------------- |
    # Combine ----
    # ----------------------------------------------------------------------- |
    
    int.comparison = rbind(int.region.NI,int.region.price)
    
    int.comparison = data.table(melt(int.comparison, id.vars = c("Period","var"),
                                     measure.vars = interfaces.to.plot))
    
    setnames(int.comparison,c('var','variable'),c('variable','Interface'))
    
    int.comparison = data.table(dcast(int.comparison,Period + Interface ~ variable,value.var = 'value'))
    
    int.comparison = merge(int.comparison,int.region.price.pct,by = c('Period','Interface'))
    
    # ----------------------------------------------------------------------- |
    # Plot ----
    # ----------------------------------------------------------------------- |
    
    options(scipen = 9999)
    
    # A. outlier data
    outlier.data = int.comparison[Interchange < xlim[1] |
                                  Interchange > xlim[2] |
                                  Price < ylim[1] |
                                  Price > ylim[2]]
    
    # B. statistics
    int.stat = copy(int.comparison)
    
    int.stat[,Price:=abs(Price)]
    
    # center of mass
    int.stat.center = int.stat[,lapply(.SD,mean),by = c('Interface'),
                               .SDcols = c('Interchange','Price')]
    
    # squared distance from center
    int.stat.squared = copy(int.stat)
    
    int.stat.squared[,`Interchange` :=(`Interchange` - mean(`Interchange`))^2]
    int.stat.squared[,`Price` :=(`Price` - mean(`Price`))^2]
    
    int.stat.squared = int.stat.squared[,lapply(.SD,mean),by = c('Interface'),
                                        .SDcols = c('Interchange','Price')]
    
    # regression
    trend.line = data.table(Interface = interfaces.to.plot)
    
    for(jj in seq(length(interfaces.to.plot))){
      
      trend.line[Interface == interfaces.to.plot[jj],
                 c('intercept','slope'):=as.list(coef(lm(Price~Interchange,data = int.comparison[Interface == interfaces.to.plot[jj]])))]
      
    }
    
    trend.endpoints = copy(int.comparison)
    trend.endpoints[,x:=min(Interchange),by = c('Interface')]
    trend.endpoints[,xend:=max(Interchange),by = c('Interface')]
    trend.endpoints = unique(trend.endpoints[,.(Interface,x,xend)])
    trend.endpoints[,x:=x+xlim[1]/8]
    trend.endpoints[,xend:=xend+xlim[2]/8]
    
    trend.line = merge(trend.line,trend.endpoints,by = c('Interface'))
    trend.line[,y:=intercept + slope * x]
    trend.line[,yend:=intercept + slope * xend]
    
    rm(trend.endpoints)
    
    # C. price-flow plot
    plot.text = data.table(Interface = "PJM - MISO",
                           x = c(4000,-4000,-4000,4000),
                           y = c(70,70,-70,-70),
                           text = rep(c('Agreement','Disagreement'),2))
    
    int.comparison$Interface = factor(int.comparison$Interface,levels = interfaces.to.plot)
    plot.text$Interface = factor(plot.text$Interface,levels = interfaces.to.plot)
    
    p1 <- ggplot() + stat_binhex(data = int.comparison,
                                 aes(x = Interchange,y = Price,color = ..density..*100,fill = ..density..*100),
                                 binwidth = c(200,3)) + 
        scale_fill_gradientn(name = "(%)",colours = c('gray80','gray20','black'),
                             guide = 'colourbar') + 
        scale_color_gradientn(name = "(%)",colours = c('gray80','gray20','black'),
                              guide = 'colourbar') + 
        geom_label(data = plot.text,aes(x = x,y = y,label = text),color = 'black',fill = 'wheat2',size = 2.4) + 
        geom_vline(xintercept = 0,size = 0.01,color = 'black') +
        geom_hline(yintercept = 0,size = 0.01,color = 'black') +
        # geom_segment(data = trend.line,aes(x = x,xend = xend,y = y,yend = yend),color = 'firebrick',linetype = 1,size = 1) + 
        facet_grid(Interface ~ .) + plot_theme + theme(legend.title = element_text()) + 
        labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
        coord_cartesian(xlim = xlim,ylim = ylim) +
        theme(strip.background = element_rect(fill = "wheat2"))
    
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
    
    quadrants[,Disagreement := II + IV]
    quadrants[,Agreement := I + III]
    
    quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface'),
                          .SDcols = c('quad.0','I','II','III','IV','Disagreement','Agreement')]
    
    # write quadrants csv and plot
    setwd(wd)
    # write.csv(quadrants,paste0('plots/EI_single-opt-border_efficiencies_',tag,'.csv'),row.names = FALSE)
    # ggsave(paste0('plots/EI_single-opt-border_hex_',tag,'.png'),p,height = 5.5,width = 3.5)
    # write.csv(outlier.data,paste0('plots/EI_single-opt-border_outliers',tag,'.csv'),row.names = FALSE)
    output.list = list(plot.data = int.comparison,p1 = p1,p2 = p2,outlier.data = outlier.data,quadrants = quadrants,
                       border.buses.from = border.buses.from,border.buses.to = border.buses.to,
                       stat.center = int.stat.center,stat.squared = int.stat.squared,trend.line = trend.line)
    return(output.list)
    
}