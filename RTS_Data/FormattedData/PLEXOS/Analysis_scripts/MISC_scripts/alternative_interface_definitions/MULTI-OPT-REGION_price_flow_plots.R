
MULTI_OPT_REGION_price_flow_plots = function(){

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
    solutions.dir <- "//nrelqnap02/PLEXOS CEII/Projects/Interconnections_Seam_Plexos/Continental/geodecomp_compare/stage_C/old results"
    
    # interface and area map
    SEAMS.interfaces <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                  "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/EI_interfaces.csv")
    
    SEAMS.regions <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                               "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/SEAMS_regions.csv")
    
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
    
    # ----------------------------------------------------------------------- |
    # Setup queries ----
    # ----------------------------------------------------------------------- |
    
    setwd(solutions.dir)
    
    solution.dbs = list.files(recursive = TRUE)[grepl('-rplexos.db',
                                                      list.files(recursive = TRUE))]
    solution.dbs = solution.dbs[grepl('c_2024',solution.dbs)]
    
    scenario.names = gsub(" Solution-rplexos.db","",solution.dbs)
    # scenario.names = tstrsplit(scenario.names,"_single_operator_")[[2]]
    # scenario.names = gsub(" Solution/Model 2024","",scenario.names)
    
    # filter out missing single-opt partition - no longer necessary
    # solution.dbs = solution.dbs[!grepl('EI_25',solution.dbs)]
    # scenario.names = scenario.names[!grepl('EI_25',scenario.names)]
    # 
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
    model.ends = model.ends - 3600*24 # chop off last day
    
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
    
    int.node.price = merge(int.node.price,
                             ISO.Region.key,
                             by.x = 'category',by.y = 'Region')
    
    int.node.price = int.node.price[,lapply(.SD,mean),
                                    by = c('ISO','time'),
                                    .SDcols = c('value')]
    
    int.region.price = merge(SEAMS.interfaces,
                             int.node.price[,.(ISO.From = ISO,time,Price.From = value)],
                             by = c('ISO.From'))
                             
    int.region.price = merge(int.region.price,
                            int.node.price[,.(ISO.To = ISO,time,Price.To = value)],
                            by = c('ISO.To','time'))
                             
   
    int.region.price[,value:=Price.From - Price.To]
    int.region.price[,pct:=abs(Price.From - Price.To)/(Price.To/2 + Price.From/2)]
    
    int.region.price = data.table(dcast(int.region.price,time ~ Interface,value.var = c('value','pct')))
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
    # write.csv(quadrants,'plots/EI_multi-opt-border_efficiencies.csv',row.names = FALSE)
    # ggsave('plots/EI_multi-opt-border_hex.png',p,height = 5.5,width = 3.5)
    # write.csv(outlier.data,'plots/EI_multi-opt-border_outliers.csv',row.names = FALSE)
    output.list = list(plot.data = int.comparison,p1 = p1,p2 = p2,outlier.data = outlier.data,quadrants = quadrants,
                       ME.table = ME.table,
                       ME.table.all = ME.table.all)
    return(output.list)

}