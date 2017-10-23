
pacman::p_load(rplexos, RSQLite, magrittr, dplyr, lubridate, 
               rmarkdown, scales, cowplot, data.table, fasttime,
               Hmisc, gridExtra, rgdal, ggmap, Cairo, rgeos, maptools, 
               lubridate, plyr, gdata, stringr, tidyr, dtplyr, knitr,grid)

wd = dirname(parent.frame(2)$ofile)
setwd(wd)
source("source_scripts/plot_parameters.R")

# ----------------------------------------------------------------------- |
# Inputs ----
# ----------------------------------------------------------------------- |

# an undecomposed RTS-GMLC run
solution.db <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                        "/RTS_Data/FormattedData/PLEXOS/Model DAY_AHEAD Solution",
                        "Model DAY_AHEAD Solution-rplexos.db")

# decomposed RTS-GMLC runs
decomposed.db <- c(file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                             "RTS_Data/FormattedData/PLEXOS/Model DAY_AHEAD_B1 Solution",
                             "Model DAY_AHEAD_B1 Solution-rplexos.db"),
                   file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                             "RTS_Data/FormattedData/PLEXOS/Model DAY_AHEAD_B2 Solution",
                             "Model DAY_AHEAD_B2 Solution-rplexos.db"),
                   file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                             "RTS_Data/FormattedData/PLEXOS/Model DAY_AHEAD_B3 Solution",
                             "Model DAY_AHEAD_B3 Solution-rplexos.db"))

# matpower output with decomposed power flows
flows <- file.path("//nrelqnap01d/PLEXOS/Projects/GMLC-MSPCM/geo-decomp-matpower-solns",
                      "/geo-decomp-data/flows.csv")

network.table <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data",
                        "SourceData/branch.csv")

dc.network.table <- data.table(name = "113_316_1",Node.From = "113",Node.To = "316")

# ----------------------------------------------------------------------- |
# Setup queries ----
# ----------------------------------------------------------------------- |

# solution dbs & scenario.names
scenario.names = c('Non-decomposed')
decomposed.names = c('1','2','3')

flows = fread(flows)

network.table = fread(network.table)[,.(name = `UID`,Node.From = `From Bus`,Node.To = `To Bus`)]
# network.table = rbind(network.table,dc.network.table)

# ----------------------------------------------------------------------- |
# Queries ----
# ----------------------------------------------------------------------- |

# 1. interval line flow

interval.line.flow <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  interval.line.flow <- rbind(interval.line.flow,
                                expand_time(data.table(tbl(src_sqlite(solution.db[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Line_Flow 
                                WHERE collection IS 'Line' AND 
                                property IS 'Flow' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),solution.db[i]))
}

# 2. interval node price

interval.node.price <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  interval.node.price <- rbind(interval.node.price,
                              expand_time(data.table(tbl(src_sqlite(solution.db[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Node_Price 
                                WHERE collection IS 'Node' AND 
                                property IS 'Price' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),solution.db[i]))
}

int.node.price = copy(interval.node.price)

# 3. interval decomposed price
interval.decomposed.price <- data.table()
for(i in 1:length(decomposed.names)){
  print(i)
  interval.decomposed.price<- rbind(interval.decomposed.price,
                               expand_time(data.table(tbl(src_sqlite(decomposed.db[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Node_Price 
                                WHERE collection IS 'Node' AND 
                                property IS 'Price' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = decomposed.names[i]) %>% 
                                collect(n = Inf)),decomposed.db[i]))
}

interval.decomposed.price[,region:=substr(name,1,1)]
int.decomposed.price = interval.decomposed.price[region == scenario]

# 4. interval decomposed price
line.flow.limits <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  line.flow.limits<- rbind(line.flow.limits,
                                expand_time(data.table(tbl(src_sqlite(solution.db[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Line_ExportLimit 
                                WHERE collection IS 'Line' AND 
                                property IS 'Export Limit' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),solution.db[i]))
  
  line.flow.limits<- rbind(line.flow.limits,
                                expand_time(data.table(tbl(src_sqlite(solution.db[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Line_ImportLimit 
                                WHERE collection IS 'Line' AND 
                                property IS 'Import Limit' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),solution.db[i]))
}

# assumes that line flow limits do not vary between scenarios
line.flow.limits = unique(line.flow.limits[,.(name,property,category,value)])


# ----------------------------------------------------------------------- |
# Reformat matpower flows ----
# ----------------------------------------------------------------------- |

# flow
flow.data = copy(flows)
names(flow.data) = paste0(flow.data[1,],"_",flow.data[2,],"_",seq(length(flow.data[1,])))
setnames(flow.data,names(flow.data)[grepl("NaN",names(flow.data))],"Period")
flow.data = flow.data[!(is.nan(Period))]

flow.data = data.table(melt(flow.data,id.vars = "Period",
                            measure.vars = names(flow.data)[!grepl("Period",names(flow.data))]))
flow.data[,c("Node.From","Node.To"):=tstrsplit(as.character(variable),"_")[1:2]]
flow.data[,scenario:="Decomposed"]
flow.data[,variable:=NULL]

# check overloading ################
overloading.table = flow.data[,lapply(.SD,sum),by = c("Period","Node.From","Node.To","scenario"),
                              .SDcols = "value"]

limits.table = merge(line.flow.limits,
                    network.table[,.(name,Node.From = as.character(Node.From),
                                          Node.To = as.character(Node.To))],
                    by = 'name')

limits.table = limits.table[,lapply(.SD,sum),by = c('Node.From','Node.To','category','property'),
                            .SDcols = 'value']

overloading.table = merge(overloading.table,limits.table,
                          by = c('Node.From','Node.To'),allow.cartesian = TRUE)

setnames(overloading.table,c('value.x','value.y'),c('flow','limit'))
overloading.table = data.table(dcast(overloading.table,
                                     Period + Node.From + Node.To + category + scenario + flow ~ property,
                                     value.var = c('limit')))

overloading.table[,loading:=0]
overloading.table[flow > 0,loading:=100*(flow/`Export Limit`)]
overloading.table[flow < 0,loading:=100*(flow/`Import Limit`)]

overloading.table[,overloaded:=ifelse(loading>100.001,100,0)]
overloading.pct = overloading.table[,lapply(.SD,function(x) round(mean(x),2)),by = c('scenario'),
                                    .SDcols = 'overloaded']

# check overloading ################

int.line.flow = merge(interval.line.flow,
                      data.table(time = sort(unique(interval.line.flow[,time])),
                                 Period = seq(length(unique(interval.line.flow[,time])))),
                      by = 'time')

int.line.flow = merge(int.line.flow,network.table,by = 'name')
int.line.flow = int.line.flow[,.(Period,Node.From,Node.To,scenario,value)]

# hard code eliminate intervals beyond 8760
int.line.flow = int.line.flow[Period<=8760]
flow.data = flow.data[Period<=8760]

compare.flow = rbind(int.line.flow,flow.data)
compare.flow[,Region.From:=substr(Node.From,1,1)]
compare.flow[,Region.To:=substr(Node.To,1,1)]
compare.flow = compare.flow[!(Region.From==Region.To)]
compare.flow[,Interface:=paste0(pmin(Region.From,Region.To)," - ",pmax(Region.From,Region.To))]

border.buses = unique(compare.flow[,.(Node.From,Node.To,Region.From,Region.To,Interface)])

compare.flow[Region.From>Region.To,value:=(-1)*value]
compare.flow = compare.flow[,lapply(.SD,sum),by = c('Interface','Period','scenario'),.SDcols = 'value']
setnames(compare.flow,'value','Flow')


# price
# border.buses = unique(compare.flow[,.(Node.From,Node.To)])
# border.buses[,Region.From:=substr(Node.From,1,1)]
# border.buses[,Region.To:=substr(Node.To,1,1)]
# border.buses = border.buses[!(Region.From==Region.To)]
# border.buses[,Interface:=paste0(pmin(Region.From,Region.To)," - ",pmax(Region.From,Region.To))]

border.buses = rbind(border.buses[,.(Node = Node.From,Region = Region.From,Interface)],
                     border.buses[,.(Node = Node.To,Region = Region.To,Interface)])


int.node.price = merge(int.node.price[,.(Node = name,time,value)],
                       border.buses,by = 'Node')

int.decomposed.price = merge(int.decomposed.price[,.(Node = name,time,value)],
                       border.buses,by = 'Node')

int.node.price = int.node.price[,lapply(.SD,mean),by = c('Region','Interface','time'),
                                .SDcols = 'value']

int.decomposed.price = int.decomposed.price[,lapply(.SD,mean),by = c('Region','Interface','time'),
                                .SDcols = 'value']

int.node.price = merge(int.node.price,
                      data.table(time = sort(unique(int.node.price[,time])),
                                 Period = seq(length(unique(int.node.price[,time])))),
                      by = 'time')

int.decomposed.price = merge(int.decomposed.price,
                       data.table(time = sort(unique(int.decomposed.price[,time])),
                                  Period = seq(length(unique(int.decomposed.price[,time])))),
                       by = 'time')

# hard code eliminate intervals beyond 8760
int.node.price = int.node.price[Period<=8760]
int.decomposed.price = int.decomposed.price[Period<=8760]

int.node.price[tstrsplit(Interface," - ")[1][[1]] == Region,Region:= "From"]
int.node.price[tstrsplit(Interface," - ")[2][[1]] == Region,Region:= "To"]
int.decomposed.price[tstrsplit(Interface," - ")[1][[1]] == Region,Region:= "From"]
int.decomposed.price[tstrsplit(Interface," - ")[2][[1]] == Region,Region:= "To"]

int.node.price = data.table(dcast(int.node.price,Interface + Period ~ Region,value.var = 'value'))
int.node.price[,Price:=From - To]

int.decomposed.price = data.table(dcast(int.decomposed.price,Interface + Period ~ Region,value.var = 'value'))
int.decomposed.price[,Price:=From - To]

int.node.price[,scenario:='Non-decomposed']
int.decomposed.price[,scenario:='Decomposed']

compare.price = rbind(int.node.price[,.(Interface,Period,scenario,Price)],
                      int.decomposed.price[,.(Interface,Period,scenario,Price)])

compare.table = merge(compare.price,compare.flow,by = c('Interface','Period','scenario'))

# ----------------------------------------------------------------------- |
# Plot ----
# ----------------------------------------------------------------------- |

# RTS efficiencies plot only considers intervals where price differences is less than 100

color.code.1 = c("1 - 2" = "navyblue","1 - 3" = "forestgreen","2 - 3" = "firebrick")

p1 <- ggplot() + geom_point(data = compare.table[abs(Price) < 100],
                           aes(x = Flow,y = Price,color = Interface),size = 0.3) + 
  scale_color_manual(values = color.code.1) + 
  geom_vline(xintercept = 0,size = 0.3,color = 'black') +
  geom_hline(yintercept = 0,size = 0.3,color = 'black') +
  facet_grid(scenario~Interface) + plot_theme + labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
  theme(legend.position = 'none')

compare.table[,c('quad.1','quad.2','quad.3','quad.4'):=0]
compare.table[Price > 0 & Flow > 0,quad.1:=1]
compare.table[Price >= 0 & Flow <= 0,quad.2:=1]
compare.table[Price < 0 & Flow < 0,quad.3:=1]
compare.table[Price <= 0 & Flow >= 0,quad.4:=1]

quadrants = compare.table[,lapply(.SD,mean),by = c('Interface','scenario'),
                           .SDcols = c('quad.1','quad.2','quad.3','quad.4')]

quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface','scenario'),
                      .SDcols = c('quad.1','quad.2','quad.3','quad.4')]

quadrants = data.table(melt(quadrants,id.vars = c('Interface','scenario')))
quadrants[variable == 'quad.1',x:=400]
quadrants[variable == 'quad.1',y:=75]
quadrants[variable == 'quad.2',x:=-350]
quadrants[variable == 'quad.2',y:=75]
quadrants[variable == 'quad.3',x:=-350]
quadrants[variable == 'quad.3',y:=-75]
quadrants[variable == 'quad.4',x:=400]
quadrants[variable == 'quad.4',y:=-75]
quadrants[variable %in% c('quad.1','quad.3'),efficient:="no"]
quadrants[variable %in% c('quad.2','quad.4'),efficient:="yes"]

p1 <- p1 + geom_label(data = quadrants,aes(x = x,y = y,label = value,fill = efficient),
                    color = 'black',size = 3) +
  scale_fill_manual(values = c('no' = 'gray80','yes' = 'goldenrod')) + 
  theme(legend.position = 'none')


# plot of matpower line overloading 

overloading.table[,category:=gsub("AC_","Region ",category)]
overloading.table[,category:=gsub("_AC","",category)]

color.code.2 = c("Region 1" = "navyblue","Region 2" = "firebrick","Region 3" = "forestgreen",
                 "Interregion" = "goldenrod")

p2 <- ggplot() + geom_jitter(data = overloading.table,aes(x = scenario,y = loading, color = category),
                           alpha = 0.2,size = 0.3,width = 0.4) + 
  geom_label(data = overloading.pct,aes(x = scenario,
  y = 175,label = paste0(overloaded,"% chance that a line overloads in any given interval")),
             fill = 'gray80') +
  geom_hline(yintercept = 100,color = 'black',linetype = 2,size = 1.2) +
  scale_color_manual(values = color.code.2) + plot_theme + 
  guides(color= guide_legend(override.aes = list(size=1.5)))

setwd(wd)
ggsave('RTS_efficiencies.png',p1,height = 5.5,width = 6.5)
ggsave('RTS_overloading.png',p2,height = 5.5,width = 6.5)


