
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

# decomposed RTS-GMLC runs
decomposed.db <- c(file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                             "RTS_Data/FormattedData/PLEXOS/MIP_0.1/Model DAY_AHEAD_B1 Solution",
                             "Model DAY_AHEAD_B1 Solution-rplexos.db"),
                   file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                             "RTS_Data/FormattedData/PLEXOS/MIP_0.1/Model DAY_AHEAD_B2 Solution",
                             "Model DAY_AHEAD_B2 Solution-rplexos.db"),
                   file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                             "RTS_Data/FormattedData/PLEXOS/MIP_0.1/Model DAY_AHEAD_B3 Solution",
                             "Model DAY_AHEAD_B3 Solution-rplexos.db"))

# an undecomposed RTS-GMLC run
nondecomposed.db <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                              "/RTS_Data/FormattedData/PLEXOS/MIP_0.1/Model DAY_AHEAD Solution",
                              "Model DAY_AHEAD Solution-rplexos.db")

# matpower output with decomposed power flows
decomposed.flows <- file.path("//nrelqnap01d/PLEXOS/Projects",
                              "GMLC-MSPCM/geo-decomp-matpower-solns",
                              "flow_decomposed.csv")
nondecomposed.flows <- file.path("//nrelqnap01d/PLEXOS/Projects",
                              "GMLC-MSPCM/geo-decomp-matpower-solns",
                              "flow_non-decomposed.csv")

network.table <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data",
                        "SourceData/branch.csv")

dc.network.table <- data.table(name = "113_316_1",Node.From = "113",Node.To = "316")

# ----------------------------------------------------------------------- |
# Setup queries ----
# ----------------------------------------------------------------------- |

# scenario.names
decomposed.names = c('1','2','3')
nondecomposed.names = c('Non-decomposed')

# scenario.order
scenario.order = c('Decomposed','Non-decomposed')

decomposed.flows = fread(decomposed.flows)
nondecomposed.flows = fread(nondecomposed.flows)

network.table = fread(network.table)[,.(name = `UID`,Node.From = `From Bus`,Node.To = `To Bus`)]
# network.table = rbind(network.table,dc.network.table)

# ----------------------------------------------------------------------- |
# Queries ----
# ----------------------------------------------------------------------- |

# 1. interval node price
interval.nondecomposed.price <- data.table()

for(i in 1:length(nondecomposed.names)){
  print(i)
  interval.nondecomposed.price <- rbind(interval.nondecomposed.price,
                              expand_time(data.table(tbl(src_sqlite(nondecomposed.db[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Node_Price 
                                WHERE collection IS 'Node' AND 
                                property IS 'Price' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = nondecomposed.names[i]) %>% 
                                collect(n = Inf)),nondecomposed.db[i]))
}

int.nondecomposed.price = copy(interval.nondecomposed.price)

# 2. interval decomposed price
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

# 3. line flow limits
line.flow.limits <- data.table()
for(i in 1:length(nondecomposed.names)){
  print(i)
  line.flow.limits<- rbind(line.flow.limits,
                                expand_time(data.table(tbl(src_sqlite(nondecomposed.db[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Line_ExportLimit 
                                WHERE collection IS 'Line' AND 
                                property IS 'Export Limit' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = nondecomposed.names[i]) %>% 
                                collect(n = Inf)),nondecomposed.db[i]))
  
  line.flow.limits<- rbind(line.flow.limits,
                                expand_time(data.table(tbl(src_sqlite(nondecomposed.db[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Line_ImportLimit 
                                WHERE collection IS 'Line' AND 
                                property IS 'Import Limit' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = nondecomposed.names[i]) %>% 
                                collect(n = Inf)),nondecomposed.db[i]))
}

# assumes that line flow limits do not vary between scenarios
line.flow.limits = unique(line.flow.limits[,.(name,property,category,value)])


# 5. period table

period.table = data.table(time = sort(unique(interval.nondecomposed.price[,time])),
                                 Period = seq(length(unique(interval.nondecomposed.price[,time]))))

period.table = period.table[Period <= 8760]

# ----------------------------------------------------------------------- |
# Reformat matpower flows ----
# ----------------------------------------------------------------------- |


# decomposed flow
names(decomposed.flows) = paste0(decomposed.flows[1,],"_",decomposed.flows[2,],"_",seq(length(decomposed.flows[1,])))
setnames(decomposed.flows,names(decomposed.flows)[grepl("NaN",names(decomposed.flows))],"Period")
decomposed.flows = decomposed.flows[!(is.nan(Period))]

decomposed.flows = data.table(melt(decomposed.flows,id.vars = "Period",
                            measure.vars = names(decomposed.flows)[!grepl("Period",names(decomposed.flows))]))
decomposed.flows[,c("Node.From","Node.To"):=tstrsplit(as.character(variable),"_")[1:2]]
decomposed.flows[,scenario:="Decomposed"]
decomposed.flows[,variable:=NULL]

# nondecomposed flow
names(nondecomposed.flows) = paste0(nondecomposed.flows[1,],"_",nondecomposed.flows[2,],"_",seq(length(nondecomposed.flows[1,])))
setnames(nondecomposed.flows,names(nondecomposed.flows)[grepl("NaN",names(nondecomposed.flows))],"Period")
nondecomposed.flows = nondecomposed.flows[!(is.nan(Period))]

nondecomposed.flows = data.table(melt(nondecomposed.flows,id.vars = "Period",
                                       measure.vars = names(nondecomposed.flows)[!grepl("Period",names(nondecomposed.flows))]))
nondecomposed.flows[,c("Node.From","Node.To"):=tstrsplit(as.character(variable),"_")[1:2]]
nondecomposed.flows[,scenario:="Non-decomposed"]
nondecomposed.flows[,variable:=NULL]

flow.data = rbind(decomposed.flows,nondecomposed.flows)

# ----------------------------------------------------------------------- |
# Check overloading ----
# ----------------------------------------------------------------------- |

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

# ----------------------------------------------------------------------- |
# Prepare to plot ----
# ----------------------------------------------------------------------- |

# flow
flow.data[,Region.From:=substr(Node.From,1,1)]
flow.data[,Region.To:=substr(Node.To,1,1)]
flow.data = flow.data[!(Region.From==Region.To)]
flow.data[,Interface:=paste0(pmin(Region.From,Region.To)," - ",pmax(Region.From,Region.To))]

border.buses = unique(flow.data[,.(Node.From,Node.To,Region.From,Region.To,Interface)])

flow.data[Region.From>Region.To,value:=(-1)*value]
flow.data = flow.data[,lapply(.SD,sum),by = c('Interface','Period','scenario'),.SDcols = 'value']
setnames(flow.data,'value','Flow')

# price
border.buses = rbind(border.buses[,.(Node = Node.From,Region = Region.From,Interface)],
                     border.buses[,.(Node = Node.To,Region = Region.To,Interface)])

price.data = rbind(int.nondecomposed.price[,.(scenario,Node = name,time,value)],
                  int.decomposed.price[,.(scenario = 'Decomposed',Node = name,time,value)])

price.data = merge(price.data,border.buses,
                  by = 'Node')

price.data = price.data[,lapply(.SD,mean),by = c('scenario','Region','Interface','time'),
                                .SDcols = 'value']

price.data = merge(price.data,period.table,
                      by = 'time')

price.data[tstrsplit(Interface," - ")[1][[1]] == Region,Region:= "From"]
price.data[tstrsplit(Interface," - ")[2][[1]] == Region,Region:= "To"]

price.data = data.table(dcast(price.data,scenario + Interface + Period ~ Region,value.var = 'value'))
price.data[,Price:=From - To]
price.data[,pct:=abs(Price)/((From + To)/2)]

compare.table = merge(price.data,flow.data,by = c('Interface','Period','scenario'))
compare.table[is.nan(pct),pct:=0]

# ----------------------------------------------------------------------- |
# Plot ----
# ----------------------------------------------------------------------- |

# RTS efficiencies plot only considers intervals where price differences is less than 100

color.code.1 = c("1 - 2" = "navyblue","1 - 3" = "forestgreen","2 - 3" = "firebrick")
compare.table$scenario = factor(compare.table$scenario,levels = scenario.order)

# RMSE table
MSE.table = compare.table[abs(Price) < 100]
MSE.table = MSE.table[,lapply(.SD, function(x) sum(x^2)),by = c('scenario'),
                        .SDcols = 'Price']

p1 <- ggplot() + geom_point(data = compare.table[abs(Price) < 100],
                           aes(x = Flow,y = Price,color = Interface),size = 0.3) + 
  scale_color_manual(values = color.code.1) + 
  geom_vline(xintercept = 0,size = 0.3,color = 'black') +
  geom_hline(yintercept = 0,size = 0.3,color = 'black') +
  facet_grid(scenario~Interface) + plot_theme + labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
  theme(legend.position = 'none')

compare.table[,c('quad.0','quad.1','quad.2','quad.3','quad.4'):=0]
compare.table[pct<0.01,quad.0:=1]
compare.table[Price > 0 & Flow > 0 & quad.0 == 0,quad.1:=1]
compare.table[Price >= 0 & Flow <= 0 & quad.0 == 0,quad.2:=1]
compare.table[Price < 0 & Flow < 0 & quad.0 == 0,quad.3:=1]
compare.table[Price <= 0 & Flow >= 0 & quad.0 == 0,quad.4:=1]

quadrants = compare.table[,lapply(.SD,mean),by = c('Interface','scenario'),
                           .SDcols = c('quad.0','quad.1','quad.2','quad.3','quad.4')]

quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface','scenario'),
                      .SDcols = c('quad.0','quad.1','quad.2','quad.3','quad.4')]

quadrants = data.table(melt(quadrants,id.vars = c('Interface','scenario')))
quadrants[variable == 'quad.0',x:= 0]
quadrants[variable == 'quad.0',y:= 85]
quadrants[variable == 'quad.1',x:=250]
quadrants[variable == 'quad.1',y:=75]
quadrants[variable == 'quad.2',x:=-325]
quadrants[variable == 'quad.2',y:=75]
quadrants[variable == 'quad.3',x:=-325]
quadrants[variable == 'quad.3',y:=-75]
quadrants[variable == 'quad.4',x:=250]
quadrants[variable == 'quad.4',y:=-75]
quadrants[variable %in% c('quad.1','quad.3'),efficient:="no"]
quadrants[variable %in% c('quad.2','quad.4'),efficient:="somewhat"]
quadrants[variable %in% c('quad.0'),efficient:="yes"]

p1 <- p1 + geom_label(data = quadrants,aes(x = x,y = y,label = value,fill = efficient),
                    color = 'black',size = 3) +
  scale_fill_manual(values = c('no' = 'gray80','somewhat' = 'skyblue3','yes' = 'goldenrod')) + 
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


