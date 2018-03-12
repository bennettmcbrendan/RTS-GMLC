
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

xlim = c(-7000,7000)
ylim = c(-200,200)

# single-operator seams solution directory for price query
solutions.dir <- "//nrelqnap01d/PLEXOS CEII/Projects/Interconnections_Seam_Plexos/bmcbenne"

# Matpower flow outputs
matpower.flows.file <- "//nrelqnap01d/PLEXOS/Projects/GMLC-MSPCM/SEAMS-data-nondecomposed/EI_SEAMS_flows.csv"

# Node key file
node.key.file <- "//nrelqnap01d/PLEXOS/Projects/GMLC-MSPCM/SEAMS-data-decomposed-matpower/bus_id_map.csv"

# interface and area map
SEAMS.interfaces <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                              "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/EI_interfaces.csv")

SEAMS.regions <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                           "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/SEAMS_regions.csv")

# node LPF map - 98280 nodes
node.LPFs <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                       "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/node_LPF.csv")

GPFs <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                  "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/generation_participation_factors.csv")

# ----------------------------------------------------------------------- |
# Region region key ----
# ----------------------------------------------------------------------- |

ISO.Region.key = fread(SEAMS.regions)

# ----------------------------------------------------------------------- |
# Decomposition key ----
# ----------------------------------------------------------------------- |

source('SEAMS_database/make_decomposition_key.R')
output = make_decomposition_key()

generator.nodes = output[['generator.nodes']]
node.regions = output[['node.regions']]

# ----------------------------------------------------------------------- |
# Node - Region map ----
# ----------------------------------------------------------------------- |

Node.key = copy(node.regions)[,.(Region = region, Node)]
Node.key = merge(Node.key,ISO.Region.key,by = c('Region'))
Node.key[,Node:=tstrsplit(Node,"_")[[1]]]

matpower.node.key = fread(node.key.file)
setnames(matpower.node.key,c('bus_i','nodenum'),c('Bus','Node'))
matpower.node.key = matpower.node.key[,lapply(.SD,function(x) as.character(x)),
                                      by = c(''),.SDcols = c('Node','Bus')]

Node.key = merge(Node.key,matpower.node.key,by = c('Node'),all.x = TRUE)

# ----------------------------------------------------------------------- |
# LPFs and GPFs ----
# ----------------------------------------------------------------------- |

node.LPFs = fread(node.LPFs)
node.LPFs[Scenario == "",Scenario:='DEFAULT'] # check other scenarios besides 'Sum LPFs'
node.LPFs = data.table(dcast(node.LPFs,Node + Timeslice ~ Scenario,value.var = "LPF")) # check numbers
setnames(node.LPFs,'Sum of LPFs equals one','LPF')
node.LPFs[is.na(LPF),LPF:=DEFAULT]
node.LPFs[,DEFAULT:=NULL]

node.LPFs[Timeslice == "",Timeslice:='DEFAULT']
node.LPFs = data.table(dcast(node.LPFs,Node ~ Timeslice,value.var = 'LPF'))
node.LPFs[is.na(`2024_Autumn_WI`),`2024_Autumn_WI`:=DEFAULT]
node.LPFs[is.na(`2024_Spring_WI`),`2024_Spring_WI`:=DEFAULT]
node.LPFs[is.na(`2024_Summer_WI`),`2024_Summer_WI`:=DEFAULT]
node.LPFs[is.na(`2024_Winter_WI`),`2024_Winter_WI`:=DEFAULT]

node.LPFs = data.table(melt(node.LPFs,id.vars = c('Node'),
                            measure.vars = c("2024_Autumn_WI","2024_Spring_WI","2024_Summer_WI","2024_Winter_WI")))
setnames(node.LPFs,c('variable','value'),c('Timeslice','LPF'))
node.LPFs = merge(node.LPFs,node.regions,by = 'Node')

GPFs = fread(GPFs)
GPFs[,count:=.N,by = 'Generator']
GPFs = GPFs[count > 1]
GPFs = GPFs[,.(name = Generator,Node,value)]

GPFs = merge(GPFs,generator.nodes[,.(name,Node)],by = c('name','Node'),all = TRUE)
GPFs[is.na(value),value:=1]
GPFs[name == 'PathfinderImaginaryCC_WI',value:=0.33333333] # exception


# ----------------------------------------------------------------------- |
# Read flows file ----
# ----------------------------------------------------------------------- |

if(TRUE){
    matpower.flows = fread(matpower.flows.file)
    power.flows = copy(matpower.flows)[1:674,]
}

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

scenario.names = gsub(" Solution-rplexos.db","",solution.dbs)
scenario.names = tstrsplit(scenario.names,"_single_operator_")[[2]]
scenario.names = gsub(" Solution/Model 2024","",scenario.names)

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
model.ends = parse_date_time(model.timesteps$end, orders = "ymd H:M:S")
model.ends = model.ends - 3600*24

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
                                    collect(n = Inf)),solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i]])
}

# time decomposition - hard code
int.node.price = copy(interval.node.price)
int.node.price = int.node.price[!(day(time) %in% c(10,11))]

# 2. interval generator pumping

interval.gen.pump <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  interval.gen.pump <- rbind(interval.gen.pump,
                               expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                    sql("SELECT key, name, category, time_from, time_to, value, property, region 
                                    FROM Generator_PumpLoad 
                                    WHERE collection IS 'Generator' AND 
                                    property IS 'Pump Load' AND
                                    phase_id IS 4")) %>% 
                                    dplyr::mutate(scenario = scenario.names[i]) %>% 
                                    collect(n = Inf)),solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i]])
}

# time decomposition - hard code
int.gen.pump = copy(interval.gen.pump)
int.gen.pump = int.gen.pump[!(day(time) %in% c(10,11))]

interval.region.load <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  interval.region.load <- rbind(interval.region.load,
                             expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                    sql("SELECT key, name, category, time_from, time_to, value, property 
                                    FROM Region_Load 
                                    WHERE collection IS 'Region' AND 
                                    property IS 'Load' AND
                                    phase_id IS 4")) %>% 
                                    dplyr::mutate(scenario = scenario.names[i]) %>% 
                                    collect(n = Inf)),solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i]])
}

# time decomposition - hard code
int.region.load = copy(interval.region.load)
int.region.load = int.region.load[!(day(time) %in% c(10,11))]


# ----------------------------------------------------------------------- |
# Price ----
# ----------------------------------------------------------------------- |

# (1) subtract region pumping from region load

int.region.pump = int.gen.pump[,lapply(.SD,sum),by = c('scenario','time','region'),
                               .SDcols = c('value')]
setnames(int.region.pump,c('value'),c('pump'))

int.region.load = int.region.load[,lapply(.SD,sum),by = c('scenario','time','name'),
                                  .SDcols = c('value')]
setnames(int.region.load,c('name','value'),c('region','load'))

int.region.load = merge(int.region.load,int.region.pump,by = c('region','scenario','time'),
                        all = TRUE)
int.region.load[is.na(pump),pump:=0]
int.region.load[is.na(load),load:=0]
int.region.load[,load:=load - pump]

load.table = int.region.load[,.(region,scenario,time,load)]

# (2) add in LPFs

LPF.cast = data.table(dcast(node.LPFs,Node + region ~ Timeslice,value.var = 'LPF'))
LPF.cast = LPF.cast[`2024_Autumn_WI` > 0 |
                      `2024_Spring_WI` > 0 |
                      `2024_Winter_WI` > 0 |
                      `2024_Summer_WI` > 0]

# excludes the zero-load DC tie regions
# "BLACKWATER-ACDC" "LAMAR-ACDC" "MC-ACDC" "NPCC" "RC-ACDC" "SIDNEY-ACDC" "STEGAL-ACDC" "island"    

load.table = merge(load.table,LPF.cast,by = 'region',allow.cartesian = TRUE)

load.table[,month:=month(time)]
load.table[month %in% c(12,1,2),load:=load * `2024_Winter_WI`]
load.table[month %in% c(3,4,5),load:=load * `2024_Spring_WI`]
load.table[month %in% c(6,7,8),load:=load * `2024_Summer_WI`]
load.table[month %in% c(9,10,11),load:=load * `2024_Autumn_WI`]
load.table = load.table[,.(Node,time,load)]

# (3) get node pump load

pump.table = merge(int.gen.pump,GPFs[,.(name,Node,GPF = value)],
                   by = 'name',allow.cartesian = TRUE)

pump.table[,pump:=GPF*value]
pump.table = pump.table[,lapply(.SD,sum),by = c('Node','time'),.SDcols = c('pump')]

# (4) add pump load and nodal base load

load.table = merge(load.table,pump.table,by = c('Node','time'),all = TRUE)
load.table[is.na(pump),pump:=0]
load.table[is.na(load),load:=0]
load.table[,demand:=load+pump]

# (5) merge with price, weight by nodal load

int.node.price = merge(int.node.price,
                       node.regions,
                       by.x = 'name',by.y = 'Node',
                       all.x = TRUE)

if(nrow(int.node.price[is.na(region)])>0){
  message('some int.node.price nodes are not present in node.regions')
}

int.region.price = merge(int.node.price[,.(Node = name,region,time,price = value)],
                    load.table[,.(Node,time,demand)],
                    by = c('Node','time'),
                    all.y = TRUE)

int.region.price = int.region.price[,lapply(.SD,weighted.mean,w = demand),
                          by = c('region','time'),
                          .SDcols = c('price')]

# (6) remaining script directly from MULTI-OPT injections

int.region.price = merge(int.region.price,ISO.Region.key,
                        by.x = 'region',by.y = 'Region')

# average price by ISO
setnames(int.region.price,'price','value')
int.region.price = int.region.price[,lapply(.SD,mean),by = c('ISO','time'),.SDcols = 'value']

int.region.price = data.table(dcast(int.region.price,time ~ ISO,value.var = 'value'))

for(j in seq(length(interfaces.to.plot))){
  
  int.region.price[,eval(interfaces.to.plot[j]):=get(ISO.From[j]) - get(ISO.To[j])]
  int.region.price[,eval(paste0(interfaces.to.plot[j],"_pct")):=abs(get(ISO.From[j]) - get(ISO.To[j]))/(get(ISO.From[j])/2 + get(ISO.To[j])/2)]
  
}

# percentages
int.region.price.pct = int.region.price[,.SD,.SDcols = c(paste0(interfaces.to.plot,"_pct"),'time')]
names(int.region.price.pct) = sub("_pct","",names(int.region.price.pct))
int.region.price.pct = data.table(melt(int.region.price.pct,id.vars = c('time'),
                                       measure.vars = interfaces.to.plot))
setnames(int.region.price.pct,c('variable','value'),c('Interface','pct'))

int.region.price = int.region.price[,.SD,.SDcols = c(interfaces.to.plot,'time')]
int.region.price[,var:='Price']

# change from time to period
time.key = data.table(time = sort(unique(int.region.price[,time])),
                      Period = seq(length(unique(int.region.price[,time]))))
int.region.price = merge(int.region.price,time.key,by = c('time'))
int.region.price[,time:=NULL]
int.region.price.pct = merge(int.region.price.pct,time.key,by = c('time'))
int.region.price.pct[,time:=NULL]


# ----------------------------------------------------------------------- |
# Flows ----
# ----------------------------------------------------------------------- |

# reduce size

power.flow.nodes = data.table(Node.From = as.character(unlist(power.flows[1,])),
                              Node.To = as.character(unlist(power.flows[2,])))

power.flow.nodes[,Line:=paste0(Node.From,'_',Node.To,'_',seq(nrow(power.flow.nodes)))]

power.flow.nodes = merge(power.flow.nodes,Node.key[,.(Node.From = Bus,ISO.From = ISO)],
                         by = c('Node.From'))

power.flow.nodes = merge(power.flow.nodes,Node.key[,.(Node.To = Bus,ISO.To = ISO)],
                         by = c('Node.To'))

power.flow.nodes = power.flow.nodes[!(ISO.From == ISO.To)]

power.flow.lines = c('NaN_NaN_1',power.flow.nodes[,Line])
  
names(power.flows) = paste0(as.character(unlist(power.flows[1,])),"_",as.character(unlist(power.flows[2,])),'_',
                            seq(length(power.flows[1,])))

power.flows.short = copy(power.flows[,power.flow.lines,with = FALSE])

# melt

setnames(power.flows.short,names(power.flows.short)[grepl("NaN",names(power.flows.short))],"Period")
power.flows.short = power.flows.short[!(is.nan(Period))]

power.flows.short = data.table(melt(power.flows.short,id.vars = "Period",
                                   variable.name = 'Line',
                                   value.name = 'flow'))

power.flows.short = merge(power.flows.short,power.flow.nodes,by = c('Line'))
power.flows.short = merge(power.flows.short,SEAMS.interfaces,by = c('ISO.From','ISO.To'),
                          all.x = TRUE)
power.flows.short[is.na(Interface),c('ISO.From','ISO.To'):=.(ISO.To,ISO.From)]
power.flows.short[is.na(Interface),flow:=(-1)*flow]
power.flows.short[,c("Interface","color"):=NULL]

power.flows.short = merge(power.flows.short,SEAMS.interfaces,by = c('ISO.From','ISO.To'),
                          all.x = TRUE)

power.flows.short = power.flows.short[,lapply(.SD,sum),by = c('Interface','Period'),.SDcols = 'flow']

power.flows.short = data.table(dcast(power.flows.short,Period ~ Interface,value.var = 'flow'))
power.flows.short[,var:= "Interchange"]

power.flows.short = power.flows.short[,.SD,.SDcols = names(int.region.price)]

# ----------------------------------------------------------------------- |
# Combine ----
# ----------------------------------------------------------------------- |

# account for look-ahead time

int.comparison = rbind(power.flows.short,int.region.price)

int.comparison = data.table(melt(int.comparison, id.vars = c("Period","var"),
                                 measure.vars = interfaces.to.plot))

setnames(int.comparison,c('var','variable'),c('variable','Interface'))

int.comparison = data.table(dcast(int.comparison,Period + Interface ~ variable,value.var = 'value'))

int.comparison = merge(int.comparison,int.region.price.pct,by = c('Period','Interface'))

# ----------------------------------------------------------------------- |
# Plot ----
# ----------------------------------------------------------------------- |

outlier.data = int.comparison[Interchange < xlim[1] |
                              Interchange > xlim[2] |
                              Price < ylim[1] |
                              Price > ylim[2]]

options(scipen = 9999)
p <- ggplot() + geom_point(data = int.comparison,aes(x = Interchange,y = Price,color = Interface),
                            size = 0.3,alpha = 0.4) + 
    scale_color_manual(values = color.code) + 
    geom_vline(xintercept = 0,size = 0.3,color = 'black') +
    geom_hline(yintercept = 0,size = 0.3,color = 'black') +
    facet_grid(Interface ~ .) + plot_theme + labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
    theme(legend.position = 'none') + coord_cartesian(xlim = xlim,ylim = ylim) 

# calculate quadrants
int.comparison[,c('quad.0','I','II','III','IV'):=0]
int.comparison[pct<0.01,quad.0:=1]
int.comparison[Price > 0 & Interchange > 0 & quad.0 == 0,I:=1]
int.comparison[Price >= 0 & Interchange <= 0 & quad.0 == 0,II:=1]
int.comparison[Price < 0 & Interchange < 0 & quad.0 == 0,III:=1]
int.comparison[Price <= 0 & Interchange >= 0 & quad.0 == 0,IV:=1]

quadrants = int.comparison[,lapply(.SD,mean),by = c('Interface'),
                           .SDcols = c('quad.0','I','II','III','IV')]

quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface'),
                      .SDcols = c('quad.0','I','II','III','IV')]

# write quadrants csv and plot
setwd(wd)
write.csv(quadrants,'EI_single-opt_efficiencies.csv',row.names = FALSE)
ggsave('EI_single-opt_efficiencies.png',p,height = 5.5,width = 3.5)
write.csv(outlier.data,'EI_single-opt_outliers.csv',row.names = FALSE)