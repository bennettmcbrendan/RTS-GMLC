
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
solutions.dir <- "//nrelqnap01d/PLEXOS CEII/Projects/Interconnections_Seam_Plexos/bmcbenne/03_09_2018"

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

node.voltage <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                          "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/node_voltage.csv")

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

node.voltage = fread(node.voltage)

# ----------------------------------------------------------------------- |
# Read flows file ----
# ----------------------------------------------------------------------- |

if(TRUE){
    matpower.flows = fread(matpower.flows.file)
    power.flows = copy(matpower.flows)[1:170,]
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

# 1. interval region price

interval.region.price <- data.table()
for(i in 1:length(scenario.names)){
    print(i)
    interval.region.price <- rbind(interval.region.price,
                                   expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                    sql("SELECT key, name, category, time_from, time_to, value, property 
                                    FROM Region_Price 
                                    WHERE collection IS 'Region' AND 
                                    property IS 'Price' AND
                                    phase_id IS 4")) %>% 
                                    dplyr::mutate(scenario = scenario.names[i]) %>% 
                                    collect(n = Inf)),solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i]])
}

# time decomposition - hard code
int.region.price = copy(interval.region.price)
int.region.price = int.region.price[!(day(time) %in% c(10,11))]

stop()

# ----------------------------------------------------------------------- |
# Flows ----
# ----------------------------------------------------------------------- |

# reduce size

power.flow.nodes = data.table(Bus.From = as.character(unlist(power.flows[1,])),
                              Bus.To = as.character(unlist(power.flows[2,])))

power.flow.nodes[,Line:=paste0(Bus.From,'_',Bus.To,'_',seq(nrow(power.flow.nodes)))]

power.flow.nodes = merge(power.flow.nodes,Node.key[,.(Bus.From = Bus,Node.From = Node,ISO.From = ISO)],
                         by = c('Bus.From'))

power.flow.nodes = merge(power.flow.nodes,Node.key[,.(Bus.To = Bus,Node.To = Node,ISO.To = ISO)],
                         by = c('Bus.To'))

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

# power.flows.short = power.flows.short[,.SD,.SDcols = names(int.region.price)]

# ----------------------------------------------------------------------- |
# Price ----
# ----------------------------------------------------------------------- |

int.region.price = merge(int.region.price,ISO.Region.key,
                         by.x = 'name',
                         by.y = 'Region')

int.region.price = int.region.price[,lapply(.SD,mean),by = c('ISO','time'),
                                    .SDcols = c('value')]

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

int.fold = copy(int.comparison)
# int.fold[,Interchange:=ifelse(Interchange*Price < 0,abs(Interchange),(-1)*abs(Interchange))]
# int.fold[Price == 0,Interchange:=0]
# int.fold[,Price:=abs(Price)]

fold.text = data.table(Interface = "PJM - MISO",
                       x = c(4000,-4000,-4000,4000),
                       y = c(150,150,-150,-150),
                       text = rep(c('Counter-intuitive','Under-utilized'),2))

options(scipen = 9999)
p1 <- ggplot() + geom_point(data = int.comparison,aes(x = Interchange,y = Price,color = Interface),
                            size = 0.3,alpha = 0.4) + 
    scale_color_manual(values = color.code) + 
    geom_vline(xintercept = 0,size = 0.3,color = 'black') +
    geom_hline(yintercept = 0,size = 0.3,color = 'black') +
    facet_grid(Interface ~ .) + plot_theme + labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
    theme(legend.position = 'none') + coord_cartesian(xlim = xlim,ylim = ylim) 

int.fold$Interface = factor(int.fold$Interface,levels = interfaces.to.plot)
fold.text$Interface = factor(fold.text$Interface,levels = interfaces.to.plot)

p2 <- ggplot() + stat_binhex(data = int.fold,
                             aes(x = Interchange,y = Price,color = ..count..),binwidth = c(200,5)) + 
  scale_fill_gradientn(name = "Hours",colours = c('gray80','lightpink','darkred'),
                       guide = 'colourbar') + 
  scale_color_gradientn(name = "Hours",colours = c('gray80','lightpink','darkred'),
                        guide = 'colourbar') + 
  geom_label(data = fold.text,aes(x = x,y = y,label = text),color = 'black',fill = 'lightblue',size = 2.4) + 
  geom_vline(xintercept = 0,size = 0.01,color = 'black') +
  geom_hline(yintercept = 0,size = 0.01,color = 'black') +
  facet_grid(Interface ~ .) + plot_theme + theme(legend.title = element_text()) + 
  labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
  coord_cartesian(xlim = xlim,ylim = ylim) 

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
# write.csv(quadrants,'plots/EI_single-opt-border_efficiencies.csv',row.names = FALSE)
# ggsave('plots/EI_single-opt-border_efficiencies.png',p1,height = 5.5,width = 3.5)
ggsave('plots/EI_single-opt-REGION_hex.png',p2,height = 5.5,width = 3.5)
# write.csv(outlier.data,'plots/EI_single-opt-border_outliers.csv',row.names = FALSE)