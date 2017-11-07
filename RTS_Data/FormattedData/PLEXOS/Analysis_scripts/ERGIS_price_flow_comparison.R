
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

solutions.dir <- "//nrelqnap01d/PLEXOS/Projects/ERGIS/ERGIS Solutions/c_RT_loVG"

# map of ERGIS to desired region names
ERGIS.regions <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                           "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/ERGIS_regions.csv")

# interface map
ERGIS.interfaces <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                           "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/ERGIS_interfaces.csv")

# Clayton's EIA scrubbed data
ISO.data <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                      "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/real-world-iso-efficiency-data.csv")

scenario.order = c('ISO data (\"Decomposed\")','ERGIS results (Nondecomposed)')

# ----------------------------------------------------------------------- |
# Setup queries ----
# ----------------------------------------------------------------------- |

setwd(solutions.dir)

# solution dbs & scenario.names
ERGIS.solutions = list.files()[grepl("-rplexos.db",list.files())]
scenario.names = as.character(seq(1,length(ERGIS.solutions),1))

ERGIS.regions = fread(ERGIS.regions)

ERGIS.interfaces = fread(ERGIS.interfaces)
interfaces.to.plot = ERGIS.interfaces[,Interface]
ISO.From = ERGIS.interfaces[,ISO.From]
ISO.To = ERGIS.interfaces[,ISO.To]
color.code = ERGIS.interfaces[,color]
names(color.code) = interfaces.to.plot

ISO.data = fread(ISO.data)

# ----------------------------------------------------------------------- |
# Queries ----
# ----------------------------------------------------------------------- |

# 1. interval region price

interval.region.price <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  interval.region.price <- rbind(interval.region.price,
                                expand_time(data.table(tbl(src_sqlite(ERGIS.solutions[i]), 
                                sql("SELECT key, name, category, time_from, time_to, value, property 
                                FROM Region_Price 
                                WHERE collection IS 'Region' AND 
                                property IS 'Price' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),ERGIS.solutions[i]))
}

# time partitions
int.region.price = copy(interval.region.price)
int.region.price[,min.scenario:=min(scenario),by = c('name','time')]
int.region.price = int.region.price[scenario == min.scenario]

# 2. interval Region.Region net interchange

interval.region.NI <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  interval.region.NI <- rbind(interval.region.NI,
                                expand_time(data.table(tbl(src_sqlite(ERGIS.solutions[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM RegionRegions_NetInterchange 
                                WHERE collection IS 'Region.Regions' AND 
                                property IS 'Net Interchange' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),ERGIS.solutions[i]))
}

# time partitions
int.region.NI = copy(interval.region.NI)
int.region.NI[,min.scenario:=min(scenario),by = c('name','time')]
int.region.NI = int.region.NI[scenario == min.scenario]


# ----------------------------------------------------------------------- |
# Calculations ERGIS results ----
# ----------------------------------------------------------------------- |

# prices

int.region.price = merge(int.region.price[,.(Region = name,time,value)],
                         ERGIS.regions,
                         by = 'Region')

# average price by ISO
int.region.price = int.region.price[,lapply(.SD,mean),by = c('ISO','time'),.SDcols = 'value']

int.region.price = data.table(dcast(int.region.price,time ~ ISO,value.var = 'value'))


for(j in seq(length(interfaces.to.plot))){

  int.region.price[,eval(interfaces.to.plot[j]):=get(ISO.From[j]) - get(ISO.To[j])]
  int.region.price[,eval(paste0(interfaces.to.plot[j],"_pct")):=abs(get(ISO.From[j]) - get(ISO.To[j]))/(get(ISO.From[j])/2 + get(ISO.To[j])/2)]
  
}

int.region.price.pct = int.region.price[,.SD,.SDcols = c(paste0(interfaces.to.plot,"_pct"),'time')]
names(int.region.price.pct) = sub("_pct","",names(int.region.price.pct))
int.region.price.pct = data.table(melt(int.region.price.pct,id.vars = c('time'),
                                       measure.vars = interfaces.to.plot))
setnames(int.region.price.pct,c('variable','value'),c('Interface','pct'))

int.region.price = int.region.price[,.SD,.SDcols = c(interfaces.to.plot,'time')]
int.region.price[,var:='Price']

# flows

setnames(int.region.NI,c('parent','name'),c('Region.From','Region.To'))
int.region.NI = merge(int.region.NI,
                      ERGIS.regions[,.(Region.From = Region,ISO.From = ISO)],
                      by = 'Region.From')

int.region.NI = merge(int.region.NI,
                      ERGIS.regions[,.(Region.To = Region,ISO.To = ISO)],
                      by = 'Region.To')

int.region.NI = int.region.NI[,lapply(.SD,sum),by = c('ISO.From','ISO.To','time'),.SDcols = 'value']

int.region.NI[,Interface:=paste0(ISO.From," - ",ISO.To)]
int.region.NI = data.table(dcast(int.region.NI,time ~ Interface,value.var = 'value'))
int.region.NI[,var:= "Interchange"]

int.region.NI = int.region.NI[,.SD,.SDcols = names(int.region.price)]

# combine

int.comparison = rbind(int.region.NI,int.region.price)

int.comparison = data.table(melt(int.comparison, id.vars = c("time","var"),
                                 measure.vars = interfaces.to.plot))

setnames(int.comparison,c('var','variable'),c('variable','Interface'))

int.comparison = data.table(dcast(int.comparison,time + Interface ~ variable,value.var = 'value'))
int.comparison[,scenario:='ERGIS results (Nondecomposed)']

int.comparison = merge(int.comparison,int.region.price.pct,by = c('time','Interface'))

# ----------------------------------------------------------------------- |
# Calculations real data ----
# ----------------------------------------------------------------------- |

# Clayton's EIA scrubbed data
ISO.data <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                      "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/real-world-iso-efficiency-data.csv")

ISO.data = fread(ISO.data)

ISO.data = ISO.data[,.(time = timestamp,Interface = interface, Price = dLMP, 
                       pct = abs(dLMP)/(from.lmp/2 + to.lmp/2),
                       Interchange = interfaceflows,scenario = "ISO data (\"Decomposed\")")]
ISO.data[,Interface:=gsub("PJM-MISO","PJM - MISO",Interface)]
ISO.data[,Interface:=gsub("PJM-NYISO","PJM - NYISO",Interface)]
ISO.data[,Interface:=gsub("NYISO-ISONE","NYISO - ISO-NE",Interface)]



int.comparison = rbind(int.comparison[,.(Interface,Interchange,Price,scenario,pct)],
                       ISO.data[,.(Interface,Interchange,Price,scenario,pct)])

# ----------------------------------------------------------------------- |
# Plot ----
# ----------------------------------------------------------------------- |

int.comparison$scenario = factor(int.comparison$scenario,levels = scenario.order)

p <- ggplot() + geom_point(data = int.comparison,aes(x = Interchange,y = Price,color = Interface),
                           size = 0.3,alpha = 0.4) + 
  scale_color_manual(values = color.code) + 
  geom_vline(xintercept = 0,size = 0.3,color = 'black') +
  geom_hline(yintercept = 0,size = 0.3,color = 'black') +
  facet_grid(scenario~Interface) + plot_theme + labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
  theme(legend.position = 'none')

# add quadrants
int.comparison[,c('quad.0','quad.1','quad.2','quad.3','quad.4'):=0]
int.comparison[pct<0.01,quad.0:=1]
int.comparison[Price > 0 & Interchange > 0 & quad.0 == 0,quad.1:=1]
int.comparison[Price >= 0 & Interchange <= 0 & quad.0 == 0,quad.2:=1]
int.comparison[Price < 0 & Interchange < 0 & quad.0 == 0,quad.3:=1]
int.comparison[Price <= 0 & Interchange >= 0 & quad.0 == 0,quad.4:=1]


quadrants = int.comparison[,lapply(.SD,mean),by = c('Interface','scenario'),
                           .SDcols = c('quad.0','quad.1','quad.2','quad.3','quad.4')]

quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface','scenario'),
                           .SDcols = c('quad.0','quad.1','quad.2','quad.3','quad.4')]

quadrants = data.table(melt(quadrants,id.vars = c('Interface','scenario')))
quadrants[variable == 'quad.0',x:= 0]
quadrants[variable == 'quad.0',y:= 60]
quadrants[variable == 'quad.1',x:=4500]
quadrants[variable == 'quad.1',y:=50]
quadrants[variable == 'quad.2',x:=-5000]
quadrants[variable == 'quad.2',y:=50]
quadrants[variable == 'quad.3',x:=-5000]
quadrants[variable == 'quad.3',y:=-50]
quadrants[variable == 'quad.4',x:=4500]
quadrants[variable == 'quad.4',y:=-50]
quadrants[variable %in% c('quad.1','quad.3'),efficient:="no"]
quadrants[variable %in% c('quad.2','quad.4'),efficient:="somewhat"]
quadrants[variable %in% c('quad.0'),efficient:="yes"]

p <- p + geom_label(data = quadrants,aes(x = x,y = y,label = value,fill = efficient),
                    color = 'black',size = 3) +
  scale_fill_manual(values = c('no' = 'gray80','somewhat' = 'skyblue3','yes' = 'goldenrod')) + 
  theme(legend.position = 'none')

setwd(wd)
ggsave('ERGIS_efficiencies.png',p,height = 5.5,width = 6.5)



