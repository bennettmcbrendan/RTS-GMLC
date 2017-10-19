
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

interfaces.to.plot <- c("PJM - MISO","PJM - NYISO","NYISO - ISO-NE")
color.code = c("PJM - MISO" = 'navyblue',"PJM - NYISO" = 'forestgreen',"NYISO - ISO-NE" = 'firebrick')

solutions.dir <- "//nrelqnap01d/PLEXOS/Projects/ERGIS/ERGIS Solutions/c_RT_loVG"

# map of ERGIS to desired region names
ERGIS.regions <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                           "RTS_Data/FormattedData/PLEXOS/ERGIS_regions.csv")

# interface map
ERGIS.interfaces <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                           "RTS_Data/FormattedData/PLEXOS/ERGIS_interfaces.csv")

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
# Calculations ----
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
  
}

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

# ----------------------------------------------------------------------- |
# Plot ----
# ----------------------------------------------------------------------- |

p <- ggplot() + geom_point(data = int.comparison,aes(x = Interchange,y = Price,color = Interface),size = 0.3) + 
  scale_color_manual(values = color.code) + 
  facet_grid(.~Interface) + plot_theme + labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
  theme(legend.position = 'none')

setwd(wd)
ggsave('ERGIS_efficiencies.png',p,height = 4,width = 6.5)



