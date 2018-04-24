
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
ylim = c(-75,75)

# interface and area map
SEAMS.interfaces <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                              "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/EI_interfaces.csv")

# Clayton's EIA scrubbed data
ISO.data <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                      "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/data/real-world-iso-efficiency-data.csv")

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
# Calculations real data ----
# ----------------------------------------------------------------------- |

ISO.data = fread(ISO.data)

ISO.data = ISO.data[,.(Period = timestamp,Interface = interface, Price = dLMP, 
                       pct = abs(dLMP)/(from.lmp/2 + to.lmp/2),
                       Interchange = interfaceflows)]
ISO.data[,Interface:=gsub("PJM-MISO","PJM - MISO",Interface)]
ISO.data[,Interface:=gsub("PJM-NYISO","PJM - NYISO",Interface)]
ISO.data[,Interface:=gsub("NYISO-ISONE","NYISO - ISO-NE",Interface)]

int.comparison =  ISO.data[,.(Period,Interface,Interchange,Price,pct)]

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

# C. price-flow plot
plot.text = data.table(Interface = "PJM - MISO",
                       x = c(4000,-4000,-4000,4000),
                       y = c(60,60,-60,-60),
                       text = rep(c('Counter-intuitive','Under-utilized'),2))

int.comparison$Interface = factor(int.comparison$Interface,levels = interfaces.to.plot)
plot.text$Interface = factor(plot.text$Interface,levels = interfaces.to.plot)

p <- ggplot() + stat_binhex(data = int.comparison,
                            aes(x = Interchange,y = Price,color = ..count..),binwidth = c(200,3)) + 
  scale_fill_gradientn(name = "Hours",colours = c('gray80','lightpink','darkred'),
                       guide = 'colourbar') + 
  scale_color_gradientn(name = "Hours",colours = c('gray80','lightpink','darkred'),
                        guide = 'colourbar') + 
  geom_label(data = plot.text,aes(x = x,y = y,label = text),color = 'black',fill = 'lightblue',size = 2.4) + 
  geom_vline(xintercept = 0,size = 0.01,color = 'black') +
  geom_hline(yintercept = 0,size = 0.01,color = 'black') +
  facet_grid(Interface ~ .) + plot_theme + theme(legend.title = element_text()) + 
  labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
  coord_cartesian(xlim = xlim,ylim = ylim) 

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

quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface'),
                      .SDcols = c('quad.0','I','II','III','IV')]

# write quadrants csv and plot
setwd(wd)
write.csv(quadrants,'plots/EI_ISO_efficiencies.csv',row.names = FALSE)
ggsave('plots/EI_ISO_hex.png',p,height = 5.5,width = 3.5)
write.csv(outlier.data,'plots/EI_ISO_outliers.csv',row.names = FALSE)