
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

xlim = c(-600,600)
ylim = c(-100,100)

# decomposed RTS-GMLC runs
solution.dbs <- c(file.path("//nrelqnap02/PLEXOS CEII/Projects/Interconnections_Seam_Plexos/Continental/geodecomp_compare/RTS-GMLC",
                             "Model REAL_TIME_C_FIX Solution/Model REAL_TIME_C_FIX Solution-rplexos.db"),
                 file.path("//nrelqnap02/PLEXOS CEII/Projects/Interconnections_Seam_Plexos/Continental/geodecomp_compare/RTS-GMLC",
                           "Model REAL_TIME_FIX Solution/Model REAL_TIME_FIX Solution-rplexos.db"))

# interface.table <- data.table(Interface = rep('1 - 2',3),
#                               Line = c('AB1','AB2','AB3'),
#                               Node.From = c('107','113','123'),
#                               Region.From = c('1'),
#                               Node.To = c('203','215','217'),
#                               Region.To = c('2'),
#                               Coefficient = c(1,1,1))

interface.table <- data.table(Interface = rep('1 - 3',2),
                              Line = c('CA-1','113_316_1'),
                              Node.From = c('121','113'),
                              Region.From = c('1','1'),
                              Node.To = c('325','316'),
                              Region.To = c('3','3'),
                              Coefficient = c(-1,1))

# interface.table <- data.table(Interface = rep('2 - 3',1),
#                               Line = c('CB-1'),
#                               Node.From = c('223'),
#                               Region.From = c('2'),
#                               Node.To = c('318'),
#                               Region.To = c('3'),
#                               Coefficient = c(-1))

# ----------------------------------------------------------------------- |
# Setup queries ----
# ----------------------------------------------------------------------- |

# scenario.names
scenario.names = c('Multi-operator','Single-operator')

# scenario.order
scenario.order = c('Single-operator','Multi-operator')

node.names = c(unique(interface.table[,Node.From]),
               unique(interface.table[,Node.To]))

line.names = unique(interface.table[,Line])

# ----------------------------------------------------------------------- |
# Queries ----
# ----------------------------------------------------------------------- |

# 1. interval node price
interval.node.price <- data.table()

for(i in 1:length(scenario.names)){
  print(i)
  interval.node.price <- rbind(interval.node.price,
                              expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Node_Price 
                                WHERE collection IS 'Node' AND 
                                property IS 'Price' AND
                                phase_id IS 4")) %>% 
                                filter(name %in% node.names) %>%
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),solution.dbs[i]))
}

int.node.price = copy(interval.node.price)[!(month(time) == 12 & day(time) == 31)]

# 2. interval line flow
interval.line.flow <- data.table()

for(i in 1:length(scenario.names)){
  print(i)
  interval.line.flow <- rbind(interval.line.flow,
                               expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Line_Flow 
                                WHERE collection IS 'Line' AND 
                                property IS 'Flow' AND
                                phase_id IS 4")) %>% 
                                filter(name %in% line.names) %>%
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),solution.dbs[i]))
}

int.line.flow = copy(interval.line.flow)[!(month(time) == 12 & day(time) == 31)]

# ----------------------------------------------------------------------- |
# Reformat flows ----
# ----------------------------------------------------------------------- |

int.flow = merge(int.line.flow[,.(Line = name,scenario,time,Interchange = value)],
                      interface.table,
                      by = c('Line'))

int.flow[,Interchange:=Interchange * Coefficient]

int.flow = int.flow[,lapply(.SD,sum),by = c('scenario','Interface','time'),
                    .SDcols = c('Interchange')]

# ----------------------------------------------------------------------- |
# Reformat price ----
# ----------------------------------------------------------------------- |

int.price = merge(int.node.price[,.(Node.From = name,scenario,time,Price.From = value)],
                  interface.table,
                  by = c('Node.From'))

int.price = merge(int.node.price[,.(Node.To = name,scenario,time,Price.To = value)],
                  int.price,
                  by = c('Node.To','scenario','time'))

int.price = int.price[,lapply(.SD,mean),by = c('scenario','Interface','time'),
                      .SDcols = c('Price.From','Price.To')]

int.price[,Price:=Price.To - Price.From]
int.price[,pct:=abs(Price.To - Price.From)/(Price.To/2 + Price.From/2)]
int.price[is.nan(pct),pct:=0]

int.price[,c('Price.From','Price.To'):=NULL]

# ----------------------------------------------------------------------- |
# Combine ----
# ----------------------------------------------------------------------- |

int.comparison = merge(int.flow,int.price,
                       by = c('scenario','Interface','time'))


# ----------------------------------------------------------------------- |
# Plot ----
# ----------------------------------------------------------------------- |

options(scipen = 9999)

interfaces.to.plot = unique(interface.table[,(Interface)])

# A. outlier data
outlier.data = int.comparison[Interchange < xlim[1] |
                                Interchange > xlim[2] |
                                Price < ylim[1] |
                                Price > ylim[2]]

# B. statistics
int.stat = copy(int.comparison)

int.stat[,Price:=abs(Price)]

# center of mass
int.stat.center = int.stat[,lapply(.SD,mean),by = c('Interface','scenario'),
                           .SDcols = c('Interchange','Price')]

# squared distance from center
int.stat.squared = copy(int.stat)

int.stat.squared[,`Interchange` :=(`Interchange` - mean(`Interchange`))^2]
int.stat.squared[,`Price` :=(`Price` - mean(`Price`))^2]

int.stat.squared = int.stat.squared[,lapply(.SD,mean),by = c('Interface','scenario'),
                                    .SDcols = c('Interchange','Price')]

# regression
trend.line = data.table(Interface = interfaces.to.plot,scenario = scenario.names)

for(jj in seq(length(scenario.names))){
  
  trend.line[scenario == scenario.names[jj],
             c('intercept','slope'):=as.list(coef(lm(Price~Interchange,data = int.comparison[scenario == scenario.names[jj]])))]
  
}

trend.endpoints = copy(int.comparison)
trend.endpoints[,x:=min(Interchange),by = c('scenario')]
trend.endpoints[,xend:=max(Interchange),by = c('scenario')]
trend.endpoints = unique(trend.endpoints[,.(Interface,scenario,x,xend)])
trend.endpoints[,x:=x+xlim[1]/8]
trend.endpoints[,xend:=xend+xlim[2]/8]

trend.line = merge(trend.line,trend.endpoints,by = c('Interface','scenario'))
trend.line[,y:=intercept + slope * x]
trend.line[,yend:=intercept + slope * xend]

rm(trend.endpoints)

# C. price-flow plot
plot.text = data.table(Interface = interfaces.to.plot[1],
                       scenario = "Single-operator",
                       x = c(350,-350,-350,350),
                       y = c(70,70,-70,-70),
                       text = rep(c('Agreement','Disagreement'),2))

int.comparison$Interface = factor(int.comparison$Interface,levels = interfaces.to.plot)
plot.text$Interface = factor(plot.text$Interface,levels = interfaces.to.plot)

int.comparison$scenario = factor(int.comparison$scenario,levels = scenario.order)
plot.text$scenario = factor(plot.text$scenario,levels = scenario.order)

p1 <- ggplot() + stat_binhex(data = int.comparison,
                             aes(x = Interchange,y = Price,color = ..density..*100,fill = ..density..*100),
                             binwidth = c(25,3)) + 
  scale_fill_gradientn(name = "(%)",colours = c('gray80','gray20','black'),
                       guide = 'colourbar') + 
  scale_color_gradientn(name = "(%)",colours = c('gray80','gray20','black'),
                        guide = 'colourbar') + 
  geom_label(data = plot.text,aes(x = x,y = y,label = text),color = 'black',fill = 'wheat1',size = 2.4) + 
  geom_vline(xintercept = 0,size = 0.01,color = 'black') +
  geom_hline(yintercept = 0,size = 0.01,color = 'black') +
  # geom_segment(data = trend.line,aes(x = x,xend = xend,y = y,yend = yend),color = 'firebrick',linetype = 1,size = 1) + 
  facet_grid(scenario~Interface) + plot_theme + theme(legend.title = element_text(),strip.text.x = element_blank()) + 
  labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
  coord_cartesian(xlim = xlim,ylim = ylim) +
  theme(strip.background = element_rect(fill = "wheat1"))

# p2 <- folded_price_flow_plots(plot.data = copy(int.comparison),
#                               interfaces.to.plot = interfaces.to.plot)

# D. quadrants
quadrants = copy(int.comparison)

quadrants[,c('quad.0','I','II','III','IV'):=0]
quadrants[pct<0.01,quad.0:=1]
quadrants[Price > 0 & Interchange > 0 & quad.0 == 0,I:=1]
quadrants[Price >= 0 & Interchange <= 0 & quad.0 == 0,II:=1]
quadrants[Price < 0 & Interchange < 0 & quad.0 == 0,III:=1]
quadrants[Price <= 0 & Interchange >= 0 & quad.0 == 0,IV:=1]

quadrants = quadrants[,lapply(.SD,mean),by = c('Interface','scenario'),
                      .SDcols = c('quad.0','I','II','III','IV')]

quadrants[,Disagreement := II + IV]
quadrants[,Agreement := I + III]

quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface','scenario'),
                      .SDcols = c('quad.0','I','II','III','IV','Disagreement','Agreement')]

# ----------------------------------------------------------------------- |
# Write out ----
# ----------------------------------------------------------------------- |

ggsave("plots_RTS/rts-price-flow.png",p1,height = 4.5,width = 3.5)
write.csv(quadrants,'plots_RTS/rts-efficiency-data.csv',row.names = FALSE)
write.csv(int.stat.center,'plots_RTS/rts-avg-price-diff.csv',row.names = FALSE)