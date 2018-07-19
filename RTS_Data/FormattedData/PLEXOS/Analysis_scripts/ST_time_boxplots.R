
setwd(dirname(parent.frame(2)$ofile))
setwd("data")

# ---------------------------------------------------------------------------- |
# seams ST step plot ----
# ---------------------------------------------------------------------------- |

library(data.table)
library(ggplot2)
# ST times
ST.times <- file.path("ST_time.csv")

ST.times = fread(ST.times)

ST.times[,Region:=Simulation]
ST.times[,Region:=gsub("log_files/Model","",Region)]
#ST.times[,Region:=gsub("/projects/PLEXOSMODEL/seams/PCM_runs/cb_log_files/~Model","",Region)]

ST.times = ST.times[grepl("CAN|MISO|NYISO|ISONE|PJM|SPP|so",Region) & `UC-Time` != ""]

ST.times[grepl("so",Region),Region:="EI"]
ST.times[grepl("CAN",Region),Region:="CAN"]
ST.times[grepl("MISO",Region),Region:="MISO"]
ST.times[grepl("NYISO",Region),Region:="NYISO"]
ST.times[grepl("ISONE",Region),Region:="ISONE"]
ST.times[grepl("PJM",Region),Region:="PJM"]
ST.times[grepl("SPP",Region),Region:="SPP"]

ST.times[,c('hours','minutes','seconds'):=tstrsplit(`UC-Time`,":")]
ST.times[,time:=(as.numeric(hours)*3600 + as.numeric(minutes)*60 + as.numeric(seconds))]


color.code = c('MISO' = 'skyblue3','CAN' = 'firebrick','NYISO' = 'navyblue','ISONE' = 'goldenrod1','PJM' = 'forestgreen',
               'SPP' = 'peru','EI' = 'gray20')

ST.times$Region = factor(ST.times$Region,levels = c("CAN",
                                                    "NYISO",
                                                    "ISONE",
                                                    "SPP",
                                                    "PJM",
                                                    "MISO",
                                                    "EI"))
ST.times <- ST.times[!(Region=="CAN"&time>360)]

# --------------------------------------------------- |
# ST times table ----
# --------------------------------------------------- |

ST.times.table = copy(ST.times)[,model:=ifelse(Region == "EI","single-operator","multi-operator")]
ST.times.table = ST.times.table[,lapply(.SD,mean),by = c('model'),
                              .SDcols = c('time')]

# --------------------------------------------------- |
# plot theme & plots ----
# --------------------------------------------------- |

# size of text in plots
large.text.size <- 7.875
small.text.size <- 9.1666
text.plot = 11

# plot theme
plot_theme <- 
    theme(legend.key = element_rect(color = "grey80", size = 0.8), 
          legend.key.size = grid::unit(1.0, "lines"),
          legend.text = element_text(size = small.text.size), 
          legend.title = element_blank(), 
          axis.text = element_text(size = small.text.size), 
          axis.text.x = element_text(size = small.text.size),
          axis.title = element_text(size = large.text.size, face = "bold"),
          axis.title.x= element_text(size=large.text.size, vjust = 1.2, face = "bold"),
          axis.title.y = element_text(size=large.text.size, vjust = 1.2, face = "bold"),
          strip.text = element_text(size=small.text.size),
          panel.spacing = unit(0.5, "lines"))

# ST.times = ST.times[,Integers:=as.character(Integers)]
setwd("../plots_computation")
theme_set(theme_bw()) 

p <- ggplot(data = ST.times[Region!="EI"],aes(x = Integers,y = time,group = Integers,color = Region)) + scale_x_continuous() +
  geom_boxplot(outlier.shape = 1,width = 1000) +
  scale_color_manual(values = color.code) + 
  labs(x = '',y= '') +
  plot_theme +
  theme(legend.position = 'none') 

ggsave('ST-time-hpc_reduced.emf',p,height = 2,width = 2.5,dpi = 600)

p <- ggplot(data = ST.times,aes(x = Integers,y = time,group = Integers,color = Region)) + scale_x_continuous() +
  geom_boxplot(outlier.shape = 1,width = 5000) +
  scale_color_manual(values = color.code) + 
  plot_theme + 
  labs(x = 'Integers',y= 'Solution Time (Seconds)') +
  theme(legend.position = 'bottom') 



ggsave('ST-time-hpc.emf',p,height = 3.5,width = 3.5,dpi=600)


