
wd = dirname(parent.frame(2)$ofile)
setwd(wd)

# ---------------------------------------------------------------------------- |
# seams ST step plot ----
# ---------------------------------------------------------------------------- |


# ST times
ST.times <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                           "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/data/ST_time_hpc.csv")

ST.times = fread(ST.times)

ST.times[,Region:=Simulation]
ST.times[,Region:=gsub("/projects/PLEXOSMODEL/seams/PCM_runs/cb_log_files/Model","",Region)]
ST.times[,Region:=gsub("/projects/PLEXOSMODEL/seams/PCM_runs/cb_log_files/~Model","",Region)]

ST.times = ST.times[grepl("CAN|MISO|NE|PJM|SPP",Region) & `UC-Time` != ""]


ST.times[grepl("CAN",Region),Region:="CAN"]
ST.times[grepl("MISO",Region),Region:="MISO"]
ST.times[grepl("NE",Region),Region:="NE"]
ST.times[grepl("PJM",Region),Region:="PJM"]
ST.times[grepl("SPP",Region),Region:="SPP"]

ST.times[,c('hours','minutes','seconds'):=tstrsplit(`UC-Time`,":")]
ST.times[,time:=(as.numeric(hours) + as.numeric(minutes)/60 + as.numeric(seconds)/3600)]

color.code = c('MISO' = 'navyblue','CAN' = 'firebrick','NE' = 'skyblue3','PJM' = 'forestgreen',
               'SPP' = 'peru')

ST.times$Region = factor(ST.times$Region,levels = c("CAN",
                                                    "NE",
                                                    "SPP",
                                                    "PJM",
                                                    "MISO"))



p <- ggplot(data = ST.times,aes(x = Integers,y = time,group = Integers,color = Region)) + scale_x_continuous() +
  geom_boxplot(outlier.shape = 1) +
  scale_color_manual(values = color.code) + 
  plot_theme + labs(x = 'Integers',y= 'Solution Time (Hours)') +
  theme(legend.position = 'bottom') 

  

ggsave('ST-time-hpc.png',p,height = 3,width = 3.5)



# ---------------------------------------------------------------------------- |
# RTS ST step plot ----
# ---------------------------------------------------------------------------- |
  
ST.times.01 <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                      "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/data/ST_time_rts_win_01.csv")

ST.times.05 <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                         "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/data/ST_time_rts_win_05.csv")


ST.times.01 = fread(ST.times.01)
ST.times.05 = fread(ST.times.05)

ST.times.01[,MIP:="MIP - 0.1%"]
ST.times.05[,MIP:="MIP - 0.5%"]

ST.times = rbind(ST.times.01,ST.times.05)

ST.times[,Region:=Simulation]
ST.times[grepl("DAY_AHEAD",Simulation),Region:="Non-decomposed"]
ST.times[grepl("DAY_AHEAD_B1",Simulation),Region:="Stage B1 & B2"]
ST.times[grepl("DAY_AHEAD_B2",Simulation),Region:="Stage B1 & B2"]
ST.times[grepl("DAY_AHEAD_B3",Simulation),Region:="Stage B3"]
ST.times[grepl("DAY_AHEAD_A",Simulation),Region:="Stage A"]

ST.times[,c('hours','minutes','seconds'):=tstrsplit(`UC-Time`,":")]
ST.times[,time:=(as.numeric(hours)*3600 + as.numeric(minutes)*60 + as.numeric(seconds))]
ST.times[is.na(Integers),Integers:=0]

color.code = c('Non-decomposed' = 'navyblue','Stage A' = 'firebrick','Stage B1 & B2' = 'peru',
                  'Stage B3' = 'skyblue3')

ST.times$Region = factor(ST.times$Region,levels = c("Stage A",
                                                    "Stage B1 & B2",
                                                    "Stage B3",
                                                    "Non-decomposed"))

p <- ggplot(data = ST.times,aes(x = Integers,y = log(time),group = Integers,color = Region)) + scale_x_continuous() +
  geom_boxplot(outlier.shape = 1) +
  scale_color_manual(values = color.code) + 
  plot_theme + labs(x = 'Integers',y= 'Solution Time - ln(seconds)') +
  facet_wrap(~MIP,ncol = 1) +
  theme(legend.position = 'bottom',legend.direction = 'vertical') 



ggsave('ST-time-win.png',p,height = 4.5,width = 3.5)
