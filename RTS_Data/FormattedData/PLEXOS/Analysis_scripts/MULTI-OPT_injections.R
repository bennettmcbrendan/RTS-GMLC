
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

solutions.dir <- "//nrelqnap01d/PLEXOS CEII/Projects/Interconnections_Seam_Plexos/Continental/a_SEAMS_stage_b_zip"
output.dir <- "//nrelqnap01d/PLEXOS/Projects/GMLC-MSPCM/SEAMS-data-decomposed"

# node LPF map - 98280 nodes
node.LPFs <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                           "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/node_LPF.csv")

GPFs <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                  "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/generation_participation_factors.csv")

# ----------------------------------------------------------------------- |
# Decomposition key ----
# ----------------------------------------------------------------------- |

source('SEAMS_database/make_decomposition_key.R')
output = make_decomposition_key()

generator.nodes = output[['generator.nodes']]
node.regions = output[['node.regions']]
decomposition.key = output[['generator.table']]

# ----------------------------------------------------------------------- |
# Read Inputs ----
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
# Setup queries ----
# ----------------------------------------------------------------------- |

setwd(solutions.dir)

solution.dbs = list.files(recursive = TRUE)[grepl('-rplexos.db',
                                        list.files(recursive = TRUE))]

solution.dbs = solution.dbs[grepl('base_',solution.dbs)]

scenario.names = gsub(" Solution-rplexos.db","",solution.dbs)
scenario.names = tstrsplit(scenario.names,"_base_")[[2]]

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

# 2. interval generator generation

interval.gen.generation <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  interval.gen.generation <- rbind(interval.gen.generation,
                                expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Generator_Generation 
                                WHERE collection IS 'Generator' AND 
                                property IS 'Generation' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i]])
}

# time decomposition & remove look-ahead
int.gen.generation = copy(interval.gen.generation)
int.gen.generation[,c('scenario.name','scenario.no'):=tstrsplit(scenario,"_")]
int.gen.generation[,scenario.no:=as.numeric(scenario.no)]
int.gen.generation[,min.scenario:=min(scenario.no),by = c('name','time','scenario.name')]
int.gen.generation = int.gen.generation[scenario.no == min.scenario]

# geographic decomposition
int.gen.generation = merge(int.gen.generation,decomposition.key,by.x = 'name',by.y = 'Generator')
int.gen.generation = int.gen.generation[Region == scenario.name]

gc()
# 3. generator pump load

interval.gen.pump <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  interval.gen.pump <- rbind(interval.gen.pump,
                                expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property, region 
                                FROM Generator_PumpLoad 
                                WHERE collection IS 'Generator' AND 
                                property IS 'Pump Load' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i]])
}

# check that all pumps are single region

dual.region.generators = make_decomposition_key()[['dual.region.generators']]
dual.region.pumps = interval.gen.pump[name %in% dual.region.generators]

if(nrow(dual.region.pumps)>0){
  message('some pumps connect to multiple regions')
}

# time decomposition
int.gen.pump = copy(interval.gen.pump)
int.gen.pump[,c('scenario.name','scenario.no'):=tstrsplit(scenario,"_")]
int.gen.pump[,scenario.no:=as.numeric(scenario.no)]
int.gen.pump[,min.scenario:=min(scenario.no),by = c('name','time','scenario.name')]
int.gen.pump = int.gen.pump[scenario.no == min.scenario]

# interval region pump
int.region.pump = int.gen.pump[,lapply(.SD,sum),by = c('region','time','scenario'),
                               .SDcols = 'value']
setnames(int.region.pump,c('region','value'),c('name','pump'))

# geographic decomposition
int.gen.pump = merge(int.gen.pump,decomposition.key,by.x = 'name',by.y = 'Generator')
int.gen.pump = int.gen.pump[Region == scenario.name]

gc()
# 3. region load

interval.region.load <- data.table()
for(i in 1:length(scenario.names)){
  print(i)
  interval.region.load <- rbind(interval.region.load,
                              expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                              sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                              FROM Region_Load 
                              WHERE collection IS 'Region' AND 
                              property IS 'Load' AND
                              phase_id IS 4")) %>% 
                              dplyr::mutate(scenario = scenario.names[i]) %>% 
                              collect(n = Inf)),solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i]])
}

# time partitions
int.region.load = copy(interval.region.load)
int.region.load[,c('scenario.name','scenario.no'):=tstrsplit(scenario,"_")]
int.region.load[,scenario.no:=as.numeric(scenario.no)]
int.region.load[,min.scenario:=min(scenario.no),by = c('name','time','scenario.name')]
int.region.load = int.region.load[scenario.no == min.scenario]

gc()
# ----------------------------------------------------------------------- |
# Calculations on SEAMS results ----
# ----------------------------------------------------------------------- |

# nodal injections

gen.table = merge(int.gen.generation,GPFs[,.(name,Node,GPF = value)],
                  by = 'name',allow.cartesian = TRUE)

gen.table[,generation:=GPF*value]

time.vector = unique(gen.table[,time])

setwd(output.dir)

if(FALSE){
for(i in seq(length(time.vector))){
  
  gen.temp = gen.table[time == time.vector[i],.(BUS = Node,GEN = name,VALUE = generation)]
  write.csv(gen.temp,paste0('GEN_',i,".csv"),row.names = FALSE)
  
}
}

# nodal withdrawals

# subtract region pump load from region load

load.table = int.region.load[,.(name,time,scenario,scenario.name,scenario.no,value)]
load.table = merge(load.table,int.region.pump,by = c('name','time','scenario'),all = TRUE)
load.table = load.table[!is.na(value)] # Diamond Lake generators do not have outputs for at least the 20th partition
load.table[is.na(pump),pump:=0]

load.table[,value:=value - pump]
load.table = unique(load.table[,.(name,time,scenario.name,value)])

load.table.east = load.table[!(scenario.name %in% c('CAISO','NT','CG','West'))]
load.table.west = load.table[(scenario.name %in% c('CAISO','NT','CG','West'))]

load.table.east = data.table(dcast(load.table.east,name + time ~ scenario.name,
                                  value.var = 'value'))
load.table.west = data.table(dcast(load.table.west,name + time ~ scenario.name,
                                   value.var = 'value'))

# write.csv(load.table.east,'load_table_east.csv',row.names = FALSE) # checked by hand
# write.csv(load.table.west,'load_table_west.csv',row.names = FALSE)

load.table = rbind(load.table[scenario.name == 'MISO'],
                   load.table[scenario.name == 'CAISO'])

setnames(load.table,'name','region')

LPF.cast = data.table(dcast(node.LPFs,Node + region ~ Timeslice,value.var = 'LPF'))
LPF.cast = LPF.cast[`2024_Autumn_WI` > 0 |
                    `2024_Spring_WI` > 0 |
                    `2024_Winter_WI` > 0 |
                    `2024_Summer_WI` > 0]

load.table = merge(load.table,LPF.cast,by = 'region',allow.cartesian = TRUE)

load.table[,month:=month(time)]
load.table[month %in% c(12,1,2),load:=value * `2024_Winter_WI`]
load.table[month %in% c(3,4,5),load:=value * `2024_Spring_WI`]
load.table[month %in% c(6,7,8),load:=value * `2024_Summer_WI`]
load.table[month %in% c(9,10,11),load:=value * `2024_Autumn_WI`]
load.table = load.table[,.(Node,time,load)]

pump.table = merge(int.gen.pump,GPFs[,.(name,Node,GPF = value)],
                  by = 'name',allow.cartesian = TRUE)

pump.table[,pump:=GPF*value]
pump.table = pump.table[,lapply(.SD,sum),by = c('Node','time'),.SDcols = c('pump')]

load.table = merge(load.table,pump.table,all = TRUE,by = c('Node','time'))

sum(pump.table[,pump])
sum(load.table[,load])
asdf.3 = load.table[demand<0]

load.table[is.na(pump),pump:=0]
load.table[is.na(load),load:=0]
load.table[,demand:=load+pump]

time.vector = unique(load.table[,time])

load.table[demand < 0,demand:=0] # machine precision

setwd(output.dir)

if(FALSE){
for(i in seq(length(time.vector))){
  
  load.temp = load.table[time == time.vector[i],.(BUS = Node,VALUE = demand)]
  write.csv(load.temp,paste0('LOAD_',i,".csv"),row.names = FALSE)
  
}
}


