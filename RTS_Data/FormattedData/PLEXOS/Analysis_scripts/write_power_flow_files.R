
# ----------------------------------------------------------------- |
# Inputs ----
# ----------------------------------------------------------------- |
pacman::p_load(rplexos, RSQLite, magrittr, dplyr, lubridate, 
               rmarkdown, scales, cowplot, data.table, fasttime,
               Hmisc, gridExtra, rgdal, ggmap, Cairo, rgeos, maptools, 
               lubridate, plyr, gdata, stringr, tidyr, dtplyr, knitr,grid)

wd = "//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS"
setwd(wd)

solution.dbs = solution.dbs = c(file.path(wd,"Model DAY_AHEAD_B1 Solution",
                                        "Model DAY_AHEAD_B1 Solution-rplexos.db"),
                                file.path(wd,"Model DAY_AHEAD_B2 Solution",
                                          "Model DAY_AHEAD_B2 Solution-rplexos.db"),
                                file.path(wd,"Model DAY_AHEAD_B3 Solution",
                                          "Model DAY_AHEAD_B3 Solution-rplexos.db"))
  
  
scenario.names = c("1","2","3")

mapping.table = fread("../../SourceData/gen.csv")[,.(name = `GEN UID`,bus = `Bus ID`)]
  
output.dir = "//nrelqnap01d/PLEXOS/Projects/GMLC-MSPCM/geo-decomp-data"


# ----------------------------------------------------------------- |
# Functions ----
# ----------------------------------------------------------------- |

pacman::p_load(rplexos, RSQLite, magrittr, dplyr, lubridate, 
               rmarkdown, scales, cowplot, data.table, fasttime,
               Hmisc, gridExtra, rgdal, ggmap, Cairo, rgeos, maptools, 
               lubridate, plyr, gdata, stringr, tidyr, dtplyr, knitr,grid)

expand_time = function(data, db.path, phaseid = 4, look.ahead = 0) {
  setnames(data, "time_from", "time")
  data$time = ymd_hms(data$time)
  datadt = data.table(data, key = "key,time")
  
  # get time from database being queried
  timedt = data.table(tbl(src_sqlite(db.path), sql("SELECT * FROM time")) %>% 
                        filter(phase_id == phaseid) %>% collect())
  timedt$time = ymd_hms(timedt$time)  #R format time
  
  # drop extra look ahead days
  keep.days = unique(timedt[,.(day = floor_date(time, unit = "day"))])
  keep.days = keep.days[1:(nrow(keep.days) - look.ahead)]
  timedt = timedt[floor_date(time, unit = "day") %in% keep.days$day,]
  
  # Expand data
  cj2 = CJ(key = unique(datadt$key), time = timedt$time)
  cj3 = datadt[cj2, roll = TRUE]
  
  # Restore timezone (UTC)
  attributes(cj3$time) = attributes(timedt$time)
  cj3 = cj3[, `:=`(time_to, NULL)]
  
  cj3
}



error_handler <- function(query){
  result <- tryCatch(query,error = function(cond) { return('ERROR') } )
  if(class(result)[1] != "character"){
    if(nrow(result) == 0) {result <- 'ERROR'}
  }
  return(result)
}

# ----------------------------------------------------------------- |
# Queries ----
# ----------------------------------------------------------------- |

interval.gen.generation <- data.table()
for(i in 1:length(scenario.names)){
  interval.gen.generation <- error_handler(rbind(interval.gen.generation,
                                    expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                    sql("SELECT key, name, category, time_from, time_to, value, property, region 
                                    FROM Generator_Generation 
                                    WHERE collection IS 'Generator' AND 
                                    property IS 'Generation' AND
                                    phase_id IS 4")) %>% 
                                    dplyr::mutate(scenario = scenario.names[i]) %>% 
                                    collect(n = Inf)),solution.dbs[i])))
}

interval.node.load <- data.table()
for(i in 1:length(scenario.names)){
  interval.node.load <- error_handler(rbind(interval.node.load,
                                    expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                    sql("SELECT key, name, category, time_from, time_to, value, property 
                                    FROM Node_Load 
                                    WHERE collection IS 'Node' AND 
                                    property IS 'Load' AND
                                    phase_id IS 4")) %>% 
                                    dplyr::mutate(scenario = scenario.names[i]) %>% 
                                    collect(n = Inf)),solution.dbs[i])))
}

interval.node.load[,region:=substr(name,1,1)]

interval.node.dump <- data.table()
for(i in 1:length(scenario.names)){
  interval.node.dump <- error_handler(rbind(interval.node.dump,
                                    expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                    sql("SELECT key, name, category, time_from, time_to, value, property
                                    FROM Node_DumpEnergy 
                                    WHERE collection IS 'Node' AND 
                                    property IS 'Dump Energy' AND
                                    phase_id IS 4")) %>% 
                                    dplyr::mutate(scenario = scenario.names[i]) %>% 
                                    collect(n = Inf)),solution.dbs[i])))
}

interval.node.unserved <- data.table()
for(i in 1:length(scenario.names)){
  interval.node.unserved <- error_handler(rbind(interval.node.unserved,
                                    expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                    sql("SELECT key, name, category, time_from, time_to, value, property
                                    FROM Node_UnservedEnergy 
                                    WHERE collection IS 'Node' AND 
                                    property IS 'Unserved Energy' AND
                                    phase_id IS 4")) %>% 
                                    dplyr::mutate(scenario = scenario.names[i]) %>% 
                                    collect(n = Inf)),solution.dbs[i])))
}

# extract desired region-scenario pairs

interval.node.load = interval.node.load[region == scenario]
interval.gen.generation = interval.gen.generation[region == scenario]

# ----------------------------------------------------------------- |
# Warnings ----
# ----------------------------------------------------------------- |

if(nrow(interval.node.dump[value>0])>0){
  message("...models have dump energy. will distort generation")
}

if(nrow(interval.node.unserved[value>0])>0){
  message("...models have unserved energy. will distort load")
}

# ----------------------------------------------------------------- |
# Delete current contents of output dir ----
# ----------------------------------------------------------------- |


# ----------------------------------------------------------------- |
# Write load ----
# ----------------------------------------------------------------- |

load.table = interval.node.load[,lapply(.SD,sum),by = c('name','time'),.SDcols = 'value']

setnames(load.table,c('name','value'),c('BUS','VALUE'))
setkeyv(load.table,c('time','BUS'))

setwd(output.dir)
time.vector = unique(load.table[,time])

for(i in seq(length(time.vector))){
  
  load.temp = load.table[time == time.vector[i],.(BUS,VALUE)]
  write.csv(load.temp,paste0('LOAD_',i,".csv"),row.names = FALSE)
  
}


# ----------------------------------------------------------------- |
# Write generation ----
# ----------------------------------------------------------------- |


gen.table = interval.gen.generation[,lapply(.SD,sum),by = c('name','time'),.SDcols = 'value']

gen.table = merge(gen.table[,.(name,time,value)],
                       mapping.table,
                       by = c('name'))

gen.table = gen.table[,.(BUS = bus,time,VALUE = value)]

setkeyv(gen.table,c('time','BUS'))

setwd(output.dir)
time.vector = unique(gen.table[,time])

for(i in seq(length(time.vector))){
  
  gen.temp = gen.table[time == time.vector[i],.(BUS,VALUE)]
  write.csv(gen.temp,paste0('GEN_',i,".csv"),row.names = FALSE)
  
}
