
pacman::p_load(rplexos, RSQLite, magrittr, dplyr, lubridate, 
               rmarkdown, scales, cowplot, data.table, fasttime,
               Hmisc, gridExtra, rgdal, ggmap, Cairo, rgeos, maptools, 
               lubridate, plyr, gdata, stringr, tidyr, dtplyr, knitr,grid)

setwd(dirname(parent.frame(2)$ofile))
setwd("..")
solutions.dir = getwd()

solution.files = list.files()[grepl('Model DAY_AHEAD_A',list.files())]

for(i in 1:length(solution.files)){
  
  process_folder(solution.files[i])

  }


da.solutions = c('Model DAY_AHEAD_A Solution')

scenario.names <- c("Day Ahead LP")

da.solution.paths <- sapply(da.solutions, function(x) file.path(solutions.dir,x))

da.solution.dbs <- sapply(da.solution.paths, function(x){
  file.path(x, list.files(x, pattern = "-rplexos.db"))})

# query function
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


generator.reserves <- data.table()

for(i in 1:length(scenario.names)){
  
  generator.reserves <- rbind(generator.reserves,
                                expand_time(data.table(tbl(src_sqlite(da.solution.dbs[i]), 
                                sql("SELECT * 
                                FROM ReserveGenerators_Provision 
                                WHERE phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                collect(n = Inf)),da.solution.dbs[i]))
}

setwd("../../timeseries_data_files/Reserves")
reserve.products = unique(generator.reserves[,parent])[!grepl("Spin_Up",unique(generator.reserves[,parent]))]
region.names = unique(generator.reserves[,region])

for(ii in 1:length(reserve.products)){
  
  for(jj in 1:length(region.names)){
    
    gen.reserves.temp = generator.reserves[parent == reserve.products[ii] &
                                          region == region.names[jj]]
    
    gen.reserves.temp = gen.reserves.temp[,lapply(.SD,sum),by = c('time'),
                                          .SDcols = 'value']
    
    gen.reserves.temp[,Year:=year(time)]
    gen.reserves.temp[,Month:=month(time)]
    gen.reserves.temp[,Day:=day(time)]
    gen.reserves.temp[,Hour:=hour(time) + 1]
    gen.reserves.temp[,value:=round(value,3)]
    gen.reserves.temp = data.table(dcast(gen.reserves.temp,Year + Month + Day ~ Hour,
                                         value.var = 'value'))
    
    write.csv(gen.reserves.temp,
              paste0("DAY_AHEAD_regional_",reserve.products[ii],"_R",region.names[jj],".csv"),
              row.names = FALSE)
    
    
  }
  
}