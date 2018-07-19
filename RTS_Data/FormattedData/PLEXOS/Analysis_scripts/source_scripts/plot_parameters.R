
# ----------------------------------------------------------------------- |
# functions ----
# ----------------------------------------------------------------------- |

# A. Expand time function
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

# B. Error handling
error_handler <- function(query){
  result <- tryCatch(query,error = function(cond) { return('ERROR') } )
  if(class(result)[1] != "character"){
    if(nrow(result) == 0) {result <- 'ERROR'}
  }
  return(result)
}

# ----------------------------------------------------------------------- |
# plot theme ----
# ----------------------------------------------------------------------- |

# set ggplot theme - make background nicer
theme_set(theme_bw())

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


