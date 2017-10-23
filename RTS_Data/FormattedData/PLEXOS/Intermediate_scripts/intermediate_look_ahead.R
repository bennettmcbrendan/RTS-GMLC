
pacman::p_load(data.table)

setwd(dirname(parent.frame(2)$ofile))
setwd("../Model DAY_AHEAD_A Solution/interval")


for(i in seq(length(list.files()))){
    
    file.temp = fread(list.files()[i])
    
    LA.day = file.temp[grepl('12/30/2020',DATETIME)]
    LA.day[,DATETIME:=gsub('12/30/2020','12/31/2020',DATETIME)]
    
    file.temp = rbind(file.temp,LA.day)
    
    write.csv(file.temp,list.files()[i],row.names = FALSE)
    
}