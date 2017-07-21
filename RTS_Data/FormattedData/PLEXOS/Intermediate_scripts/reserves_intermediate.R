
# calculate each region's share of reserves

setwd("//plexossql/Data/bmcbenne/RTS-GMLC-decomp/RTS-GMLC/RTS_Data/SourceData")

maxcap.table = fread('gen.csv')[,.(`GEN UID`,`PMax MW`,Category)]
maxcap.table[,region := as.numeric(substr(`GEN UID`,1,1))]

reserves.table = fread('reserves.csv')[,.(`Reserve Product`,`Elegible Gen Categories`)]
reserves.table[,`Elegible Gen Categories` := gsub("\\(",'',`Elegible Gen Categories`)]
reserves.table[,`Elegible Gen Categories` := gsub("\\)",'',`Elegible Gen Categories`)]
reserves.table = cbind(reserves.table, 
                       setDT(tstrsplit(reserves.table$`Elegible Gen Categories`,",")))[,`Elegible Gen Categories`:=NULL]
reserves.table = melt(reserves.table,id.vars = 'Reserve Product',value.name = 'Elegible Gen Categories')[,variable:=NULL]

reserves.table = merge(reserves.table,
                       maxcap.table,
                       by.x = "Elegible Gen Categories",
                       by.y = "Category",
                       allow.cartesian = TRUE)

setnames(reserves.table,'PMax MW','regional_capacity')

reserves.table = reserves.table[,lapply(.SD,sum),by = c("Reserve Product","region"),
                                .SDcols = "regional_capacity"]

reserves.table[,regional_fraction := regional_capacity/sum(regional_capacity),by = c('Reserve Product')]

setkeyv(reserves.table,c('Reserve Product','region'))

# apply regional shares to timeseries data files


setwd('//plexossql/Data/bmcbenne/RTS-GMLC-decomp/RTS-GMLC/RTS_Data/timeseries_data_files/Reserves')

reserve.data.files = list.files()[grepl('DAY_AHEAD',list.files())]
region.list = unique(reserves.table[,region])

for(ii in 1:length(reserve.data.files)){
  
  reserve.provision = fread(reserve.data.files[ii])
  
  for(jj in 1:length(region.list)){
    
    fraction = reserves.table[`Reserve Product` == gsub('DAY_AHEAD_regional_|.csv','',reserve.data.files)[ii] &
                              region == region.list[jj],regional_fraction]
    
    cols.to.divide = names(reserve.provision)[grepl("[0-9]",names(reserve.provision))]
    print(cols.to.divide)
    
    reserve.provision[,eval(cols.to.divide) := lapply(.SD, function(x) x*fraction),
                               by = c(''),
                               .SDcols = cols.to.divide]
    
    write.csv(reserve.provision,paste0(strsplit(reserve.data.files[ii],'.csv')[[1]],
                                       "_R",
                                       region.list[[jj]],
                                       ".csv"),
              row.names = FALSE)
    
    rm(fraction)
    }
  

}
