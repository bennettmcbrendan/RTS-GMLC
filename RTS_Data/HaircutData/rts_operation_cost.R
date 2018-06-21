
rts_operation_cost = function(rts.dir = file.path("//plexossql/Data/bmcbenne/RTS-GMLC-redispatch-price-taker/RTS-GMLC-FORK/RTS_Data/",
                                  "FormattedData/PLEXOS/Create_PLEXOS_database/1-parse-SourceData/outputs")){
  
  setwd(rts.dir)
  
  fuel.data = fread("fuel.data.csv")
  gen.cost.data = fread("gen.cost.data.csv")
  gen.cost.data.base = fread("gen.cost.data.base.csv")
  gen.map = fread("generator.data.csv")[,.(Generator,category)]
  
  gen.cost.data[,LP.shift:=shift(`Load Point`,type = 'lag'),by = c('Generator')]
  gen.cost.data[,MW:=ifelse(is.na(LP.shift),`Load Point`,`Load Point`-LP.shift)]
  gen.cost.data[,Cap:=max(`Load Point`),by = c('Generator')]
  gen.cost.data[,Heat:=`MW` * `Heat Rate Incr`]
  
  gen.cost.data = gen.cost.data[,lapply(.SD,function(x) sum(x)/1000),by = c('Generator','Cap'),
                                .SDcols = c('Heat')]
  
  gen.cost.data = merge(gen.cost.data,gen.cost.data.base,by = c('Generator'))
  gen.cost.data[,Heat:=Heat + `Heat Rate Base`]
  gen.cost.data[,`Heat Rate Base`:=NULL]
  
  gen.cost.data = merge(gen.cost.data,gen.map,by = c('Generator'))
  gen.cost.data[grepl('Oil',category),category:="Oil"]
  gen.cost.data[grepl('Gas',category),category:="NG"]
  gen.cost.data = merge(gen.cost.data,fuel.data,by.x = c('category'),
                        by.y = c('Fuel'))
  
  gen.cost.data[,var.cost:=Heat/Cap*Price]
  setkeyv(gen.cost.data,'var.cost')
  
  return(gen.cost.data)

}



