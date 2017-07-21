

setwd("//plexossql/Data/bmcbenne/RTS-GMLC-decomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Model DAY_AHEAD_A Solution/interval")

interregional.lines = c('AB1',
                        'AB2',
                        'AB3',
                        'CA-1',
                        'CB-1',
                        '113_316_1')


flow.files = list.files()[grepl('Flow.csv',list.files())]

for(ii in 1:length(flow.files)){
  
  flow.temp = fread(flow.files[ii])
  
  flow.temp[,VALUE.POS:=ifelse(VALUE>0,VALUE,0)]
  flow.temp[,VALUE.NEG:=ifelse(VALUE<0,(-1)*VALUE,0)]
  
  new.name = strsplit(flow.files[ii],'.csv')[[1]]
  
  write.csv(flow.temp[,.(DATETIME,VALUE = VALUE.POS)],
            paste0(new.name,'_pos.csv'),
            row.names = FALSE)
  
  write.csv(flow.temp[,.(DATETIME,VALUE = VALUE.NEG)],
            paste0(new.name,'_neg.csv'),
            row.names = FALSE)
  
}

setwd('..')

id2name = fread('id2name.csv')

id2name.generator = id2name[class == "Line" & name %in% interregional.lines]
id2name.generator[,class:="Generator"]
id2name.generator[,name:=paste0(name,"_G")]

id2name.purchaser = id2name[class == "Line" & name %in% interregional.lines]
id2name.purchaser[,class:="Purchaser"]
id2name.purchaser[,name:=paste0(name,"_P")]

id2name = rbind(id2name[class != "Line"],
                id2name.purchaser,
                id2name.generator
                )

setkeyv(id2name,c('class','id','name'))

write.csv(id2name,'id2name.csv',row.names = FALSE)


