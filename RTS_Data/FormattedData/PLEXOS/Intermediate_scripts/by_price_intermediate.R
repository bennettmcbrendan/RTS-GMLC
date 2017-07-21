

setwd("//plexossql/Data/bmcbenne/RTS-GMLC-decomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Model DAY_AHEAD_A Solution")

id2name = fread('id2name.csv')

id2name = id2name[class != "Market"]
id2name[class == 'Node',class:="Market"]

write.csv(id2name,'id2name.csv',row.names = FALSE)


