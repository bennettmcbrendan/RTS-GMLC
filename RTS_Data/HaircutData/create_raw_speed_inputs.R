
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

pacman::p_load(data.table)

source('rts_operation_cost.R')
gen.cost.data = rts_operation_cost()

this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

# -----------------------------------------------------------------------------|
# load ----
# -----------------------------------------------------------------------------|

load.template = fread("data_template/load.csv",header = TRUE)
load = fread("../timeseries_data_files/Load/DAY_AHEAD_regional_Load.csv",header = TRUE)

if(identical(names(load.template),names(load))){
  message('load column names match')
}
if(nrow(load.template) == nrow(load)){
  message('load rows match')
}

load.annual = load[,lapply(.SD,function(x) sum(x)/1000),by = c('Year'),
                   .SDcols = c('1','2','3')]
load.total = 1000*(load.annual[,`1`] + load.annual[,`2`] + load.annual[,`3`])

write.csv(load,'data/load.csv',row.names = FALSE)
write.csv(load.annual,'data_annual/load_annual.csv',row.names = FALSE)

# -----------------------------------------------------------------------------|
# interfaces ----
# -----------------------------------------------------------------------------|

write.csv(fread('data_template/interfaces.csv',header = TRUE),
                'data/interfaces.csv',row.names = FALSE)


# -----------------------------------------------------------------------------|
# config ----
# -----------------------------------------------------------------------------|

config = fread('data_template/config.csv',header = TRUE)
config[,`total annual load`:=load.total]
config[,`VG penetration`:='25%']

write.csv(config,'data/config.csv',row.names = FALSE)

# -----------------------------------------------------------------------------|
# vg ----
# -----------------------------------------------------------------------------|

vg.template = fread('data_template/vg.csv',header = TRUE)
print(names(vg.template))

vg.wind = fread("../timeseries_data_files/WIND/DAY_AHEAD_wind.csv",header = TRUE)
vg.pv = fread("../timeseries_data_files/PV/DAY_AHEAD_pv.csv",header = TRUE)
vg.rtpv = fread("../timeseries_data_files/RTPV/DAY_AHEAD_rtpv.csv",header = TRUE)
vg.hydro = fread("../timeseries_data_files/HYDRO/DAY_AHEAD_hydro.csv",header = TRUE)

vg = merge(vg.wind,vg.pv,by = c('Year','Month','Day','Period'))
vg = merge(vg,vg.rtpv,by = c('Year','Month','Day','Period'))
vg = merge(vg,vg.hydro,by = c('Year','Month','Day','Period'))

vg.annual = vg[,lapply(.SD,function(x) sum(x)/1000),by = c('Year'),
               .SDcols = names(vg)[!(names(vg) %in% c('Year','Month','Day','Period'))]]

vg.annual = data.table(melt(vg.annual,id.vars = c("Year")))
vg.annual = vg.annual[,.(genID = variable,GWh = value)]

write.csv(vg,'data/vg.csv',row.names = FALSE)
write.csv(vg.annual,'data_annual/vg_annual.csv',row.names = FALSE)

# -----------------------------------------------------------------------------|
# generators ----
# -----------------------------------------------------------------------------|

generators.template = fread('data_template/generators.csv',header = TRUE)
print(names(generators.template))

generators = fread('../SourceData/gen.csv',header = TRUE)

# modify
setnames(generators,'GEN UID','genID')
setnames(generators,'Bus ID','busID')
generators[,busID:=substr(busID,1,1)]
setnames(generators,'Category','gen cat')

generators = merge(generators,gen.cost.data[,.(genID = Generator,
                                               `operation cost` = var.cost)],
                   by = c('genID'),
                   all.x = TRUE)
generators[is.na(`operation cost`),`operation cost`:=0]

generators = merge(generators,fread('inputs/expansion_cost.csv',header = TRUE),
                   by = c('gen cat'),all.x = TRUE)
generators[is.na(`expansion cost`),`expansion cost`:=0]

setnames(generators,'PMin MW','min gen')
setnames(generators,'PMax MW','max gen')
generators[,`reserve factor`:=0]
generators[,`start up cost`:= (`Start Heat Cold MBTU` * `Fuel Price $/MMBTU`) + `Non Fuel Start Cost $`]
generators[,`shut down cost`:= (`Start Heat Cold MBTU` * `Fuel Price $/MMBTU`) + `Non Fuel Shutdown Cost $`]
generators[,`number on system`:=0]
generators[,VG:=ifelse(grepl('Solar|Wind|CSP',`gen cat`),TRUE,FALSE)]
generators[,exist:=FALSE]
generators[,expand:=TRUE]
generators = merge(generators,fread('inputs/re_buildout.csv',header = TRUE),by = 'genID',all = TRUE)
generators[`min new units` == 0,`max new units`:=0]
generators[`min new units` == 1,`max new units`:=1]
generators[is.na(`min new units`),`min new units`:=0]
generators[is.na(`max new units`),`max new units`:=1]
generators[,`provides reserves`:=0]

generators = merge(generators,vg.annual,by = c('genID'),all = TRUE)
generators[is.na(GWh),GWh:=`max gen`*8784/1000]
generators[,`capacity discount`:=GWh/(`max gen`*8784/1000)]
generators[is.nan(`capacity discount`),`capacity discount`:=1]

generators[,FOR:=100*FOR]
generators[,`gen type`:=`genID`]

generators = generators[,names(generators.template),with = FALSE]
setcolorder(generators,names(generators.template))

write.csv(generators,'data/generators.csv',row.names = FALSE)

# -----------------------------------------------------------------------------|
# gen units scenario ----
# -----------------------------------------------------------------------------|

