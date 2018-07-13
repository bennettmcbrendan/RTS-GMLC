
# use border.buses table from SINGLE-OPT-BORDER_price_flow_plots or MULTI-OPT-BORDER_price_flow_plots

interface.lines = unique(border.buses[,name])

interval.line.flow <- data.table()
for(i in 1:length(scenario.names)){
    print(i)
    interval.line.flow <- rbind(interval.line.flow,
                                expand_time(data.table(tbl(src_sqlite(solution.dbs[i]), 
                                sql("SELECT key, name, category, time_from, time_to, value, property
                                FROM Line_Flow 
                                WHERE collection IS 'Line' AND 
                                property IS 'Flow' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = scenario.names[i]) %>% 
                                filter(name %in% interface.lines) %>%
                                collect(n = Inf)),
                                solution.dbs[i])[,time := fastPOSIXct(time, "UTC")][time <= model.ends[i] & time >= model.starts[i]])
}

int.line.flow = copy(interval.line.flow)[,.(name,time,value)]

int.line.flow = merge(int.line.flow,border.buses,by = c('name'))

int.line.flow = merge(int.line.flow,node.voltage[,.(Node.From = Node,Voltage.From = Voltage)],
                      by = c('Node.From'))

int.line.flow = merge(int.line.flow,node.voltage[,.(Node.To = Node,Voltage.To = Voltage)],
                      by = c('Node.To'))

int.line.flow[,Voltage:=pmin(Voltage.From,Voltage.To)]

line.flow = int.line.flow[,lapply(.SD,function(x) sum(abs(x))),
                          by = c('Interface','Voltage'),
                          .SDcols = c('value')]

line.flow[,pct:=100*value/sum(value),by = c('Interface')]
line.flow = unique(line.flow[,.(Interface,Voltage,pct)])
line.flow = data.table(dcast(line.flow,Interface~Voltage,value.var = c('pct')))

wd = "//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Analysis_scripts"
setwd(wd)

write.csv(line.flow,'multi-opt_flow_by_voltage.csv',row.names = FALSE)

