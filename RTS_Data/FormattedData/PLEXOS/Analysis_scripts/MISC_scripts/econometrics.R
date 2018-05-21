
int.congest = data.table(dcast(line.flow.limits,name ~ property,
                               value.var = 'value'))


decomposed.line.flow <- data.table()
for(i in 1:length(decomposed.names)){
  print(i)
  decomposed.line.flow <- rbind(decomposed.line.flow,
                              expand_time(data.table(tbl(src_sqlite(decomposed.db[i]), 
                                sql("SELECT key, name, parent, category, time_from, time_to, value, property 
                                FROM Line_Flow 
                                WHERE collection IS 'Line' AND 
                                property IS 'Flow' AND
                                phase_id IS 4")) %>% 
                                dplyr::mutate(scenario = decomposed.names[i]) %>% 
                                collect(n = Inf)),decomposed.db[i]))
}

int.congest = merge(int.congest,
                    rbind(decomposed.line.flow,interval.line.flow),
                    by = 'name')

int.congest[,congest:=0]
int.congest[(value > `Export Limit` - 0.001 |
            value < `Import Limit` - 0.001),congest:=1]

int.congest = int.congest[,.(name,time,scenario,congest)]

int.congest[scenario %in% c(1,2,3),scenario:='Decomposed']

# for now just apply max
int.congest = int.congest[,lapply(.SD,max),by = c('name','time','scenario'),
                          .SDcols = 'congest']

int.congest = data.table(dcast(int.congest,time + scenario ~ name,
                               value.var = 'congest'))

int.congest = merge(int.congest,period.table,by = 'time')





# ------------------------------------------------------------------ |
# econometrics - Model 1 ----
# ------------------------------------------------------------------ |

# exclude extreme outliers
reg.table = compare.table[abs(Price) < 100]

reg.table = merge(reg.table,int.congest,
                  by = c('scenario','Period'))

reg.table.cast = data.table(dcast(reg.table,Period + time + interface ~ scenario,
                                  value.var = 'Price'))

reg.col.names = names(reg.table)[!grepl("Flow|Price")]

# ------------------------------------------------------------------ |
# econometrics - Model 2 ----
# ------------------------------------------------------------------ |

nodes.CJ = CJ(unique(interval.decomposed.price[,name]),
              unique(interval.decomposed.price[,name]),
              unique(interval.decomposed.price[,time]))
setnames(nodes.CJ,c('V1','V2','V3'),c('Node.1','Node.2','time'))
nodes.CJ = nodes.CJ[Node.1 != Node.2]
nodes.CJ = merge(nodes.CJ,period.table,by = 'time')

int.decomposed.price = interval.decomposed.price[region == scenario]
int.decomposed.price[,scenario:='Decomposed']

int.decomposed.dP = merge(nodes.CJ,
                          int.decomposed.price[,.(Node.1 = name,scenario,time,Price.1 = value)],
                          by = c('Node.1','time'))

int.decomposed.dP = merge(int.decomposed.dP,
                          int.decomposed.price[,.(Node.2 = name,scenario,time,Price.2 = value)],
                          by = c('Node.2','time','scenario'))

int.nondecomposed.dP = merge(nodes.CJ,
                             interval.node.price[,.(Node.1 = name,scenario,time,Price.1 = value)],
                             by = c('Node.1','time'))

int.nondecomposed.dP = merge(int.nondecomposed.dP,
                             interval.node.price[,.(Node.2 = name,scenario,time,Price.2 = value)],
                             by = c('Node.2','time','scenario'))

int.dP = rbind(int.decomposed.dP,int.nondecomposed.dP)
int.dP[,dP:=abs(Price.2 - Price.1)]
int.dP[,region.dummy:=ifelse(substr(Node.1,1,1) == substr(Node.2,1,1),0,1)]
int.dP[,decomp.dummy:=ifelse(scenario == "Non-decomposed",0,1)]

reg = lm(dP ~ decomp.dummy + region.dummy + region.dummy*decomp.dummy,
         data = int.dP)

summary(reg)


