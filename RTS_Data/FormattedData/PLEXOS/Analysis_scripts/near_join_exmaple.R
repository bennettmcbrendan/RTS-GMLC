

# example 1
# flow table gives ranked interface 'flow' points
# range table gives equidistant 'range' points for mapping
# each range point identifies with the nearest flow point and assumes its rank

range.table = data.table(name = 'namen',value=seq(-1,1,0.1))
flow.table = data.table(name = 'namen',value = runif(21, min = -1,max = 1))
flow.table[,rank:=frank(value,ties.method = 'first')/nrow(flow.table)]

setkeyv(range.table,c('name','value'))
setkeyv(flow.table,c('name','value'))

KSI.table = flow.table[range.table,roll = "nearest"]

# example 2
# flow table gives ranked interface 'flow' points
# range table gives equidistant 'range' points for mapping
# each range point identifies with the nearest flow point and assumes its rank
# problem: there are two flow points exactly at the same range endpoint and the range
# point identifies with both

range.table = data.table(name = 'namen',value=seq(-1,1,0.1))
flow.table = data.table(name = 'namen',value = runif(21, min = -1,max = 1))
flow.table[1,value:= -1]
flow.table[2,value:= -1]
flow.table[10,value:=-0.000005] # note 
flow.table[11,value:=0.000005] # note
flow.table[12,value:=0.1] # note
flow.table[13,value:=0.1] # note
flow.table[20,value:=1]
flow.table[21,value:=1]
flow.table[,rank:=frank(value,ties.method = 'first')/nrow(flow.table)]

setkeyv(range.table,c('name','value'))
setkeyv(flow.table,c('name','value'))

KSI.table = flow.table[range.table,roll = "nearest"]

# solution: take max rank for each value
KSI.table = unique(KSI.table[,rank:=max(rank),by = c('name','value')])

# note that if a range point is equidistant from two flow points which are not endpoints, it identifies
# with the lower of the two
# dissimilarly, if a range point is equal to two flow points which are not endpoints, it identifies with
# the higher of the two