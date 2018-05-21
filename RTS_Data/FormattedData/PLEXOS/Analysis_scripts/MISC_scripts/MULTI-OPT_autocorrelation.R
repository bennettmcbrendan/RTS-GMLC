
# mean average interface flow magnitude
int.comparison[,lapply(.SD,function(x) mean(abs(x))),by = c('Interface'),.SDcols = c('Interchange')]

corr.table = data.table()
for(tau in 1:5){
  
  # one-interval price and flow autocorrelation
  setkeyv(int.comparison,c('Interface','Period'))
  int.correlation = copy(int.comparison)
  int.correlation[,scenario:=((Period - 1) %/% 168) + 1]
  int.correlation[,Interchange.shift:=shift(Interchange,tau,type = 'lag'),by = c('Interface','scenario')]
  int.correlation[,Price.shift:=shift(Price,tau,type = 'lag'),by = c('Interface','scenario')]
  int.correlation = int.correlation[!is.na(Interchange.shift)]
  int.correlation[,Price.corr := sum(Price * Price.shift)/sqrt(sum(Price^2)*sum(Price.shift^2)),by = c('Interface')]
  int.correlation[,Interchange.corr := sum(Interchange * Interchange.shift)/sqrt(sum(Interchange^2)*sum(Interchange.shift^2)),
                  by = c('Interface')]
  int.correlation = unique(int.correlation[,.(Interface,Price.corr,Interchange.corr,tau = tau,
                                              scenario = 'Single-operator')])

  corr.table = rbind(corr.table,int.correlation)

}
p <- ggplot() + geom_line(data = corr.table,aes(x = tau,y = Price.corr,color = Interface),size = 1.5) + 
  scale_color_manual(values = color.code) + facet_grid(.~scenario) + ylim(0,1) +
  plot_theme + labs(x = 'Hours',y = 'Price Autocorrelation')

ggsave('EI_multi-opt_autocorrelation.png',p,height = 2.5,width = 4.5)

