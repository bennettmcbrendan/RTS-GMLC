

regression.output = data.table(Interface = c('PJM - \nMISO','PJM - \nNYISO','NYISO - \nISO-NE'),
                               Internal = c(1,4,6),
                               Border = c(1,0,1),
                               Internal.scenario = c(1,4,2),
                               Border.scenario = c(2,2,3))

regression.output = data.table(melt(regression.output,id.vars = c('Interface')))
regression.output[,scenario:=ifelse(grepl('scenario',variable),'gamma','alpha')]
regression.output[,variable:=gsub('.scenario','',variable)]

regression.output$Interface = factor(regression.output$Interface,levels = c('PJM - \nMISO','PJM - \nNYISO','NYISO - \nISO-NE'))
regression.output$variable = factor(regression.output$variable,levels = c('Border','Internal'))

p <- ggplot() + geom_bar(data = regression.output,aes(x = Interface,y = value,fill = variable),color = 'black',stat = 'identity') +
  scale_fill_manual(values = c('Internal' = 'navyblue','Border' = 'darkorange')) +
  facet_grid(scenario~.,scales = 'fixed') + labs(x = '',y = 'Significant constraints') + 
  scale_y_continuous(breaks = c(1,3,5,7)) + plot_theme + theme(legend.position = 'none')

ggsave('regression-coefficients.png',p,height = 2.75,width = 3.5)
