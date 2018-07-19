
pacman::p_load(ggplot2, data.table)

wd = dirname(parent.frame(2)$ofile)
setwd(wd)
source("source_scripts/plot_parameters.R")

regression.output = data.table(Interface = c('PJM - MISO','PJM - NYISO','NYISO - ISO-NE'),
                               Internal = c(1,4,6),
                               Border = c(1,0,1),
                               Internal.scenario = c(1,4,2),
                               Border.scenario = c(2,2,3))

regression.output = data.table(melt(regression.output,id.vars = c('Interface')))
regression.output[,scenario:=ifelse(grepl('scenario',variable),'gamma','alpha')]
regression.output[,variable:=gsub('.scenario','',variable)]

regression.output$Interface = factor(regression.output$Interface,levels = c('PJM - MISO','PJM - NYISO','NYISO - ISO-NE'))
regression.output$variable = factor(regression.output$variable,levels = c('Border','Internal'))

p <- ggplot() + geom_bar(data = regression.output,aes(x = scenario,y = value,fill = variable),color = 'black',stat = 'identity') +
  scale_fill_manual(values = c('Internal' = 'navyblue','Border' = 'darkorange')) +
  scale_x_discrete(labels = c('alpha' = expression(alpha),'gamma' = expression(gamma))) + 
  facet_grid(.~Interface,scales = 'fixed') + labs(x = '',y = 'Significant constraints') + 
  scale_y_continuous(breaks = c(1,3,5,7)) + plot_theme + theme(legend.position = 'none') +
  theme(strip.text = element_text(size=8))

ggsave('plots_regression/regression-coefficients.png',p,height = 2.25,width = 3.5,dpi = 600)
ggsave('plots_regression/regression-coefficients.emf',p,height = 2.25,width = 3.5,dpi = 600)