
tx.status = 'HURDLE'

# assume loading of RTS_price_flow_plots.R environment 

lim = ylim[2]

if(tx.status == 'noTX'){

    # add out-of-range data
    int.fold = rbind(int.fold,
                     data.table(Interface = c(rep(c('1 - 2'),10),rep(c('1 - 3'),10),rep(c('2 - 3'),10)),
                                Period = 9000,
                                scenario = 'Single-operator',
                                From = 0,To = 0,Price = rep(c(1010,1020,1030,1040,1050,1060,1070,1080,1090,2000),3),pct = 0,Flow = -200))

}

scenario.names = unique(int.fold[,scenario])

for(jj in seq(length(scenario.names))){
    
    int.fold.temp = int.fold[scenario == scenario.names[jj]]
    scenario.temp = paste0(scenario.names[jj]," ",tx.status)
    int.fold.temp[,scenario:=scenario.temp]
    
    fold.text = data.table(Interface = "1 - 2",
                           scenario = c(rep(scenario.temp,4)),
                           x = rep(c(200,-225,-225,200),1),y = rep(c(75,75,-75,-75),1),
                           text = c(rep(c('Counter-intuitive','Under-utilized'),2)))
    
    p1 <- ggplot() + stat_binhex(data = int.fold.temp,
                                 aes(x = Flow,y = Price,color = ..count..),binwidth = c(15,7.5)) + 
        scale_fill_gradientn(name = "Hours",colours = c('gray80','lightpink','darkred'),
                             guide = 'colourbar') + 
        scale_color_gradientn(name = "Hours",colours = c('gray80','lightpink','darkred'),
                              guide = 'colourbar') + 
        geom_label(data = fold.text,aes(x = x,y = y,label = text),color = 'black',fill = 'lightblue',size = 2.4) + 
        geom_vline(xintercept = 0,size = 0.01,color = 'black') +
        geom_hline(yintercept = 0,size = 0.01,color = 'black') +
        facet_grid(Interface~scenario) + plot_theme + theme(legend.title = element_text()) + 
        labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') + coord_cartesian(xlim = xlim,ylim = ylim)

    filepath = paste0('UVIG_plots/RTS_price_flow_',scenario.temp,'.png')
    
    ggsave(filepath,p1,height = 5.5,width = 3.5)

}