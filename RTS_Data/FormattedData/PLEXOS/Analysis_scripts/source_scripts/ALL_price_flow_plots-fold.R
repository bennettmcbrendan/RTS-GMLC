
folded_price_flow_plots = function(plot.data = data.table(),
                                   interfaces.to.plot = c()){

    int.fold = copy(plot.data)
    int.fold[,Price.Fold:=Price]
    
    int.fold[Interchange * Price > 0,Price.Fold:=(-1)*abs(Price.Fold)]
    int.fold[Interchange * Price < 0,Price.Fold:=(1)*abs(Price.Fold)]
    int.fold[,Interchange:=abs(Interchange)]
    
    # C. price-flow plot
    xlim.fold = c(0,7000)
    ylim.fold = c(-100,100)
    
    plot.text.fold = data.table(Interface = "PJM - MISO",
                           x = c(3500,3500),
                           y = c(70,-70),
                           text = c('Under-utilized','Counterintuitive'))
    
    int.fold$Interface = factor(int.fold$Interface,levels = interfaces.to.plot)
    plot.text.fold$Interface = factor(plot.text.fold$Interface,levels = interfaces.to.plot)
    
    p <- ggplot() + stat_binhex(data = int.fold,
                                 aes(x = Interchange,y = Price,color = ..density..*100,fill = ..density..*100),
                                 binwidth = c(100,3)) + 
        scale_fill_gradientn(name = "Pct.",colours = c('gray80','gray20','black'),
                             guide = 'colourbar') + 
        scale_color_gradientn(name = "Pct.",colours = c('gray80','gray20','black'),
                              guide = 'colourbar') + 
        geom_label(data = plot.text.fold,aes(x = x,y = y,label = text),color = 'black',fill = 'lightblue',size = 2.4) + 
        geom_vline(xintercept = 0,size = 0.2,color = 'darkred') +
        geom_hline(yintercept = 0,size = 0.2,color = 'darkred') +
        facet_grid(Interface ~ .) + plot_theme + theme(legend.title = element_text()) + 
        labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
        coord_cartesian(xlim = xlim.fold,ylim = ylim.fold) 
    
    # ggsave("EI_ISO_fold.png",p,height = 5.5,width = 3.5)
    # ggsave("EI_single-opt-border_fold.png",p,height = 5.5,width = 3.5)
    # ggsave("EI_multi-opt-border_fold.png",p,height = 5.5,width = 3.5)
    
    return(p)
}