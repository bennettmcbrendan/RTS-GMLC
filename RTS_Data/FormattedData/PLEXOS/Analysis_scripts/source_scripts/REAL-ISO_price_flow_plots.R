
REAL_ISO_price_flow_plots = function(){

    pacman::p_load(rplexos, RSQLite, magrittr, dplyr, lubridate, 
                   rmarkdown, scales, cowplot, data.table, fasttime,
                   Hmisc, gridExtra, rgdal, ggmap, Cairo, rgeos, maptools, 
                   lubridate, plyr, gdata, stringr, tidyr, dtplyr, knitr,grid)
    
    wd = "//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Analysis_scripts"
    setwd(wd)
    source("source_scripts/plot_parameters.R")
    source("source_scripts/ALL_price_flow_plots-fold.R")
    
    # ----------------------------------------------------------------------- |
    # Inputs ----
    # ----------------------------------------------------------------------- |
    
    xlim = c(-7000,7000)
    ylim = c(-125,125)
    
    # interface and area map
    SEAMS.interfaces <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                  "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/EI_interfaces.csv")
    
    # Clayton's EIA scrubbed data
    ISO.data <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                          "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/data/real-world-iso-efficiency-data.csv")
    
    # ----------------------------------------------------------------------- |
    # Plot inputs ----
    # ----------------------------------------------------------------------- |
    
    SEAMS.interfaces = fread(SEAMS.interfaces)
    interfaces.to.plot = SEAMS.interfaces[,Interface]
    ISO.From = SEAMS.interfaces[,ISO.From]
    ISO.To = SEAMS.interfaces[,ISO.To]
    color.code = SEAMS.interfaces[,color]
    names(color.code) = interfaces.to.plot
    
    # ----------------------------------------------------------------------- |
    # Calculations real data ----
    # ----------------------------------------------------------------------- |
    
    ISO.data = fread(ISO.data)
    
    ISO.data = ISO.data[,.(Period = timestamp,Interface = interface, Price = (-1)*dLMP, 
                           pct = abs(dLMP)/(from.lmp/2 + to.lmp/2),
                           Interchange = interfaceflows)]
    ISO.data[,Interface:=gsub("PJM-MISO","PJM - MISO",Interface)]
    ISO.data[,Interface:=gsub("PJM-NYISO","PJM - NYISO",Interface)]
    ISO.data[,Interface:=gsub("NYISO-ISONE","NYISO - ISO-NE",Interface)]
    
    int.comparison =  ISO.data[,.(Period,Interface,Interchange,Price,pct)]
    # remove each ISO's missing data from all
    int.comparison[,count:=.N,by = c('Period')]
    int.comparison = int.comparison[count == 3]
    int.comparison[,count:=NULL]
    
    # ----------------------------------------------------------------------- |
    # Plot ----
    # ----------------------------------------------------------------------- |
    
    options(scipen = 9999)
    
    # A. outlier data
    outlier.data = int.comparison[Interchange < xlim[1] |
                                    Interchange > xlim[2] |
                                    Price < ylim[1] |
                                    Price > ylim[2]]
    
    # B. statistics
    int.stat = copy(int.comparison)
    
    int.stat[,Price:=abs(Price)]
    
    # center of mass
    int.stat.center = int.stat[,lapply(.SD,mean),by = c('Interface'),
                               .SDcols = c('Interchange','Price')]
    
    # squared distance from center
    int.stat.squared = copy(int.stat)
    
    int.stat.squared[,`Interchange` :=(`Interchange` - mean(`Interchange`))^2]
    int.stat.squared[,`Price` :=(`Price` - mean(`Price`))^2]

    int.stat.squared = int.stat.squared[,lapply(.SD,mean),by = c('Interface'),
                                        .SDcols = c('Interchange','Price')]
    
    # regression
    trend.line = data.table(Interface = interfaces.to.plot)
    
    for(jj in seq(length(interfaces.to.plot))){
      
      trend.line[Interface == interfaces.to.plot[jj],
                 c('intercept','slope'):=as.list(coef(lm(Price~Interchange,data = int.comparison[Interface == interfaces.to.plot[jj]])))]
      
    }
    
    trend.endpoints = copy(int.comparison)
    trend.endpoints[,x:=min(Interchange),by = c('Interface')]
    trend.endpoints[,xend:=max(Interchange),by = c('Interface')]
    trend.endpoints = unique(trend.endpoints[,.(Interface,x,xend)])
    trend.endpoints[,x:=x+xlim[1]/8]
    trend.endpoints[,xend:=xend+xlim[2]/8]
    
    trend.line = merge(trend.line,trend.endpoints,by = c('Interface'))
    trend.line[,y:=intercept + slope * x]
    trend.line[,yend:=intercept + slope * xend]
    
    rm(trend.endpoints)
    
    # C. price-flow plot
    plot.text = data.table(Interface = "PJM - MISO",
                           x = c(4000,-4000,-4000,4000),
                           y = c(70,70,-70,-70),
                           text = rep(c('Agreement','Disagreement'),2))
    
    int.comparison$Interface = factor(int.comparison$Interface,levels = interfaces.to.plot)
    plot.text$Interface = factor(plot.text$Interface,levels = interfaces.to.plot)
    trend.line$Interface = factor(trend.line$Interface,levels = interfaces.to.plot)
    
    p1 <- ggplot() + stat_binhex(data = int.comparison,
                    aes(x = Interchange,y = Price,color = ..density..*100,fill = ..density..*100),
                    binwidth = c(200,3)) + 
      scale_fill_gradientn(name = "(%)",colours = c('gray80','gray20','black'),
                           guide = 'colourbar') + 
      scale_color_gradientn(name = "(%)",colours = c('gray80','gray20','black'),
                            guide = 'colourbar') + 
      geom_label(data = plot.text,aes(x = x,y = y,label = text),color = 'black',fill = 'goldenrod1',size = 2.4) + 
      geom_vline(xintercept = 0,size = 0.01,color = 'black') +
      geom_hline(yintercept = 0,size = 0.01,color = 'black') +
      # geom_segment(data = trend.line,aes(x = x,xend = xend,y = y,yend = yend),color = 'firebrick',linetype = 1,size = 1) + 
      facet_grid(Interface ~ .,scales = 'free') + plot_theme + theme(legend.title = element_text()) + 
      labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
      coord_cartesian(xlim = xlim,ylim = ylim) +
      theme(strip.background = element_rect(fill = "goldenrod1"))
    
    p2 <- folded_price_flow_plots(plot.data = copy(int.comparison),
                                  interfaces.to.plot = interfaces.to.plot)
    
    # D. quadrants
    quadrants = copy(int.comparison)
    
    quadrants[,c('quad.0','I','II','III','IV'):=0]
    quadrants[pct<0.01,quad.0:=1]
    quadrants[Price > 0 & Interchange > 0 & quad.0 == 0,I:=1]
    quadrants[Price >= 0 & Interchange <= 0 & quad.0 == 0,II:=1]
    quadrants[Price < 0 & Interchange < 0 & quad.0 == 0,III:=1]
    quadrants[Price <= 0 & Interchange >= 0 & quad.0 == 0,IV:=1]
    
    quadrants = quadrants[,lapply(.SD,mean),by = c('Interface'),
                          .SDcols = c('quad.0','I','II','III','IV')]
    
    quadrants[,Disagreement := II + IV]
    quadrants[,Agreement := I + III]
    
    quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface'),
                          .SDcols = c('quad.0','I','II','III','IV','Disagreement','Agreement')]
    
    # write quadrants csv and plot
    setwd(wd)
    # write.csv(quadrants,'plots/EI_ISO_efficiencies.csv',row.names = FALSE)
    # ggsave('plots/EI_ISO_hex.png',p,height = 5.5,width = 3.5)
    # write.csv(outlier.data,'plots/EI_ISO_outliers.csv',row.names = FALSE)

    output.list = list(plot.data = int.comparison,p1 = p1,p2 = p2,outlier.data = outlier.data,quadrants = quadrants,
                       stat.center = int.stat.center,stat.squared = int.stat.squared,trend.line = trend.line)
    
    output.list
    
}    
    