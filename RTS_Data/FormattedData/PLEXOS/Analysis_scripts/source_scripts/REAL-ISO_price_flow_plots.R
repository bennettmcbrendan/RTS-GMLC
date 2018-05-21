
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
    ylim = c(-100,100)
    
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
    
    ISO.data = ISO.data[,.(Period = timestamp,Interface = interface, Price = dLMP, 
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
    
    # B. ME table
    ME.table = int.comparison[abs(Price)<ylim[2]]
    ME.table = ME.table[,lapply(.SD, function(x) mean(abs(x))),by = c('Interface'),
                        .SDcols = 'Price']
    
    ME.table.all = copy(int.comparison)
    ME.table.all = ME.table.all[,lapply(.SD, function(x) mean(abs(x))),by = c('Interface'),
                        .SDcols = 'Price']
    
    # C. price-flow plot
    plot.text = data.table(Interface = "PJM - MISO",
                           x = c(4000,-4000,-4000,4000),
                           y = c(70,70,-70,-70),
                           text = rep(c('Counter-intuitive','Under-utilized'),2))
    
    int.comparison$Interface = factor(int.comparison$Interface,levels = interfaces.to.plot)
    plot.text$Interface = factor(plot.text$Interface,levels = interfaces.to.plot)
    
    p1 <- ggplot() + stat_binhex(data = int.comparison,
                    aes(x = Interchange,y = Price,color = ..density..*100,fill = ..density..*100),
                    binwidth = c(200,3)) + 
      scale_fill_gradientn(name = "Pct.",colours = c('gray90','lightpink','darkred'),
                           guide = 'colourbar') + 
      scale_color_gradientn(name = "Pct.",colours = c('gray90','lightpink','darkred'),
                            guide = 'colourbar') + 
      geom_label(data = plot.text,aes(x = x,y = y,label = text),color = 'black',fill = 'lightblue',size = 2.4) + 
      geom_vline(xintercept = 0,size = 0.01,color = 'black') +
      geom_hline(yintercept = 0,size = 0.01,color = 'black') +
      facet_grid(Interface ~ .) + plot_theme + theme(legend.title = element_text()) + 
      labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
      coord_cartesian(xlim = xlim,ylim = ylim) 
    
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
    
    quadrants[,Underutilized := II + IV]
    quadrants[,Counterintuitive := I + III]
    
    quadrants = quadrants[,lapply(.SD,function(x) paste0(round(x*100,1),"%")),by = c('Interface'),
                          .SDcols = c('quad.0','I','II','III','IV','Underutilized','Counterintuitive')]
    
    # write quadrants csv and plot
    setwd(wd)
    # write.csv(quadrants,'plots/EI_ISO_efficiencies.csv',row.names = FALSE)
    # ggsave('plots/EI_ISO_hex.png',p,height = 5.5,width = 3.5)
    # write.csv(outlier.data,'plots/EI_ISO_outliers.csv',row.names = FALSE)

    output.list = list(plot.data = int.comparison,p1 = p1,p2 = p2,outlier.data = outlier.data,quadrants = quadrants,
                       ME.table = ME.table, ME.table.all = ME.table.all)
    
    output.list
    
}    
    