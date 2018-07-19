
# -----------------------------------------------------------------------------|
# Setup ----
# -----------------------------------------------------------------------------|

pacman::p_load(rplexos,dplyr,RSQLite,lubridate, data.table,ggplot2)

wd = "//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Analysis_scripts"
setwd(wd)
voltage = 'border'

source("source_scripts/REAL-ISO_price_flow_plots.R")
source("source_scripts/SINGLE-OPT-BORDER_price_flow_plots.R")
source("source_scripts/MULTI-OPT-BORDER_price_flow_plots.R")

render.RTS.analysis = FALSE

# -----------------------------------------------------------------------------|
# SEAMS - REAL ISO OUTPUTS ----
# -----------------------------------------------------------------------------|

real.iso.output = REAL_ISO_price_flow_plots()

ggsave(paste0("plots_SEAMS/EI-ISO-hex_",voltage,".png"),real.iso.output[["p1"]],height = 4.25,width = 3.5)
ggsave(paste0("plots_SEAMS/EI-ISO-fold_",voltage,".png"),real.iso.output[["p2"]],height = 4.25,width = 3.5)
write.csv(real.iso.output[["plot.data"]],paste0("plots_SEAMS/EI-ISO-plot-data_",voltage,".csv"),row.names = FALSE)
write.csv(real.iso.output[["outlier.data"]],paste0("plots_SEAMS/EI-ISO-outlier-data_",voltage,".csv"),row.names = FALSE)
write.csv(real.iso.output[["quadrants"]],paste0("plots_SEAMS/EI-ISO-efficiency-data_",voltage,".csv"),row.names = FALSE)
write.csv(real.iso.output[["stat.center"]],paste0("plots_SEAMS/EI-ISO-stat-center_",voltage,".csv"),row.names = FALSE)
write.csv(real.iso.output[["stat.squared"]],paste0("plots_SEAMS/EI-ISO-stat-squared_",voltage,".csv"),row.names = FALSE)
write.csv(real.iso.output[["trend.line"]],paste0("plots_SEAMS/EI-ISO-trend-line_",voltage,".csv"),row.names = FALSE)

# -----------------------------------------------------------------------------|
# SEAMS - SINGLE OPERATOR OUTPUTS ----
# -----------------------------------------------------------------------------|

single.opt.output = SINGLE_OPT_BORDER_price_flow_plots()

ggsave(paste0("plots_SEAMS/EI-single-opt-hex_",voltage,".png"),single.opt.output[["p1"]],height = 4.25,width = 3.5)
ggsave(paste0("plots_SEAMS/EI-single-opt-fold_",voltage,".png"),single.opt.output[["p2"]],height = 4.25,width = 3.5)
write.csv(single.opt.output[["border.buses.from"]],paste0("plots_SEAMS/EI-single-opt-border-buses-from_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["border.buses.to"]],paste0("plots_SEAMS/EI-single-opt-border-buses-to_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["plot.data"]],paste0("plots_SEAMS/EI-single-opt-plot-data_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["outlier.data"]],paste0("plots_SEAMS/EI-single-opt-outlier-data_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["quadrants"]],paste0("plots_SEAMS/EI-single-opt-efficiency-data_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["stat.center"]],paste0("plots_SEAMS/EI-single-opt-stat-center_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["stat.squared"]],paste0("plots_SEAMS/EI-single-opt-stat-squared_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["trend.line"]],paste0("plots_SEAMS/EI-single-opt-trend-line_",voltage,".csv"),row.names = FALSE)

# -----------------------------------------------------------------------------|
# SEAMS - MULTI OPERATOR OUTPUTS ----
# -----------------------------------------------------------------------------|

multi.opt.output = MULTI_OPT_BORDER_price_flow_plots()
    
ggsave(paste0("plots_SEAMS/EI-multi-opt-hex_",voltage,".png"),multi.opt.output[["p1"]],height = 4.25,width = 3.5)
ggsave(paste0("plots_SEAMS/EI-multi-opt-fold_",voltage,".png"),multi.opt.output[["p2"]],height = 4.25,width = 3.5)
write.csv(multi.opt.output[["border.buses.from"]],paste0("plots_SEAMS/EI-multi-opt-border-buses-from_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["border.buses.to"]],paste0("plots_SEAMS/EI-multi-opt-border-buses-to_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["plot.data"]],paste0("plots_SEAMS/EI-multi-opt-plot-data_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["outlier.data"]],paste0("plots_SEAMS/EI-multi-opt-outlier-data_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["quadrants"]],paste0("plots_SEAMS/EI-multi-opt-efficiency-data_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["stat.center"]],paste0("plots_SEAMS/EI-multi-opt-stat-center_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["stat.squared"]],paste0("plots_SEAMS/EI-multi-opt-stat-squared_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["trend.line"]],paste0("plots_SEAMS/EI-multi-opt-trend-line_",voltage,".csv"),row.names = FALSE)

# -----------------------------------------------------------------------------|
# SEAMS - FOLDED PRICE-FLOW PLOT ----
# -----------------------------------------------------------------------------|

opt.data = rbind(# real.iso.output[["plot.data"]][,scenario:='ISO Data'],
                 single.opt.output[["plot.data"]][,scenario:='Single-operator'],
                 multi.opt.output[["plot.data"]][,scenario:='Multi-operator'])

interfaces.to.plot = c('PJM - MISO','PJM - NYISO','NYISO - ISO-NE')

opt.data$Interface = factor(opt.data$Interface,levels = interfaces.to.plot)

opt.data$scenario = factor(opt.data$scenario,levels = c(# 'ISO Data',
                                                        'Single-operator','Multi-operator'))

p1 <- folded_price_flow_plots(plot.data = copy(opt.data),
                              interfaces.to.plot = interfaces.to.plot)

p1 <- p1 + facet_grid(Interface ~ scenario)

ggsave('plots_SEAMS/price-flow-combine-fold.png',p1,height = 4.5,width = 7,dpi = 600)
ggsave('plots_SEAMS/price-flow-combine-fold.emf',p1,height = 4.5,width = 7,dpi = 600)

# -----------------------------------------------------------------------------|
# SEAMS - UNFOLDED PRICE-FLOW PLOT ----
# -----------------------------------------------------------------------------|

plot.text = data.table(scenario = 'Single-operator', # change based on which plots are being made
                       Interface = "PJM - MISO",
                       x = c(4000,-4000,-4000,4000),
                       y = c(70,70,-70,-70),
                       text = rep(c('Agreement','Disagreement'),2))

xlim = c(-7000,7000)
ylim = c(-125,125)

plot.text$Interface = factor(plot.text$Interface,levels = interfaces.to.plot)
plot.text$scenario = factor(plot.text$scenario,levels = c(# 'ISO Data',
                                                          'Single-operator','Multi-operator'))

p2 <- ggplot() + stat_binhex(data = opt.data,
                             aes(x = Interchange,y = Price,color = ..density..*100,fill = ..density..*100),
                             binwidth = c(200,3)) + 
    scale_fill_gradientn(name = "(%)",colours = c('gray80','gray20','black'),
                         guide = 'colourbar') + 
    scale_color_gradientn(name = "(%)",colours = c('gray80','gray20','black'),
                          guide = 'colourbar') + 
    geom_label(data = plot.text,aes(x = x,y = y,label = text),color = 'black',fill = 'lightblue',size = 2.5) + 
    geom_vline(xintercept = 0,size = 0.01,color = 'black') +
    geom_hline(yintercept = 0,size = 0.01,color = 'black') +
    facet_grid(Interface ~ .) + plot_theme + theme(legend.title = element_text()) + 
    labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
    coord_cartesian(xlim = xlim,ylim = ylim) +
    theme(strip.background = element_rect(fill = "lightblue"))

p2 <- p2 + facet_grid(Interface ~ scenario)

ggsave('plots_SEAMS/price-flow-combine-unfold.png',p2,height = 4.5,width = 7,dpi = 600)
ggsave('plots_SEAMS/price-flow-combine-unfold.emf',p2,height = 4.5,width = 7,dpi = 400)

# -----------------------------------------------------------------------------|
# RTS - Analysis ----
# -----------------------------------------------------------------------------|

if(render.RTS.analysis == TRUE){
    
    output.dir <- "//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Analysis_scripts/plots_RTS"
    fig.path.name <- output.dir
    output.file = "RTS_geodecomp_analysis"
    
    render(input = 'RTS_geodecomp_analysis.Rmd',
           output_format = "html_document",
           output_file = paste0(output.file, ".html"),
           output_dir = output.dir)
}

