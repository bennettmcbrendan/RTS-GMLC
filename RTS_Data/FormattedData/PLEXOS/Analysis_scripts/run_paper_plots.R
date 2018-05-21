
# -----------------------------------------------------------------------------|
# Setup ----
# -----------------------------------------------------------------------------|

wd = "//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Analysis_scripts"
setwd(wd)

voltage = 220
source("source_scripts/REAL-ISO_price_flow_plots.R")
source("source_scripts/SINGLE-OPT-BORDER_price_flow_plots.R")
source("source_scripts/MULTI-OPT-BORDER_price_flow_plots.R")

render.RTS.analysis = TRUE

# -----------------------------------------------------------------------------|
# SEAMS - REAL ISO OUTPUTS ----
# -----------------------------------------------------------------------------|

real.iso.output = REAL_ISO_price_flow_plots()

ggsave(paste0("plots_SEAMS/EI-ISO-hex_",voltage,".png"),real.iso.output[["p1"]],height = 5.5,width = 3.5)
ggsave(paste0("plots_SEAMS/EI-ISO-fold_",voltage,".png"),real.iso.output[["p2"]],height = 5.5,width = 3.5)
write.csv(real.iso.output[["plot.data"]],paste0("plots_SEAMS/EI-ISO-plot-data_",voltage,".csv"),row.names = FALSE)
write.csv(real.iso.output[["outlier.data"]],paste0("plots_SEAMS/EI-ISO-outlier-data_",voltage,".csv"),row.names = FALSE)
write.csv(real.iso.output[["quadrants"]],paste0("plots_SEAMS/EI-ISO-efficiency-data_",voltage,".csv"),row.names = FALSE)
write.csv(real.iso.output[["ME.table"]],paste0("plots_SEAMS/EI-ISO-mean-error-data_",voltage,".csv"),row.names = FALSE)
write.csv(real.iso.output[["ME.table.all"]],paste0("plots_SEAMS/EI-ISO-mean-error-data-all_",voltage,".csv"),row.names = FALSE)

# -----------------------------------------------------------------------------|
# SEAMS - SINGLE OPERATOR OUTPUTS ----
# -----------------------------------------------------------------------------|
    
single.opt.output = SINGLE_OPT_BORDER_price_flow_plots(voltage = voltage)
    
ggsave(paste0("plots_SEAMS/EI-single-opt-hex_",voltage,".png"),single.opt.output[["p1"]],height = 5.5,width = 3.5)
ggsave(paste0("plots_SEAMS/EI-single-opt-fold_",voltage,".png"),single.opt.output[["p2"]],height = 5.5,width = 3.5)
write.csv(single.opt.output[["border.buses.from"]],paste0("plots_SEAMS/EI-single-opt-border-buses-from_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["border.buses.to"]],paste0("plots_SEAMS/EI-single-opt-border-buses-to_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["plot.data"]],paste0("plots_SEAMS/EI-single-opt-plot-data_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["outlier.data"]],paste0("plots_SEAMS/EI-single-opt-outlier-data_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["quadrants"]],paste0("plots_SEAMS/EI-single-opt-efficiency-data_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["ME.table"]],paste0("plots_SEAMS/EI-single-opt-mean-error-data_",voltage,".csv"),row.names = FALSE)
write.csv(single.opt.output[["ME.table.all"]],paste0("plots_SEAMS/EI-single-opt-mean-error-data-all_",voltage,".csv"),row.names = FALSE)

# -----------------------------------------------------------------------------|
# SEAMS - MULTI OPERATOR OUTPUTS ----
# -----------------------------------------------------------------------------|

multi.opt.output = MULTI_OPT_BORDER_price_flow_plots(voltage = voltage)
    
ggsave(paste0("plots_SEAMS/EI-multi-opt-hex_",voltage,".png"),multi.opt.output[["p1"]],height = 5.5,width = 3.5)
ggsave(paste0("plots_SEAMS/EI-multi-opt-fold_",voltage,".png"),multi.opt.output[["p2"]],height = 5.5,width = 3.5)
write.csv(multi.opt.output[["border.buses.from"]],paste0("plots_SEAMS/EI-multi-opt-border-buses-from_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["border.buses.to"]],paste0("plots_SEAMS/EI-multi-opt-border-buses-to_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["plot.data"]],paste0("plots_SEAMS/EI-multi-opt-plot-data_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["outlier.data"]],paste0("plots_SEAMS/EI-multi-opt-outlier-data_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["quadrants"]],paste0("plots_SEAMS/EI-multi-opt-efficiency-data_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["ME.table"]],paste0("plots_SEAMS/EI-multi-opt-mean-error-data_",voltage,".csv"),row.names = FALSE)
write.csv(multi.opt.output[["ME.table.all"]],paste0("plots_SEAMS/EI-multi-opt-mean-error-data-all_",voltage,".csv"),row.names = FALSE)

# -----------------------------------------------------------------------------|
# SEAMS - FOLDED PRICE-FLOW PLOT ----
# -----------------------------------------------------------------------------|

opt.data = rbind(real.iso.output[["plot.data"]][,scenario:='ISO Data'],
                 single.opt.output[["plot.data"]][,scenario:='Single-Opt'],
                 multi.opt.output[["plot.data"]][,scenario:='Multi-Opt'])

interfaces.to.plot = c('PJM - MISO','PJM - NYISO','NYISO - ISO-NE')

opt.data$Interface = factor(opt.data$Interface,levels = interfaces.to.plot)

opt.data$scenario = factor(opt.data$scenario,levels = c('ISO Data','Single-Opt','Multi-Opt'))

p1 <- folded_price_flow_plots(plot.data = copy(opt.data),
                              interfaces.to.plot = interfaces.to.plot)

p1 <- p1 + facet_grid(Interface ~ scenario)

ggsave('plots_SEAMS/price-flow-combine-fold.png',p1,height = 5.5,width = 7,dpi = 600)
ggsave('plots_SEAMS/price-flow-combine-fold.emf',p1,height = 5.5,width = 7,dpi = 600)

# -----------------------------------------------------------------------------|
# SEAMS - UNFOLDED PRICE-FLOW PLOT ----
# -----------------------------------------------------------------------------|

plot.text = data.table(scenario = 'ISO Data',
                       Interface = "PJM - MISO",
                       x = c(4000,-4000,-4000,4000),
                       y = c(70,70,-70,-70),
                       text = rep(c('Counterintuitive','Under-utilized'),2))

xlim = c(-7000,7000)
ylim = c(-100,100)

plot.text$Interface = factor(plot.text$Interface,levels = interfaces.to.plot)
plot.text$scenario = factor(plot.text$scenario,levels = c('ISO Data','Single-Opt','Multi-Opt'))

p2 <- ggplot() + stat_binhex(data = opt.data,
                             aes(x = Interchange,y = Price,color = ..density..*100,fill = ..density..*100),
                             binwidth = c(200,3)) + 
    scale_fill_gradientn(name = "Pct.",colours = c('gray80','gray20','black'),
                         guide = 'colourbar') + 
    scale_color_gradientn(name = "Pct.",colours = c('gray80','gray20','black'),
                          guide = 'colourbar') + 
    geom_label(data = plot.text,aes(x = x,y = y,label = text),color = 'black',fill = 'lightblue',size = 2) + 
    geom_vline(xintercept = 0,size = 0.01,color = 'black') +
    geom_hline(yintercept = 0,size = 0.01,color = 'black') +
    facet_grid(Interface ~ .) + plot_theme + theme(legend.title = element_text()) + 
    labs(x = 'Interchange (MW)',y = 'LMP difference (USD)') +
    coord_cartesian(xlim = xlim,ylim = ylim) 

p2 <- p2 + facet_grid(Interface ~ scenario)

ggsave('plots_SEAMS/price-flow-combine-unfold.png',p2,height = 5.5,width = 7,dpi = 600)
ggsave('plots_SEAMS/price-flow-combine-unfold.emf',p2,height = 5.5,width = 7,dpi = 600)

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

