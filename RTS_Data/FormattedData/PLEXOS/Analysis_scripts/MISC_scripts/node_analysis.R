
wd = "//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC/RTS_Data/FormattedData/PLEXOS/Analysis_scripts/plots"
setwd(wd)

node.files = list.files()[grepl('-buses-',list.files())]

node.table = data.table()
for(jj in seq(length(node.files))){
    
    node.table.temp = fread(node.files[jj])
    setnames(node.table.temp,c("Node","ISO","Interface","Voltage"))
    node.table.temp[,scenario:=node.files[jj]]
    
    
    node.table = rbind(node.table,
                       node.table.temp)
}

node.table[,type:=ifelse(grepl('-from',scenario),'From','To')]
node.table[,scenario:=tstrsplit(scenario,"_")[[2]]]
node.table[,scenario:=gsub(".csv","",scenario)]

node.table = unique(node.table[,.(Node,Interface,Voltage,scenario,type)])
stop()
node.table.join = merge(node.table[scenario == "OLD"],
                        node.table[scenario == "220"],
                        by = c('Node','Interface','Voltage','type'),
                        all = TRUE)


node.table = node.table[,lapply(.SD,function(x) .N),by = c('Interface','Voltage','scenario','type'),
                        .SDcols = c('Node')]
node.table[,Voltage:=as.character(round(Voltage))]

# --------------------------------------------------- |
# plot theme ----
# --------------------------------------------------- |

theme_set(theme_bw()) 

# size of text in plots
large.text.size <- 10.5
small.text.size <- 9.1666
text.plot = 11

# plot theme
plot_theme <- 
    theme(legend.key = element_rect(color = "grey80", size = 0.8), 
          legend.key.size = grid::unit(1.0, "lines"),
          legend.text = element_text(size = small.text.size), 
          legend.title = element_blank(), 
          axis.text = element_text(size = small.text.size), 
          axis.text.x = element_text(size = small.text.size),
          axis.title = element_text(size = large.text.size, face = "bold"),
          axis.title.x= element_text(size=large.text.size, vjust = 1.2, face = "bold"),
          axis.title.y = element_text(size=large.text.size, vjust = 1.2, face = "bold"),
          strip.text = element_text(size=small.text.size),
          panel.spacing = unit(0.5, "lines"))

# --------------------------------------------------- |
# make plot ----
# --------------------------------------------------- |

color.code = c('765' = 'firebrick',
               '500' = 'peru',
               '345' = 'steelblue3',
               '230' = 'magenta',
               '138' = 'darkblue',
               '115' = 'forestgreen',
               '69' = 'darkorange',
               '46' = 'cyan',
               '34' = 'gray20',
               '12' = 'black',
               '1' = 'white')

node.table$Voltage = factor(node.table$Voltage,levels = names(color.code))
node.table$Interface = factor(node.table$Interface,levels = c('PJM - MISO','PJM - NYISO','NYISO - ISO-NE'))

p <- ggplot() + geom_bar(data = node.table,aes(x = scenario,y = Node,fill = Voltage),
                         color = 'black',stat = 'identity') + plot_theme + 
    scale_fill_manual(values = color.code) + 
    labs(x = '',y = 'Number of Nodes',fill = "Voltage") + facet_grid(Interface~type,scales = 'fixed')
    

ggsave('node_analysis.png',p,height = 7,width = 8.5)    
