
make_decomposition_key = function(){

  # nonfocus genereators
  
  decomposition.map = c(file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                 "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/EI_nonfocus_generators.csv"),
                       file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                 "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/WI_nonfocus_generators.csv"))
  
  
  EI.nonfocus = fread(decomposition.map[1])
  WI.nonfocus = fread(decomposition.map[2])
  
  EI.nonfocus[,Scenario:=gsub('nonfocus_','',Scenario)]
  WI.nonfocus[,Scenario:=gsub('nonfocus_','',Scenario)]
  
  setnames(EI.nonfocus,c('Scenario','Category'),c('Region','category'))
  setnames(WI.nonfocus,c('Scenario','Category'),c('Region','category'))
  
  # focus generators
  
  decomposition.map = c(file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                  "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/EI_focus_generators.csv"),
                        file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                  "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/WI_focus_generators.csv"))
  
  
  EI.focus = fread(decomposition.map[1])[,.(Generator,Region = Scenario)]
  WI.focus = fread(decomposition.map[2])[,.(Generator,Region = Scenario)]
  
  EI.focus[,Region:=gsub('focus_','',Region)]
  WI.focus[,Region:=gsub('focus_','',Region)]
  
  EI.focus[,category:='']
  WI.focus[,category:='']
  
  # JLJ generators
  
  decomposition.map = c(file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                  "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/EI_generators_JLJ.csv"),
                        file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                  "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/WI_generators_JLJ.csv"))
  
  EI.jlj = fread(decomposition.map[1])
  WI.jlj = fread(decomposition.map[2])
  
  setnames(EI.jlj,'region','Region')
  setnames(WI.jlj,'region','Region')
  
  
  # missing generators
  
  decomposition.map = file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                                "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/missing_generators.csv")
  
  missing.generators = fread(decomposition.map)[,.(Generator,category,Region)]
  
  # combine
  
  generator.table = rbind(EI.focus,WI.focus,EI.nonfocus,WI.nonfocus,EI.jlj,WI.jlj,missing.generators)
  
  generator.table[Region %in% c('FRCC','TVA','SERC'),Region:='SE']
  generator.table[Region %in% c('ISONE','NYISO'),Region:='NE']
  generator.table[Region %in% c('West_Connect'),Region:='West']
  generator.table[Region %in% c('Columbia_Grid'),Region:='CG']
  generator.table[Region %in% c('Northern_Tier'),Region:='NT']
  generator.table[Region %in% c('IESO','MRO','NBSO','NPCC','HQ','MH','Canada'),Region:='CAN']
  
  generator.table[Generator %in% c('FtJames1_WI','HermistnGenCC1-Total_WI'),Region:='CG']
  generator.table = unique(generator.table[,.(Generator,Region)])
  
  
  # add PLEXOS regions
  
  # node regions map - 98280 nodes
  node.regions <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                            "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/node_region.csv")
  
  # generator node map - 13695 generators
  generator.nodes <- file.path("//plexossql/Data/bmcbenne/RTS-GMLC-geodecomp/RTS-GMLC",
                               "RTS_Data/FormattedData/PLEXOS/Analysis_scripts/SEAMS_database/generator_node.csv")
  
  node.regions = fread(node.regions)[Collection == "Node.Region"]
  node.regions = node.regions[,.(Node = `Parent Name`,region = `Child Name`)]
  
  generator.nodes = fread(generator.nodes)[Collection == "Generator.Nodes"]
  generator.nodes = generator.nodes[,.(name = `Parent Name`,Node = `Child Name`)]
  
  generator.map = merge(generator.nodes,node.regions,by = 'Node')
  generator.regions = unique(generator.map[,.(Generator = name,region)])
  
  key.table = merge(generator.table,generator.regions,by = 'Generator',all = TRUE)
  
  # find two-region generators and map PLEXOS regions to decomposition regions
  
  generator.regions[,count:=.N,by = 'Generator']
  dual.region.generators = generator.regions[count > 1][,Generator]
  
  region.table = key.table[!(Generator %in% dual.region.generators)]
  region.table = unique(region.table[,.(Region,region)])
  region.table[,count:=.N,by = 'region']
  
  dual.regions = unique(region.table[count>1,region])
  
  # find multi-node generators
  
  dual.node.generators = copy(generator.nodes)
  dual.node.generators[,count:=.N,by = 'name']
  dual.node.generators = dual.node.generators[count > 1]
  
  return(list(generator.table = generator.table,
              region.table = region.table,
              key.table = key.table,
              dual.region.generators = dual.region.generators,
              dual.node.generators = dual.node.generators,
              generator.nodes = generator.nodes,
              node.regions = node.regions))
  
  
  
  

}

