
library("rplexos")

setwd(dirname(parent.frame(2)$ofile))
setwd("..")

solution.files = list.files()[grepl('Model ',list.files())]


for(i in 1:length(solution.files)){
  
  process_folder(solution.files[i])
}

