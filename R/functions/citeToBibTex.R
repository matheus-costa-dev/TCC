require(tidyverse)
require(openxlsx)

citeToBibTex= function(packs = c("corrplot","ggplot2","knitr","PerformanceAnalytics","quantmod","rbcb","RColorBrewer","tidyverse","xts","base")){
  if (length(packs)>1){
    map(packs, 
        ~ write_lines(toBibtex(citation(.x)),paste0("source/citacoesR/",.x,".bib"))
    )
  } else {
    write_lines(toBibtex(citation(packs)),paste0("source/citacoesR/",packs,".bib"))
  }
 
}
