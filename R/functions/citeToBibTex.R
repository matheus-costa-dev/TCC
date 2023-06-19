require(tidyverse)
require(openxlsx)

citeToBibTex= function(packs = c("base","tidyverse","quantmod","scales","PerformanceAnalytics")){
  map(packs, 
      ~ write_lines(toBibtex(citation(.x)),paste0("citacao/",.x,".bib"))
  )
}

