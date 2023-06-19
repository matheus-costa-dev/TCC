require(tidyverse)
f = list.files("./functions",pattern = ".R",full.names = T)  %>% .[-grep("allFunctions.R|citeToBibTex.R",.)]
invisible(lapply(f,source,chdir =F)) 
