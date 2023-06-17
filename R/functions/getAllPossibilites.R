require(tidyverse)

getAllPossibilites = function(symbols,by){
  
  Nsymbols = length(symbols)
  
  possibilities = rep(list(seq(0,1,by)),Nsymbols) %>%
    reduce(cbind) %>% 
    as_tibble() %>%
    setNames(symbols)
  
  possibilities = do.call(expand.grid, possibilities)
  
  print("vai iniciar o processo de filtrar soma dos pesos == 1")
  
  possibilities =   tryCatch({
    possibilities %>% 
      mutate(total = rowSums(pick(where(is.numeric)))) %>%
      filter(total == 1) %>%
      select(-total)
  },error = function(e){
    print('deu erro o metodo padrão, vai tentar de outra forma')
    
    start = 1
    end = 10000000
    endBy = end
    t = nrow(possibilities)/end
    i = 1
    allPossibilites = data.frame()
    
    print(paste0(i," - ",start,":",endBy))
    
    while(i < t ){
      
      tmp = possibilities %>% 
        slice(start:endBy) %>%
        mutate(total = rowSums(pick(where(is.numeric)))) %>%
        filter(total == 1) %>%
        select(-total)
      
      allPossibilites = rbind(allPossibilites,tmp)
      
      i = i+1
      start = endBy + 1
      endBy = endBy + end
      
      gc()
      print(paste0(i," - ",start,":",endBy))
      
    }
    
    print('finalizou')
    
    return(allPossibilites)
    
    #possibilities = allPossibilites
    #rm(allPossibilites)
    
  })
  
  
  
  return(possibilities)
}
