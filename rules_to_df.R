### High level function in order to convert "rules"-class object into dataframe 
### It also converts many-to-one into one-to-one rules format

arules.to.df <- function(rules) {
  
  require(tidyverse)
  
  ### Control on object class
  if(class(rules) != 'rules'){
    warning('L\'argomento non è di classe \'rules\'')
  }
  
  ### Starting modifying object
  else {
    rules.df <- as.data.frame(inspect(rules))
    if(nrow(rules.df) == 0){
      warning('Non ci sono regole')
    }
    
    else {
      ### Dropping unused column and splitting LHS based on ',' character
      rules.df[, 2] <- NULL
      rules.df <- rules.df %>% mutate_if(is.factor, as.character) %>% select(lhs, rhs, count)
      new.cols <- as.data.frame(str_split(rules.df$lhs, ',', simplify = TRUE)) %>% mutate_if(is.factor, as.character)
      
      if(ncol(new.cols) == 1){
        ### Cleaning from { }
        rules.df <- rules.df %>% mutate_all(funs(gsub('}', '', .))) %>%
          mutate_all(funs(ifelse(nchar(.)==11, substr(., start = 2, 12), .)))
        
        return(rules.df)
      }
      
      else{
        ### Renaming new columns with lhs splitted
        for(i in 1:ncol(new.cols)){
          names(new.cols)[i] <- paste0('lhs',i)
        }
        
         rules.df <- cbind(rules.df, new.cols)
         rules.df <- rules.df %>% select(-lhs)
         
         ### Starting loop
         new.rules.df <- data.frame(lhs = "0", rhs = "0", count = 0, stringsAsFactors = FALSE)
         tmp.rules <- data.frame(lhs = "0", rhs = "0", count = 0, stringsAsFactors = FALSE)
         for(righe in 1:nrow(rules.df)){
           for(nuove_colonne in 1:ncol(new.cols)){
             tmp.rules[nuove_colonne, ] <- c(rules.df[righe,2+nuove_colonne], rules.df$rhs[righe], rules.df$count[righe])
           }
           new.rules.df <- rbind(new.rules.df, tmp.rules)
         }
         
         ### Cleaning C10 from { } 
         new.rules.df <- new.rules.df %>% filter(lhs != "") %>% filter(lhs != '0') %>% 
           mutate_all(funs(gsub('}', '', .))) %>%
           mutate_all(funs(ifelse(nchar(.)==11, substr(., start = 2, 12), .)))
         
         return(new.rules.df)
      }
    }
  }
}
