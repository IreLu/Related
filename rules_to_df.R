### High level function in order to convert "rules"-class object into dataframe 
### It also converts many-to-one into one-to-one rules format

arules.to.df <- function(rules) {
  
  require(dplyr)
  
  ### Control on object class
  if(class(rules) != 'rules'){
    warning('L\'argomento non è di classe \'rules\'')
  }
  
  ### Eseg
  else {
    rules.df <- as.data.frame(inspect(rules))
    return(rules.df)
  }
}
