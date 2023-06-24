dataqualityreport <-  function(dataframe, numeric){
  # creates a data quality report
  # for continuous data...
  # for categorical data...
  if(numeric == TRUE){
    continuous_data <- dataframe %>% 
      summarise(
        Count = n(),
        Miss% = NULL,
        Uniq = NULL,
        Min = min(Temp),
        Q1 = quantile(Temp, 0.25),
        Mean = mean(Temp),
        Med = median(Temp),
        Q3 = quantile(Temp, 0.75),
        Max = max(Temp),
        Sd = sd(Temp))    
    
    return(continuous_data)
  } 
  
  elif(numeric == FALSE){
    categorical_data <- dataframe
    return(categorical_data)
  } 
  
  else{
    print("ERROR: numeric must be TRUE/FALSE")
  }
    
}