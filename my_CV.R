
library(forecast)


# Alternative to tsCV, to use only subset of last time_steps 
# Warning: shpuld be generalized
my_CV = function(y, 
                 forecastfunction,
                 h=12,
                 last_year_months=NULL,
                 freq = 5,
                 start_year = 1997,
                 ...){
  
  if(is.null(last_year_months)){
    i = 1
    j = 1
    last_year_months = list()
    for(year in start_year:2010) {
      for(month in 1:12){
        if(i%%freq==0){
          if ( (year!=2010) | (month <3) ){
            last_year_months[[j]] <- c(year,month)
            j = j+1
          }
        }
        i = i+1
      }
    }
  } 
  
  CV_residuals = NULL
  
  for(last_year_month in last_year_months){
    
    predictions = forecastfunction(window(y,end = last_year_month),h=h,...)$mean
    true_values = window(y,start = last_year_month)[2:(h+1)]
    
    CV_residuals = rbind(CV_residuals,true_values-predictions)
  }
  
  rownames(CV_residuals) = last_year_months
  colnames(CV_residuals) = 1:h
  
  return(CV_residuals)
}