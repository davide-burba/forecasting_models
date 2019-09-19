
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



# Some utility functions
my_forecast_plot = function(model,model_name){
  fit = model(window(train_data, end=2007),h=h)
  autoplot(window(train_data, start = 2004,end=2008)) +
    autolayer(fit, PI=TRUE,alpha = 0.25,color = "red") + 
    autolayer(fit, PI=FALSE, series=model_name,size = 1) + 
    xlab(plot_time) + ylab("New orders index") +
    ggtitle("Electrical equipment manufacturing (Euro area)") +
    guides(colour=guide_legend(title="Forecast"))
}


print_scores = function(models_mae){
  for (model_name in unique(models_mae$model)){
    print(paste(model_name,":",round(mean(models_mae[models_mae$model==model_name,"mae"]),2)))
  }
}


plot_scores = function(models_mae,naive = TRUE){
  if(naive){
    ggplot(models_mae,aes(x=horizon,y=mae,factor=model,color = model))+geom_line()
  }else{
    ggplot(models_mae[(models_mae$model != "Naïve") & (models_mae$model != "Seasonal Naïve"),],aes(x=horizon,y=mae,factor=model,color = model))+geom_line()
  }
}


next_time_step = function(y, is_end = F){
  if(is_end){
    new_start = y
  }else{
    new_start =  end(y)
  }
  if(new_start[2]==12){
    new_start[1]=new_start[1]+1
    new_start[2]=1
  }else{
    new_start[2]=new_start[2]+1
  }
  return(new_start)
}


previous_time_step = function(y){
  new_start =  end(y)
  if(new_start[2]==1){
    new_start[1]=new_start[1]-1
    new_start[2]=12
  }else{
    new_start[2]=new_start[2]-1
  }
  return(new_start)
}
