
library(forecast)


# Alternative to tsCV, to use only subset of last time_steps 
# Warning: should be generalized
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
    xlab("Year") + ylab("New orders index") +
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
  ggplot(models_mae,aes(x=horizon,y=mae,factor=model,color = model))+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks = 1:12) 
  }else{
    ggplot(models_mae[(models_mae$model != "Naïve") & (models_mae$model != "Seasonal Naïve"),],aes(x=horizon,y=mae,factor=model,color = model))+
      geom_point()+
      geom_line()+
      scale_x_continuous(breaks = 1:12) 
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



get_best_models = function(models_mae){
  best_models = NULL
  for(i in 1:12){
    tmp = models_mae[models_mae$horizon==i,]
    best_models  = rbind(best_models,tmp[tmp$mae==min(tmp$mae),])
  }
  rownames(best_models)=NULL
  return(best_models)
}


evaluate_test_residuals = function(fitted_model, model_function,data,...){
  errors = NULL
  for(i in 1:35){
    data[1:(length(train_data)-12+i)] %>%
      ts(frequency = 12,start = start(data)) %>%
      model_function(model = fitted_model) %>%   # use the fitted model to forecast test data
      forecast(h=h,...) -> pred
    
    true_values = c(window(data,start = start(pred$mean),end=end(pred$mean)))
    pred_mean = c(pred$mean)
    if(i < 12){
      pred_mean[1:(12-i)]=NA
    }
    if(length(true_values) < 12){
      true_values = c(true_values, rep(NA,12-length(true_values)))
    }
    errors = rbind(errors,true_values-pred_mean)
  }
  return(errors)
}