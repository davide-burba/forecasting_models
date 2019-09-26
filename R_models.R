
# just once
#library(fpp2)
#write.csv(elecequip,file = "elecequip.csv",row.names = FALSE)

# Monthly manufacture of electrical equipment: computer, electronic and optical products. 
# January 1996 - March 20h. Data adjusted by working days; Euro area (17 countries). Industry new orders index. 2005=100.

# ("Adjusted" means "divided by number of working days in a month")
setwd("~/Documents/nerd/time_series_forecasting")

library(forecast)
library(expsmooth)
library(ggplot2)
library(seasonal)
library(rugarch)
source("utils.R")

df = read.csv("elecequip.csv")[,1]
length_test_set = 24
h = 12

data = ts(df,start = c(1996,1),frequency = 12)
train_data = ts(df[1:(length(df)-length_test_set)],start = c(1996,1), frequency = h)



############################################# EXPLORATION
autoplot(train_data) +
  ggtitle("Manufacture of electrical equipment") + 
  xlab("Year") + ylab("New Orders Index")
# non-linear trend, cycles, clear seasonality

ggseasonplot(train_data, year.labels=TRUE, year.labels.left=TRUE) + ylab("New Orders Index") +
  ggtitle("Seasonal plot: monthly manufacture of electrical equipment")

ggseasonplot(train_data, polar=TRUE) + ylab("New Orders Index") +
  ggtitle("Polar seasonal plot: monthly manufacture of electrical equipment")

ggsubseriesplot(train_data)  + ylab("New Orders Index") +
  ggtitle("Seasonal subseries plot: monthly manufacture of electrical equipment")

ggAcf(train_data)

pacf(train_data)

# Decomposition
fit = stl(train_data,t.window=13, s.window="periodic", robust=TRUE)
autoplot(fit)

# Seasonally adjusted time serie
autoplot(train_data, series="Data") + 
  autolayer(trendcycle(fit), series="Trend") + 
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("gray","blue","red"),breaks=c("Data","Seasonally Adjusted","Trend"))



########################################## NOISE
wn = data.frame(x = 1:200,y = rnorm(200,mean = 6))
ggplot(wn,aes(x=x,y=y)) + 
  geom_line() + 
  ggtitle("White Noise")


########################################## MODELS VALIDATION
models_mae = NULL
initial_obs = 84



####### NAIVE (Benchmark)
model_name = "Naïve"
model = naive

my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



####### SNAIVE (Benchmark)
model_name = "Seasonal Naïve"
model = snaive

my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



###### RW + DECOMPOSITION
model_name = "RW+Decomposition"
model = function(y, h){
  stl_decomposition = stl(y, t.window=13, s.window="periodic",robust=TRUE)
  return(forecast(stl_decomposition,method = "naive",h=h))
}

# perform decomposition
my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



####### Exponential smoothing
model_name = "Exp Smoothing"
model = function(y, h){
  fitted_model = ets(y)
  return(forecast(fitted_model,h=h))
}

# perform decomposition
my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



####### Exponential smoothing + DECOMPOSITION
model_name = "Exp Smoothing+Decomposition"
model = function(y, h){
  stl_decomposition = stl(y, t.window=13, s.window="periodic",robust=TRUE)
  return(forecast(stl_decomposition,method = "ets",h=h))
}
# perform decomposition
my_forecast_plot(model,model_name)

autoplot(model(train_data,24))

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



####### SARIMA
print(auto.arima(train_data,stepwise = FALSE,approximation = FALSE))
model_name = "SARIMA"
model = function(y, h){forecast(Arima(y, order=c(3,1,0), seasonal = c(0,1,1)), h=h)}

my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



####### ARIMA + Decomposition
stl_decomposition = stl(train_data, t.window=13, s.window="periodic",robust=TRUE)
print(auto.arima(seasadj(stl_decomposition),stepwise = FALSE,approximation = FALSE))

model_name = "ARIMA + Decomposition"
model = function(y, h){
  fit = stlm(y,s.window="periodic", robust=TRUE,modelfunction=Arima, order=c(3,1,1))
  return(forecast(fit,h=h))
}

my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



####### GARCH
model_name = "GARCH + Decomposition"
model = function(y, h){
  # decomposition
  stl_decomposition = stl(y, t.window=13, s.window="periodic",robust=TRUE)
  
  # garch on y_adjusted 
  y_adjusted = seasadj(stl_decomposition)
  diff_y_adjusted = diff(y_adjusted)
  ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
             mean.model = list(armaOrder = c(3, 1))) %>%
    ugarchfit(diff_y_adjusted, solver = 'hybrid',numderiv.control = list(hess.zero.tol=1e-7)) %>%
    ugarchforecast(n.ahead = h) %>% 
    fitted %>%
    ts(frequency = h,start = next_time_step(y)) ->output_garch
  
  # add last observation:
  output_garch = output_garch + y_adjusted[length(y_adjusted)]
  
  # get last year seasonality 
  tmp = end(y)
  tmp[1] = tmp[1]-1
  new_start = next_time_step(tmp,is_end = T)
  last_year_seasonality = window(seasonal(stl_decomposition),start = new_start)
  
  # sum them up
  fcast = output_garch+ts(last_year_seasonality,frequency = h,start = start(output_garch))
  
  # make it a forecast object
  fcast = structure(list(mean = fcast), class='forecast')
  return(fcast)
}

my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



######## Dynamic linear model + Decomposition
library(dlm)
# define the dlm model 
buildFun <- function(x) {
  m = dlmModPoly(2, dV = exp(x[1]), dW = exp(c(x[2],x[3])), m0 = c(x[5],x[6]), C0 = exp(x[7])*diag(nrow = 2) ) 
  return(m)
}

model_name = "DLM + Decomposition"
model = function(y, h){
  # decomposition
  stl_decomposition = stl(y, t.window=13, s.window="periodic",robust=TRUE)
  
  # dlm on y_adjusted 
  y_adjusted = seasadj(stl_decomposition)
  
  # fit the model 
  fit <- dlmMLE(y_adjusted, parm = rep(0,7), build = buildFun)
  my_model = buildFun(fit$par)
  
  # Forecast
  my_Fore = dlmForecast(my_model,nAhead = h)
  output = ts(my_Fore$f[,1], frequency = h,start = next_time_step(y))
  print(output)
  
  # get last year seasonality 
  tmp = end(y)
  tmp[1] = tmp[1]-1
  new_start = next_time_step(tmp,is_end = T)
  last_year_seasonality = window(seasonal(stl_decomposition),start = new_start)
  
  # sum them up
  fcast = output+ts(last_year_seasonality,frequency = h,start = start(output))
  
  # make it a forecast object
  fcast = structure(list(mean = fcast), class='forecast')
  
  return(fcast)
}

my_forecast_plot(model,model_name)

# compute CV MAE
#cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
#mae = colMeans(abs(cv_residuals), na.rm = T)
#print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
#models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



####### TBATS
m = tbats(train_data,seasonal.periods = c(12))
m

model_name = "TBATS"
model = function(y, h){
  return(forecast(tbats(y,use.box.cox = TRUE,use.damped.trend = F,seasonal.periods = c(12),start.p=2,max.p=2,start.q=1,max.q=1), h=h))}

my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



######## Prophet
library(prophet)
library(TSstudio)

model_name = "Prophet"
model = function(y, h){
  df = ts_to_prophet(y)
  # fit the model
  m <- prophet(df,yearly.seasonality = T,weekly.seasonality = F,daily.seasonality = F)
  # make predictions
  future <- make_future_dataframe(m, freq= "month", include_history = F, periods = h)
  m %>% predict(future) -> pred
  pred$yhat %>% ts(start = next_time_step(y),frequency = h) -> fcast
  # make it a forecast object
  fcast = structure(list(mean = fcast), class='forecast')
  
  #print(end(y))
  return(fcast)
}

my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



####### NNETAR
model_name = "NNETAR"
model = function(y, h){forecast(nnetar(y,p = 5, P = 2), h=h)}

my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))



####### NNETAR + Decomposition
model_name = "NNETAR + Decomposition"
model = function(y, h){
  fit = stlm(y,s.window="periodic", robust=TRUE,modelfunction=nnetar, p=2,P=0)
  return(forecast(fit,h=h))
}
my_forecast_plot(model,model_name)

# compute CV MAE
cv_residuals = tsCV(train_data,model,initial = initial_obs,h=h) 
mae = colMeans(abs(cv_residuals), na.rm = T)
print(paste("Average MAE across all the horizons for model",model_name,":",round(mean(mae),1)))

# store MAE
models_mae = rbind(models_mae,data.frame(model = model_name, mae,horizon = 1:h,row.names = NULL))





############## show results CV
print_scores(models_mae)
get_best_models(models_mae)

plot_scores(models_mae)
plot_scores(models_mae,naive= F)
plot_scores(models_mae[models_mae$model %in% c("SARIMA","TBATS","RW+Decomposition","NNETAR + Decomposition"),])



############################################# EVALUATION

mae_test = NULL

# fit the best model on training data
fitted_nnetar = stlm(train_data,s.window="periodic", robust=TRUE,modelfunction=nnetar, p=2,P=0)
test_residuals = colMeans(abs(evaluate_test_residuals(fitted_nnetar,stlm,data)),na.rm=T)
print(paste("Mean abs. error NNETAR+Decomposition on test set:",round(mean(test_residuals),2)))

ggplot(NULL,aes(x = 1:12, y = test_residuals))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = 1:12) +
  xlab("horizon [months]")+
  ylab("MAE")+
  ggtitle("NNETAR+Decomposition - MAE test set")
  
