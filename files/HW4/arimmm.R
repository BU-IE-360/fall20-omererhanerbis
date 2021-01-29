library(forecast)
library(tseries)
library(ggplot2)
library(zoo)
library(xts)
library(dplyr)
library(tidyr)
library(MLmetrics)
library(urca)
library(astsa)
library(readr)
library(data.table)
library(readr)





# write_csv(bulk_consumption_with_temp, "/Users/omererhanerbis/Downloads/ARIMA_IE/IEdata.csv")
# consumption indices and data
getwd()
bulk_consumption_with_temp <- read.csv("IEdata.csv")
names(bulk_consumption_with_temp) <- c("Date", "Hour", "Consumption")
bulk_consumption_with_temp <- as.data.table(bulk_consumption_with_temp)
bulk_consumption =bulk_consumption_with_temp %>% 
  group_by(Date) %>% 
  summarise(Consumption = sum(Consumption)
  )
bulk_consumption$Date <- as.Date(bulk_consumption$Date)
head(bulk_consumption)






# feature indices and data
all_features_df <- read.csv("all_features_with_dummies.csv")
head(all_features_df)
tail(all_features_df)
dt_daily <- head(all_features_df, 1484)
tail(dt_daily)
dt_daily <- as.data.table(dt_daily)
tail(dt_daily)





# test train prediction determination
test_dt_daily<- dt_daily[1471:(nrow(dt_daily)),]
dt_daily <- dt_daily[1:1470,]
guess_ahead <- nrow(test_dt_daily)

train_consumption_daily <- bulk_consumption$Consumption[1:1470]
test_consumption_daily <- tail(bulk_consumption$Consumption, (length(bulk_consumption$Consumption)-nrow(dt_daily)))







# time series conversion
ts_train_consumption_daily <- ts(train_consumption_daily, frequency = 7)
ts_test_consumption_daily <- ts(test_consumption_daily, frequency = 7)

decomposed_ts_train_consumption_daily <- decompose(ts_train_consumption_daily)
plot(decomposed_ts_train_consumption_daily)
decomp_ts_random <- decomposed_ts_train_consumption_daily$random
summary(ur.kpss(decomp_ts_random))


par(mfrow = c(3,1))
plot(decomp_ts_random)
acf(decomp_ts_random, na.action = na.pass)
pacf(decomp_ts_random, na.action = na.pass)
par(mfrow = c(1,1))


# logging the data
logged_ts_train_consumption_daily <- log(ts_train_consumption_daily)
logged_ts_test_consumption_daily <- log(ts_test_consumption_daily)

decomposed_logged_ts_train_consumption_daily <- decompose(logged_ts_train_consumption_daily)
plot(logged_ts_train_consumption_daily)
decomp_logged_ts_random <- decomposed_logged_ts_train_consumption_daily$random
summary(ur.kpss(decomp_logged_ts_random))


par(mfrow = c(3,1))
plot(decomp_logged_ts_random)
acf(decomp_logged_ts_random, na.action = na.pass)
pacf(decomp_logged_ts_random, na.action = na.pass)
par(mfrow = c(1,1))


# differencing with lag7
differenced_logged_ts_train_consumption_daily <- ts(diff(ts_train_consumption_daily, 7), frequency = 7)
decomposed_differenced_logged_ts_train_consumption_daily <- decompose(differenced_logged_ts_train_consumption_daily)
decomp_diffed_logged_ts_random <- decomposed_differenced_logged_ts_train_consumption_daily$random
summary(ur.kpss(decomp_diffed_logged_ts_random))





# outlier_indexes <- tsoutliers(decomp_ts_random)$index
# outlier_repl <- tsoutliers(decomp_ts_random)$replacements
# head(as.data.table(bulk_consumption[outlier_indexes,],),77)
# newyear_indices <- c(1,outlier_indexes[c(15,33,51,76)])
# left_outlier_indexes <- outlier_indexes[-c(15,33,51,76)]
# 
# 
# 
# outlier_indexes <- tsoutliers(decomp_ts_random)$index
# outlier_values <- tsoutliers(decomp_ts_random)$pred
# head(as.data.table(bulk_consumption[outlier_indexes,],),77)
# newyear_indices <- c(1,outlier_indexes[c(15,33,51,76)])
# newyear_indices
# s<-tsoutliers(decomp_ts_random)
# s$
# # newyear_corrector <- function(vec, data)
# {
#   new_values <- c()
#   for(i in vec)
#   {
#     if(i-7 < 0)
#     {
#       new_values  <- c(new_values, mean(mean(data[c(c((i+1):(i+6)))]), mean(data[i+7])))
#     }
#     else if(i + 7 > length(data))
#     {
#       new_values <- c(new_values, mean(mean(data[c(c((i+1):(i+6)))]), mean(c(data[i-7]))))
#     }
#     else
#     {
#       new_values <- c(new_values, mean(mean(data[c(c((i-6):(i-1)), c((i+1):(i+6)))]), mean(c(data[i-7],data[i+7]))))
#     }
#   }
#   return(new_values)
# }

# head(newyear_corrector(newyear_indices, ts_train_consumption_daily))
# 
# 
# ts_train_consumption_daily[newyear_indices]
# # ts_train_consumption_daily[newyear_indices] <- newyear_corrector(newyear_indices, ts_train_consumption_daily)
# summary(ur.kpss(decompose(ts_train_consumption_daily)$random))
# plot(decompose(ts_train_consumption_daily)$random)
# 
# 
# 
# 
# 
# other_outliers_corrector <- function(vec, data)
# {
#   new_values <- c()
#   for(i in vec)
#   {
#     if(i-2 < 0)
#     {
#       new_values  <- c(new_values, mean(c(data[(i):(i+4)])))
#     }
#     else if(i + 2 > length(data))
#     {
#       new_values <- c(new_values, mean(c(data[(i-4):(i)])))
#     }
#     else
#     {
#       new_values <- c(new_values, mean(c(data[(i-2):(i+2)])))
#     }
#   }
#   return(new_values)
# }
# 
# head(other_outliers_corrector(left_outlier_indexes, ts_train_consumption_daily))
# 
# 
# ts_train_consumption_daily[left_outlier_indexes] <- other_outliers_corrector(left_outlier_indexes, ts_train_consumption_daily)
# summary(ur.kpss(decompose(ts_train_consumption_daily)$random))
# plot(decompose(ts_train_consumption_daily)$random)

tsoutliers(decomp_ts_random)$index
outlier_indexes <- tsoutliers(decomp_ts_random)$index
outlier_values <- tsoutliers(decomp_ts_random)$replacements
newyear_indices <- c(1,outlier_indexes[c(15,33,51,76)])
newyear_indices
ts_train_consumption_daily[outlier_indexes] <- outlier_values
summary(ur.kpss(decompose(ts_train_consumption_daily)$random))
plot(decompose(ts_train_consumption_daily)$random)



still_outliers <- tsoutliers(ts_train_consumption_daily)
ts_train_consumption_daily[still_outliers$index] <- still_outliers$replacements

summary(ur.kpss(decompose(ts_train_consumption_daily)$random))
plot(decompose(ts_train_consumption_daily)$random)





xregr <- cbind(dt_daily$covid_severity,
               dt_daily$sunlight_time_minutes,
               dt_daily$production_capacity_rate,
               dt_daily$price_of_electricity,
               dt_daily$new_year,
               dt_daily$nat_holiday,
               dt_daily$sacrifice_holiday,
               dt_daily$sacrifice_eve,
               dt_daily$ramadan_holiday,
               dt_daily$ramadan_eve,
               dt_daily$monday_or_friday_between_holidays,
               dt_daily$extra_holidays,
               dt_daily$full_lockdown)


newxregr <- cbind(test_dt_daily$covid_severity,
                  test_dt_daily$sunlight_time_minutes,
                  test_dt_daily$production_capacity_rate,
                  test_dt_daily$price_of_electricity,
                  test_dt_daily$new_year,
                  test_dt_daily$nat_holiday,
                  test_dt_daily$sacrifice_holiday,
                  test_dt_daily$sacrifice_eve,
                  test_dt_daily$ramadan_holiday,
                  test_dt_daily$ramadan_eve,
                  test_dt_daily$monday_or_friday_between_holidays,
                  test_dt_daily$extra_holidays,
                  test_dt_daily$full_lockdown)





# auto.arima parameter finding

auto.arima(ts_train_consumption_daily, xreg = xregr, seasonal = T, trace = T)









mmmf <- sarima.for(ts_train_consumption_daily, 1,0,0,0,0,0,7, n.ahead = guess_ahead, xreg = xregr,  newxreg = newxregr)


perf_dt(actual = test_consumption_daily, forecast = mmmf$pred)




mmmf <- sarima.for(ts_train_consumption_daily, 0,0,0,1,0,0,7, n.ahead = guess_ahead, xreg = xregr,  newxreg = newxregr)


perf_dt(actual = test_consumption_daily, forecast = mmmf$pred)






# sarima model

mmm <- sarima(ts_train_consumption_daily, 2,0,0,2,1,0,7, xreg = xregr)
mmm$ttable


# sarima forecasting

mmmf <- sarima.for(ts_train_consumption_daily, 2,0,0,2,1,0,7, n.ahead = guess_ahead, xreg = xregr, newxreg = newxregr)


# plot for forecast and actual
par(mfrow= c(2,1))
plot(mmmf$pred)
plot(ts_test_consumption_daily, type = "l")
par(mfrow= c(1,1))



# performance metrics function

perf_dt=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  FBias=sum(error)/sum(actual)
  MPE=sum(error/actual)/n
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2))/n
  MAD=sum(abs(error))/n
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,FBias,MAPE,RMSE,MAD,WMAPE)
  return(l)
}



# performance metrics

perf_dt(actual = test_consumption_daily, forecast = mmmf$pred)








# moving_window approach



sliding_is_fun <- function(regressed = ts_train_consumption_daily, train_regressor = xregr, test_regressor = newxregr, sliding_window = 1)
{
  slicet <- nrow(test_regressor)
  holder <- nrow(regressed)
  pred <- c()
  # tester_regressor <- matrix(rep(1, 13), nrow = 1)
  for(i in 1:slicet)
  {
    tester_regressor <- matrix(c(test_regressor[i,]), nrow = 1)
    guessor <- sarima.for(regressed, 2,0,0,2,1,0,7, n.ahead = sliding_window, xreg = train_regressor, newxreg = tester_regressor)
    regressed <- c(regressed, guessor$pred)
    pred <- c(pred, guessor$pred)
    train_regressor <- rbind(train_regressor, tester_regressor)
  }
  return(pred)
}

moving_forecasted_values <- sliding_is_fun()

perf_dt(actual = test_consumption_daily, forecast = moving_forecasted_values)








moving_forecasted_values

