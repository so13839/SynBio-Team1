library(tidyverse)
library(xgboost)
library(lubridate)

data("economics")
#economics

#There are several time related columns, but for this 
#time series forecast I will use one – unemploy.
data <- economics %>% dplyr::select(date, unemploy)
data
#Now I will generate index values for my future forecast. 
#Let that be a 12 months prediction.

extended_data <- data %>% 
  rbind(tibble::tibble(date = seq.Date(from = lubridate::as_date("2015-05-01"),
                                       by = "month", length.out = 12), unemploy = rep(NA, 12)))
tail(extended_data)

#Now we need to take care of the date column. xgboost does not tackle date columns well, so we need to split it into several columns, 
#describing the granularity of the time. In this case months and years:

extended_data_mod <- extended_data %>%
  dplyr::mutate(., 
                months = lubridate::month(date),
                years = lubridate::year(date))

#Now we can split the data into training set and prediction set:
train <- extended_data_mod[1:nrow(data), ] # initial data

pred <- extended_data_mod[(nrow(data) + 1):nrow(extended_data), ] # extended time index

#In order to use xgboost we need to transform the data into a matrix form and extract the target variable. Additionally we need to 
#get rid of the dates columns and just use the newly created ones:

x_train <- xgboost::xgb.DMatrix(as.matrix(train %>%
                                            dplyr::select(months, years)))
x_pred <- xgboost::xgb.DMatrix(as.matrix(pred %>% 
                                          dplyr::select(months, years)))
y_train <- train$unemploy

#xgboost prediction
#With data prepared as in a previous section we can perform the modeling in the same manner as if we were not dealing with the time series data. We need to provide the space of parameters for model tweaking. We specify the cross-validation 
#method with number of folds and also enable parallel computations.

# Convert x_train to a matrix and y_train to a numeric vector or factor

####New with everything
data <- economics %>% dplyr::select(date, unemploy)

extended_data <- data %>% 
  rbind(tibble::tibble(date = seq(from = lubridate::as_date("2015-05-01"),
                                  by = "month", length.out = 12), 
                       unemploy = rep(NA, 12)))

#Months
extended_data_mod <- extended_data %>%
  dplyr::mutate(., 
                months = lubridate::month(date),
                years = lubridate::year(date))

train <- extended_data_mod[1:nrow(data), ] # initial data

pred <- extended_data_mod[(nrow(data) + 1):nrow(extended_data), ] # extended time index

#trainig <- sparse.model.matrix( ~ .-1, data = train)

x_train <-  as.matrix(train %>%
                        dplyr::select(months, years))
x_pred <-  as.matrix(pred %>% 
                       dplyr::select(months, years))

y_train <- train$unemploy

xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = 0.1, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))

xgb_model <- caret::train(
  x_train, y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1
)

### Let’s check the best values that were chosen as hyperparameters:
xgb_model$bestTune

### And perform the forecast:

xgb_pred <- xgb_model %>% stats::predict(x_pred)
xgb_pred

###Forecast object
###As we have the values predicted, 
###we can turn the results into the forecast 
###object, as we would get if using the forecast package.
###That will allow i.e. to use the forecast::autoplot
###function to plot the results of the prediction. 
###In order to do so, we need to define several objects that build a forecast object.

### prediction on a train set
fitted <- xgb_model %>%
  stats::predict(x_train) %>%
  stats::ts(start = zoo::as.yearmon(min(train$date)), 
            end = zoo::as.yearmon(max(train$date)),
            frequency = 12)

### prediction in a form of ts object
xgb_forecast <- xgb_pred %>%
  stats::ts(start = zoo::as.yearmon(min(pred$date)),
            end = zoo::as.yearmon(max(pred$date)),
            frequency = 12)

### original data as ts object
ts <- y_train %>% 
  stats::ts(start = zoo::as.yearmon(min(train$date)), 
            end = zoo::as.yearmon(max(train$date)), 
            frequency = 12)

### forecast object
forecast_list <- list(
  model = xgb_model$modelInfo,
  method = xgb_model$method,
  mean = xgb_forecast,
  x = ts, 
  fitted = fitted,
  residuals = as.numeric(ts) - as.numeric(fitted)
)
class(forecast_list) <- "forecast"

forecast_list

### Now we can easily plot the data
forecast::autoplot(forecast_list)

### xgboost forecast with regressors
### Nice thing about forecasting with 
### xgboost is that we can use different regressors with
### our time series easily. To do so, we just need to extend the 
### xgboost data matrices properly.

### x_train <- xgboost::xgb.DMatrix(cbind(
### as.matrix(dataTT %>% dplyr::select(Year)),
### reg_train))
#x_pred <- xgboost::xgb.DMatrix(as.matrix(pred %>% 
#                                           dplyr::select(months, years)),
# reg_pred)
#
#y_train <- data$value


### Question
### Is there an easy way to convert this 
### solution to be working on weekly data (or even daily) ??

# Yes! The key is to specify your artificial time related columns properly. If daily data – daily granularity, if weekly data – weekly granularity ect.

### For some daily dataset:
  extended_data_mod <- extended_data %>%
  dplyr::mutate(.,
                days = lubridate::day(Date),
                months = lubridate::month(Date),
                years = lubridate::year(Date))

### Then for the fitted values and prediction you need to pass daily index:
  fitted <- xgb_model %>%
  stats::predict(x_train) %>%
  stats::ts(start = c(lubridate::year(min(train$Date)), lubridate::yday(min(train$Date))),
            end = c(lubridate::year(max(train$Date)), lubridate::yday(max(train$Date))),
            frequency = 365)

xgb_forecast <- xgb_pred %>%
  stats::ts(start = c(lubridate::year(min(pred$Date)), lubridate::yday(min(pred$Date))),
            end = c(lubridate::year(max(pred$Date)), lubridate::yday(max(pred$Date))),
            frequency = 365)

