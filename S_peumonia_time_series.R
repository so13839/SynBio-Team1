library(tidyverse)
library(xgboost)
library(lubridate)

#Data <- read_csv("2024_05_28 atlas_antibiotics.csv")

#Select Africa countries from the merged data
#Africa
merged_dataA <- Data %>%
  filter(Country %in% c("Cameroon", "Egypt", "Ghana", "Ivory Coast", "Kenya",
                        "Malawi", "Mauritius", "Morocco", "Namibia", "Nigeria",
                        "South Africa", "Tunisia", "Uganda"))

# North America
merged_dataNAm <- Data %>%
  filter(Country %in% c("Canada", "United States"
  ))

#Select Streptococcus pneumoniae
#Africa 
Staphy_AF <- subset(merged_dataA, Species == "Streptococcus pneumoniae")

#North America
Staphy_NA <- subset(merged_dataNAm, Species == "Streptococcus pneumoniae")


#Select antibiotics
Staphy_AF_NA <- Staphy_NA %>%
  select(`Isolate Id`, Gender, `Age Group`, Speciality, Source, Country, `In / Out Patient`,Year, Ceftazidime_I, Erythromycin_I, Clarithromycin_I)

# Create a data frame with the provided data
# Function to calculate the percentage of "Resistant" cases for each year
calculate_resistant_percentage <- function(df) {
  # Filter the data for "Resistant" entries
  resistant_data <- df[df == "Resistant"]
  
  # Calculate the percentage for each year
  df %>%
    group_by(Year) %>%
    summarise(
      Ceftazidime_Resistant = sum(Ceftazidime_I == "Resistant", na.rm = TRUE),
      Clarithromycin_Resistant = sum(Clarithromycin_I == "Resistant", na.rm = TRUE),
      Erythromycin_Resistant = sum(Erythromycin_I == "Resistant", na.rm = TRUE),
      Total_Entries = n()
    ) %>%
    mutate(
      Ceftazidime_Percentage = (Ceftazidime_Resistant / Total_Entries) * 100,
      Clarithromycin_Percentage = (Clarithromycin_Resistant / Total_Entries) * 100,
      Erythromycin_Percentage = (Erythromycin_Resistant / Total_Entries) * 100
    ) %>%
    select(Year, Ceftazidime_Percentage, Clarithromycin_Percentage, Erythromycin_Percentage)
}

# Calculate the resistant percentage by year
resistant_percentages <- calculate_resistant_percentage(Staphy_AF_NA)

# Print the result
print("Resistant Percentage by Year:")
print(resistant_percentages)
#This will be a time series data 

dataT <- data.frame(
  Year = 2005:2022,
  Y = c(28.8, 32.5, 24.5, 32.5, 33.0, 36.4, 38.2, 37.9, 40.9, 40.2, 34.6,
        39.9, 34.7, 35.6, 34.0, 32.6, 43.7,32.0)
)


#Convert Year numeric to year month
dataT$Year <- as.Date.yearmon(dataT$Year)
dataT
str(dataT)
#Change to tibble

dataTT <- as_tibble(dataT)

#Now I will generate index values for my future forecast. 
#Let that be a 12 years prediction.
extended_dataTT <- dataTT %>% 
  rbind(tibble::tibble(Year = seq.Date(from = lubridate::as_date("2023-01-01"),
                                       by = "year", length.out = 12), Y = rep(NA, 12)))
tail(extended_dataTT)

#Now we need to take care of the date column. xgboost does not tackle date columns well, so we need to split it into several columns, 
#describing the granularity of the time. In this case months and years:

extended_data_modTT <- extended_dataTT %>%
  dplyr::mutate(., 
                months = lubridate::month(Year),
                years = lubridate::year(Year))
#Now we can split the data into training set and prediction set:

train <- extended_data_modTT[1:nrow(dataTT), ] # initial data
pred <- extended_data_modTT[(nrow(dataTT) + 1):nrow(extended_dataTT), ] # extended time index

#In order to use xgboost we need to transform the data into a matrix form and extract the target variable. Additionally we need to 
#get rid of the dates columns and just use the newly created ones:


#trainig <- sparse.model.matrix( ~ .-1, data = train)

x_train <-  as.matrix(train %>%
                        dplyr::select(years))
x_pred <-  as.matrix(pred %>% 
                       dplyr::select(years))

y_train <- train$Y

xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(10, 200),
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
  stats::ts(start = zoo::as.yearmon(min(dataTT$Year)), 
            end = zoo::as.yearmon(max(dataTT$Year)),
            frequency = 12)

### prediction in a form of ts object
xgb_forecast <- xgb_pred %>%
  stats::ts(start = zoo::as.yearmon(min(dataTT$Year)),
            end = zoo::as.yearmon(max(dataTT$Year)),
            frequency = 12)

### original data as ts object
ts <- y_train %>% 
  stats::ts(start = zoo::as.yearmon(min(dataTT$Y)), 
            end = zoo::as.yearmon(max(dataTT$Y)), 
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




