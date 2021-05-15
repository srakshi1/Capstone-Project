library(R.matlab)
library(tidyverse)
library(ggplot2)
library(plotly)
library(glmnet)
library("readxl")

linreg = function(country,feature,predict_year){
  my_data <- read_excel("reconstructed_data.xlsx")
  my_data <- as.data.frame(my_data)
  my_data$'2020' = NULL
  my_data$'2019' = NULL
  my_data$'2018' = NULL
  my_data$'2017' = NULL
  my_data$'1960' = NULL
  my_data <- as.data.frame(na.omit(my_data))
  df = filter(my_data,CountryName==country)
  df$'...1' = NULL
  rownames(df) <- df$IndicatorName
  df$IndicatorName = NULL
  df$CountryCode = NULL
  df$CountryName = NULL
  df$IndicatorCode = NULL
  df <- as.data.frame(t(df))
  year <- as.integer(rownames(df))
  y_feature <- df %>% pull(feature)
  # Take log of data
  y_log_feature <- log(y_feature)
  fit <-lm(formula = y_log_feature ~ year)
  # Calculate smoothed line
  linear_smoothed = year * coef(fit)[2]+coef(fit)[1]
  # Calculate predicted value
  predict_value = predict_year * coef(fit)[2]+coef(fit)[1]
  # Preping predicted visualization
  new_year = 1961:predict_year
  predict_line = new_year * coef(fit)[2]+coef(fit)[1]
  df_smoothed <- data.frame(year = year, original = y_log_feature, smoothed = linear_smoothed)
  df_predict_plot <- data.frame(year = new_year, data = predict_line)
  
  my_list <- list("feature" = feature, "processed_data" = df_smoothed, "predict_plot" = df_predict_plot, "predict_year" = predict_year, "predict_result" = predict_value)
  return(my_list)
}
fitted <- linreg('China','CO2 emissions (kt)',2040)
p <- ggplot(fitted$processed_data,aes(year,original)) + geom_point() + geom_line(aes(y = smoothed), color = "red", linetype = "dashed")+geom_point(aes(x=fitted$predict_year, y=fitted$predict_result), colour="blue")
print(p)
# Change predicted value to different shape. maybe showing value of it.
p <- p+labs(y = 'Percentage % (Log)')+labs(title = fitted$feature)+geom_point(aes(x=fitted$predict_year, y=fitted$predict_result), colour="blue")
print(p)



