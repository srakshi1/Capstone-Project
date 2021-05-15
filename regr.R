library(R.matlab)
library(tidyverse)
library(ggplot2)
library(plotly)
library(glmnet)
library("readxl")

linreg = function(country){
  my_data <- read_excel("reconstructed_data.xlsx")
  my_data <- as.data.frame(my_data)
  my_data$'2020' = NULL
  my_data$'2019' = NULL
  my_data$'2018' = NULL
  my_data$'2017' = NULL
  my_data$'1960' = NULL
  my_data <- as.data.frame(na.omit(my_data))
  df = filter(my_data,CountryName==country)
  if (length(rownames(df)) != 14){
    print('Invalid Country')
    return(NULL)
  }
  df$'...1' = NULL
  rownames(df) <- df$IndicatorName
  df$IndicatorName = NULL
  df$CountryCode = NULL
  df$CountryName = NULL
  df$IndicatorCode = NULL
  df <- as.data.frame(t(df))
  y <- df$`CO2 emissions (kt)`
  fit <- lm(formula = y ~ df$`Land area (sq. km)`+df$`Surface area (sq. km)`+df$`Arable land (% of land area)`+df$`Total fisheries production (metric tons)`+df$`Capture fisheries production (metric tons)`+df$`Agricultural land (% of land area)`+df$`CO2 emissions from liquid fuel consumption (kt)`+df$`CO2 emissions (metric tons per capita)`+df$`CO2 emissions from gaseous fuel consumption (kt)`+df$`CO2 emissions from solid fuel consumption (kt)`+df$`CO2 emissions from solid fuel consumption (% of total)`+df$`CO2 emissions from liquid fuel consumption (% of total)`+df$`CO2 emissions from gaseous fuel consumption (% of total)`)
  return(fit)
}
h <- linreg('Afghanistan')
h <- as.data.frame(coefficients(h))
for (i in 2:length(rownames(h))){
  rownames(h)[i]=substr(rownames(h)[i],4,nchar(rownames(h)[i]))
}
bar_plot <- ggplot(h,aes(x=rownames(h),y=coefficients(h)))+geom_bar(stat='identity')+ theme(axis.text.x=element_text(angle=90,hjust=1))
