#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(plotly)
library(glmnet)
library(markdown)
library(gridExtra)

globDat <- as.data.frame(read_csv("https://raw.githubusercontent.com/bgraha13/Capstone-Project/main/EmissionsTracker/reconstructed_data.csv", col_names = T))
globDat$'2020' = NULL
globDat$'2019' = NULL
globDat$'2018' = NULL
globDat$'2017' = NULL
globDat$'1960' = NULL
globDat <- as.data.frame(na.omit(globDat))
globLocTot <- as.data.frame(read_csv("https://raw.githubusercontent.com/bgraha13/Capstone-Project/main/EmissionsTracker/locations.csv", col_names = T))


# This part includes all data
all_regressors <- unique(globDat$IndicatorName)
globLoc_all <- c()
for(i in 1:length(unique(globDat$CountryName))) {
  if(unique(globDat$CountryName)[i] %in% globLocTot$name){
    globLoc_all <- rbind(globLoc_all, globLocTot[which(unique(globDat$CountryName)[i] == globLocTot$name),])
  }
}
countries_all <- c(globLoc_all$name)
selected_df_all <- data.frame()
for (k in 1:length(countries_all)) {
  temp = filter(globDat, CountryName == countries_all[k])
  if (dim(temp)[1] == 15) {
    selected_df_all <-rbind(selected_df_all,temp)
  }
}
selected_countries_all <-unique(selected_df_all$CountryName)




#This part extracted CO2 related data
CO2_identifier <- c()
for (i in 1:dim(globDat)[1]) {
  CO2_identifier <- c(CO2_identifier,grepl("CO2", globDat$IndicatorName[i], fixed=TRUE))
}

glob_CO2Dat <- globDat[CO2_identifier,]
CO2_regressors <- unique(glob_CO2Dat$IndicatorName)

globLoc_CO2 <- c()
for(i in 1:length(unique(glob_CO2Dat$CountryName))) {
  if(unique(glob_CO2Dat$CountryName)[i] %in% globLocTot$name){
    globLoc_CO2 <- rbind(globLoc_CO2, globLocTot[which(unique(glob_CO2Dat$CountryName)[i] == globLocTot$name),])
  }
}
#This is the final CO2 data for display, selected_df as data and selected_countries as country name
countries_CO2 <- c(globLoc_CO2$name)
selected_df_CO2 <- data.frame()
for (j in 1:length(countries_CO2)) {
  temp = filter(glob_CO2Dat, CountryName == countries_CO2[j])
  if (dim(temp)[1] == 8) {
    selected_df_CO2 <-rbind(selected_df_CO2,temp)
  }
}
selected_countries_CO2 <-unique(selected_df_CO2$CountryName)








# Define UI for application that draws a histogram
ui <- navbarPage("Environmental Tracker Tool",
  tabPanel("Global environmental data plots",
    sidebarLayout(
      sidebarPanel(
        selectInput('checkGroup', label = h3("Select Country:"), multiple = F, choices = c(selected_countries_all), selected = "United States"),
        selectInput('metrics', label = h3("Select Metric:"), multiple = F, choices = c(all_regressors)),
        numericInput('projectedYear', label = h3("Input Year Projection:"), value = 2030)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("World Map", leafletOutput({"mymap"})),
          tabPanel("Metric Projection", plotOutput({"plot"}))
        ),
        textOutput("test")
      )
    )
  ),
  tabPanel("CO2 emission plots",
           sidebarLayout(
             sidebarPanel(
               selectInput('Country1', label = h3("Select Country1:"), multiple = F, choices = c(selected_countries_CO2), selected = "United States"),
               selectInput('Country2', label = h3("Select Country2:"), multiple = F, choices = c(selected_countries_CO2), selected = "China"),
               selectInput('feature', label = h3("Select Metric:"), multiple = F, choices = c(CO2_regressors)),
               numericInput('predictYear', label = h3("Input Year Projection:"), value = 2030)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          fluidRow(
                            plotOutput("CO2_plots")
                          )
                 )
               )
             )
           )
  )
)

  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  locs <- reactive({
    dataset <- selected_df
    countries <- input$checkGroup
    
    locations <- c()
    for(i in countries) {
      locations <- rbind(locations, globLoc[which(i == globLoc$name), c(2,3)])
    }
    
    as.matrix(locations)
  })
  
  #output$test <- renderPrint({globLoc[2,]})
  
  output$mymap <- renderLeaflet({
    locs <- locs()
    countries <- input$checkGroup
    
    map <- leaflet() %>% addTiles()
    numreg <- length(countries)
    
    if(numreg > 1) {
      for(i in (1:length(countries))) {
        map <- addMarkers(map, lng = locs[i,2], lat = locs[i,1],popup = countries[i])
      }
    } else {
      map <- addMarkers(map, lng = locs[2], lat = locs[1], popup = countries[1]) %>% setView(lng = locs[2], lat = locs[1], zoom = 03)
    } 
    map
  })
  
  globRegDat <- reactive({globReg(input$checkGroup, input$metrics, input$projectedYear)})
  
  output$plot <- renderPlot({
    fitted <- globRegDat()
    p <- ggplot(fitted$processed_data,aes(year,original)) + geom_point() + geom_line(aes(y = smoothed), color = "red", linetype = "dashed")+
      geom_point(aes(x=fitted$predict_year, y=fitted$predict_result), colour="blue",shape=23, fill="blue", size=3)+
      geom_text(x=fitted$predict_year, y=fitted$predict_result, label=paste(fitted$predict_result),color='blue',size = 3.5)
    # Change predicted value to different shape. maybe showing value of it.
    p <- p+labs(y = 'Logarithmic data')+labs(title = paste(fitted$feature,'and prediction in year',fitted$predict_year))
    p
  })
  
  country1_CO2 <- reactive({linreg(input$Country1, input$feature, input$predictYear)})
  country2_CO2 <- reactive({linreg(input$Country2, input$feature, input$predictYear)})
  
  output$CO2_plots <- renderPlot({
    fitted1 <- country1_CO2()
    fitted2 <- country2_CO2()
    p1 <- ggplot(fitted1$processed_data,aes(year,original)) + geom_point() + geom_line(aes(y = smoothed), color = "red", linetype = "dashed")+
      geom_point(aes(x=fitted1$predict_year, y=fitted1$predict_result), colour="blue",shape=23, fill="blue", size=3)+
      geom_text(x=fitted1$predict_year, y=fitted1$predict_result, label=paste(fitted1$predict_result),color='blue',size = 3.5)+
      labs(y = 'Logarithmic data')
    p2 <- ggplot(fitted2$processed_data,aes(year,original)) + geom_point() + geom_line(aes(y = smoothed), color = "red", linetype = "dashed")+
      geom_point(aes(x=fitted2$predict_year, y=fitted2$predict_result), colour="blue",shape=23, fill="blue", size=3)+
      geom_text(x=fitted2$predict_year, y=fitted2$predict_result, label=paste(fitted2$predict_result),color='blue',size = 3.5)+
      labs(y = 'Logarithmic data')
    grid.arrange(p1,p2)
  })
}

globReg <- function(country,feature,predict_year){
  df = filter(selected_df_all,CountryName==country)
  df$'...1' = NULL
  rownames(df) <- df$IndicatorName
  df$IndicatorName = NULL
  df$CountryCode = NULL
  df$CountryName = NULL
  df$IndicatorCode = NULL
  df <- as.data.frame(t(df))
  year <- as.integer(rownames(df))
  y_feature <- df %>% pull(feature)
  #Eliminate zeros
  y_feature_edit = y_feature[y_feature !=0]
  year_edit = year[y_feature !=0]
  # Take log of data
  y_log_feature <- log(y_feature_edit)
  fit <-lm(formula = y_log_feature ~ year_edit)
  # Calculate smoothed line
  linear_smoothed = year_edit * coef(fit)[2]+coef(fit)[1]
  # Calculate predicted value
  predict_value = predict_year * coef(fit)[2]+coef(fit)[1]
  if (max(y_log_feature) > predict_value){
    max_limit = max(y_log_feature)
  } else{
    max_limit = predict_value
  }
  min_limit = min(y_log_feature)
  df_smoothed <- data.frame(year = year_edit, original = y_log_feature, smoothed = linear_smoothed)
  my_list <- list("feature" = feature, "processed_data" = df_smoothed, "predict_year" = predict_year, "predict_result" = predict_value,"stats"=format(round(summary(fit)$r.squared, 2), nsmall = 2),"max" = max_limit, "min" = min_limit)
  return(my_list)
}

linreg = function(country,feature,predict_year){
  df = filter(selected_df_CO2,CountryName==country)
  df$'...1' = NULL
  rownames(df) <- df$IndicatorName
  df$IndicatorName = NULL
  df$CountryCode = NULL
  df$CountryName = NULL
  df$IndicatorCode = NULL
  df <- as.data.frame(t(df))
  year <- as.integer(rownames(df))
  y_feature <- df %>% pull(feature)
  #Eliminate zeros
  y_feature_edit = y_feature[y_feature !=0]
  year_edit = year[y_feature !=0]
  # Take log of data
  y_log_feature <- log(y_feature_edit)
  fit <-lm(formula = y_log_feature ~ year_edit)
  # Calculate smoothed line
  linear_smoothed = year_edit * coef(fit)[2]+coef(fit)[1]
  # Calculate predicted value
  predict_value = predict_year * coef(fit)[2]+coef(fit)[1]
  if (max(y_log_feature) > predict_value){
    max_limit = max(y_log_feature)
  } else{
    max_limit = predict_value
  }
  min_limit = min(y_log_feature)
  df_smoothed <- data.frame(year = year_edit, original = y_log_feature, smoothed = linear_smoothed)
  my_list <- list("feature" = feature, "processed_data" = df_smoothed, "predict_year" = predict_year, "predict_result" = predict_value,"stats"=format(round(summary(fit)$r.squared, 2), nsmall = 2),"max" = max_limit, "min" = min_limit)
  return(my_list)
}

# Run the application 
shinyApp(ui = ui, server = server)