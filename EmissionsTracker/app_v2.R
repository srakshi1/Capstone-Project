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

globDat <- as.data.frame(read_csv("https://raw.githubusercontent.com/bgraha13/Capstone-Project/main/EmissionsTracker/reconstructed_data.csv", col_names = T))
globDat$'2020' = NULL
globDat$'2019' = NULL
globDat$'2018' = NULL
globDat$'2017' = NULL
globDat$'1960' = NULL
globDat <- as.data.frame(na.omit(globDat))

CO2_identifier <- c()
for (i in 1:dim(globDat)[1]) {
  CO2_identifier <- c(CO2_identifier,grepl("CO2", globDat$IndicatorName[i], fixed=TRUE))
}
#This is the final data for display
glob_CO2Dat <- globDat[CO2_identifier,]
regressors <- unique(glob_CO2Dat$IndicatorName)


globLocTot <- as.data.frame(read_csv("https://raw.githubusercontent.com/bgraha13/Capstone-Project/main/EmissionsTracker/locations.csv", col_names = T))

globLoc <- c()
for(i in 1:length(unique(globDat$CountryName))) {
  if(unique(globDat$CountryName)[i] %in% globLocTot$name){
    globLoc <- rbind(globLoc, globLocTot[which(unique(globDat$CountryName)[i] == globLocTot$name),])
  }
}

countries <- c(globLoc$name)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Environmental Tracker Tool"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('checkGroup', label = h3("Select Country:"), multiple = F, choices = c(countries), selected = "United States"),
      selectInput('metrics', label = h3("Select Metric:"), multiple = F, choices = c(regressors)),
      numericInput('projectedYear', label = h3("Input Year Projection:"), value = 2030)
    ),
    
    # Show a map of the selected country
    mainPanel(
      tabsetPanel(
        tabPanel("World Map", leafletOutput({"mymap"})),
        tabPanel("Metric Projection", plotlyOutput({"plot"}))
      ),
      dataTableOutput("test")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  locs <- reactive({
    dataset <- glob_CO2Dat
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
  
  linregDat <- reactive({linreg(input$checkGroup, input$metrics, input$projectedYear)})
  
  output$plot <- renderPlotly({
    fitted <- linregDat()
    p <- ggplot(fitted$processed_data,aes(year,original)) + geom_point() + geom_line(aes(y = smoothed), color = "red", linetype = "dashed")+
      geom_point(aes(x=fitted$predict_year, y=fitted$predict_result), colour="blue",shape=23, fill="blue", size=3)+
      geom_text(x=fitted$predict_year, y=fitted$predict_result, label=paste(fitted$predict_result),color='blue',size = 3.5)+
      annotate("text", x=2000, y=fitted$predict_result+1,label = paste("Rsquare =",fitted$stats)) +coord_cartesian(ylim = c(fitted$min,fitted$max) ) 
    # Change predicted value to different shape. maybe showing value of it.
    p <- p+labs(y = 'Logarithmic data')+labs(title = paste(fitted$feature,'and prediction in year',fitted$predict_year))
    ggplotly(p)
  })
  
}

linreg = function(country,feature,predict_year){
  df = filter(glob_CO2Dat,CountryName==country)
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
  if (max(y_log_feature) > predict_value){
    max_limit = max(y_log_feature)
  } else{
    max_limit = predict_value
  }
  min_limit = min(y_log_feature)
  df_smoothed <- data.frame(year = year, original = y_log_feature, smoothed = linear_smoothed)
  my_list <- list("feature" = feature, "processed_data" = df_smoothed, "predict_year" = predict_year, "predict_result" = predict_value,"stats"=format(round(summary(fit)$r.squared, 2), nsmall = 2),"max" = max_limit, "min" = min_limit)
  return(my_list)
}

# Run the application 
shinyApp(ui = ui, server = server)