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

globDat <- as.data.frame(read_csv("reconstructed_data.csv", col_names = T))
regressors <- unique(globDat$IndicatorName)

globLocTot <- as.data.frame(read_csv("locations.csv", col_names = T))

globLoc <- c()
for(i in 1:length(unique(globDat$CountryName))) {
    if(unique(globDat$CountryName)[i] %in% globLocTot$name){
        globLoc <- rbind(globLoc, globLocTot[i,])
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
                tabPanel("Metric Projection", plotOutput({"plot"}))
            ),
            textOutput("test")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    locs <- reactive({
        dataset <- globDat
        countries <- input$checkGroup
        
        locations <- c()
        for(i in countries) {
            locations <- rbind(locations, globLoc[which(i == globLoc$name), c(2,3)])
        }
        
        as.matrix(locations)
    })
    
    output$test <- renderPrint({globLoc[2,]})
    
    output$mymap <- renderLeaflet({
        locs <- locs()
        countries <- input$checkGroup
        
        map <- leaflet() %>% addTiles()
        numreg <- length(countries)
        
        if(numreg > 1) {
            for(i in (1:length(countries))) {
                map <- addMarkers(map, lng = locs[i,2], lat = locs[i,1], popup = countries[i])
            }
        } else {
            map <- addMarkers(map, lng = locs[2], lat = locs[1], popup = countries[1]) %>% setView(lng = locs[2], lat = locs[1], zoom = 03)
        } 
        map
    })
    
    linregDat <- reactive({linreg(input$checkGroup, input$metrics, input$projectedYear)})
    
    output$plot <- renderPlot({
        fitted <- linregDat()
        ylimMin <- min(fitted$processed_data)
        ylimMax <- max(fitted$processed_data)
        p <- ggplot(fitted$processed_data,aes(year,original)) + geom_point() + geom_line(aes(y = smoothed), color = "red", linetype = "dashed")+
            geom_point(aes(x=fitted$predict_year, y=fitted$predict_result), colour="blue",shape=23, fill="blue", size=3)+
            geom_text(x=fitted$predict_year, y=fitted$predict_result, label=paste(fitted$predict_result),color='blue',size = 3.5)
        # Change predicted value to different shape. maybe showing value of it.
        p <- p+labs(y = 'Logarithmic data')+labs(title = paste(fitted$feature,'and prediction in year',fitted$predict_year))
        p
    })
    
}

linreg = function(country,feature,predict_year){
    my_data <- read_csv("reconstructed_data.csv")
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
    my_list <- list("feature" = feature, "processed_data" = df_smoothed, "predict_year" = predict_year, "predict_result" = predict_value)
    return(my_list)
}

# Run the application 
shinyApp(ui = ui, server = server)
