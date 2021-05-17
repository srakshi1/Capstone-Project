Project description: 

Our project focuses on tracking greenhouse gas emissions with respect to time (1961-2016) of 60 countries worldwide. The carbon dioxide, known as the greenhouse gas, is very closely related to public health, especially to heat-related morbidity and mortality. The objective of our project is to develop a global environmental tracker tool. Our shiny app consists of three pages. On the first page, we offered a colorful scatterplot for displaying the metric users select of all the countries. This gives the user an overview of the environmental trend globally. 
Our second page provides an interactive global map paired with metric analysis of a specific country.
Our third page, which is CO2 emission specific, displays CO2 emissions with respect to time and offers predictions of the future CO2 emissions. We assumed the environmental data changes exponentially with respect to time and we implemented exponential fit to the available data for making the predictions. The users are able to select environmental features, countries and input a year they want to predict when using our app. All the data was downloaded from WorldBank.org, and there are no library requirements for our shiny app to run since the csv file is online in a github repository.


Personal contribution:

Yingnan Zhang: Data preprocessing; feature selection; created page "Global environmental data distribution"; displayed time series and implemented exponential fit prediction; project description drafting.

Brice: Shiny app UI design, created shiny app code skeleton; created panel for user inputs; displayed interactive global map;

Shardul: Implemented linear regression algorithm; helped debugging, participated in UI design; 

Shiny App link:
Video link: 
