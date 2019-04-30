#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
options(shiny.sanitize.errors = FALSE)

library(ggplot2)
library(leaflet)
library(shiny)


navbarPage("NYC Green Taxi", id = 'map1',
           #################### 1st panel done ####################
           
           tabPanel("Taxi Rides Map",
                    
                    div(class = "outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        tags$style(
                          ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"
                        ),
                        leafletOutput("map1", width = "100%", height = "100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 40, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Taxi Rides Map"),
                                      
                                      selectInput("tmp", label = "Temperature", 
                                                  choices = c("< -10 ℃", "-10 ℃ -- 0 ℃", "0 ℃ -- 10 ℃", 
                                                              "10 ℃ -- 20 ℃", "20 ℃ <"), 
                                                  selected = "Sunny"),
                                      checkboxInput("pickup", "Pick-up", TRUE),
                                      checkboxInput("dropoff", "Drop-off", FALSE),

                                      
                                      sliderInput("hour", "Hours of Day:", label = "Choose the time of the day:",
                                                  min = 0, max = 23, value = 12, step = 1)
                                      
                        ),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",
                                      width = 160, height = 200,
                                      radioButtons("info", label = "Select Information",
                                                   choices = list("Number of trips", "Average trip duration",
                                                                  "Average fare", "Average % of tips"), 
                                                   selected = "Number of trips")
                                      
                                    
                    
                        
                    )
           )
           )
        

           
)
