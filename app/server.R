#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(readr)
library(ggpubr)
library(data.table)


color = list(color1 = c('#F2D7D5','#D98880','#CD6155','#C0392B',"#A60000",'#641e16',"#3a110d"),
               color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#002744',"#00182a"),
               color3 = c("#ffe6b4", "#ffd581", "#ffc34e", "#ffb21b", "#e69a00"),
               color4 = c("#d2ffb4", "#abff9a", "#95ff81", "#80ff67"))
label = list(label1 = c("< 100", "100 -- 200", "200 -- 300", "300 -- 400","400 -- 500","500 -- 1000", "1000+"),
               label2 = c("0 -- 10","10 -- 15","15 -- 20","20 -- 25","25 -- 30","30 -- 35","35 -- 40", "40+"),
               label3 = c("0 -- 10","10 -- 20","20 -- 30","30 -- 40", "40+"),
               label4 = c("0","0 -- 0.25","0.25 -- 0.5","0.5+"))
title = list(t1 = "Number of trips", t2 = "Average trip duration", 
             t3 = "Average fare", t4 = "Average % of tips")

load('PU_data.RData')
load('DO_data.RData')
load('nyc_nbhd.RData')

data.list.name <- c("PU.temp1.count", "PU.temp2.count", "PU.temp3.count", "PU.temp4.count", "PU.temp5.count",
               "DO.temp1.count", "DO.temp2.count", "DO.temp3.count", "DO.temp4.count", "DO.temp5.count",
               "PU.temp1.duration", "PU.temp2.duration", "PU.temp3.duration", "PU.temp4.duration",
               "PU.temp5.duration", "DO.temp1.duration", "DO.temp2.duration", "DO.temp3.duration",
               "DO.temp4.duration", "DO.temp5.duration", "PU.temp1.fare", "PU.temp2.fare",
               "PU.temp3.fare", "PU.temp4.fare", "PU.temp5.fare", "DO.temp1.fare", "DO.temp2.fare",
               "DO.temp3.fare", "DO.temp4.fare", "DO.temp5.fare", "PU.temp1.tips", "PU.temp2.tips", 
               "PU.temp3.tips", "PU.temp4.tips", "PU.temp5.tips", "DO.temp1.tips", "DO.temp2.tips",
               "DO.temp3.tips", "DO.temp4.tips", 'DO.temp5.tips')
filename <- NULL
for(i in 1:length(data.list.name)){
  filename[i] <- paste(data.list.name[i], ".RData", sep = "")
}

PU.temp1.count <- get(load(filename[1]))
PU.temp2.count<- get(load(filename[2]))
PU.temp3.count<- get(load(filename[3]))
PU.temp4.count<- get(load(filename[4]))
PU.temp5.count<- get(load(filename[5]))
DO.temp1.count<- get(load(filename[6]))
DO.temp2.count<- get(load(filename[7]))
DO.temp3.count<- get(load(filename[8]))
DO.temp4.count<- get(load(filename[9]))
DO.temp5.count<- get(load(filename[10]))
PU.temp1.duration<- get(load(filename[11]))
PU.temp2.duration<- get(load(filename[12]))
PU.temp3.duration<- get(load(filename[13]))
PU.temp4.duration<- get(load(filename[14]))
PU.temp5.duration<- get(load(filename[15]))
DO.temp1.duration<- get(load(filename[16]))
DO.temp2.duration<- get(load(filename[17]))
DO.temp3.duration<- get(load(filename[18]))
DO.temp4.duration<- get(load(filename[19]))
DO.temp5.duration<- get(load(filename[20]))
PU.temp1.fare<- get(load(filename[21]))
PU.temp2.fare<- get(load(filename[22]))
PU.temp3.fare<- get(load(filename[23]))
PU.temp4.fare<- get(load(filename[24]))
PU.temp5.fare<- get(load(filename[25]))
DO.temp1.fare<- get(load(filename[26]))
DO.temp2.fare<- get(load(filename[27]))
DO.temp3.fare<- get(load(filename[28]))
DO.temp4.fare<- get(load(filename[29]))
DO.temp5.fare<- get(load(filename[30]))
PU.temp1.tips<- get(load(filename[31]))
PU.temp2.tips<- get(load(filename[32]))
PU.temp3.tips<- get(load(filename[33]))
PU.temp4.tips<- get(load(filename[34]))
PU.temp5.tips<- get(load(filename[35]))
DO.temp1.tips<- get(load(filename[36]))
DO.temp2.tips<- get(load(filename[37]))
DO.temp3.tips<- get(load(filename[38]))
DO.temp4.tips<- get(load(filename[39]))
DO.temp5.tips<- get(load(filename[40]))


# load shape data 
load('myShape1.RData')
subdat <- spTransform(myShape1, CRS("+init=epsg:4326"))



######### Shiny App ########

shinyServer(function(input, output, session) {
  
  output$map1 <- renderLeaflet({
    
    if(input$dropoff == TRUE){
      if (input$tmp == "< -10 ℃"){
        time.count <- DO.temp1.count %>% filter(as.integer(DropoffHour) == input$hour)
        time.duration <- DO.temp1.duration %>% filter(as.integer(DropoffHour) == input$hour)
        time.fare <- DO.temp1.fare %>% filter(as.integer(DropoffHour) == input$hour)
        time.tips <- DO.temp1.tips %>% filter(as.integer(DropoffHour) == input$hour)
      }
      else if(input$tmp == "-10 ℃ -- 0 ℃"){
        time.count <- DO.temp2.count %>% filter(as.integer(DropoffHour) == input$hour)
        time.duration <- DO.temp2.duration %>% filter(as.integer(DropoffHour) == input$hour)
        time.fare <- DO.temp2.fare %>% filter(as.integer(DropoffHour) == input$hour)
        time.tips <- DO.temp2.tips %>% filter(as.integer(DropoffHour) == input$hour)
      }
      else if(input$tmp == "0 ℃ -- 10 ℃"){
        time.count <- DO.temp3.count %>% filter(as.integer(DropoffHour) == input$hour) 
        time.duration <- DO.temp3.duration %>% filter(as.integer(DropoffHour) == input$hour)
        time.fare <- DO.temp3.fare %>% filter(as.integer(DropoffHour) == input$hour)
        time.tips <- DO.temp3.tips %>% filter(as.integer(DropoffHour) == input$hour)
      }
      else if(input$tmp == "10 ℃ -- 20 ℃"){
        time.count <- DO.temp4.count %>% filter(as.integer(DropoffHour) == input$hour)
        time.duration <- DO.temp4.duration %>% filter(as.integer(DropoffHour) == input$hour)
        time.fare <- DO.temp4.fare %>% filter(as.integer(DropoffHour) == input$hour)
        time.tips <- DO.temp4.tips %>% filter(as.integer(DropoffHour) == input$hour)
      }
      else{
        time.count <- DO.temp5.count %>% filter(as.integer(DropoffHour) == input$hour) 
        time.duration <- DO.temp5.duration %>% filter(as.integer(DropoffHour) == input$hour)
        time.fare <- DO.temp5.fare %>% filter(as.integer(DropoffHour) == input$hour)
        time.tips <- DO.temp5.tips %>% filter(as.integer(DropoffHour) == input$hour)
      }
      
      map_data <- geo_join(nyc_nbhd, time.count, "neighborhood", "DO_Zone")
      duration_map_data <- geo_join(nyc_nbhd, time.duration, "neighborhood", "DO_Zone")
      fare_map_data <- geo_join(nyc_nbhd, time.fare, "neighborhood", "DO_Zone")
      tip_map_data <- geo_join(nyc_nbhd, time.tips, "neighborhood", "DO_Zone")
    }
    else{
      if (input$tmp == "< -10 ℃"){
        time.count <- PU.temp1.count %>% filter(as.integer(PickupHour) == input$hour)
        time.duration <- PU.temp1.duration %>% filter(as.integer(PickupHour) == input$hour)
        time.fare <- PU.temp1.fare %>% filter(as.integer(PickupHour) == input$hour)
        time.tips <- PU.temp1.tips %>% filter(as.integer(PickupHour) == input$hour)
      }
      else if(input$tmp == "-10 ℃ -- 0 ℃"){
        time.count <- PU.temp2.count %>% filter(as.integer(PickupHour) == input$hour)
        time.duration <- PU.temp2.duration %>% filter(as.integer(PickupHour) == input$hour)
        time.fare <- PU.temp2.fare %>% filter(as.integer(PickupHour) == input$hour)
        time.tips <- PU.temp2.tips %>% filter(as.integer(PickupHour) == input$hour)
      }
      else if(input$tmp == "0 ℃ -- 10 ℃"){
        time.count <- PU.temp3.count %>% filter(as.integer(PickupHour) == input$hour)
        time.duration <- PU.temp3.duration %>% filter(as.integer(PickupHour) == input$hour)
        time.fare <- PU.temp3.fare %>% filter(as.integer(PickupHour) == input$hour)
        time.tips <- PU.temp3.tips %>% filter(as.integer(PickupHour) == input$hour)
      }
      else if(input$tmp == "10 ℃ -- 20 ℃"){
        time.count <- PU.temp4.count %>% filter(as.integer(PickupHour) == input$hour)
        time.duration <- PU.temp4.duration %>% filter(as.integer(PickupHour) == input$hour)
        time.fare <- PU.temp4.fare %>% filter(as.integer(PickupHour) == input$hour)
        time.tips <- PU.temp4.tips %>% filter(as.integer(PickupHour) == input$hour)
      }
      else{
        time.count <- PU.temp5.count %>% filter(as.integer(PickupHour) == input$hour)
        time.duration <- PU.temp5.duration %>% filter(as.integer(PickupHour) == input$hour)
        time.fare <- PU.temp5.fare %>% filter(as.integer(PickupHour) == input$hour)
        time.tips <- PU.temp5.tips %>% filter(as.integer(PickupHour) == input$hour)
      }
  
      map_data <- geo_join(nyc_nbhd, time.count, "neighborhood", "PU_Zone")
      duration_map_data <- geo_join(nyc_nbhd, time.duration, "neighborhood", "PU_Zone")
      fare_map_data <- geo_join(nyc_nbhd, time.fare, "neighborhood", "PU_Zone")
      tip_map_data <- geo_join(nyc_nbhd, time.tips, "neighborhood", "PU_Zone")
    }
    
    
    pal1 <- colorBin(color[[1]], bins = c(0, 100, 200, 300, 400, 500))
    popup1 = paste0('<strong>Neighborhood: </strong><br>', map_data@data$neighborhood, 
                    '<br><strong>Number of Trip: </strong><br>', map_data@data$Number.of.Trips)
    
    
    pal2 <- colorBin(color[[2]], bins = c(0, 10, 20, 30, 40, 50))
    popup2 = paste0('<strong>Neighborhood: </strong><br>', duration_map_data@data$neighborhood, 
                    '<br><strong>Avg Trip Duration(min): </strong><br>', duration_map_data@data$Avg.Trip.Duration)
    
    pal3 <- colorBin(color[[3]], bins = c(0, 10, 20, 30, 40, 50))
    popup3 = paste0('<strong>Neighborhood: </strong><br>', fare_map_data@data$neighborhood, 
                    '<br><strong>Avg Fare: </strong><br>', fare_map_data@data$Avg.Fare)
    
    pal4 <- colorBin(color[[4]], bins = c(0, 0.1, 0.2, 0.3, 0.4, 0.5))
    popup4 = paste0('<strong>Neighborhood: </strong><br>', tip_map_data@data$neighborhood, 
                    '<br><strong>Avg Tips: </strong><br>', tip_map_data@data$Avg.Tips)
    
    
    pic.count <- leaflet(map_data) %>%
      setView(-73.98, 40.75, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron")
    
    pic.duration <- leaflet(duration_map_data) %>%
      setView(-73.98, 40.75, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron")
    
    pic.fare <- leaflet(fare_map_data) %>%
      setView(-73.98, 40.75, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron")
    
    pic.tip <- leaflet(tip_map_data) %>%
      setView(-73.98, 40.75, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron")
    
    
    if (input$info == "Number of trips"){
      pic.count <- pic.count %>%
        addPolygons(fillColor = ~pal1(Number.of.Trips), color = 'grey', weight = 1,
                    popup = popup1, fillOpacity = .6) %>%
        addLegend(position = "bottomright",
                  colors = color[[1]],
                  labels = label[[1]],
                  opacity = 0.6,
                  title = title[[1]])
    }
    
    else if (input$info == "Average trip duration"){
      pic.duration <- pic.duration %>%
        addPolygons(fillColor = ~pal2(Avg.Trip.Duration), color = 'grey', weight = 1,
                    popup = popup2, fillOpacity = .6) %>%
        addLegend(position = 'bottomright',
                  colors = color[[2]],
                  labels = label[[2]], ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = title[[2]])
    }
    
    else if (input$info == "Average fare"){
      pic.fare <- pic.fare %>%
        addPolygons(fillColor = ~pal3(Avg.Fare), color = 'grey', weight = 1,
                    popup = popup3, fillOpacity = .6) %>%
        addLegend(position = 'bottomright',
                  colors = color[[3]],
                  labels = label[[3]],
                  opacity = 0.6,     
                  title = title[[3]])
    }

    else if(input$info == "Average % of tips"){
      pic.tip <- pic.tip %>%
        addPolygons(fillColor = ~pal4(Avg.Tips), color = 'grey', weight = 1,
                    popup = popup4, fillOpacity = .6) %>%
        addLegend(position = 'bottomright',
                  colors = color[[4]],
                  labels = label[[4]],
                  opacity = 0.6,  
                  title = title[[4]])
    }
    
  })
  
  
  observe({
    updateCheckboxInput(session, 'dropoff', value = !(input$pickup))


  })
  observe({

    updateCheckboxInput(session, 'pickup', value = !(input$dropoff))
    
  })
  
})
