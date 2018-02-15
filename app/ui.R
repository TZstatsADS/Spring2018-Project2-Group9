#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggmap)
library(dplyr)
library(RColorBrewer)
library(leaflet)

df <- read.csv("deep_sea_corals_USA.csv", as.is=TRUE)
category_level = unique(df$VernacularNameCategory)
category_choices=c(as.character(sort(category_level)))
#category_choices=c('All',as.character(sort(category_level)))

council_level = unique(df$FishCouncilRegion)
council_choices=c(as.character(sort(council_level)))
#council_choices=c('All',as.character(sort(council_level)))

shinyUI(
  navbarPage("Corals", theme="styles.css",
             
             #Home
             tabPanel("Home"
             ),
             
             #Map
             tabPanel("Map",
                      #map
                      leafletOutput("map", width="100%", height = "600px"),
                      
                      #filter
                      absolutePanel(id = "Corals", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                    top = 80, left = "auto", right = 20, bottom = "auto", width = 200, height = "auto",
                                    
                                    checkboxGroupInput("category", "Category", choices = category_choices, selected = category_choices),
                                    br(),
                                    checkboxGroupInput("council", "Council", choices = council_choices, selected = council_choices),
                                    br(),
                                    sliderInput("depth", "Depth",min = -999, max = 4500,value = c(0,4000))
                      )
             ),
             
             #About Us
             tabPanel("About Us"
             )
  )
)

