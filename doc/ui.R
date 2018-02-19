#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# library packages
library(shiny)
library(ggplot2)
library(ggmap)
library(dplyr)
library(RColorBrewer)
library(leaflet)
library(shinythemes)



######################################## Data Processing ########################################
df <- read.csv("deep_sea_corals_USA.csv", as.is=TRUE)
category_level = unique(df$VernacularNameCategory)
category_choices = c(as.character(sort(category_level)))

council_level = unique(df$FishCouncilRegion)
council_choices = c(as.character(sort(council_level)))



######################################## UI Begins ########################################
shinyUI(
  navbarPage("Corals", theme="styles.css",

             
             ### Map begins
             tabPanel("MAP MAGE",
                      # map
                      leafletOutput("map", width="100%", height = "600px"),
                      # filter
                      absolutePanel(id = "Corals", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                    top = "auto", left = "auto", right = 171, bottom = 180, width = 150, height = "auto",
                                    checkboxGroupInput("category", "Category", 
                                                       choices = category_choices, selected = category_choices),
                                    actionButton("select_corals", "Select All"),
                                    actionButton("clear_corals", "Clear All")
                                    
                      ),
                      absolutePanel(id = "Corals", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                    top = "auto", left = "auto", right = 20, bottom = 180, width = 150, height = "auto",
                                    checkboxGroupInput("council", "Council", 
                                                       choices = council_choices, selected = council_choices),
                                    actionButton("select_councils", "Select All"),
                                    actionButton("clear_councils", "Clear All")
                      ),
                      absolutePanel(id = "Corals", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                    top = "auto", left = "auto", right = 20, bottom = 76, width = 301, height = "auto",
                                    sliderInput("depth", "Depth",min = -999, max = 4500,value = c(0,4000))
                      )
             )
             ### Map ends
  )     
  
)
