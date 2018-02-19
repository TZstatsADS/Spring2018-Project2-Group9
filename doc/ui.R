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
library(plotly)



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
                                    top = "auto", left = "auto", right = 176, bottom = 180, width = 155, height = "auto",
                                    checkboxGroupInput("category", label = h4("Category"), 
                                                       choices = category_choices, selected = category_choices),
                                    actionButton("select_corals", "Select All"),
                                    actionButton("clear_corals", "Clear All")
                                    
                      ),
                      absolutePanel(id = "Corals", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                    top = "auto", left = "auto", right = 20, bottom = 180, width = 155, height = "auto",
                                    checkboxGroupInput("council", label = h4("Council"), 
                                                       choices = council_choices, selected = council_choices),
                                    actionButton("select_councils", "Select All"),
                                    actionButton("clear_councils", "Clear All")
                      ),
                      absolutePanel(id = "Corals", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                    top = "auto", left = "auto", right = 20, bottom = 56, width = 311, height = "auto",
                                    sliderInput("depth", label = h4("Depth (m)"),min = -999, max = 4500,value = c(0,4000))
                      )
             ),
             ### Map ends
             
             ### Stat begins
             tabPanel("STAT",
                      # 3d plot
                      plotlyOutput("plot_3d", width = "100%", height = "500px"),
                      absolutePanel(id = "Corals2", fixed = TRUE, draggable = TRUE,
                                    top = 80, left = 20, right = "auto", bottom = "auto", width = 150, height = "auto",
                                    selectInput("coral_category", label = h5("Coral Category"), 
                                                 choices = category_choices, selected = category_choices)
                      )
             )
             ### Stat ends
  ) 
)
