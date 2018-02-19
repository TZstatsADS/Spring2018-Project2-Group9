#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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
library(plotly)



######################################## Data Processing ########################################
# load cleaned data
df <- read.csv("deep_sea_corals_USA.csv", as.is=TRUE)
df <- df[sample(nrow(df), 1000), ]
category_level = unique(df$VernacularNameCategory)
council_level = unique(df$FishCouncilRegion)
category_choices = c(as.character(sort(category_level)))
council_choices = c(as.character(sort(council_level)))

# add colors to different types 
colors.pal <- brewer.pal(length(category_level), "Set1")
df$colors <- NULL
for (i in 1:length(category_level)){
  df[df$VernacularNameCategory == sort(category_level)[i],'colors'] <- colors.pal[i]
}

# select data function
select <- function(category, council, depth) {
  category_index = which(df$VernacularNameCategory%in%category)
  council_index = which(df$FishCouncilRegion%in%council)
  depth_index = which((df$DepthInMeters>=depth[1])&(df$DepthInMeters<=depth[2]))
  index = Reduce(intersect, list(category_index, council_index, depth_index))
  selectdf <- df[index,]
  return(selectdf)
}

select_by_type <- function(coral_category = "stony coral") {
  if (is.null(coral_category)){coral_category="stony coral"} 
  category_index = which(df$VernacularNameCategory%in%coral_category)
  select_by_df <- df[category_index, 
                     c("VernacularNameCategory", "DepthInMeters", "Condition", "FishCouncilRegion")]
  return(select_by_df)
}

######################################## Server Begins ########################################
shinyServer(function(input, output, session) {
  
  ### map begins
  output$map <- renderLeaflet({
  selectdf <- select(input$category, input$council, input$depth)
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(selectdf$longitude, selectdf$latitude, color = (selectdf$colors),
                     fillOpacity = 1, radius = 2.5, stroke = FALSE,
                       # pop up corals information
                     popup = paste('<strong> Catalog Number </strong>:', selectdf$CatalogNumber, '<br/>', 
                                   '<strong> Name (scientific name) </strong>: ', selectdf$ScientificName, '<br/>',
                                   '<strong> Observation Date </strong>:', selectdf$ObservationDate, '<br/>',
                                   '<strong> Depth (m) </strong>:', selectdf$DepthInMeters,'<br/>',
                                   '<strong> Class (sub class) </strong>:', selectdf$Class,'(',selectdf$Subclass,')','<br/>',
                                   '<strong> Position (lat lon) </strong>:', selectdf$latitude,selectdf$longitude, '<br/>',
                                   '<strong> Location </strong>:', selectdf$Locality, '<br/>',
                                   '<strong> Condition </strong>:', selectdf$Condition,'<br/>',
                                   '<strong> Data Provider </strong>:', selectdf$DataProvider,'<br/>',
                                   '<strong> Website </strong>:', a(selectdf$Website, href=selectdf$Website), '<br/>')) %>%
    addLegend("topleft", colors=colors.pal, labels=sort(category_level)) %>%
    addProviderTiles(providers$Esri.OceanBasemap)%>%
    setView(lat = 28, lng = -100, zoom = 3)
  })
  
  # select all/clear all corals categories
  observeEvent(input$select_corals, {
    updateCheckboxGroupInput(session, "category", 
                             choices = category_choices, selected = category_choices)
  })
  observeEvent(input$clear_corals, {
    updateCheckboxGroupInput(session, "category",
                             choices = category_choices, selected = NULL)
  })
  
  # select all/clear all councils
  observeEvent(input$select_councils, {
    updateCheckboxGroupInput(session, "council", 
                             choices = council_choices, selected = council_choices)
  })
  observeEvent(input$clear_councils, {
    updateCheckboxGroupInput(session, "council",
                             choices = council_choices, selected = NULL)
  })
  ### map ends
  
  ### stat begins
  output$plot_3d <- renderPlotly({
    select_by_df <- select_by_type(input$coral_category)
    p <- plot_ly(type = "scatter3d", mode = "markers", showlegend = TRUE) %>%
         add_trace(p, x = ~DepthInMeters, y = ~Condition, z = ~FishCouncilRegion,
                   data = select_by_df)
  })
  ### stat ends
})