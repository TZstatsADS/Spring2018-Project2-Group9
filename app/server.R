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
library(ggmap)
library(dplyr)
library(RColorBrewer)
library(leaflet)

#data processing
df <- read.csv("deep_sea_corals_USA.csv", as.is=TRUE)
df <- df[sample(nrow(df), 1000), ]
category_level = unique(df$VernacularNameCategory)
council_level = unique(df$FishCouncilRegion)

#add colors to different types 
colors.pal <- brewer.pal(length(category_level), "Set1")
df$colors <- NULL
for (i in 1:length(category_level)){
  df[df$VernacularNameCategory == sort(category_level)[i],'colors'] <- colors.pal[i]
}

#select data function
select <- function(category, council, depth) {
  category_index=which(df$VernacularNameCategory%in%category)
  council_index=which(df$FishCouncilRegion%in%council)
  depth_index = which((df$DepthInMeters>=depth[1])&(df$DepthInMeters<=depth[2]))
  index = Reduce(intersect, list(category_index, council_index, depth_index))
  selectdf <- df[index,]
  return(selectdf)
}

shinyServer(function(input, output, session) {
  output$map <- renderLeaflet({
    selectdf <- select(input$category, input$council, input$depth)
    leaflet() %>%
      addTiles() %>%
      addCircles(selectdf$longitude, selectdf$latitude, 
                 color = (selectdf$colors),
                 popup = paste('ScientificName: ', selectdf$ScientificName, '<br/>',
                               'Website:', a(selectdf$Website, href=selectdf$Website), '<br/>')) %>%
      addLegend("topleft", colors=colors.pal, labels=sort(category_level)) %>%
      addProviderTiles(providers$Esri.OceanBasemap)%>%
      setView(lat = 28, lng = -150, zoom = 2)
  })
})
