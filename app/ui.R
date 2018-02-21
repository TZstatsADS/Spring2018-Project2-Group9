####################################################################################################
#####################        Begin Install Packages And Load Libraries         #####################
packages.used <- c("shiny", "dplyr", "RColorBrewer", "leaflet", "plotly", "plotrix")

# Check packages that need to be installed.
packages.needed <- setdiff(packages.used, 
                           intersect(installed.packages()[,1], packages.used))

# Install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}
# Load the packages
library(shiny)
library(dplyr)
library(RColorBrewer)
library(leaflet)
library(plotly)
library(plotrix)

#####################          END Install Packages And Load Libraries         #####################
####################################################################################################

####################################################################################################
#####################                 BEGIN Data Processing                    #####################
category_choices <- c("black coral", "gold coral", "gorgonian coral", "lace coral", "sea pen",
                      "soft coral", "stoloniferan coral", "stony coral")
council_choices <- c("Caribbean", "Gulf of Mexico", "Mid-Atlantic", "New England", "North Pacific",
                     "Pacific", "South Atlantic", "Western Pacific")

#####################                  END Data Processing                     #####################
####################################################################################################

####################################################################################################
#####################                     BEGIN Shiny UI                       #####################
shinyUI(navbarPage(
    ### Application Theme  
    theme = "styles.css",
    
    ### Application Title
    title = strong('Finding Coral', style='color:#d0f7ef;'),
    
    ################################################################################################ 
    #####################                BEGIN WELCOME TAB                       ###################
    tabPanel('WELCOME', div(id="canvas"),
        mainPanel(width = 12, 
            htmlOutput("text_hp")
    )),
    #####################                END WELCOME TAB                         ###################
    ################################################################################################

    ################################################################################################
    #####################              BEGIN GET ON BOARD TAB                  #####################
    navbarMenu('GET ON BOARD',
               tabPanel("What Are Deep-Sea Corals", div(id="canvas"), 
                        mainPanel(width=12, htmlOutput("text_gob1"))),       
               tabPanel("Why We Care", div(id="canvas"), 
                        navlistPanel(well = FALSE, widths = c(3,9),
                                     tabPanel("Values of Deep-Sea Corals", htmlOutput("text_gob2")),
                                     tabPanel("Current Challenges",htmlOutput("text_gob3"))
                        )         
               ),
               tabPanel("How You Can Help", div(id="canvas"), 
                        mainPanel(width=12, htmlOutput("text_gob4"))),
               "----",
               tabPanel("Learn from Videos", div(id="canvas"), 
                        mainPanel(width=12,
                            fluidRow(
                                column(6,htmlOutput("gallery_video1")),
                                column(6,htmlOutput("gallery_video2"))
                            ),
                            fluidRow(
                                column(6,htmlOutput("gallery_video3")),
                                column(6,htmlOutput("gallery_video4"))
                            ),
                            fluidRow(
                                column(6,htmlOutput("gallery_video5")),
                                column(6,htmlOutput("gallery_video6"))
                            )
                        )
               ),
               tabPanel("Explore More", div(id="canvas"), 
                        mainPanel(width=12, htmlOutput("text_gob5")))
    ),
    #####################                END GET ON BOARD TAB                  #####################
    ################################################################################################    
  
    ################################################################################################ 
    #####################                BEGIN Photo Gallery TAB                 ###################    
    tabPanel("PHOTO GALLERY", div(id="canvas"), 
        navlistPanel(well = FALSE, widths = c(3,9),
            tabPanel("Black Coral",
                fluidRow(
                    column(4,uiOutput("slider_to_anim1")),
                    column(4,offset=1, uiOutput("speed_value1"))
                ),
                hr(),
                textOutput("imagenumber1"),
                textOutput("imagefile1"),
                uiOutput("gallery_black")
            ),    
            tabPanel("Gold Coral",
                     fluidRow(
                       column(4,uiOutput("slider_to_anim2")),
                       column(4,offset=1, uiOutput("speed_value2"))
                     ),
                     hr(),
                     textOutput("imagenumber2"),
                     textOutput("imagefile2"),
                     uiOutput("gallery_gold")
            ),     
            tabPanel("Gorgonian Corals",
                     fluidRow(
                       column(4,uiOutput("slider_to_anim3")),
                       column(4,offset=1, uiOutput("speed_value3"))
                     ),
                     hr(),
                     textOutput("imagenumber3"),
                     textOutput("imagefile3"),
                     uiOutput("gallery_gorgonian")
            ),
            tabPanel("Lace Corals",
                     fluidRow(
                       column(4,uiOutput("slider_to_anim4")),
                       column(4,offset=1, uiOutput("speed_value4"))
                     ),
                     hr(),
                     textOutput("imagenumber4"),
                     textOutput("imagefile4"),
                     uiOutput("gallery_lace")
            ),
            tabPanel("Sea Pen",
                fluidRow(
                   column(4,uiOutput("slider_to_anim5")),
                   column(4,offset=1, uiOutput("speed_value5"))
                ),
                hr(),
                textOutput("imagenumber5"),
                textOutput("imagefile5"),
                uiOutput("gallery_seapen")
            ),
            tabPanel("Soft Corals",
                fluidRow(
                    column(4,uiOutput("slider_to_anim6")),
                    column(4,offset=1, uiOutput("speed_value6"))
                ),
                hr(),
                textOutput("imagenumber6"),
                textOutput("imagefile6"),
                uiOutput("gallery_soft")
            ),
            tabPanel("Stoloniferan Corals",
                     fluidRow(
                       column(4,uiOutput("slider_to_anim7")),
                       column(4,offset=1, uiOutput("speed_value7"))
                     ),
                     hr(),
                     textOutput("imagenumber7"),
                     textOutput("imagefile7"),
                     uiOutput("gallery_stoloniferan")
            ),
            tabPanel("Stony Corals",
                     fluidRow(
                       column(4,uiOutput("slider_to_anim8")),
                       column(4,offset=1, uiOutput("speed_value8"))
                     ),
                     hr(),
                     textOutput("imagenumber8"),
                     textOutput("imagefile8"),
                     uiOutput("gallery_stony")
            )
        )
    ),
    #####################                END Photo Gallery TAB                   ###################
    ################################################################################################    

    ################################################################################################ 
    #####################                   BEGIN MAP TAB                        ###################      
    tabPanel("MAP", div(id="canvas"), 
             
             # map
             leafletOutput("map", width="100%", height = "600px"),
             
             # filter
             absolutePanel(id = "Corals", class = "panel panel-default", style = "opacity: 0.7",
                           fixed = TRUE, draggable = TRUE,
                           top = 140, left = "auto", right = 176, bottom = "auto", width = 155, height = 300,
                           checkboxGroupInput("category", label = h4("Category"), 
                                              choices = category_choices, selected = category_choices),
                           actionButton("select_corals", "Select All",
                                        style = " color: #404040;
                                        border-color: #f2f2f2;
                                        background-color: #f2f2f2;
                                        height: 25px;
                                        width: 67px;
                                        font-size: 12px;
                                        padding-left: 6.5px;
                                        padding-top: 3px;"),
                           actionButton("clear_corals", "Clear All",
                                        style = " color: #404040;
                                        border-color: #f2f2f2;
                                        background-color: #f2f2f2;
                                        height: 25px;
                                        width: 67px;
                                        font-size: 12px;
                                        padding-left: 6.5px;
                                        padding-top: 3px;")
             ),
             
             absolutePanel(id = "Corals", class = "panel panel-default", style = "opacity: 0.7",
                           fixed = TRUE, draggable = TRUE,
                           top = 140, left = "auto", right = 20, bottom = "auto", width = 155, height = 300,
                           checkboxGroupInput("council", label = h4("Council"), 
                                              choices = council_choices, selected = council_choices),
                           actionButton("select_councils", "Select All",
                                        style = " color: #404040;
                                        border-color: #f2f2f2;
                                        background-color: #f2f2f2;
                                        height: 25px;
                                        width: 67px;
                                        font-size: 12px;
                                        padding-left: 6.5px;
                                        padding-top: 3px;"),
                           actionButton("clear_councils", "Clear All",
                                        style = " color: #404040;
                                        border-color: #f2f2f2;
                                        background-color: #f2f2f2;
                                        height: 25px;
                                        width: 67px;
                                        font-size: 12px;
                                        padding-left: 6.5px;
                                        padding-top: 3px;")
             ),
             
             absolutePanel(id = "Corals", class = "panel panel-default", style = "opacity: 0.7",
                           fixed = TRUE, draggable = TRUE,
                           top = 441, left = "auto", right = 20, bottom = "auto", width = 311, height = 120,
                           sliderInput("depth", label = h5("Depth"),min = 0, max = 5000, value = c(0,5000)),
                           textInput("number", "CatalogNumber"),
                           verbatimTextOutput("error")
             )
    ),
    #####################                     END MAP TAB                        ###################      
    ################################################################################################ 
    
    ################################################################################################ 
    #####################                BEGIN STATISTICS TAB                    ###################      
    ###
    tabPanel("STATISTICS", div(id="canvas"),
             tabsetPanel(
               tabPanel( "Overall", plotlyOutput("overall", width = "100%", height = "600px")),
               tabPanel("Amount by Regions",
                        sidebarPanel(fixed = TRUE, draggable = TRUE,
                                     top = 80, left = 20, right = "auto", bottom = "auto", width = 100, height = "auto",
                                     selectInput("category_2", label = "Coral Category", 
                                                 choices = category_choices, selected = category_choices),
                                     plotOutput("pie", width = "100%", height = "500px")
                        )
               ),                      
               tabPanel( "Depth Distribution",
                         sidebarPanel(fixed = TRUE, draggable = TRUE,
                                      top = 80, left = 20, right = "auto", bottom = "auto", width = 100, height = "auto",
                                      selectInput("category_1", label = "Coral Category",
                                                  choices = category_choices, selected = category_choices),
                                      plotOutput("hist", width = "100%", height = "500px"))
               )                   
             )
    )        
    #####################                  END STATISTICS TAB                    ###################      
    ################################################################################################     
    
))

#####################                     END Shiny UI                         #####################
####################################################################################################
