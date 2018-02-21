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
# Load data
load("deep_sea_corals_sample.Rdata")

### BEGIN PHOTO GALLERY TAB ###
#data_coral[!is.na(data_coral$ImageURL),]%>%group_by(VernacularNameCategory)%>%summarise(n=n())
data_coral_image <- data_coral[!is.na(data_coral$ImageURL),]
data_black_image <- data_coral_image[data_coral_image$VernacularNameCategory=="black coral",]
data_gold_image <- data_coral_image[data_coral_image$VernacularNameCategory=="gold coral",]
data_gorgonian_image <- data_coral_image[data_coral_image$VernacularNameCategory=="gorgonian coral",]
data_lace_image <- data_coral_image[data_coral_image$VernacularNameCategory=="lace coral",]
data_seapen_image <- data_coral_image[data_coral_image$VernacularNameCategory=="sea pen",]
data_soft_image <- data_coral_image[data_coral_image$VernacularNameCategory=="soft coral",]
data_stoloniferan_image <- data_coral_image[data_coral_image$VernacularNameCategory=="stoloniferan coral",]
data_stony_image <- data_coral_image[data_coral_image$VernacularNameCategory=="stony coral",]
### END PHOTO GALLERY TAB ###

### BEGIN MAP TAB ###
category_level = unique(data_coral$VernacularNameCategory)
category_choices = c(as.character(sort(category_level)))
council_level = unique(data_coral$FishCouncilRegion)
council_choices = c(as.character(sort(council_level)))

# Add colors to different types
colors.pal <- brewer.pal(length(category_choices), "Set1")
data_coral$colors <- NULL
for (i in 1:length(category_choices)){
  data_coral[data_coral$VernacularNameCategory == category_choices[i],'colors'] <- colors.pal[i]
}

# function
selectdf <- function(category, council, depth, number) {
  category_index = which(data_coral$VernacularNameCategory%in%category)
  council_index = which(data_coral$FishCouncilRegion%in%council)
  depth_index = which((data_coral$DepthInMeters>=depth[1])&(data_coral$DepthInMeters<=depth[2]))
  number_index = which(data_coral$CatalogNumber==number)
  flag = 0
  if (number%in%data_coral$CatalogNumber){
    flag = 1
    number_index = which(data_coral$CatalogNumber==number)
    selectdf <- data_coral[number_index,]
  }
  else{
    if (number != ""){
      flag = 2
    }
    index = Reduce(intersect, list(category_index, council_index, depth_index))
    selectdf <- data_coral[index,]
  }
  return(list("df"=selectdf, "f"=flag))
}

select_by_type <- function(coral_category) {
  if (is.null(coral_category)){coral_category="stony coral"} 
  category_index = which(data_coral$VernacularNameCategory%in%coral_category)
  select_by_df <- data_coral[category_index, 
                     c("VernacularNameCategory", "DepthInMeters", "Condition", "FishCouncilRegion")]
  return(select_by_df)
}

### END MAP TAB ###

#####################                  END Data Processing                     #####################
####################################################################################################

####################################################################################################
#####################                 BEGIN Shiny Server                       #####################

shinyServer(function(input, output, session) {

    ################################################################################################ 
    #####################                BEGIN WELCOME TAB                       ###################    
    output$text_hp <- renderUI({
        HTML("
            <p style='font-size:1.2vw'>Welcome to Finding Coral!</p>
            <p style='font-size:1.2vw'>We are very excited to help you learn more about 
               the deep-sea corals within the U.S. fishing councils.</p>
            <p style='font-size:1.2vw'>Let our journey begin now!</p>
            <br/>
            <h2 style='font-size:2.0vw;'>OUR MISSION</h2>
            <p style='font-size:1.2vw'>We aim to increase the recognition of the values of 
                deep-sea corals and the awareness of the deep-sea coral protection.</p>
            <br/>
            <h2 style='font-size:2.0vw;'>OUR GOAL</h2>
            <ul style='font-size:1.2vw;'>
              <li>Facilitate the researchers in the study of the deep-sea corals</li>
              <li>Help organizations who have recognized the values of deep-sea corals, such as 
                   medical researchers, to identify the deep-sea corals of their interest</li>
              <li>Help individual and business to recognize the values of deep-sea corals 
                   as well as to engage in deep-sea coral protection</li>
            </ul>
            <br/>
            <h2 style='font-size:2.0vw;'>OUR PEOPLE</h2>
            <p style='font-size:1.2vw'>Xueyao Li, Yuhao Kang, Yuehan Kong, and Yiyi Zhang</p>
            <br/>
            <a href='https://github.com/TZstatsADS/Spring2018-Project2-Group9'>Our Github Link</a>
        ")
    })
    #####################                END WELCOME TAB                         ###################
    ################################################################################################

    ################################################################################################ 
    #####################         BEGIN What Are Deep-Sea Corals TAB             ###################  
    output$text_gob1 <- renderUI({
        HTML("
            <h2>What Are Deep-Sea Corals?</h2>
            <br/>
            <p>Over half of all known coral species are found in deep, dark waters where 
            temperatures range from 4-12 &deg;C. For this reason, these corals are called the 
            <q>cold-water</q> or <q>deep-sea</q> corals. They are among the oldest living organisms; some 
            reefs are several thousand years old, and some individual corals live several hundred 
            years. They can appear in various sizes and forms from massive reefs, tens of meters 
            high, to individual colonies less than a meter high. </p> 
            <br/> 
            <p>Deep-sea corals occur throughout coastal-ocean waters in deeper and colder habitats 
            all over the world, starting at about 40 meters (about 130 feet) in depth, down to about 
            3,000 meters (about 10,000 feet), but mostly between 300 and 800 meters (1,000 and 
            2,600 feet). Most deep-sea corals live on hard substrates on the seafloor, such as 
            boulders and rocky outcrops.</p>  
            <br/>
            <p>Unlike most tropical shallow-water species, deep-sea corals lack zooxanthellae, the 
            symbiotic photosynthetic algae that produce food from sunlight, which is absent or 
            limited at the depths where deep-sea corals typically occur. So deep-sea corals feed by 
            waiting for plankton and organic material to flow past, and then use their stinging 
            cells to capture them. They need quite turbid areas with high currents to get enough food. 
            At night, coral polyps look like fragile flowers, but they are actually voracious 
            carnivores.</p>
            <br/>
            <p>More than 3,300 species of deep-sea corals have been identified, and the numbers keep 
            climbing as new species are continually being discovered. To learn more about different 
            species, please visit 
            <a href='http://ocean.si.edu/slideshow/diversity-deep-sea-corals'>Diversity of Deep-Sea 
            Corals</a> </p>
        ")
    })  
    #####################          END What Are Deep-Sea Corals TAB              ###################      
    ################################################################################################     

    ################################################################################################ 
    #####################                BEGIN Why We Care TAB                   ###################      
    output$text_gob2 <- renderUI({
        HTML("    
            <h2>Why We Care?</h2>
            <h4><i>Values of Deep-Sea Corals</i></h4>
            <br/>
            <p>The complex three-dimensional structure of some deep-sea corals creates habitat for 
            diverse communities of invertebrates and fishes, including commercially valuable species 
            such as rockfish, shrimp, and crab. Deep coral ecosystems are considered biodiversity 
            hot spots and are sometimes called the rainforests of the sea because of the highly 
            diverse and complex land environments they mirror.</p>    
            <br/>
            <p>Some deep-sea corals may also be sources of compounds for the development of new 
            drugs and medical treatments. Rencent research shows that they are a potential source 
            of new antibiotics and anti-cancer chemicals.</p>
        ")
    })
    
    output$text_gob3 <- renderUI({
        HTML('    
            <h2>Why We Care?</h2>
            <h4><i>Current Challenges</i></h4>
            <br/>
            <p>The remoteness and depth of deep-sea coral habitats does not protect them from human 
            activity, especially as we use up coastal resources and move further offshore into 
            deeper waters. Due to their exposed structure, slow growth and recruitment rates, 
            deep-sea corals may be especially vulnerable to natural or human disturbance such as 
            bottom trawling, mineral extraction, and cable trenching - activities that result in 
            physical disruption of the seafloor. Animals in cold tend to grow slowly and live a 
            long time. Once damage occurs, recovery is extremely slow or may not happen at all.</p>
            <br/>
            <p>Removing or damaging deep coral habitats can have impacts that resonate throughout the 
            ocean. Destroying these complex habitats may harm or eliminate sources of food, shelter,
            or spawning grounds for other animals and may even have implications for human health. 
            Regulations to prevent physical damage cannot help with impacts from oil spills or 
            climate change, which do not respect boundaries. With our planet changing quickly, it is 
            increasingly important to protect entire ecosystems to maintain diversity and hopefully 
            resilience into the future. </p>
            <br/>
            <p>While we have known about cold-water corals for hundreds of years, aside from a few 
            areas, they have been poorly studied. Science still has a long way to go before we 
            really understand deep coral ecosystems. Many nations are beginning to recognize the 
            value of deep-sea corals and take steps to manage and protect them. With the 
            availability of new underwater survey technologies have come recent scientific surveys 
            and new opportunities to document more of these resources.</p>
        ')
    })    
    #####################                   END Why We Care TAB                  ###################      
    ################################################################################################     

    ################################################################################################ 
    #####################            BEGIN How You Can Help TAB                  ###################      
    output$text_gob4 <- renderUI({
        HTML('    
           <h2>How You Can Help?</h2>
           <br/>
           <ul>
              <li>Support organizations that are working for coral protection</li>
              <li>Do not buy jewelry or curios made from deep-sea corals</li>
              <li>Make seafood choices that support sustainable fishing practices </li>
              <li>Spread the words and get people around you involved in the family of deep-sea 
                  coral protection</li>
           </ul>
        ')
    })        
    #####################             END How You Can Help TAB                   ###################    
    ################################################################################################     
        
    ################################################################################################ 
    #####################            BEGIN Multimedia Video TAB                  ###################        
    output$gallery_video1 <- renderUI({
      HTML("<iframe width='420' height='236.25' 
            src='https://www.youtube.com/embed/317hLtDzMjA' 
            frameborder='0' allowfullscreen></iframe>")
    })
    
    output$gallery_video2 <- renderUI({
      HTML("<iframe width='420' height='236.25' 
           src='https://www.youtube.com/embed/mQScNhWz8gM' 
           frameborder='0' allowfullscreen></iframe>")
    })     
    
    output$gallery_video3 <- renderUI({
      HTML("<iframe width='420' height='236.25' 
           src='https://www.youtube.com/embed/gq57Yg2o_CU' 
           frameborder='0' allowfullscreen></iframe>")
    })    
    
    output$gallery_video4 <- renderUI({
      HTML("<iframe width='420' height='236.25' 
           src='https://www.youtube.com/embed/ugImYwyyq0M' 
           frameborder='0' allowfullscreen></iframe>")
    }) 
    
    output$gallery_video5 <- renderUI({
      HTML("<iframe width='420' height='236.25' 
           src='https://www.youtube.com/embed/LVmvaNYcjeg' 
           frameborder='0' allowfullscreen></iframe>")
    }) 
    
    output$gallery_video6 <- renderUI({
      HTML("<iframe width='420' height='236.25' 
           src='https://www.youtube.com/embed/epheAZ7NOsc' 
           frameborder='0' allowfullscreen></iframe>")
    }) 
    
    #####################             END Multimedia Video TAB                   ###################         
    ################################################################################################ 

    ################################################################################################ 
    #####################              BEGIN Explore More TAB                    ###################      
    output$text_gob5 <- renderUI({
      HTML(' 
          <h4>References and Other Suggested Readings</h4>
          <br/>
          <a href="https://deepseacoraldata.noaa.gov/">
             NOAA Deep Sea Coral Research and Technology Program</a>
          <br/>
          <a href="https://www.fisheries.noaa.gov/national/habitat-conservation/deep-sea-coral-habitat">
             NOAA Fisheries - Deep-Sea Coral Habitat</a>
          <br/>
          <a href="http://oceanexplorer.noaa.gov/edu/themes/deep-sea-corals/">
             Ocean Explorer - Deep-Sea Corals</a>
          <br/>  
          <a href="http://ocean.si.edu/deep-sea-corals">
             The Ocean Portal - Deep-sea Corals</a>
          <br/>
          <a href="https://marine-conservation.org/what-we-do/program-areas/coral-conservation/deep-sea-corals/">
             Marine Conservation Institute - Deep Sea Corals</a>
          <br/>
          <a href="https://www.coris.noaa.gov/activities/resourceCD/resources/deep_coral_bm.pdf">
             Oceana - Deep Sea Corals: Out of Sight, But No Longer Out of Mind</a>
          <br/>
          <a href="https://oceantoday.noaa.gov/every-full-moon/full-moon-coralcomeback.html">
             Ocean Today - Coral Comeback</a> 
          <br/>
          <a href="http://www.pewtrusts.org/en/research-and-analysis/q-and-a/2018/01/why-we-need-to-protect-deep-sea-corals-now">
             The PEW Charitable Trusts - Why We Need to Protect Deep Sea Corals Now</a>
      ')
    })        
    #####################               END Explore More TAB                     ###################    
    ################################################################################################         

#    Animation_Speed <- function(speed_id){return(renderUI({
#      sliderInput(speed_id,"Animation Speed", 
#                  min =0 , max=30, value = 1, step = 1, 
#                  width = '100%', post = "s")
#    }))}
    
#    Animation <- function(anim_id, dataset,speed_id){return(renderUI({
#      sliderInput(inputId = anim_id, 
#                  label = "Animation", 
#                  min = 1, max = length(dataset$ImageURL), 
#                  value = 1, step = 1,
#                  ticks = FALSE, width = '100%', 
#                  animate = animationOptions(interval = input$speed_id*1000, 
#                                             loop = TRUE))  
#    }))}
#    
#    PhotoCataNum <- function(dataset, anim_id){return(renderText({
#      paste0("Catalog Number: ",dataset$CatalogNumber[input$anim_id])
#    }))}
#    
#    PhotoSource <- function(dataset, anim_id){return(renderText({
#      file.path(dataset$ImageURL[input$anim_id])
#    }))}
#    
#    PhotoDisplay <- function(dataset, anim_id){return(renderUI({
#      tags$img(src = file.path(dataset$ImageURL[input$anim_id]), width = "70%", height = "auto")
#    }))}
    
#    output$speed_value1 <- Animation_Speed("speed1")
#    output$slider_to_anim1 <- Animation("slider1",data_seapen,speed1)
#    output$imagenumber1 <- PhotoCataNum(data_seapen,slider1)
#    output$imagefile1 <- PhotoSource(data_seapen,slider1)
#    output$gallery_seapan <- PhotoDisplay(data_seapen,slider1)    

    ################################################################################################ 
    #####################    BEGIN Photo Gallery: Black Coral TAB                ###################  
    output$speed_value1 <- renderUI({
      sliderInput("speed1","Animation Speed", 
                  min =0 , max=30, value = 3, step = 1, 
                  width = '100%', post = "s")
    })
    
    output$slider_to_anim1 <- renderUI({
      sliderInput(inputId = "slider1", 
                  label = "Animation", 
                  min = 1, max = length(data_black_image$ImageURL), 
                  value = 1, step = 1,
                  ticks = FALSE, width = '100%', 
                  animate = animationOptions(interval = input$speed1*1000, 
                                             loop = TRUE))  
    })
    
    output$imagenumber1 <- renderText({
      paste0("Catalog Number: ",data_black_image$CatalogNumber[input$slider1])
    })
    
    output$imagefile1 <- renderText({
      file.path(data_black_image$ImageURL[input$slider1])
    })
    
    output$gallery_black <- renderUI({
      tags$img(src = file.path(data_black_image$ImageURL[input$slider1]), 
               width = "70%", height = "auto")
    })
    #####################      END Photo Gallery: Black Coral TAB                ###################   
    ################################################################################################   
    
    ################################################################################################ 
    #####################      BEGIN Photo Gallery: Gold Coral TAB               ###################
    output$speed_value2 <- renderUI({
      sliderInput("speed2","Animation Speed", 
                  min =0 , max=30, value = 3, step = 1, 
                  width = '100%', post = "s")
    })
    
    output$slider_to_anim2 <- renderUI({
      sliderInput(inputId = "slider2", 
                  label = "Animation", 
                  min = 1, max = length(data_gold_image$ImageURL), 
                  value = 1, step = 1,
                  ticks = FALSE, width = '100%', 
                  animate = animationOptions(interval = input$speed2*1000, 
                                             loop = TRUE))  
    })
    
    output$imagenumber2 <- renderText({
      paste0("Catalog Number: ",data_gold_image$CatalogNumber[input$slider2])
    })
    
    output$imagefile2 <- renderText({
      file.path(data_gold_image$ImageURL[input$slider2])
    })
    
    output$gallery_gold <- renderUI({
      tags$img(src = file.path(data_gold_image$ImageURL[input$slider2]), 
               width = "70%", height = "auto")
    })
    #####################         END Photo Gallery: Gold Coral TAB              ###################   
    ################################################################################################   

    ################################################################################################ 
    #####################        BEGIN Photo Gallery: Gorgonian TAB              ###################      
    output$speed_value3 <- renderUI({
      sliderInput("speed3","Animation Speed", 
                  min =0 , max=30, value = 3, step = 1, 
                  width = '100%', post = "s")
    })
    
    output$slider_to_anim3 <- renderUI({
      sliderInput(inputId = "slider3", 
                  label = "Animation", 
                  min = 1, max = length(data_gorgonian_image$ImageURL), 
                  value = 1, step = 1,
                  ticks = FALSE, width = '100%', 
                  animate = animationOptions(interval = input$speed3*1000, 
                                             loop = TRUE))  
    })
    
    output$imagenumber3 <- renderText({
      paste0("Catalog Number: ",data_gorgonian_image$CatalogNumber[input$slider3])
    })
    
    output$imagefile3 <- renderText({
      file.path(data_gorgonian_image$ImageURL[input$slider3])
    })
    
    output$gallery_gorgonian <- renderUI({
      tags$img(src = file.path(data_gorgonian_image$ImageURL[input$slider3]), 
               width = "70%", height = "auto")
    })
    
    #####################          END Photo Gallery: Gorgonian TAB              ###################   
    ################################################################################################        
    
    ################################################################################################ 
    #####################      BEGIN Photo Gallery: Lace Coral TAB               ###################
    output$speed_value4 <- renderUI({
      sliderInput("speed4","Animation Speed", 
                  min =0 , max=30, value = 3, step = 1, 
                  width = '100%', post = "s")
    })
    
    output$slider_to_anim4 <- renderUI({
      sliderInput(inputId = "slider4", 
                  label = "Animation", 
                  min = 1, max = length(data_lace_image$ImageURL), 
                  value = 1, step = 1,
                  ticks = FALSE, width = '100%', 
                  animate = animationOptions(interval = input$speed4*1000, 
                                             loop = TRUE))  
    })
    
    output$imagenumber4 <- renderText({
      paste0("Catalog Number: ",data_lace_image$CatalogNumber[input$slider4])
    })
    
    output$imagefile4 <- renderText({
      file.path(data_lace_image$ImageURL[input$slider4])
    })
    
    output$gallery_lace <- renderUI({
      tags$img(src = file.path(data_lace_image$ImageURL[input$slider4]), 
               width = "70%", height = "auto")
    })
    #####################         END Photo Gallery: Lace Coral TAB              ###################   
    ################################################################################################       
    
    ################################################################################################ 
    #####################        BEGIN Photo Gallery: Sea Pen TAB                ###################  
    output$speed_value5 <- renderUI({
      sliderInput("speed5","Animation Speed", 
                  min =0 , max=30, value = 3, step = 1, 
                  width = '100%', post = "s")
    })
    
    output$slider_to_anim5 <- renderUI({
      sliderInput(inputId = "slider5", 
                  label = "Animation", 
                  min = 1, max = length(data_seapen_image$ImageURL), 
                  value = 1, step = 1,
                  ticks = FALSE, width = '100%', 
                  animate = animationOptions(interval = input$speed5*1000, 
                                             loop = TRUE))  
    })
    
    output$imagenumber5 <- renderText({
      paste0("Catalog Number: ",data_seapen_image$CatalogNumber[input$slider5])
    })
    
    output$imagefile5 <- renderText({
      file.path(data_seapen_image$ImageURL[input$slider5])
    })
    
    output$gallery_seapen <- renderUI({
      tags$img(src = file.path(data_seapen_image$ImageURL[input$slider5]), 
               width = "70%", height = "auto")
    })
    
    #####################         END Photo Gallery: Sea Pen TAB                 ###################   
    ################################################################################################     
    
    ################################################################################################ 
    #####################      BEGIN Photo Gallery: Soft Corals TAB              ###################       
    output$speed_value6 <- renderUI({
      sliderInput("speed6","Animation Speed", 
                  min =0 , max=30, value = 3, step = 1, 
                  width = '100%', post = "s")
    })
    
    output$slider_to_anim6 <- renderUI({
      sliderInput(inputId = "slider6", 
                  label = "Animation", 
                  min = 1, max = length(data_soft_image$ImageURL), 
                  value = 1, step = 1,
                  ticks = FALSE, width = '100%', 
                  animate = animationOptions(interval = input$speed6*1000, 
                                             loop = TRUE))  
    })
    
    output$imagenumber6 <- renderText({
      paste0("Catalog Number: ",data_soft_image$CatalogNumber[input$slider6])
    })
    
    output$imagefile6 <- renderText({
      file.path(data_soft_image$ImageURL[input$slider6])
    })
    
    output$gallery_soft <- renderUI({
      tags$img(src = file.path(data_soft_image$ImageURL[input$slider6]), 
               width = "70%", height = "auto")
    })
    #####################       END Photo Gallery: Soft Corals TAB               ###################      
    ################################################################################################ 
    
    ################################################################################################ 
    #####################     BEGIN Photo Gallery: Stoloniferan Corals TAB       ###################       
    output$speed_value7 <- renderUI({
      sliderInput("speed7","Animation Speed", 
                  min =0 , max=30, value = 3, step = 1, 
                  width = '100%', post = "s")
    })
    
    output$slider_to_anim7 <- renderUI({
      sliderInput(inputId = "slider7", 
                  label = "Animation", 
                  min = 1, max = length(data_stoloniferan_image$ImageURL), 
                  value = 1, step = 1,
                  ticks = FALSE, width = '100%', 
                  animate = animationOptions(interval = input$speed7*1000, 
                                             loop = TRUE))  
    })
    
    output$imagenumber7 <- renderText({
      paste0("Catalog Number: ",data_stoloniferan_image$CatalogNumber[input$slider7])
    })
    
    output$imagefile7 <- renderText({
      file.path(data_stoloniferan_image$ImageURL[input$slider7])
    })
    
    output$gallery_stoloniferan <- renderUI({
      tags$img(src = file.path(data_stoloniferan_image$ImageURL[input$slider7]), 
               width = "70%", height = "auto")
    })
    #####################      END Photo Gallery: Stoloniferan Corals TAB        ###################    
    ################################################################################################ 

    ################################################################################################    
    #####################      BEGIN Photo Gallery: Stony Corals TAB             ###################       
    output$speed_value8 <- renderUI({
      sliderInput("speed8","Animation Speed", 
                  min =0 , max=30, value = 3, step = 1, 
                  width = '100%', post = "s")
    })
    
    output$slider_to_anim8 <- renderUI({
      sliderInput(inputId = "slider8", 
                  label = "Animation", 
                  min = 1, max = length(data_stony_image$ImageURL), 
                  value = 1, step = 1,
                  ticks = FALSE, width = '100%', 
                  animate = animationOptions(interval = input$speed8*1000, 
                                             loop = TRUE))  
    })
    
    output$imagenumber8 <- renderText({
      paste0("Catalog Number: ",data_stony_image$CatalogNumber[input$slider8])
    })
    
    output$imagefile8 <- renderText({
      file.path(data_stony_image$ImageURL[input$slider8])
    })
    
    output$gallery_stony <- renderUI({
      tags$img(src = file.path(data_stony_image$ImageURL[input$slider8]), 
               width = "70%", height = "auto")
    })
    #####################       END Photo Gallery: Stony Corals TAB              ###################      
    ################################################################################################ 
    
    ################################################################################################ 
    #####################                   BEGIN MAP TAB                        ###################      
    output$map <- renderLeaflet({
      res <- selectdf(input$category, input$council, input$depth, input$number)
      selectdf <- res$df
      flag <- res$f
      popupInfo <- paste('<strong> Catalog Number </strong>:', selectdf$CatalogNumber, '<br/>', 
                         '<strong> Scientific Name </strong>: ', selectdf$ScientificName, '<br/>',
                         '<strong> Class (Subclass) </strong>:', selectdf$Class,'(',selectdf$Subclass,')','<br/>',
                         '<strong> Location </strong>:', selectdf$Locality, '<br/>',
                         '<strong> Position (lat lon) </strong>:', selectdf$latitude,selectdf$longitude, '<br/>',
                         '<strong> Depth (m) </strong>:', selectdf$DepthInMeters,'<br/>',
                         '<strong> Condition </strong>:', selectdf$Condition,'<br/>',
                         '<strong> Observation Date </strong>:', selectdf$ObservationDate, '<br/>',
                         '<strong> Data Provider </strong>:', selectdf$DataProvider,'<br/>')
      image_index <- which(!is.na(selectdf$ImageURL))  
      popupInfo[image_index] <- paste(popupInfo[image_index],
                                      '<img src = ', selectdf[image_index,]$ImageURL, 'height = "100" width = "100" >')

      if (flag == 0){
        output$error <- renderPrint({invisible(NULL)})
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(as.numeric(selectdf$longitude), as.numeric(selectdf$latitude), color = (selectdf$colors),
                           fillOpacity = 1, radius = 2.5, stroke = FALSE,
                           popup = popupInfo) %>%
          addLegend("topleft", colors=colors.pal, labels=sort(category_level),
                    title = "Coral Category") %>%          
          addProviderTiles(providers$Esri.OceanBasemap)%>%
          setView(lat = 30, lng = -100, zoom = 3)
        
      }
      
      else if (flag == 1){
        output$error <- renderPrint({invisible(NULL)})
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(as.numeric(selectdf$longitude), as.numeric(selectdf$latitude), color = (selectdf$colors),
                           fillOpacity = 1, radius = 2.5, stroke = FALSE,
                           popup = popupInfo) %>%
          addLegend("topleft", colors=colors.pal, labels=sort(category_level),
                    title = "Coral Category") %>%
          addProviderTiles(providers$Esri.OceanBasemap)%>%
          setView(lat = as.numeric(selectdf$latitude), lng = as.numeric(selectdf$longitude), zoom = 8)
      }
      
      else{
        output$error <- renderPrint({"Please enter the right number."})
        leaflet() %>%
          addTiles() %>%
          addLegend("topleft", colors=colors.pal, labels=sort(category_level),
                    title = "Coral Category") %>%
          addProviderTiles(providers$Esri.OceanBasemap)%>%
          setView(lat = 30, lng = -160, zoom = 3)
      }
      
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
    
    #####################                     END MAP TAB                        ###################      
    ################################################################################################ 
    
    ################################################################################################ 
    #####################                BEGIN STATISTICS TAB                    ###################      
    output$overall <- renderPlotly({
      plot_ly(x = ~FishCouncilRegion, y = ~sort(VernacularNameCategory), z = ~DepthInMeters,
              type = 'scatter3d', mode = 'markers', color = ~sort(VernacularNameCategory), 
              data = data_coral,marker = list(size = 4)) %>% 
        layout(plot_bgcolor='black') %>% 
        layout(paper_bgcolor='transparent')
    })
    
    output$hist<- renderPlot({
      selectdf_1 <- select_by_type(input$category_1)
      g <- ggplot(selectdf_1, aes(DepthInMeters)) + scale_fill_brewer(palette = "Blues")
      g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
        geom_histogram(aes(fill=Condition), bins=20, col="black", size=.1, alpha = 0.8) +
        labs(title="Histogram of Depth")+
        theme(
          plot.title = element_text(size=20, face="bold",hjust = 0.5),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold")
        )
    })
    
    output$pie<-renderPlot({
      selectdf_2 <- select_by_type(input$category_2)
      df <- data.frame(table(selectdf_2$FishCouncilRegion))
      colnames(df)<- c("Region","Freq")
      df = df[order(df$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df$Region)   
      myLabel = paste(myLabel, "(", round(df$Freq / sum(df$Freq) * 100, 2), "%)", sep = "")
      colors.pal <- brewer.pal(nrow(df), "Set2")
      lp <- pie3D(df$Freq,
                  radius=1.75, height=0.15, explode=0.1,
                  main="Pie Chart of Coral Amount by Regions", col= colors.pal)
      pie3D.labels(lp ,labels=myLabel,labelrad=2.5,minsep=1,labelcex = 1.2)
      
    })
    #####################                  END STATISTICS TAB                    ###################      
    ################################################################################################     
    
})
#####################                   END Shiny Server                       #####################
####################################################################################################