df <- read.csv("Desktop/5243 ADS/Project2_9/deep_sea_corals_customized.csv", as.is=TRUE)
df <- df[-1,]

#USA
dataUSA <- df[which(df$Country=="USA"),]
unique(dataUSA$VernacularNameCategory)

#delete sponges & coral-like & lithotelestid coral(3)
dataUSA <- dataUSA[-which(dataUSA$VernacularNameCategory=="calcareous sponge"|
                            dataUSA$VernacularNameCategory=="homoscleromorph sponge"|
                            dataUSA$VernacularNameCategory=="demosponge"|
                            dataUSA$VernacularNameCategory=="glass sponge"|
                            dataUSA$VernacularNameCategory=="sponge (unspecified)"|
                            dataUSA$VernacularNameCategory=="other coral-like hydrozoan"|
                            dataUSA$VernacularNameCategory=="lithotelestid coral"),]

#combine stony
dataUSA$VernacularNameCategory[which(dataUSA$VernacularNameCategory=="stony coral (branching)"|
                                       dataUSA$VernacularNameCategory=="stony coral (cup coral)"|
                                       dataUSA$VernacularNameCategory=="stony coral (unspecified)")] <- "stony coral"
unique(dataUSA$VernacularNameCategory)

#choose variables
vars <- c("CatalogNumber","VernacularNameCategory","ScientificName","Class","Subclass",
          "Locality","latitude","longitude","DepthInMeters","ObservationDate",
          "Condition","FishCouncilRegion","DataProvider","ImageURL")
dataUSA <- dataUSA[,vars]

#delete NA
colSums(is.na(dataUSA))
for (i in 1:13){
  dataUSA <- dataUSA[which(!is.na(dataUSA[,i])),]
}
dataUSA <- dataUSA[which(dataUSA$DepthInMeters!=-999),]
range(dataUSA$DepthInMeters)

#condition
table(dataUSA$Condition)
dataUSA <- dataUSA[-which(dataUSA$Condition=="Broken or missing"|dataUSA$Condition=="Damaged/fishing line"|
                            dataUSA$Condition=="Damaged/predation"|dataUSA$Condition=="Damaged/unknown"|
                            dataUSA$Condition=="Dead & live "|dataUSA$Condition=="Dead/damaged"|
                            dataUSA$Condition=="Gear entangled"|dataUSA$Condition=="Live, dead"|
                            dataUSA$Condition=="Mat growth"|dataUSA$Condition=="Overturned-fully"|
                            dataUSA$Condition=="Overturned-partial"|dataUSA$Condition=="Toppled"|
                            dataUSA$Condition=="Unnown"),]
dataUSA[which(dataUSA$Condition=="live"),]$Condition = "Live"
dataUSA[which(dataUSA$Condition!="Dead" & dataUSA$Condition!="Live"),]$Condition = "Damaged"

write.csv(dataUSA, "deep_sea_corals_USA.csv", row.names = FALSE)

#sample
library(fifer)
table(dataUSA$VernacularNameCategory)
data_coral <- stratified(dataUSA, "VernacularNameCategory", 0.2)
save(data_coral,file="deep_sea_corals_sample.Rdata")
