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
          "Condition","FishCouncilRegion","DataProvider","Website","ImageURL")
dataUSA <- dataUSA[,vars]

#delete NA
colSums(is.na(dataUSA))
dataUSA <- dataUSA[!is.na(dataUSA$VernacularNameCategory),]
dataUSA <- dataUSA[!is.na(dataUSA$FishCouncilRegion),]

write.csv(dataUSA, "deep_sea_corals_USA.csv", row.names = FALSE)
