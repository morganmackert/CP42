#-------------------------------------------------------------------#
#                           SUMMARY STATS                           #
#-------------------------------------------------------------------#

#Research Question: How well does CP42 pollinator habitat support native bees?

#Objectives:
#Determine availability of bee habitat resources (bare ground (%), vegetation coverage (%), number of floral ramets, number of flowers, floral species richness) within CP42 sites
#Determine bee abundance and species richness at sites with CP42 habitat established

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/UIUC/Data/USDA CP42")

#Load libraries
library(lubridate)
library(dplyr)

#Read in data
Veg <- read.csv("Vegetation/USDA CP42 Vegetation.csv", na.strings = c("", "NA"))
Bees <- read.csv("Bees/2019 USDA CP-42 bees 3-4-2020.csv", na.strings = c("", "NA"))

#Add "month" column to Veg data for grouping
Veg$Date <- mdy(Veg$Date)
Veg$Month <- month(Veg$Date)

#Change column headings so they're not so weird
colnames(Veg)[which(names(Veg) == "Bare.Ground....")] <- "BareGround"
colnames(Veg)[which(names(Veg) == "Vegetation....")] <- "Vegetation"
colnames(Veg)[which(names(Veg) == "No..Ramets")] <- "No.Ramets"
colnames(Veg)[which(names(Veg) == "No..Flowers")] <- "No.Flowers"

#Bare Ground ####
#-------------------------------------------------------------------#
#                     Bare Ground Availability                      #
#-------------------------------------------------------------------#
#Calculate total bare ground in each quadrat/site/date; indicate [1] to take only the first value from each quadrat (meaning exclude multiple entries for the same quadrat due to multiple floral species observed)
bareground <- Veg %>%
  group_by(Date, Site, Transect, Quadrat) %>%
  summarise(total.bareground = BareGround[1])

#Calculate average bare ground cover for each site/date and calculate the number of quadrats 
avg.bareground <- bareground %>%
  group_by(Date, Site) %>%
  summarise(avg.bareground = mean(total.bareground), 
            number.quadrats = length(total.bareground))
#All sites/dates have 25 quadrats, which is correct for 5 transects of 5 quadrats each

#Vegetation Coverage ####
#-------------------------------------------------------------------#
#                       Vegetation Coverage                         #
#-------------------------------------------------------------------#
#Calculate total vegetation in each quadrat/site/date; indicate [1] to take only the first value from each quadrat (meaning exclude multiple entries for the same quadrat due to multiple floral species observed)
vegetation <- Veg %>%
  #filter(!is.na(Vegetation)) %>%
  group_by(Date, Site, Transect, Quadrat) %>%
  summarise(total.vegetation = Vegetation[1])

#Calculate average vegetation coverage for each site/date and calculate the number of quadrats 
avg.veg <- vegetation %>%
  group_by(Date, Site) %>%
  summarise(avg.veg = mean(total.vegetation), 
            number.quadrats = length(total.vegetation))
#All sites/dates have 25 quadrats, which is correct for 5 transects of 5 quadrats each

#Floral Ramets ####
#-------------------------------------------------------------------#
#                      Number of Floral Ramets                      #
#-------------------------------------------------------------------#
#Fill NAs with 0 in Veg$No.Ramets
Veg$No.Ramets[is.na(Veg$No.Ramets)] <- 0

#Calculate total number of ramets in bloom for each site/date
ramets <- Veg %>%
  group_by(Date, Site) %>%
  summarise(total.ramets = sum(No.Ramets))

#Flowers ####
#-------------------------------------------------------------------#
#                         Number of Flowers                         #
#-------------------------------------------------------------------#
#Convert Veg$No.Flowers to numeric
Veg$No.Flowers <- as.numeric(Veg$No.Flowers)

#Fill NAs with 0 in Veg$No.Flowers
Veg$No.Flowers[is.na(Veg$No.Flowers)] <- 0

#Calculate total number of flowers in bloom for each site/date
flowers <- Veg %>%
  group_by(Date, Site) %>%
  summarise(total.flowers = sum(No.Flowers))

#Floral Species Richness ####
#-------------------------------------------------------------------#
#                      Floral Species Richness                      #
#-------------------------------------------------------------------#
#Convert Veg$Blooming.Species to character
Veg$Blooming.Species <- as.character(Veg$Blooming.Species)

#Determine number of floral species in bloom for each site/date
floralspp <- Veg %>%
  filter(!is.na(Blooming.Species)) %>%
  group_by(Date, Site) %>%
  summarise(no.floralspp = n_distinct(Blooming.Species))
#Only 35 observations because June LEW had 0 blooming species, so the NA is filtered. Morgan to fix this.
#Bee Abundance ####