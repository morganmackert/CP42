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
library(tidyr)
library(plyr)
library(dplyr)

#Read in data
Veg <- read.csv("Vegetation/USDA CP42 Vegetation.csv", na.strings = c("", "NA"))
Bees <- read.csv("Bees/2019 USDA CP-42 bees 3-6-2020.csv", na.strings = c("", "NA"))

#Add "month" column to Veg data for grouping
Veg$Date <- mdy(Veg$Date)
Veg$Month <- month(Veg$Date)

#Merge Month Day and Year columns in Bees for grouping
Bees$Date <- paste(Bees$Year, Bees$Month, Bees$Day, sep = "-") %>%
  ymd()

#Remove "Wasp" and NA entries from Bees
Bees <- Bees %>%
  filter(!is.na(Latin.Binomial)) %>%
  filter(Family != "Wasp")

#Change all Ceratina spp. to be "Ceratina calcarata/dupla/mikmaqi group"
Bees$Latin.Binomial <- revalue(Bees$Latin.Binomial, c("Ceratina calcarata" = "Ceratina calcarata/dupla/mikmaqi group"))
Bees$Latin.Binomial <- revalue(Bees$Latin.Binomial, c("Ceratina dupla" = "Ceratina calcarata/dupla/mikmaqi group"))
Bees$Latin.Binomial <- revalue(Bees$Latin.Binomial, c("Ceratina mikmaqi" = "Ceratina calcarata/dupla/mikmaqi group"))
Bees$Latin.Binomial <- revalue(Bees$Latin.Binomial, c("Ceratina sp." = "Ceratina calcarata/dupla/mikmaqi group"))

#Change column headings in Veg so they're not so weird
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
  group_by(Month, Site, Transect, Quadrat) %>%
  summarise(total.bareground = BareGround[1])

#Calculate average bare ground cover for each site/date and calculate the number of quadrats 
avg.bareground <- bareground %>%
  group_by(Month, Site) %>%
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
  group_by(Month, Site, Transect, Quadrat) %>%
  summarise(total.vegetation = Vegetation[1])

#Calculate average vegetation coverage for each site/date and calculate the number of quadrats 
avg.veg <- vegetation %>%
  group_by(Month, Site) %>%
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
  group_by(Month, Site) %>%
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
  group_by(Month, Site) %>%
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
  group_by(Month, Site) %>%
  summarise(no.floralspp = n_distinct(Blooming.Species))
#Only 35 observations because June LEW had 0 blooming species, so the NA is filtered. Morgan to fix this.

#Determine number of floral species in bloom for each month
floralspp_month <- Veg %>%
  filter(!is.na(Blooming.Species)) %>%
  group_by(Month) %>%
  summarise(no.floralspp = n_distinct(Blooming.Species))

#Determine total number of floral species in bloom during 2019
floralspp_total <- Veg %>%
  filter(!is.na(Blooming.Species)) %>%
  summarise(no.floralspp = n_distinct(Blooming.Species))

#Total Habitat Resources ####
#-------------------------------------------------------------------#
#                      Total Habitat Resources                      #
#-------------------------------------------------------------------#
#Join all habitat resource dataframes together: avg.bareground, avg.veg, floralspp, ramets, and flowers
habi <- left_join(avg.bareground, avg.veg, by = c("Month", "Site"))
habit <- left_join(habi, floralspp, by = c("Month", "Site"))
habita <- left_join(habit, ramets, by = c("Month", "Site"))
habitat <- left_join(habita, flowers, by = c("Month", "Site"))

#Fill NAs with 0 in "habitat"
habitat$no.floralspp[is.na(habitat$no.floralspp)] <- 0

#Export as .csv file
#write.csv(habitat, "C:/Users/Morgan/Documents/UIUC/Analyses/CP42/Data/Vegetation/CP42 Total Habitat Resources.csv", row.names = FALSE)

#Bee Abundance ####
#-------------------------------------------------------------------#
#                           Bee Abundance                           #
#-------------------------------------------------------------------#
#Calculate number of bees collected for each site/date
no.bees <- Bees %>%
  group_by(Month, Site) %>%
  count(Latin.Binomial) %>%
  summarise(no.bees = sum(n))

#Export as .csv file
#write.csv(no.bees, "C:/Users/Morgan/Documents/UIUC/Analyses/CP42/Data/Bees/CP42 Bee Abundance by Site & Month.csv", row.names = FALSE)

#Calculate number of bees collected each month
no.bees_month <- Bees %>%
  group_by(Month) %>%
  count(Latin.Binomial) %>%
  summarise(no.bees = sum(n))

#Bee Species Richness ####
#-------------------------------------------------------------------#
#                        Bee Species Richness                       #
#-------------------------------------------------------------------#
#Calculate number of bee species collected for each site/date
no.beespp <- Bees %>%
  group_by(Month, Site) %>%
  summarise(no.beespp = n_distinct(Latin.Binomial))

#Export as .csv file
#write.csv(no.beespp, "C:/Users/Morgan/Documents/UIUC/Analyses/CP42/Data/Bees/CP42 Number Bee Species by Site & Month.csv", row.names = FALSE, na = "0")

#Determine which bee species were collected from each site/date
beespp <- Bees %>%
  group_by(Month, Site) %>%
  count(Latin.Binomial)

#Reformat no.beespp from long to wide to determine abundance of each bee species collected from each site/date
beespp_wide <- beespp %>%
  spread(Latin.Binomial, n)

#Export as .csv file
#write.csv(beespp_wide, "C:/Users/Morgan/Documents/UIUC/Analyses/CP42/Data/Bees/CP42 Bee Species by Site & Month.csv", row.names = FALSE, na = "0")

#Calculate number of bee species collected each month
no.beespp_month <- Bees %>%
  group_by(Month) %>%
  summarise(no.beespp = n_distinct(Latin.Binomial))
  
#Determine which bee species were collected during each month
beespp_month <- Bees %>%
  group_by(Month) %>%
  count(Latin.Binomial)

#Reformat beespp_month from long to wide to determine abundance of each bee species collected each month
beespp_month_wide <- beespp_month %>%
  spread(Latin.Binomial, n)

#Data Dictionary ####
#Number: Number assigned to each specimen
#Project: Project specimens were collected for
#Year: Year in which specimens were collected
#Month: Month in which specimens were collected
#Day: Day of month on which specimens were collected
#Date Collected: Date formatted for INHS labels to be created
#Country: Country in which specimens were collected
#State: State in which specimens were collected
#County: County in which specimens were collected
#Site: Site from which specimens were collected
#Coordinates: Latitude and longitude from site from which specimens were collected
#Collection Method: Trap in which specimens were captured
#Flower Species: If specimens were netted, flower specimen was collected from
#Collector: Lead of project
#Identification Number: Unique identifier for each specimen composed of: FundingInstitution-ProjectType-Site-MonthYear-Number
#Family: Family specimen belongs to
#Genus: Genus specimen belongs to
#Species: Species specimen identified to
#Latin.Binomial: Specific epithet for each specimen: Genus species
#Sex: Sex specimen identified as
#Identifier: Person who determined identifications