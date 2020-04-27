#-------------------------------------------------------------------#
#                        PRELIMINARY PLOTS                          #
#-------------------------------------------------------------------#

#Research Question: How well does CP42 pollinator habitat support native bees?

#Objectives:
#Create preliminary plots exploring the influence of landscape characteristics (blooming forb abundance, blooming forb richness, and bare ground availability) on the bee community (bee richness and bee abundance)

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/UIUC/Analyses/CP42/Data")

#Load libraries
library(lubridate)
library(tidyr)
library(plyr)
library(dplyr)

#Read in data
veg <- read.csv("Vegetation/CP42 Total Habitat Resources.csv", na.strings = c("", "NA"))
no.bees <- read.csv("Bees/CP42 Bee Abundance by Site & Month.csv", na.strings = c("", "NA"))
no.beespp <- read.csv("Bees/CP42 Number Bee Species by Site & Month.csv", na.strings = c("", "NA"))

#Join veg and bee data together
beeshabita <- left_join(veg, no.bees, by = c("Month", "Site"))
beeshabitat <- left_join(beeshabita, no.beespp, by = c("Month", "Site"))

