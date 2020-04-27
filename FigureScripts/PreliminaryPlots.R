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
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)

#Read in data
veg <- read.csv("Vegetation/CP42 Total Habitat Resources.csv", na.strings = c("", "NA"))
no.bees <- read.csv("Bees/CP42 Bee Abundance by Site & Month.csv", na.strings = c("", "NA"))
no.beespp <- read.csv("Bees/CP42 Number Bee Species by Site & Month.csv", na.strings = c("", "NA"))

#Join veg and bee data together
beeshabita <- left_join(veg, no.bees, by = c("Month", "Site"))
beeshabitat <- left_join(beeshabita, no.beespp, by = c("Month", "Site"))

#Change Month to factor
beeshabitat$Month <- as.factor(beeshabitat$Month)

#Bee Abundance ~ Bare Ground ####
#-------------------------------------------------------------------#
#                   Bee Abundance ~ Bare Ground                     #
#-------------------------------------------------------------------#
babg <- ggplot(beeshabitat,
               aes(x = avg.bareground,
                   y = no.bees)) +
  geom_point(aes(shape = Month,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Number of Bees")
babg

#Bee Species ~ Bare Ground ####
#-------------------------------------------------------------------#
#                     Bee Species ~ Bare Ground                     #
#-------------------------------------------------------------------#
bsbg <- ggplot(beeshabitat,
               aes(x = avg.bareground,
                   y = no.beespp)) +
  geom_point(aes(shape = Month,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Number of Bee Species")
bsbg

#Bee Abundance ~ Blooming Forb Abundance ####
#-------------------------------------------------------------------#
#                Bee Abundance ~ Blooming Forb Abundance            #
#-------------------------------------------------------------------#
#Number of ramets
bara <- ggplot(beeshabitat,
               aes(x = total.ramets,
                   y = no.bees)) +
  geom_point(aes(shape = Month,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Floral Ramets",
       y = "Number of Bees")
bara

#Number of flowers
bafa <- ggplot(beeshabitat,
               aes(x = total.flowers,
                   y = no.bees)) +
  geom_point(aes(shape = Month,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Flowers",
       y = "Number of Bees")
bafa

#Bee Species ~ Blooming Forb Abundance ####
#-------------------------------------------------------------------#
#                    Bee Species ~ Blooming Forb Abundance          #
#-------------------------------------------------------------------#
#Number of ramets
bsra <- ggplot(beeshabitat,
               aes(x = total.ramets,
                   y = no.beespp)) +
  geom_point(aes(shape = Month,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Floral Ramets",
       y = "Number of Bee Species")
bsra

#Number of flowers
bsfa <- ggplot(beeshabitat,
               aes(x = total.flowers,
                   y = no.beespp)) +
  geom_point(aes(shape = Month,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Flowers",
       y = "Number of Bee Species")
bsfa

#Bee Abundance ~ Blooming Forb Species ####
#-------------------------------------------------------------------#
#                   Bee Abundance ~ Blooming Forb Species           #
#-------------------------------------------------------------------#
bafs <- ggplot(beeshabitat,
               aes(x = no.floralspp,
                   y = no.bees)) +
  geom_point(aes(shape = Month,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Floral Species",
       y = "Number of Bees")
bafs

#Bee Species ~ Blooming Forb Species ####
#-------------------------------------------------------------------#
#                   Bee Species ~ Blooming Forb Species             #
#-------------------------------------------------------------------#
bsfs <- ggplot(beeshabitat,
               aes(x = no.floralspp,
                   y = no.beespp)) +
  geom_point(aes(shape = Month,
                 color = Site),
             size = 3) +
  geom_smooth(method = "glm",
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Floral Species",
       y = "Number of Bee Species")
bsfs
