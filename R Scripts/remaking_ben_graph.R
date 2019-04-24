rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("grid")
library("devtools")
library("tibble")
library("openair")
library("gdata")
library(ggplot2)
library(grid)
library(gridExtra)
library(data.table)

## Get the data from Ben's excel that I've maniulated and output to a CSV
## Changed to the new data _revised
data                <- read.csv('/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/LU AQ Data/RevisedTubeJourneys.csv',
                                stringsAsFactors = FALSE)

setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

# DAta has changed a bit since wrote the below code. Just going to correct code for the new stuff.

data <- data.table(data)

data <- data[!(data$environment == 'Underground Out' & data$minute > 15),]
data <- data[!(data$environment == 'Underground Return' & data$minute > 15),]
data <- data[!(data$environment == 'Oxford Street' & data$minute > 120),]
data <- data[!(data$environment %in% c('V', 'Hyde Park')),]

#data[, min := seq_len(.N), by = list(Exposure, environment)]

data <- data.frame(data)

data[data$environment == 'Oxford Street',]$minute <- data[data$environment == 'Oxford Street',]$minute+15
data[data$environment == 'Underground Return',]$minute <- data[data$environment == 'Underground Return',]$minute+135

ggplot() + 
  geom_smooth(data = data, aes(minute, pm25, group = environment, colour = environment)) +
  xlab('Minute of journey') +
  ylab(expression(PM[2.5]~(ug~m^3))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        axis.title.x = element_text(size = 10, colour = 'black'),
        axis.title.y = element_text(size = 10, colour = 'black'),
        title = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(0.84,0.99),
        legend.text = element_text(size = 12),
        legend.justification = c(0.5,1),
        legend.background = element_rect(color = 'black')) +
  ggtitle('Journey environment comparisons') +
  xlim(0,150)
ggsave('oxford_timeline.png', width = 10, height = 5)

## Now use all the data again to do boxplots

## Get the data from Ben's excel that I've maniulated and output to a CSV
## Changed to the new data _revised
data                <- read.csv('/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/LU AQ Data/RevisedTubeJourneys.csv',
                                stringsAsFactors = FALSE)

setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

# DAta has changed a bit since wrote the below code. Just going to correct code for the new stuff.

data[data$environment == 'Underground Out',]$environment <- 'Underground'
data[data$environment == 'Underground Return',]$environment <- 'Underground'
data[data$environment == 'Hyde Park',]$environment <- 'Hyde Park'
data[data$environment == 'Oxford Street',]$environment <- 'Oxford Street'

ggplot(data = data[data$environment %in% c('Hyde Park', 'Oxford Street', 'Underground'),], aes(environment, pm25)) + 
  geom_boxplot() +
  geom_point(data = aggregate(pm25 ~ environment, data[data$environment %in% c('Hyde Park', 'Oxford Street', 'Underground'),], mean),
             aes(environment, pm25), colour = 'red') +
  xlab('Sampling Environment') +
  ylab(expression(PM[2.5]~(ug~m^3))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15, colour = 'black'),
        axis.text.y = element_text(size = 15, colour = 'black'),
        axis.title.x = element_text(size = 15, colour = 'black'),
        axis.title.y = element_text(size = 15, colour = 'black'),
        title = element_text(size = 18)) +
  ggtitle(expression(PM[2.5]))
ggsave('pm25_box.png', width = 6, height = 6)

ggplot(data = data[data$environment %in% c('Hyde Park', 'Oxford Street', 'Underground'),], aes(environment, pnum)) + 
  geom_boxplot() +
  geom_point(data = aggregate(pnum ~ environment, data[data$environment %in% c('Hyde Park', 'Oxford Street', 'Underground'),], mean),
             aes(environment, pnum), colour = 'red') +
  xlab('Sampling Environment') +
  ylab('Particle number') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15, colour = 'black'),
        axis.text.y = element_text(size = 15, colour = 'black'),
        axis.title.x = element_text(size = 15, colour = 'black'),
        axis.title.y = element_text(size = 15, colour = 'black'),
        title = element_text(size = 18)) +
  ggtitle('Particle Number')
ggsave('particle_number_box.png', width = 6, height = 6)

ggplot(data = data[data$environment %in% c('Hyde Park', 'Oxford Street', 'Underground'),], aes(environment, pdia)) + 
  geom_boxplot() +
  geom_point(data = aggregate(pdia ~ environment, data[data$environment %in% c('Hyde Park', 'Oxford Street', 'Underground'),], mean),
             aes(environment, pdia), colour = 'red') +
  xlab('Sampling Environment') +
  ylab('Particle diameter in nanometres') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15, colour = 'black'),
        axis.text.y = element_text(size = 15, colour = 'black'),
        axis.title.x = element_text(size = 15, colour = 'black'),
        axis.title.y = element_text(size = 15, colour = 'black'),
        title = element_text(size = 18)) +
  ggtitle('Particle diameter')
ggsave('particle_diameter_box.png', width = 6, height = 6)

# Statistics

summary(data[data$environment == 'Underground',]$pm25)
summary(data[data$environment == 'Hyde Park',]$pm25)
summary(data[data$environment == 'Oxford Street',]$pm25)

summary(data[data$environment == 'Underground',]$pdia)
summary(data[data$environment == 'Hyde Park',]$pdia)
summary(data[data$environment == 'Oxford Street',]$pdia)

summary(data[data$environment == 'Underground',]$pnum)
summary(data[data$environment == 'Hyde Park',]$pnum)
summary(data[data$environment == 'Oxford Street',]$pnum)
