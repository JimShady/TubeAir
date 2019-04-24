rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("grid")
library("openair")
library("gdata")
library(openair)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="localhost")

## Get daily background averages for London from OpenAir
background_pm25 <- importKCL(site = "kc1", year = c(2014,2015,2016), pollutant = "pm25", met = FALSE,
                             units = "mass", extra = FALSE)
background_pm25 <- data.frame(background_pm25, day = as.Date(format(background_pm25$date)))
background_pm25 <- aggregate(pm25 ~ day, background_pm25, mean)

## Note the CASE statement below. it's because want the raw PM25 data to do our own scaling, but the scaled version of other pollutants.
tube_data <- dbGetQuery(con, "
                        WITH  station_depths AS (
                        SELECT		station_depths_import.station_name,
                        station_depths_import.line,
                        station_depths_import.platform_depth,
                        station_info.shortname,
                        station_info.the_geom
                        FROM		station_depths_import
                        LEFT JOIN	(	SELECT		shortname,
                        line,
                        the_geom
                        FROM		station_geom_depth
                        GROUP BY 	shortname,
                        line,
                        the_geom) AS station_info
                        ON		station_depths_import.station_name 	= station_info.shortname
                        AND		station_depths_import.line		= station_info.line
                        )
                        SELECT		tube_pollution_mapping.species,
                        tube_pollution_mapping.environment,
                        tube_pollution_mapping.date_time,
                        CASE WHEN tube_pollution_mapping.species = 'PM25' THEN tube_pollution_mapping.concentration
                             ELSE tube_pollution_mapping.scaled_concentration
                        END AS concentration,
                        tube_pollution_mapping.tube_diary_stop,
                        tube_pollution_mapping.line,
                        station_depths.platform_depth
                        FROM		tube_pollution_mapping
                        LEFT JOIN	station_depths
                        ON		tube_pollution_mapping.tube_diary_stop 	= 	station_depths.station_name
                        AND		tube_pollution_mapping.line		=	station_depths.line
                        ORDER BY	tube_pollution_mapping.date_time,
                        tube_pollution_mapping.environment,
                        tube_pollution_mapping.species
                        ")

tube_data <- data.frame(tube_data, day = as.Date(format(tube_data$date_time)))
tube_data <- merge(tube_data, background_pm25, by="day", all.x=TRUE)

# Find that concentration for 1 Feb 2016 is missing from the London Air background PM2.5 data. Need some numbers for that.
# So going to use the data from 8 Feb 2016 instead. It's the same day of the week, the week after.
tube_data[is.na(tube_data$pm25),]$pm25 <- background_pm25[background_pm25$day == '2016-02-08',]$pm25

# Now do the correction process
tube_data$corrected_concentration <- as.numeric('')

## Now adjust the data
for (i in 1:nrow(tube_data)) {
  if (tube_data[i,]$concentration > tube_data[i,]$pm25 & tube_data[i,]$species == 'PM25') {
    tube_data[i,]$corrected_concentration <- (tube_data[i,]$pm25 * 0.6) + ((tube_data[i,]$concentration - tube_data[i,]$pm25) * 2)
  } else
  {
    if (tube_data[i,]$concentration <= tube_data[i,]$pm25 & tube_data[i,]$species == 'PM25') {
    tube_data[i,]$corrected_concentration <-tube_data[i,]$concentration * 0.6
    } else
    {}
  }
}

tube_data$corrected_concentration[is.na(tube_data$corrected_concentration)] <- tube_data$concentration[is.na(tube_data$corrected_concentration)]

tube_data$line <- factor(tube_data$line, levels = c("Victoria", "Piccadilly", "Northern", "Circle", "Jubilee", "District", "Bakerloo", "Metropolitan", "Docklands Light Railway", "Central", "Hammersmith & City"))

tube_colours <- c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333", "#CC9999")

tube_data$train_stock <- ''

tube_data[tube_data$line == 'Victoria',"train_stock"]               <- '2009'
tube_data[tube_data$line == 'Piccadilly',"train_stock"]             <- '1973'
tube_data[tube_data$line == 'Northern',"train_stock"]               <- 'S 2010'
tube_data[tube_data$line == 'Circle',"train_stock"]                 <- 'S 2010'
tube_data[tube_data$line == 'Jubilee',"train_stock"]                <- '1996'
tube_data[tube_data$line == 'District',"train_stock"]               <- 'D 1980'
tube_data[tube_data$line == 'Bakerloo',"train_stock"]               <- '1972'
tube_data[tube_data$line == 'Metropolitan',"train_stock"]           <- 'S 2010'
tube_data[tube_data$line == 'Docklands Light Railway',"train_stock"]<- 'B07 2005'
tube_data[tube_data$line == 'Central',"train_stock"]                <- '1992'
tube_data[tube_data$line == 'Hammersmith & City',"train_stock"]     <- 'S 2010'

## Set a working directory to output the graphs too.
setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

pdf("pm25_by_stock_just_car.pdf", width=11.67, height=8.27)
ggplot(tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',], aes(train_stock, corrected_concentration, fill=interaction(train_stock))) +
  stat_boxplot(geom='errorbar') +
  geom_boxplot() +
    theme(axis.line = element_line(colour = "black")) +
  theme(axis.line = element_line(colour="black"),
        axis.text=element_text(size=16, color="black"),
        axis.title=element_text(size=16, color="black"),
        axis.text.x=element_text(angle = 40, hjust = 1),
        plot.title=element_text(size=16, colour="black"),
        legend.position="none") +
  labs(title="",
       x=expression(paste("Tube stock")), 
       y=expression(paste("PM"[2.5], "(", mu, "g m" ^ "-3", ") "))
       ) +
  scale_y_continuous(labels=comma)
dev.off()

