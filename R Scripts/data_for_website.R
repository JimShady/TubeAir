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

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="10.0.4.240")

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
  if (tube_data[i,]$concentration > tube_data[i,]$pm25/0.44 & tube_data[i,]$species == 'PM25') {
    tube_data[i,]$corrected_concentration <- (tube_data[i,]$pm25 + (tube_data[i,]$concentration - tube_data[i,]$pm25/0.44) * 1.82)
  } else
  {
    if (tube_data[i,]$concentration <= tube_data[i,]$pm25/0.44 & tube_data[i,]$species == 'PM25') {
    tube_data[i,]$corrected_concentration <-tube_data[i,]$concentration * 0.44
    } else
    {}
  }
}

tube_data$corrected_concentration[is.na(tube_data$corrected_concentration)] <- tube_data$concentration[is.na(tube_data$corrected_concentration)]

tube_colours <- c("#0099CC","#000000","#996633","#000099","#660066","#CC3333","#868F98","#FFCC00","#CC9999", "#009999", "#006633")


## Set a working directory to output the graphs too.
setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

rm(background_pm25, line_names_depth, tube_colours, ordered_by_median, i)

tube_data <- tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR' & !is.na(tube_data$tube_diary_stop),]

tube_data <- tube_data[,c('line', 'tube_diary_stop', 'platform_depth', 'corrected_concentration')]

tube_data <- aggregate(cbind(platform_depth,corrected_concentration) ~ line+tube_diary_stop, tube_data, mean)

station_locations <- dbGetQuery(con, "
                        SELECT tube_diary_stop as station, line, st_x(st_transform(station_geometry, 4326)) as x, st_y(st_transform(station_geometry,4326)) as y 
                        FROM tube_pollution_mapping 
                        WHERE tube_diary_stop IS NOT NULL
                        GROUP BY tube_diary_stop, line, st_x(st_transform(station_geometry, 4326)), st_y(st_transform(station_geometry,4326))
                        ")

names(tube_data)[names(tube_data) == 'tube_diary_stop'] <- 'station'

data <- merge(tube_data, station_locations, by=c("station","line"), all.x=TRUE)


names(data)[names(data) == 'corrected_concentration'] <- 'pm25'

write.csv(data, 'website_spatial_data.csv')
