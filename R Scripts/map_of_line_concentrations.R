## Tube map of concentrations

library("RPostgreSQL")
library(openair)
library(rgeos)

latlong = "+init=epsg:4326"
ukgrid = "+init=epsg:27700"
google = "+init=epsg:3857"

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="10.0.4.240")

## Get daily background averages for London from OpenAir
background_pm25 <- importKCL(site = "kc1", year = c(2014,2015,2016), pollutant = "pm25", met = FALSE,
                             units = "mass", extra = FALSE)
background_pm25 <- data.frame(background_pm25, day = as.Date(format(background_pm25$date)))
background_pm25 <- aggregate(pm25 ~ day, background_pm25, mean)

tube_data <- dbGetQuery(con, "
SELECT 	date_time,
	concentration as pm25_concentration,
	line,
	previous_tube_stop as start_station,
	next_tube_stop as end_station,
	st_x(st_startpoint(line_geometry)) as start_x,
	st_y(st_startpoint(line_geometry)) as start_y,
	st_x(st_endpoint(line_geometry)) as end_x,
	st_y(st_endpoint(line_geometry)) as end_y
FROM	tube_pollution_mapping
WHERE	species = 'PM25'
AND	environment = 'CAR'
AND	tube_diary_stop IS NULL
  ")

## Sort out linking the daily means to the tube concentrations
tube_data <- data.frame(tube_data, day = as.Date(format(tube_data$date_time)))
tube_data <- merge(tube_data, background_pm25, by="day", all.x=TRUE)

# Find that concentration for 1 Feb 2016 is missing from the London Air background PM2.5 data. Need some numbers for that.
# So going to use the data from 8 Feb 2016 instead. It's the same day of the week, the week after.
tube_data[is.na(tube_data$pm25),]$pm25 <- background_pm25[background_pm25$day == '2016-02-08',]$pm25

tube_data$corrected_pm25 <- as.numeric('')

## Now adjust the data
for (i in 1:nrow(tube_data)) {
      if (tube_data[i,]$pm25_concentration > tube_data[i,]$pm25) {
            tube_data[i,]$corrected_pm25 <- (tube_data[i,]$pm25 * 0.6) + ((tube_data[i,]$pm25_concentration - tube_data[i,]$pm25) * 2)
      } else
      {
            tube_data[i,]$corrected_pm25 <-tube_data[i,]$pm25_concentration * 0.6
      }
}

tube_data <- tube_data[c('line', 'start_station', 'end_station', 'start_x', 'start_y', 'end_x', 'end_y', 'corrected_pm25')]

rm(con, i, drv, background_pm25)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="10.0.4.240")

dbSendQuery(con, "DROP TABLE IF EXISTS temp")

dbWriteTable(con, "temp", tube_data, row.names=FALSE)

dbSendQuery(con, "ALTER TABLE temp ADD COLUMN line_geom GEOMETRY")

dbSendQuery(con, "UPDATE temp SET line_geom = ST_MakeLine(ST_MakePoint(start_x, start_y), ST_MakePoint(end_x, end_y))")

dbSendQuery(con, "DROP TABLE IF EXISTS pm25_on_lines")

new_tube_data <- dbGetQuery(con, "
SELECT 	line,
		AVG(corrected_pm25) as pm25,
            st_x(st_startpoint(line_geom)) as start_x,
	st_y(st_startpoint(line_geom)) as start_y,
	st_x(st_endpoint(line_geom)) as end_x,
	st_y(st_endpoint(line_geom)) as end_y
FROM		temp
GROUP BY	line,
		line_geom
  ")

dbSendQuery(con, "DROP TABLE temp")

tube_data$corrected_pm25 <- NULL

final_tube_data <- merge(new_tube_data, unique(tube_data))

setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

write.csv(final_tube_data, file = "tube_results.csv", row.names = FALSE)
