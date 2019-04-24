rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("scales")
library("grid")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="james_traffic", user="james", password="brianclough", host="localhost")

setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

query <- 
"SELECT  	dynamic_exposure_stops_no_monthly.ppid,
dynamic_exposure_stops_no_monthly.point_time,
CASE	WHEN	dynamic_exposure_stops_no_monthly.mode = 1					THEN	'Walk'
WHEN	dynamic_exposure_stops_no_monthly.mode = 2					THEN	'Cycle'
WHEN	dynamic_exposure_stops_no_monthly.mode IN (5,6)					THEN	'Motorcycle'
WHEN	dynamic_exposure_stops_no_monthly.mode IN (3,4,9,10,11,12,14,15,16,21,22)	THEN 	'Driving'
WHEN	dynamic_exposure_stops_no_monthly.mode = 13					THEN	'Bus'
WHEN	dynamic_exposure_stops_no_monthly.mode IN (17,18)				THEN	'Underground'
WHEN	dynamic_exposure_stops_no_monthly.mode = 19					THEN 	'Train'
ELSE	'Indoor'
END AS	mode,
dynamic_exposure_stops_no_monthly.microenv_pm25,
st_x(hybrid_location.the_geom),
st_y(hybrid_location.the_geom)
FROM		dynamic_exposure_stops_no_monthly
LEFT JOIN	hybrid_location
ON		dynamic_exposure_stops_no_monthly.ppid = hybrid_location.ppid
AND		dynamic_exposure_stops_no_monthly.point_time = hybrid_location.point_time
WHERE		dynamic_exposure_stops_no_monthly.ppid IN (70737511101, 60601534101)
ORDER BY	dynamic_exposure_stops_no_monthly.point_time"

results <- dbGetQuery(con, query)

pdf("recalculating_lhem_pre.pdf", width=11.67*1.1, height=8.27*1.1)
p <- ggplot(results, aes(point_time, microenv_pm25, colour = mode)) + 
  geom_line(aes(group=1), size = 1) +
  facet_grid(. ~ ppid, scales = "free_x") +
  theme(panel.grid.major = element_line(colour="grey"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        axis.text=element_text(size=16, color="black", angle = 20, hjust = 1),
        axis.title=element_text(size=24, color="black"),
        plot.title=element_text(size=28, colour="black"),
        legend.title=element_blank(),
        legend.text=element_text(size=18, colour="black"),
        legend.key = element_blank(),
        legend.background=element_rect(colour="black"),
        legend.position = c(0.98, 0.98),
        legend.justification = c(1,1),
        strip.text.x = element_text(size = 24)) +
  xlab("Time") +
  ylab(expression(paste("Exposure PM"[2.5], " (", mu, "g m" ^ "-3", ") ")))
print(p)
dev.off()

###########################
############ Now get the tube data that going to adjust the above with
##########################
library("openair")

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
                        SELECT  	station_depths_import.station_name,
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

mean_per_line <- aggregate(corrected_concentration ~ line, tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',],  mean)

results[results$ppid == 60601534101 & results$mode == 'Underground',"microenv_pm25"] <- mean_per_line[mean_per_line$line == 'Piccadilly',]$corrected_concentration

results[results$ppid == 70737511101 & results$mode == 'Underground',][1:26,"microenv_pm25"] <- mean_per_line[mean_per_line$line == 'Metropolitan',]$corrected_concentration
results[results$ppid == 70737511101 & results$mode == 'Underground',][27:82,"microenv_pm25"] <- mean_per_line[mean_per_line$line == 'Jubilee',]$corrected_concentration

pdf("recalculating_lhem_post.pdf", width=11.67*1.1, height=8.27*1.1)
p <- ggplot(results, aes(point_time, microenv_pm25, colour = mode)) + 
  geom_line(aes(group=1), size = 1) +
  facet_grid(. ~ ppid, scales = "free_x") +
  theme(panel.grid.major = element_line(colour="grey"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        axis.text=element_text(size=16, color="black", angle = 20, hjust = 1),
        axis.title=element_text(size=24, color="black"),
        plot.title=element_text(size=28, colour="black"),
        legend.title=element_blank(),
        legend.text=element_text(size=18, colour="black"),
        legend.key = element_blank(),
        legend.background=element_rect(colour="black"),
        legend.position = c(0.98, 0.98),
        legend.justification = c(1,1),
        strip.text.x = element_text(size = 24)) +
  xlab("Time") +
  ylab(expression(paste("Exposure PM"[2.5], " (", mu, "g m" ^ "-3", ") ")))
print(p)
dev.off()


