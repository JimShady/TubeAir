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
con = dbConnect(drv, dbname="tube_air", user="james", password="brianclough", host="10.0.4.43")

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
                        station_info.name,
                        station_info.the_geom
                        FROM		station_depths_import
                        LEFT JOIN	(	SELECT		name,
                        line,
                        the_geom
                        FROM		station_geom_depth
                        GROUP BY 	name,
                        line,
                        the_geom) AS station_info
                        ON		station_depths_import.station_name 	= station_info.name
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

# Import tube data from geography and harmonise it with the ERG data
# Note in code below I just put any old date in. It'll need replacing with proper one.
#geog_data <- read.csv("~/mounts/James/PhD/10 - Tube Monitoring Chapter/LU AQ Data/processed_data_from_bryn.csv", stringsAsFactors=FALSE)
#geog_data <- geog_data[,c('start_station','end_station','line','pm25')]
#geog_data <- geog_data[geog_data$start_station != 'Ignore',]
#geog_data <- data.frame(species = 'PM25', environment = 'CAR', date_time = '2014-12-08 09:03:00', 
#                        concentration = geog_data$pm25, tube_diary_stop = NA,
#                        line = geog_data$line, platform_depth = NA)
#geog_data$concentration <- geog_data$concentration * 100

## NEED TO FIGURE OUT CORRECTION FACTORS FOR THE GEOGRAPHY DATA HERE

#tube_data <- rbind(tube_data, geog_data)

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


line_names_depth <- dbGetQuery(con, "SELECT line, avg(platform_depth) as depth FROM station_depths_import group by line")
line_names_depth$depth <- abs(round(line_names_depth$depth,0))
line_names_depth$depth_name <- NA
line_names_depth[line_names_depth$line != 'Docklands Light Railway',]$depth_name <- paste(line_names_depth[line_names_depth$line != 'Docklands Light Railway',]$line, " (-", line_names_depth[line_names_depth$line != 'Docklands Light Railway',]$depth, "m)", sep="")
line_names_depth[line_names_depth$line == 'Docklands Light Railway',]$depth_name <- paste(line_names_depth[line_names_depth$line == 'Docklands Light Railway',]$line, " (+", line_names_depth[line_names_depth$line == 'Docklands Light Railway',]$depth, "m)", sep="")

tube_data <- merge(tube_data, line_names_depth, by.x = 'line', by.y = 'line')

tube_data$line <- tube_data$depth_name

tube_data$depth_name <- NULL

ordered_by_median <- as.character(aggregate(corrected_concentration ~ line, data = tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',], median)[order(-aggregate(corrected_concentration ~ line, data = tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',], median)$corrected_concentration),]$line)

tube_data$line <- factor(tube_data$line, levels = ordered_by_median)


## Set a working directory to output the graphs too.
setwd("../Results")

png("pm25_on_underground.png", width =1000, height = 800, units="px")
ggplot(tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',], aes(line, corrected_concentration, fill=interaction(line))) +
  stat_boxplot(geom='errorbar') +
  geom_boxplot() +
      stat_summary(fun.y=mean, colour="white", geom="point", shape=20, size=3) +
  theme_bw() +
  theme(axis.line = element_line(colour="black"),
        axis.text=element_text(size=16, color="black"),
        axis.title=element_text(size=16, color="black"),
        axis.text.x=element_text(angle = 40, hjust = 1),
        plot.title=element_text(size=16, colour="black"),
        legend.position="none") +
  labs(title="",
       x=expression(paste("Tube Line (mean depth)")), 
       y=expression(paste("PM"[2.5], "(", mu, "g m" ^ "-3", ") "))
       ) +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=tube_colours)
dev.off()

aggregate(tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',]$corrected_concentration, by=list(tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',]$line), mean)

## Make the density plot for Northern and Victoria line

png("pm25_density_plots.png", width = 500, height = 300, units="px")
ggplot(filter(tube_data, line %in% c('Victoria (-20m)', 'Northern (-17m)') & species == 'PM25'),
       aes(corrected_concentration, colour=line)) + 
  geom_density(size = 1) +
  scale_color_discrete(name = 'Line') +
  theme_bw() +
  theme(axis.line = element_line(colour="black"),
        axis.text=element_text(size=16, color="black"),
        axis.title=element_text(size=16, color="black"),
        axis.text.x=element_text(angle = 40, hjust = 1),
        plot.title=element_text(size=16, colour="black"),
        legend.position=c(0.85, 0.80),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  labs(title="",
       y="Density", 
       x=expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))
  )
dev.off()