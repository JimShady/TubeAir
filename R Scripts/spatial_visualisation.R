rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("grid")
library("devtools")
library("openair")
library("openair")
library("gdata")
library("maptools")
library(rgdal)   # input/output, projections
library(sp)      # vector data
library(rgeos)


setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="10.0.4.240")

## Get a London shapefile
ukgrid     <- "+init=epsg:27700"

boundary   <- readShapeSpatial("/home/james/mounts/James/Mini\ Projects/mobile_phone_study/outputs/boundaries")
proj4string(boundary) = CRS(ukgrid)
london <- gUnaryUnion(boundary)
proj4string(london) = CRS(ukgrid)
rm(boundary)
london <- fortify(london)

## Get daily background averages for London from OpenAir
background_pm25 <- importKCL(site = "kc1", year = c(2014,2015,2016), pollutant = "pm25", met = FALSE,
                             units = "mass", extra = FALSE)
background_pm25 <- data.frame(background_pm25, day = as.Date(format(background_pm25$date)))
background_pm25 <- aggregate(pm25 ~ day, background_pm25, mean)

## Note the CASE statement below. it's because want the raw PM25 data to do our own scaling, but the scaled version of other pollutants.
tube_data <- dbGetQuery(con, "
                        WITH  station_depths AS (
                        SELECT    station_depths_import.station_name,
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
                        WHERE tube_pollution_mapping.environment = 'CAR'
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

colours_lines <- data.frame(line = c("Victoria","Piccadilly","Northern","Circle","Jubilee","District","Bakerloo","Metropolitan","Docklands Light Railway","Central","Hammersmith & City"),
                            colour = c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333", "#CC9999"),
                            stringsAsFactors = FALSE)

rm(background_pm25, con, drv, i)

map_plot <- aggregate(corrected_concentration ~ tube_diary_stop + line + platform_depth, tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',], mean)
names(map_plot)[names(map_plot) == 'tube_diary_stop'] <- 'station'
names(map_plot)[names(map_plot) == 'platform_depth'] <- 'depth'
names(map_plot)[names(map_plot) == 'corrected_concentration'] <- 'pm25'

map_plot$depth_categorised <- NA
map_plot[map_plot$depth < 0,]$depth_categorised  <- 'above ground (>0m)' 
map_plot[map_plot$depth < 10 & map_plot$depth > 0,]$depth_categorised  <- 'shallow (0-10m)' 
map_plot[map_plot$depth < 20 & map_plot$depth > 10,]$depth_categorised  <- 'medium (10-20m)' 
map_plot[map_plot$depth > 20,]$depth_categorised  <- 'deep (>20m)' 

map_plot$depth_categorised <- as.factor(map_plot$depth_categorised)

map_plot$depth_categorised <- factor(map_plot$depth_categorised, levels = c("above ground (>0m)", "shallow (0-10m)",
                                                                            "medium (10-20m)", "deep (>20m)"))

map_plot$line <- as.factor(map_plot$line)

map_plot$line <- factor(map_plot$line, levels = c("Victoria","Piccadilly","Northern","Circle","Jubilee",
                                                  "District","Bakerloo","Metropolitan","Docklands Light Railway","Central","Hammersmith & City"))

# need to add easiny and nothyings.

tube_information <- read.csv("~/mounts/James/PhD/10 - Tube Monitoring Chapter/Line Location Data/tube_information.csv")

map_plot <- merge(map_plot, tube_information, by.x = c("station", "line"), by.y = c("name", "line"))

thames <-  readShapeSpatial("~/mounts/James/PhD/10 - Tube Monitoring Chapter/Thames/thames")
proj4string(thames) = CRS(ukgrid)
london <- gUnaryUnion(boundary)
proj4string(thames) = CRS(ukgrid)
rm(boundary)
thames <- fortify(thames)

plot <- ggplot() + 
      geom_polygon(data=london, aes(x = long, y = lat, group = group),fill="white",colour="black") +
      geom_polygon(data=thames, aes(x = long, y = lat, group = group),fill="blue",colour="blue") +
  geom_point(data = map_plot, aes(x = x, y = y, colour = line, size = pm25), alpha = 0.5) +
  scale_size_continuous(name = expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))) +
  scale_color_manual(values=colours_lines$colour, name = "London Underground line") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "white", fill = "white"),
        legend.position = c(0.10,0.35),
        legend.text.align = 0,
        legend.box.just = "left",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        legend.background=element_rect(colour = "black", fill=alpha('white', 0.4))) +
      guides(colour = guide_legend(override.aes = list(size=8, alpha = 1)))
      

ggsave(plot, file = "tube_map_pm25.pdf", width = 29.7, height = 21, units = "cm" )