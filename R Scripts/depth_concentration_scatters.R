rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("grid")
library("devtools")
library("openair")
library("gdata")
library("RColorBrewer")
library(tidyverse)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_air", user="james", password="brianclough", host="10.0.4.43")

tube_colours <- c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333", "#CC9999")
lines_and_colours <- data.frame(line = c("Victoria", "Piccadilly", "Northern", "Circle", "Jubilee", "District", "Bakerloo", "Metropolitan", "Docklands Light Railway", "Central", "Hammersmith & City"), colour = tube_colours)
rm(tube_colours)

## Get daily background averages for London from OpenAir
background_pm25 <- importKCL(site = "kc1", year = c(2014,2015,2016), pollutant = "pm25", met = FALSE,
                             units = "mass", extra = FALSE)
background_pm25 <- data.frame(background_pm25, day = as.Date(format(background_pm25$date)))
background_pm25 <- aggregate(pm25 ~ day, background_pm25, mean)

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

tube_data <- data.frame(tube_data, day = as.Date(format(tube_data$date_time)))
tube_data <- merge(tube_data, background_pm25, by="day", all.x=TRUE)

# Find that concentration for 1 Feb 2016 is missing from the London Air background PM2.5 data. Need some numbers for that.
# So going to use the data from 8 Feb 2016 instead. It's the same day of the week, the week after.
tube_data[is.na(tube_data$pm25),]$pm25 <- background_pm25[background_pm25$day == '2016-02-08',]$pm25

# Now do the correction process
tube_data$corrected_concentration <- as.numeric('')

## Now adjust the data
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

tube_data$line <- factor(tube_data$line, levels = c("Victoria", "Piccadilly", "Northern", "Circle", "Jubilee", "District", "Bakerloo", "Metropolitan", "Docklands Light Railway", "Central", "Hammersmith & City"))

tube_data <- tube_data[order(tube_data$date_time),]
#reorganising <- tube_data[,c("species","environment", "date_time", "corrected_concentration", "tube_diary_stop", "line")]

#just_depth   <- tube_data[,c("environment", "date_time", "corrected_concentration", "tube_diary_stop", "line", "platform_depth")]

#just_depth$species <- 'depth'

#just_depth <- just_depth[,c("species", "environment", "date_time", "corrected_concentration", "tube_diary_stop", "line", "platform_depth")]
#just_depth$corrected_concentration <- just_depth$platform_depth 
#just_depth <- just_depth[,c("species", "environment", "date_time", "corrected_concentration", "tube_diary_stop", "line")]

#tube_data <- rbind(just_depth, reorganising)
#names(tube_data)[names(tube_data)=='corrected_concentration'] <- 'value'

#rm(just_depth, reorganising, background_pm25)

#south_ken_pm25 <- importKCL(site = "kc1", year = as.numeric(getYear(min(tube_data$date_time))):as.numeric(getYear(max(tube_data$date_time)))  , pollutant = "pm25")

setwd("Z:/James/PhD/10 - Tube Monitoring Chapter/Results")



##########################################################################################
##########################################################################################
# Making new graphs
## Want to make a graph which shows average platform depth per line, against average PM2.5 per line.
## So need to get those averages and combine them

station_depths <- aggregate(platform_depth ~ line,
                            data = unique(tube_data[!is.na(tube_data$tube_diary_stop), c("tube_diary_stop", "line", "platform_depth")]),
                            mean)

station_concs <- aggregate(corrected_concentration ~ line+species, data = tube_data[tube_data$environment == 'CAR',], mean)

line_depth_summary <- cbind(station_depths, station_concs)

names(line_depth_summary) <- c("line", "depth", "line", "species", "concentration")

line_depth_summary <- line_depth_summary[,-1]

line_depth_summary <- line_depth_summary[line_depth_summary$line != 'Docklands Light Railway',]

rm(station_depths, station_concs)

# Make a summary per line

pdf("depth_pm25_summary.pdf", width=11.67, height=8.27)
ggplot(line_depth_summary[line_depth_summary$species == 'PM25',], aes(depth, concentration)) +
  geom_point(aes(colour=factor(line),
                 fill = factor(line)),
             size = 12) +
  scale_fill_manual(values = c("#0099CC", "#000099", "#000000", "#FFCC00", "#868F98", "#006633", "#996633", "#660066", "#CC3333", "#CC9999")) +
  scale_colour_manual(values = c("#0099CC", "#000099", "#000000", "#FFCC00","#868F98", "#006633", "#996633", "#660066", "#CC3333", "#CC9999")) +
  theme(panel.grid.major = element_line(colour="grey"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        axis.text=element_text(size=14, color="black"),
        axis.title=element_text(size=14, color="black"),
        plot.title=element_text(size=12, colour="black"),
        legend.title=element_blank(),
        legend.text=element_text(size=10, colour="black"),
        legend.position = c(0.1, 0.68),
        legend.key = element_blank(),
        legend.background=element_rect(colour="black")) +
  xlab("Average station platform depth (metres)") +
  ylab(expression(paste("PM"[2.5], " corrected concentration (", mu, "g m" ^ "-3", ") ")))
dev.off()

pdf("depth_pcnt_summary.pdf", width=11.67, height=8.27)
ggplot(line_depth_summary[line_depth_summary$species == 'PCNT',], aes(depth, concentration)) +
  geom_point(aes(colour=factor(line),
                 fill = factor(line)),
             size = 12) +
  scale_fill_manual(values = c("#0099CC", "#000099", "#000000", "#FFCC00", "#868F98", "#006633", "#996633", "#660066", "#CC3333", "#CC9999")) +
  scale_colour_manual(values = c("#0099CC", "#000099", "#000000", "#FFCC00","#868F98", "#006633", "#996633", "#660066", "#CC3333", "#CC9999")) +
  theme(panel.grid.major = element_line(colour="grey"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        axis.text=element_text(size=14, color="black"),
        axis.title=element_text(size=14, color="black"),
        plot.title=element_text(size=12, colour="black"),
        legend.title=element_blank(),
        legend.text=element_text(size=10, colour="black"),
        legend.position = c(0.1, 0.68),
        legend.key = element_blank(),
        legend.background=element_rect(colour="black")) +
  xlab("Average station platform depth (metres)") +
  ylab("Particle numbers (000s)")
dev.off()

rm(line_depth_summary)

##########################################################################################
##########################################################################################
## Now a plot of each station depth v each station average PM2.5.
## Kind of facet by line though.

station_depths <- aggregate(platform_depth ~ line + tube_diary_stop,
                            data = unique(tube_data[!is.na(tube_data$tube_diary_stop), c("tube_diary_stop", "line", "platform_depth")]),
                            mean)
station_concs <- aggregate(corrected_concentration ~ line + tube_diary_stop,
                           data = tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',],
                           mean)

station_locations <- dbGetQuery(con, 'Select name,
                                line,
                                st_x(st_transform(the_geom, 4326)) as longitude,
                                st_y(st_transform(the_geom, 4326)) as latitude
                                From underground_stations
                                group by name, line, longitude, latitude')
  
##########
station_concs %>% 
  mutate(pm25 = round(corrected_concentration,0)) %>% 
  rename(station = tube_diary_stop) %>% 
  select(-corrected_concentration) %>% 
  arrange(line, station) %>% 
  left_join(station_locations, by = c('line' = 'line', 'station' = 'name')) %>%
  write_csv('pm25_per_station_line.csv')
##########

line_station_summary <- cbind(station_depths, station_concs)

line_station_summary <- line_station_summary[,c(1,2,3,6)]

names(line_station_summary) <- c("line", "station", "depth", "pm25")

plot_list <- list()

for (i in 1:nrow(lines_and_colours)) {
  line <- lines_and_colours[i,]$line
  colour <- lines_and_colours[i,]$colour

plot_list[[i]] <- ggplot(line_station_summary[line_station_summary$line == line,], aes(depth, pm25)) +
  geom_point(colour = colour, size = 4) +
  #geom_smooth(method=lm) +
  xlab("Platform depth (metres)") +
  ylab(expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))) +
  ggtitle(line) +
theme(panel.grid.major = element_line(colour="grey"), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      axis.line = element_line(colour="black"),
      axis.text=element_text(size=10, color="black"),
      axis.title=element_text(size=10, color="black"),
      plot.title=element_text(size=18, colour="black"),
      legend.title=element_blank(),
      legend.text=element_text(size=10, colour="black"),
      legend.key = element_blank(),
      legend.background=element_rect(colour="black"),
      plot.margin = unit(c(0.2,0.6,0.2,0.2), "cm"))
}

pdf("depth_plot_per_line_pm25.pdf", width=11.67*1.1, height=8.27*1.1)
pushViewport(viewport(layout = grid.layout(4, 3)))
k <- 1
for (j in 1:4){
for (i in 1:3){
print(paste(k, " ", j, " ", i))
print(plot_list[[k]], vp = viewport(layout.pos.row = j, layout.pos.col = i))
k <- k+1
  }
}
dev.off()

######### We also need some individual ones. So now just District line.
pdf("depth_plot_district_line.pdf", width=11.67*1.1, height=8.27*1.1)
p <- ggplot(line_station_summary[line_station_summary$line == lines_and_colours[6,1],], aes(depth, pm25)) +
  geom_point(colour = lines_and_colours[6,2], size = 4) +
  #geom_smooth(method=lm) +
  xlab("Platform depth (metres)") +
  ylab(expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))) +
  ggtitle(lines_and_colours[6,1]) +
  theme(panel.grid.major = element_line(colour="grey"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        axis.text=element_text(size=24, color="black"),
        axis.title=element_text(size=24, color="black"),
        plot.title=element_text(size=28, colour="black"),
        legend.title=element_blank(),
        legend.text=element_text(size=18, colour="black"),
        legend.key = element_blank(),
        legend.background=element_rect(colour="black"),
        plot.margin = unit(c(0.2,0.6,0.2,0.2), "cm")) +
  annotate("rect", xmin = 0, xmax = 10, ymin = 105, ymax = 195,
           alpha = .8, colour="black", size=1, fill=NA) +
  annotate("rect", xmin = 0, xmax = 10, ymin = 3, ymax = 90,
           alpha = .8, colour="blue", size=1, fill=NA)
print(p)
dev.off()

######### We also need some individual ones. So now just Central line.
pdf("depth_plot_central_line.pdf", width=11.67*1.1, height=8.27*1.1)
p <- ggplot(line_station_summary[line_station_summary$line == lines_and_colours[10,]$line,], aes(depth, pm25, label = station)) +
  geom_point(colour = lines_and_colours[10,]$colour, size = 4) +
  geom_text(data = line_station_summary[line_station_summary$line == lines_and_colours[10,]$line &
                                          line_station_summary$depth > 10 &
                                          line_station_summary$pm25 < 100 ,], aes(depth, pm25, label = station),
              hjust = 0, vjust = 0, size = 5) +
  #geom_smooth(method=lm) +
  xlab("Platform depth (metres)") +
  ylab(expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))) +
  ggtitle(lines_and_colours[10,]$line) +
  theme(panel.grid.major = element_line(colour="grey"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        axis.text=element_text(size=24, color="black"),
        axis.title=element_text(size=24, color="black"),
        plot.title=element_text(size=28, colour="black"),
        legend.title=element_blank(),
        legend.text=element_text(size=18, colour="black"),
        legend.key = element_blank(),
        legend.background=element_rect(colour="black"),
        plot.margin = unit(c(0.2,0.6,0.2,0.2), "cm")) +
  annotate("rect", xmin = 10, xmax = 29, ymin = 300, ymax = 500,
           alpha = .8, colour="black", size=1, fill=NA) +
  annotate("rect", xmin = 10, xmax = 29, ymin = 0, ymax = 150,
           alpha = .8, colour="blue", size=1, fill=NA)
print(p)
dev.off()

##########################################################################################
##########################################################################################

##########################################################################################
## For the paper Ian would like to see PM2.5 v. PCNT, but maybe related to depth. So...

station_depths <- aggregate(platform_depth ~ line + tube_diary_stop,
                            data = unique(tube_data[!is.na(tube_data$tube_diary_stop), c("tube_diary_stop", "line", "platform_depth")]),
                            mean)
station_pcnt_concs <- aggregate(corrected_concentration ~ line + tube_diary_stop,
                           data = tube_data[tube_data$species == 'PCNT' & tube_data$environment == 'CAR',],
                           mean)

station_pm25_concs <- aggregate(corrected_concentration ~ line + tube_diary_stop,
                                data = tube_data[tube_data$species == 'PM25' & tube_data$environment == 'CAR',],
                                mean)

stations_pcnt_pm25 <- merge(station_pm25_concs, station_pcnt_concs, by.x = c('line', 'tube_diary_stop'), by.y = c('line', 'tube_diary_stop'), all.x=T)

stations_pcnt_pm25_depth <- merge(stations_pcnt_pm25, station_depths, by.x = c('line', 'tube_diary_stop'), by.y = c('line', 'tube_diary_stop'), all.x=T)

names(stations_pcnt_pm25_depth) <- c("line", "station", "pm25", "particle_number", "depth")

stations_pcnt_pm25_depth$depth_categorised <- NA
stations_pcnt_pm25_depth[stations_pcnt_pm25_depth$depth <= 0,]$depth_categorised  <- 'above ground (>0m)' 
stations_pcnt_pm25_depth[stations_pcnt_pm25_depth$depth < 10 & stations_pcnt_pm25_depth$depth > 0,]$depth_categorised  <- 'shallow (0-10m)' 
stations_pcnt_pm25_depth[stations_pcnt_pm25_depth$depth < 20 & stations_pcnt_pm25_depth$depth > 10,]$depth_categorised  <- 'medium (10-20m)' 
stations_pcnt_pm25_depth[stations_pcnt_pm25_depth$depth > 20,]$depth_categorised  <- 'deep (>20m)' 

ggplot(stations_pcnt_pm25_depth, aes(pm25, particle_number, colour = depth_categorised, label = station)) + 
  geom_point(size=2)


