rm(list=ls())

library("RPostgreSQL")
library("maps")
library("ggplot2")
library("scales")
library("reshape")
library("grid")
library("devtools")
library("openair")
library("gdata")
library(gridExtra)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_air", user="james", password="brianclough", host="10.0.4.43")

## THIS IS VERY DRAFT> MOST OF THE CODE IS FROM ANOTGHER SCRIPT AND NEEDS RE-WRITING
## Get daily background averages for London from OpenAir

 background_pm25 <- importKCL(site = "kc1", year = c(2014,2015,2016), pollutant = "pm25", met = FALSE,
                             units = "mass", extra = FALSE)
background_pm25 <- data.frame(background_pm25, day = as.Date(format(background_pm25$date)))
background_pm25 <- aggregate(pm25 ~ day, background_pm25, mean)

## Note the CASE statement below. it's because want the raw PM25 data to do our own scaling, but the scaled version of other pollutants.
tube_data <- dbGetQuery(con, "
                        SELECT  	date_time,
		                    tube_diary_stop,
		                    concentration,
                        station_name,
                       ((weekday_entry * weekday_entry_weight) + (saturday_entry * saturday_entry_weight) + (sunday_entry * sunday_entry_weight)) / 364 AS daily_passenger_entrances
                        FROM		tube_pollution_mapping
                        LEFT JOIN	passenger_numbers
                        ON		tube_diary_stop = station_name
                        WHERE		species = 'PM25'
                        AND		environment = 'CAR'
                        AND		tube_diary_stop IS NOT NULL
                        AND		line <> 'Docklands Light Railway'
                        AND		tube_diary_stop <> 'Hammersmith'
                        UNION ALL
                        SELECT		date_time,
                        tube_diary_stop,
                        concentration,
                        station_name,
                        saturday_entry
                        FROM		tube_pollution_mapping
                        LEFT JOIN	passenger_numbers
                        ON		tube_diary_stop = station_name
                        AND		line = line_if_present
                        WHERE		species = 'PM25'
                        AND		environment = 'CAR'
                        AND		tube_diary_stop IS NOT NULL
                        AND		line <> 'Docklands Light Railway'
                        AND		tube_diary_stop = 'Hammersmith'
                        ORDER BY	date_time
                        ")

## Merge the background PM2.5 data with the tube data
tube_data <- data.frame(tube_data, day = as.Date(format(tube_data$date_time)))
tube_data <- merge(tube_data, background_pm25, by="day", all.x=TRUE)

# Find that concentration for 1 Feb 2016 is missing from the London Air background PM2.5 data. Need some numbers for that.
# So going to use the data from 8 Feb 2016 instead. It's the same day of the week, the week after.
tube_data[is.na(tube_data$pm25),]$pm25 <- background_pm25[background_pm25$day == '2016-02-08',]$pm25

# Now do the correction process
tube_data$corrected_concentration <- as.numeric('')

## Now adjust the data
for (i in 1:nrow(tube_data)) {
  if (tube_data[i,]$concentration > tube_data[i,]$pm25/0.44) {
    tube_data[i,]$corrected_concentration <- (tube_data[i,]$pm25 + (tube_data[i,]$concentration - tube_data[i,]$pm25/0.44) * 1.82)
  } else
  {
    if (tube_data[i,]$concentration <= tube_data[i,]$pm25/0.44) {
      tube_data[i,]$corrected_concentration <-tube_data[i,]$concentration * 0.44
    } else
    {}
  }
}

tube_data$pm25                 <- NULL
tube_data$concentration        <- NULL
tube_data$day                  <- NULL
tube_data$date_time            <- NULL

tube_data <- aggregate(cbind(daily_passenger_entrances, corrected_concentration) ~ station_name, data = tube_data, FUN = mean)

tube_data$passenger_ratio     <- tube_data$daily_passenger_entrances / mean(tube_data$daily_passenger_entrances)
tube_data$concentration_ratio <- tube_data$corrected_concentration / mean(tube_data$corrected_concentration)
tube_data$weighted_rank       <- tube_data$passenger_ratio * tube_data$concentration_ratio      

## A graph ordered by the entrance numbers
tube_data$station_name <- as.factor(tube_data$station_name)

tube_data$station_name <- factor(tube_data$station_name, levels = tube_data[order(tube_data$daily_passenger_entrances),]$station_name)

plot1 <- ggplot(tube_data[order(-tube_data$daily_passenger_entrances),][1:30,], aes(station_name)) +
  geom_bar(aes(weight = daily_passenger_entrances/1000), fill = 'red') +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01), labels = comma) +
  ylab('Daily passenger entrances (000s)') +
  xlab('') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 10, hjust = 1, colour='black'),
        plot.margin = margin(0,0,0,-0.25, "cm"),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.2))

plot1a <- ggplot(tube_data[order(-tube_data$daily_passenger_entrances),], aes(station_name)) +
  geom_bar(aes(weight = daily_passenger_entrances/1000), fill = 'red') +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01), labels = comma) +
  ylab('Daily passenger entrances (000s)') +
  xlab('') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 10, hjust = 1, colour='black'),
        plot.margin = margin(0,0,0,-0.25, "cm"),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.2))

## Now a graph ordered by PM2.5
tube_data$station_name <- factor(tube_data$station_name, levels = tube_data[order(tube_data$corrected_concentration),]$station_name)

plot2 <- ggplot(tube_data[order(-tube_data$corrected_concentration),][1:30,], aes(station_name)) +
  geom_bar(aes(weight = corrected_concentration), fill = 'darkgreen') +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01)) +
  ylab(expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))) +
  xlab('') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 10, hjust = 1, colour='black'),
        plot.margin = margin(0,0,0,-0.25, "cm"),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.2))

plot2a <- ggplot(tube_data[order(-tube_data$corrected_concentration),], aes(station_name)) +
  geom_bar(aes(weight = corrected_concentration), fill = 'darkgreen') +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01)) +
  ylab(expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))) +
  xlab('') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 10, hjust = 1, colour='black'),
        plot.margin = margin(0,0,0,-0.25, "cm"),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.2))

## Now a graph ordered by the PM and passenger weighting
tube_data$station_name <- factor(tube_data$station_name, levels = tube_data[order(tube_data$weighted_rank),]$station_name)

plot3 <- ggplot(tube_data[order(-tube_data$weighted_rank),][1:30,], aes(station_name)) +
  geom_bar(aes(weight = weighted_rank), fill = 'blue') +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01)) +
  ylab(expression(paste("Population weighted ", PM[2.5], " ranking",sep=""))) +
  xlab('') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 10, hjust = 1, colour='black'),
        plot.margin = margin(0,0,0,-0.25, "cm"),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.2))

plot3a <- ggplot(tube_data[order(-tube_data$weighted_rank),], aes(station_name)) +
  geom_bar(aes(weight = weighted_rank), fill = 'blue') +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01)) +
  ylab(expression(paste("Passenger / ", PM[2.5], " ranking",sep=""))) +
  xlab('') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 10, hjust = 1, colour='black'),
        plot.margin = margin(0,0,0,-0.25, "cm"),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.2))

g <- arrangeGrob(plot1, plot2, plot3, ncol = 3,
                 left = "",
                 top = "",
                 right = "",
                 bottom = "")

g2 <- arrangeGrob(plot1a, plot2a, plot3a, ncol = 3,
                  left = "",
                  top = "",
                  right = "",
                  bottom = "")

ggsave(g, file = "../Results/tube_pm_passenger_weighting.pdf", width = 310, height = 210, units = "mm")
ggsave(g, file = "../Results/tube_pm_passenger_weighting.png", width = 310, height = 210, units = "mm")

ggsave(g2, file = "../Results/full_tube_pm_passenger_weighting.pdf", width = 310, height = 830, units = "mm")
ggsave(g2, file = "../Results/full_tube_pm_passenger_weighting.png", width = 310, height = 830, units = "mm")

write.csv(tube_data, 'website_ranking_data.csv')

