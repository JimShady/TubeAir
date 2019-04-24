rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("grid")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="localhost")

tube_data <- dbGetQuery(con, "
SELECT  row_number() OVER (PARTITION BY tube_diary_line) as minutes,
depth,
tube_diary_line,
tube_diary_stop,
tube_air_pm25,
CASE 	WHEN	tube_diary_platform_or_train = 'sidings' THEN 'line'
ELSE	tube_diary_platform_or_train
END AS	tube_diary_platform_or_train
FROM	tube_pollution_mapping
  ")

tube_data$tube_diary_line <- factor(tube_data$tube_diary_line, levels = c("Victoria",
                                                                                "Piccadilly",
                                                                                "Northern",
                                                                                "Circle",
                                                                                "Jubilee",
                                                                                "District",
                                                                                "Bakerloo",
                                                                                "Metropolitan",
                                                                                "Docklands Light Railway",
                                                                                "Central",
                                                                                "Hammersmith & City"))

# Makes one plot of all of the lines.
# Note can/could replace stat_smooth with geom_line()
ggplot(tube_data, aes(x=minutes, y=tube_air_pm25, group=tube_diary_line, color=tube_diary_line)) +
  stat_smooth() +
  scale_color_manual(values=c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333", "#CC9999"))

setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

# What does a plot of say just the Northern Line look like
pdf("timeline_northern_central.pdf", width=11.67, height=8.27)
ggplot(tube_data, aes(x=minutes, y=tube_air_pm25)) +
  geom_line(data=tube_data[tube_data$tube_diary_line == 'Northern',], color="#000000") +
  geom_point(data=tube_data[tube_data$tube_diary_line == 'Northern' & tube_data$tube_diary_platform_or_train == 'station',], color="#000000") +
geom_line(data=tube_data[tube_data$tube_diary_line == 'Central',], color="#CC3333") +
  geom_point(data=tube_data[tube_data$tube_diary_line == 'Central' & tube_data$tube_diary_platform_or_train == 'station',], color="#CC3333")
dev.off()

# Northern line only + depth?

# Want to categorise the depths so that can use to colour stuff

cut(tube_data[tube_data$tube_diary_line == 'Northern' & !is.na(tube_data$depth),]$depth,5)

pdf("timeline_northern_depth.pdf", width=11.67, height=8.27)
ggplot() +
  annotate("rect", xmin = tube_data[tube_data$tube_diary_line == 'Northern' & !is.na(tube_data$depth),]$minutes, xmax =tube_data[tube_data$tube_diary_line == 'Northern' & !is.na(tube_data$depth),]$minutes + 1, ymin = -Inf, ymax = Inf,
           alpha = 0.1, colour = "red") +
  geom_line(data=tube_data[tube_data$tube_diary_line == 'Northern',], aes(x=minutes, y=tube_air_pm25), color="#000000") +
  geom_line(data=tube_data[tube_data$tube_diary_line == 'Northern' & tube_data$tube_diary_platform_or_train == 'station',], aes(x=minutes, y=-depth), color="#000000")

  
geom_rect(aes(xmin = 10, xmax = 50, ymin = -Inf, ymax = Inf))



dev.off()

head(tube_data)
