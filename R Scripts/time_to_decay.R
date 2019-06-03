rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("grid")
library("lattice")
library("gridExtra")
library(openair)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_air", user="james", password="brianclough", host="10.0.4.43")

## Get daily background averages for London from OpenAir
background_pm25 <- importKCL(site = "kc1", year = c(2014,2015,2016), pollutant = "pm25", met = FALSE,
                             units = "mass", extra = FALSE)
background_pm25 <- data.frame(background_pm25, day = as.Date(format(background_pm25$date)))
background_pm25 <- aggregate(pm25 ~ day, background_pm25, mean)

tube_data <- dbGetQuery(con, "
                        SELECT    date_time,
                        row_number() OVER (PARTITION BY line ORDER BY date_time) as minutes,
                        line,
                        tube_diary_stop,
                        concentration,
                        species
                        FROM		tube_pollution_mapping
                        WHERE		species 		= 	'PM25'
                        AND		environment		=	'CAR'
                        ")

## Sort out linking the daily means to the tube concentrations
tube_data <- data.frame(tube_data, day = as.Date(format(tube_data$date_time)))
tube_data <- merge(tube_data, background_pm25, by="day", all.x=TRUE)

# Find that concentration for 1 Feb 2016 is missing from the London Air background PM2.5 data. Need some numbers for that.
# So going to use the data from 8 Feb 2016 instead. It's the same day of the week, the week after.
tube_data[is.na(tube_data$pm25),]$pm25 <- background_pm25[background_pm25$day == '2016-02-08',]$pm25

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

colours_lines <- data.frame(line = c("Victoria","Piccadilly","Northern","Circle","Jubilee","District","Bakerloo","Metropolitan","Docklands Light Railway","Central","Hammersmith & City"),
                            colour = c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333", "#CC9999"),
                            stringsAsFactors = FALSE)

tube_data$line <- factor(tube_data$line, levels = c("Victoria","Piccadilly","Northern","Circle","Jubilee","District","Bakerloo","Metropolitan","Docklands Light Railway","Central","Hammersmith & City"))

setwd("C:/Users/stwb3498/Documents/Github/TubeAir/Results")

##########
### Need to make a plot of just the lines I want
line    <- colours_lines$line[5]
colour  <- colours_lines$colour[5]

plot <- ggplot(tube_data[tube_data$line == line,], aes(x=minutes, y=corrected_concentration)) +
  geom_line(colour = "black") +
  geom_point(data = tube_data[tube_data$line == line & !is.na(tube_data$tube_diary_stop),],
             aes(x=minutes, y=corrected_concentration), size = 1) +
  geom_text(data = tube_data[tube_data$line == line & !is.na(tube_data$tube_diary_stop) & (
                               tube_data$minutes > 67 & tube_data$minutes < 84 |
                                 tube_data$minutes > 121 & tube_data$minutes < 131 |
                                 tube_data$minutes > 185 & tube_data$minutes < 198 |
                                 tube_data$minutes > 236 & tube_data$minutes < 245),],
            aes(x=minutes, y=corrected_concentration, label = tube_diary_stop),
            colour = "black", angle = 30, hjust = 0, vjust = 0, size = 4, nudge_x = 2,
            check_overlap = TRUE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        axis.text=element_text(size=14, color="black"),
        axis.title=element_text(size=14, color="black"),
        plot.title=element_text(size=14, colour="black"),
        ) +
  #scale_x_continuous(breaks = seq(0,500, 100), limits = c(0,500)) +
  labs(title="",
       x="Minutes", 
       y=expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))
  ) +
  annotate("segment", x = min(tube_data[tube_data$line == line,]$minutes),
           xend = max(tube_data[tube_data$line == line,]$minutes),
           y = unique(tube_data[tube_data$line == line,]$pm25),
           yend = unique(tube_data[tube_data$line == line,]$pm25), colour = "red") +
  annotate("rect", xmin = 69, xmax = 81, ymin = 0, ymax = 430, colour = "black", fill = NA) +
  annotate("rect", xmin = 122, xmax = 129, ymin = 0, ymax = 430, colour = "black", fill = NA) +
  annotate("rect", xmin = 186, xmax = 197, ymin = 0, ymax = 430, colour = "black", fill = NA) +
  annotate("rect", xmin = 235, xmax = 246, ymin = 0, ymax = 430, colour = "black", fill = NA)

ggsave("decay_time_jubilee.png", plot = plot)
