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
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="localhost")

## Get daily background averages for London from OpenAir
background_pm25 <- importKCL(site = "kc1", year = c(2014,2015,2016), pollutant = "pm25", met = FALSE,
                             units = "mass", extra = FALSE)
background_pm25 <- data.frame(background_pm25, day = as.Date(format(background_pm25$date)))
background_pm25 <- aggregate(pm25 ~ day, background_pm25, mean)

tube_data <- dbGetQuery(con, "
SELECT  	date_time,
		row_number() OVER (PARTITION BY line ORDER BY date_time) as minutes,
		line,
		tube_diary_stop,
		concentration
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

tube_data$corrected_pm25 <- as.numeric('')

## Now adjust the data
for (i in 1:nrow(tube_data)) {
  if (tube_data[i,]$concentration > tube_data[i,]$pm25) {
    tube_data[i,]$corrected_pm25 <- (tube_data[i,]$pm25 * 0.6) + ((tube_data[i,]$concentration - tube_data[i,]$pm25) * 2)
  } else
  {
    tube_data[i,]$corrected_pm25 <-tube_data[i,]$concentration * 0.6
  }
}

colours_lines <- data.frame(line = c("Victoria","Piccadilly","Northern","Circle","Jubilee","District","Bakerloo","Metropolitan","Docklands Light Railway","Central","Hammersmith & City"),
                            colour = c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333", "#CC9999"),
                            stringsAsFactors = FALSE)

tube_data$line <- factor(tube_data$line, levels = c("Victoria","Piccadilly","Northern","Circle","Jubilee","District","Bakerloo","Metropolitan","Docklands Light Railway","Central","Hammersmith & City"))

setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

# Makes one plot of all of the lines.
# Note can/could replace stat_smooth with geom_line()
pdf("all_lines_pm25.pdf", width=11.67, height=8.27)
ggplot(tube_data, aes(x=minutes, y=corrected_pm25, group=line, color=line)) +
  geom_line() +
  scale_color_manual(values=c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333", "#CC9999")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        axis.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, color="black"),
        plot.title=element_text(size=12, colour="black"),
        legend.text=element_text(size=12, colour = "black"),
        legend.title=element_blank(),
        legend.justification=c(1,1),
        legend.position=c(1,1)) +
  labs(title="",
       x="Minute of journey on line", 
       y=expression(paste("Corrected PM"[2.5], " (", mu, "g m" ^ "-3", ") ")))
dev.off()

# Makes plot for each line
# Note can/could replace stat_smooth with geom_line()

plot_list <- list()

for (i in 1:nrow(colours_lines)) {

  line    <- colours_lines$line[i]
  colour  <- colours_lines$colour[i]
  plot <-  ggplot(tube_data[tube_data$line == line,], aes(x=minutes, y=corrected_pm25)) +
              geom_line(colour = "white") +
              theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour="black"),
              axis.text=element_text(size=10, color="black"),
              axis.title=element_text(size=10, color="black"),
              plot.title=element_text(size=10, colour="black"),
              panel.background = element_rect(fill = colour)) +
          #scale_x_continuous(breaks = seq(0,500, 100), limits = c(0,500)) +
  labs(title="",
       x="Minutes", 
       y=expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))
       ) +
  annotate("text", colour = "white", x = max(tube_data[tube_data$line == line,]$minutes), y = max(tube_data[tube_data$line == line,]$corrected_pm25)*0.9, label = line, size = 10, hjust =1)
  plot_list[[i]] <- plot
  rm(line, colour, plot)
  }

pdf("all_lines_individual_pm25_p1.pdf", width=8.27, height=11.67)
grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], plot_list[[6]], ncol = 1, nrow = 6)
dev.off()

pdf("all_lines_individual_pm25_p2.pdf", width=8.27, height=11.67)
grid.arrange(plot_list[[7]], plot_list[[8]], plot_list[[9]], plot_list[[10]], plot_list[[11]], ncol = 1, nrow = 6)
dev.off()