library("ggplot2")
library("scales")
library("plot3D")

load("~/mounts/James/PhD/10 - Tube Monitoring Chapter/Results/mapping_central_line.RData")
setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

map_plot$fake_x <- NA
map_plot$fake_y <- NA


map_plot[map_plot$line == 'Central' & map_plot$station == 'West Ruislip',9:10]            <- c(1,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Ruislip Gardens',9:10]         <- c(2,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'South Ruislip',9:10]           <- c(3,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Northolt',9:10]                <- c(4,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Greenford',9:10]               <- c(5,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Perivale',9:10]                <- c(6,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Hanger Lane',9:10]             <- c(7,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'North Acton',9]                <- 8
map_plot[map_plot$line == 'Central' & map_plot$station == 'North Acton',10]               <- 2
map_plot[map_plot$line == 'Central' & map_plot$station == 'East Acton',9:10]              <- c(9,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'White City',9:10]              <- c(10,2)
map_plot[map_plot$line == 'Central' & map_plot$station == "Shepherd's Bush",9:10]         <- c(11,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Holland Park',9:10]            <- c(12,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Notting Hill Gate',9:10]       <- c(13,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Queensway',9:10]               <- c(14,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Lancaster Gate',9:10]          <- c(15,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Marble Arch',9:10]             <- c(16,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Bond Street',9:10]             <- c(17,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Oxford Circus',9:10]           <- c(18,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Tottenham Court Road',9:10]    <- c(19,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Holborn',9:10]                 <- c(20,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Chancery Lane',9:10]           <- c(21,2)
map_plot[map_plot$line == 'Central' & map_plot$station == "St. Paul's", 9:10]             <- c(22,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Bank',9:10]                    <- c(23,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Liverpool Street',9:10]        <- c(24,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Bethnal Green',9:10]           <- c(25,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Mile End',9:10]                <- c(26,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Stratford',9:10]               <- c(27,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Leyton',9:10]                  <- c(28,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Leytonstone',9]                <- 29
map_plot[map_plot$line == 'Central' & map_plot$station == 'Leytonstone',10]               <- 2
map_plot[map_plot$line == 'Central' & map_plot$station == 'Wanstead',9:10]                <- c(30,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Redbridge',9:10]               <- c(31,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Gants Hill',9:10]              <- c(32,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Newbury Park',9:10]            <- c(33,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Barkingside',9:10]             <- c(34,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Fairlop',9:10]                 <- c(35,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Hainault',9:10]                <- c(36,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'West Acton',9:10]              <- c(6,1)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Ealing Broadway',9:10]         <- c(5,1)

map_plot$depth_categorised <- NA
map_plot[map_plot$depth < 0,]$depth_categorised  <- 'above ground (>0m)' 
map_plot[map_plot$depth < 10 & map_plot$depth > 0,]$depth_categorised  <- 'shallow (0-10m)' 
map_plot[map_plot$depth < 20 & map_plot$depth > 10,]$depth_categorised  <- 'medium (10-20m)' 
map_plot[map_plot$depth > 20,]$depth_categorised  <- 'deep (>20m)' 

map_plot$depth_categorised <- as.factor(map_plot$depth_categorised)

map_plot$depth_categorised <- factor(map_plot$depth_categorised, levels = c("above ground (>0m)", "shallow (0-10m)",
                                                                            "medium (10-20m)", "deep (>20m)"))

map_plot[map_plot$line == 'Central',"scaled_pm25"] <- rescale(map_plot[map_plot$line == 'Central',]$pm25, to=c(0,1))


scatter3D(map_plot$x, map_plot$y, map_plot$pm25, bty = "g",  type = "h", 
          ticktype = "detailed", pch = 19, cex = 0.5)
