library("ggplot2")
library("scales")

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


ggplot(map_plot[map_plot$line == 'Central',], aes(fake_x, fake_y, label = station, colour = depth_categorised)) +
      geom_segment(data = map_plot[map_plot$line == 'Central',], aes(x = fake_x, y = fake_y,
                                                                     xend = fake_x, yend = fake_y +scaled_pm25),
                   colour = "black", size = 3, lineend="round", alpha=0.4) +
      geom_line(data = data.frame(fake_x = c(1,36), fake_y = 2, station = NA, depth_categorised = NA),
                aes(x = fake_x, y = fake_y), size = 2, colour = "red") +
      geom_line(data = data.frame(fake_x = c(5,8), fake_y = 1, station = NA, depth_categorised = NA),
                aes(x = fake_x, y = fake_y), size = 2, colour = "red") +
      geom_line(data = data.frame(fake_x = 8, fake_y = c(1,2), station = NA, depth_categorised = NA),
                aes(x = fake_x, y = fake_y), size = 2, colour = "red") +
            geom_point(size = 5.5, colour = "black") +
      geom_point(size = 5) +
           scale_colour_manual(values = c("white", "pink", "red", "black"), guide = guide_legend(title = "Station depth")) +
      geom_text(angle = 55, hjust = 0, nudge_y = 0.05, size = 5, colour = "black") +
       annotate(geom = "text", x = 0, y = 2.5, label = "PM[2.5] ~mu~g/m^3", color = "black",
               angle = 90, parse = TRUE, size = 6) +
      annotate("segment", x = 0.5, xend = 0.5, y = 2, yend = 3,
               colour = "black") +
      annotate(geom = "text", x = 0.25, y = 2, label = "0", color = "black",
               angle = 0, parse = TRUE, size = 6) +
      annotate(geom = "text", x = 0, y = 3, label = "500", color = "black",
               angle = 0, parse = TRUE, size = 6) +
      ylim(1,3) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks=element_blank(),
            legend.position = c(0.38,0.35),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.key.size = unit(1, "cm"),
            legend.background = element_rect(color = "black"),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.margin = unit(c(1,1,1,1), "cm"))
ggsave("fake_central_line_pm25.pdf", width = 30, height = 24, units = "cm")
