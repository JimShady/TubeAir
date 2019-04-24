rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("grid")


drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="localhost")
black_carbon <- dbGetQuery(con, "
                      SELECT  tube_diary_line,
                    	tube_air_black_carbon,
                      tube_diary_platform_or_train
                      FROM	tube_pollution_mapping
                      WHERE	tube_diary_platform_or_train = 'line'
                      AND tube_diary_line IS NOT NULL
                      ")

particle_num <- dbGetQuery(con, "
                      SELECT  tube_diary_line,
                      tube_air_particle_num,
                      tube_diary_platform_or_train
                      FROM	tube_pollution_mapping
                      WHERE	tube_diary_platform_or_train = 'line'
                      AND tube_diary_line IS NOT NULL
                      ")

pm25 <- dbGetQuery(con, "
                            SELECT  tube_diary_line,
                           tube_air_pm25,
                           tube_diary_platform_or_train
                           FROM	tube_pollution_mapping
                           WHERE	tube_diary_platform_or_train = 'line'
                           AND tube_diary_line IS NOT NULL
                           ")

## Below numbers are provided by Ben. They're from the monitoring done by 119 subjects.
## Note that they are just a few hours, not daily averages, which would be a bit lower
oxford_street_bc              <- 10
hyde_park_bc                  <- 1.2
oxford_street_pm25            <- 18.1
hyde_park_pm25                <- 7.5
oxford_street_particle_number <- 25.5 * 1000
hyde_park_particle_number     <- 5.7 * 1000

## Set a working directory to output the graphs too.
setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

black_carbon$tube_diary_line <- factor(black_carbon$tube_diary_line, levels = c("Victoria",
                                                                                "Piccadilly",
                                                                                "Northern",
                                                                                "Circle",
                                                                                "Jubilee",
                                                                                "District",
                                                                                "Bakerloo",
                                                                                "Metropolitan",
                                                                                "Docklands Light Railway",
                                                                                "Central"))

pdf("black_carbon_on_underground.pdf", width=11.67, height=8.27)
ggplot(black_carbon, aes(tube_diary_line, tube_air_black_carbon, fill=interaction(tube_diary_line))) +
  geom_boxplot(outlier.shape=NA) +
  geom_hline(yintercept = hyde_park_bc, colour="green") +
  geom_hline(yintercept =oxford_street_bc, colour="red") +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.line = element_line(colour="black"),
        axis.text=element_text(size=16, color="black"),
        axis.title=element_text(size=16, color="black"),
        axis.text.x=element_text(angle = 40, hjust = 1),
        plot.title=element_text(size=16, colour="black"),
        legend.position="none") +
  labs(title=expression(paste("Attenuation on the London Underground, by line")),
       x=expression(paste("London Underground Line")), 
       y=expression(paste("Attenuation"))) +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333"))
dev.off()

particle_num$tube_diary_line <- factor(particle_num$tube_diary_line, levels = c("Victoria",
                                                                                "Piccadilly",
                                                                                "Northern",
                                                                                "Circle",
                                                                                "Jubilee",
                                                                                "District",
                                                                                "Bakerloo",
                                                                                "Metropolitan",
                                                                                "Docklands Light Railway",
                                                                                "Central"))

pdf("particle_number_on_underground.pdf", width=11.67, height=8.27)
ggplot(particle_num, aes(tube_diary_line, tube_air_particle_num, fill=interaction(tube_diary_line))) +
  geom_boxplot(outlier.shape=NA) +
  geom_hline(yintercept = hyde_park_particle_number, colour="green") +
  geom_hline(yintercept = oxford_street_particle_number, colour="red") +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.line = element_line(colour="black"),
        axis.text=element_text(size=16, color="black"),
        axis.title=element_text(size=16, color="black"),
        axis.text.x=element_text(angle = 40, hjust = 1),
        plot.title=element_text(size=16, colour="black"),
        legend.position="none") +
  labs(title=expression(paste("Particle number (thousands/cm3)")),
       x=expression(paste("London Underground Line")), 
       y=expression(paste("Particle number (thousands/cm3)"))) +
  scale_y_continuous(limits=c(0,35000), labels=comma) +
  scale_fill_manual(values=c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333"))
dev.off()

pm25$tube_diary_line <- factor(pm25$tube_diary_line, levels = c("Victoria",
                                                                "Piccadilly",
                                                                "Northern",
                                                                "Circle",
                                                                "Jubilee",
                                                                "District",
                                                                "Bakerloo",
                                                                "Metropolitan",
                                                                "Docklands Light Railway",
                                                                "Central"))

pdf("pm25_on_underground.pdf", width=11.67, height=8.27)
ggplot(pm25, aes(tube_diary_line, tube_air_pm25, fill=interaction(tube_diary_line))) +
  geom_boxplot(outlier.shape=NA) +
  geom_hline(yintercept = hyde_park_pm25, colour="green") +
  geom_hline(yintercept = oxford_street_pm25, colour="red") +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.line = element_line(colour="black"),
        axis.text=element_text(size=16, color="black"),
        axis.title=element_text(size=16, color="black"),
        axis.text.x=element_text(angle = 40, hjust = 1),
        plot.title=element_text(size=16, colour="black"),
        legend.position="none") +
  labs(title=expression(paste("Mean PM"[2.5], " (", mu, "g m" ^ "-3", "), by tube line (scaled)")),
       x=expression(paste("London Underground Line")), 
       y=expression(paste("PM"[2.5], "(", mu, "g m" ^ "-3", ") "))
       ) +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333"))
dev.off()

aggregate(pm25[, 2], list(pm25$tube_diary_line), mean)