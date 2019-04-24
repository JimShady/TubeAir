rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("grid")
library("openair")
library("gdata")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="localhost")

tube_data <- dbGetQuery(con, "
                        WITH  station_depths AS (
		SELECT		station_depths_import.station_name,
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
                        tube_pollution_mapping.scaled_concentration,
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

tube_data$line <- factor(tube_data$line, levels = c("Victoria", "Piccadilly", "Northern", "Circle", "Jubilee", "District", "Bakerloo", "Metropolitan", "Docklands Light Railway", "Central", "Hammersmith & City"))

tube_colours <- c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333", "#CC9999")

reorganising <- tube_data[,1:6]

just_depth   <- tube_data[,2:7]

just_depth$species <- 'depth'

just_depth <- just_depth[,c(7,1,2,3,4,5,6)]
just_depth$scaled_concentration <- just_depth$platform_depth 
just_depth <- just_depth[,1:6]

tube_data <- rbind(just_depth, reorganising)
names(tube_data)[names(tube_data)=='scaled_concentration'] <- 'value'

rm(just_depth, reorganising)

south_ken_pm25 <- importKCL(site = "kc1", year = as.numeric(getYear(min(tube_data$date_time))):as.numeric(getYear(max(tube_data$date_time)))  , pollutant = "pm25")

setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

for (i in 1:length(unique(as.character(tube_data$line)))) {
  
tube_line     <- unique(as.character(tube_data$line))[i]
colour        <- tube_colours[i]
data          <- tube_data[tube_data$environment == 'CAR' & tube_data$species == c('PM25', 'depth') & tube_data$line == tube_line & !is.na(tube_data$value),]
max_depth     <- max(data[data$species == 'PM25',]$value)
max_pm25      <- max(data[data$species == 'depth',]$value)
min_time      <- min(data$date_time)

plot <- ggplot(data, aes(x = date_time, y = value, group = species, colour = species)) + 
 # stat_smooth() +
  geom_line() +
  geom_point() +
  geom_text(data = data.frame(x=min_time, y=c(max_pm25, max_depth), label = c("(Metres)","(mg/m3)"), species=c("depth", "PM25")),
            aes(x, y, label=label), inherit.aes=FALSE, size = 7) +
  scale_color_manual(values = c(colour, colour)) +
  xlab("Time") +
  ylab("") +
  facet_grid(species ~., scale="free_y") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 15, colour = "black"),
        axis.title = element_text(size = 15, colour = "black"),
        legend.title = element_text(size = 15, colour = "black"),
        legend.text = element_text(size = 15, colour = "black"),
        strip.text.y = element_text(size = 20, colour = "black"),
        legend.position="none"
        )

ggsave(plot, file=paste("pm25_timelines/", unlist(strsplit(tube_line, " "))[1], "_pm25_depth.pdf", sep=""), width=29.7, height=21, units = "cm")

rm(tube_line, colour, data, max_depth, max_pm25, min_time)
}


aggregated <- aggregate(tube_data[!is.na(tube_data$platform_depth),c("scaled_concentration", "platform_depth")], list(tube_data[!is.na(tube_data$platform_depth),"line"], tube_data[!is.na(tube_data$platform_depth),"species"]), mean)
names(aggregated)[names(aggregated)=='Group.1'] <- 'line'
names(aggregated)[names(aggregated)=='Group.2'] <- 'species'

# Make a summary per line

pdf("depth_pm25_summary.pdf", width=11.67, height=8.27)
ggplot(aggregated[aggregated$species == 'PM25' & aggregated$line != 'Docklands Light Railway',], aes(platform_depth, scaled_concentration)) +
  geom_point(aes(colour=factor(line),
                 fill = factor(line)),
             size = 12) +
  scale_fill_manual(values = tube_colours) +
  scale_colour_manual(values = tube_colours) +
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
  ylab(expression(paste("PM"[2.5], " concentration (", mu, "g m" ^ "-3", ") ")))
dev.off()

# Plot per line
tube_lines <- unique(tube_data$line),
plot_list <- list()

for (i in 1:length(tube_lines)) {
  line <- tube_lines[[i]]
  colour <- tube_colours[[i]]
  
plot_list[[i]] <- ggplot(tube_data[tube_data$species == 'PM25' & tube_data$line == line & !is.na(tube_data$tube_diary_stop),], aes(platform_depth, scaled_concentration)) +
  geom_point(colour = colour, size = 4) +
  geom_smooth(method=lm) +
  xlab("Platform depth (metres)") +
  ylab(expression(paste("PM"[2.5], " (", mu, "g m" ^ "-3", ") "))) +
  ggtitle(line) +
theme(panel.grid.major = element_line(colour="grey"), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      axis.line = element_line(colour="black"),
      axis.text=element_text(size=10, color="black"),
      axis.title=element_text(size=10, color="black"),
      plot.title=element_text(size=14, colour="black"),
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

## PM25 Timeline plots

## Focusing on PM2.5. Timeline of each line with London background in it too.

for (i in 1:length(unique(as.character(tube_data$line)))) {
  tube_line <- unique(as.character(tube_data$line))[i]
  london_background <- south_ken_pm25[
    south_ken_pm25$date >= strptime(min(tube_data[tube_data$line == tube_line & tube_data$environment == 'CAR'  & tube_data$species == 'PM25',]$date_time), format="%Y-%m-%d %H", tz="GMT")
    & south_ken_pm25$date <= strptime(max(tube_data[tube_data$line == tube_line & tube_data$environment == 'CAR',]$date_time), format="%Y-%m-%d %H", tz="GMT")+3600
    & !is.na(south_ken_pm25$pm25),]
  
  plot <- ggplot() +
    geom_line(data=tube_data[tube_data$line == tube_line & tube_data$environment == 'CAR' & tube_data$species == 'PM25',], aes(x = date_time, y = scaled_concentration)) +
    geom_line(data=tube_data[tube_data$line == tube_line & tube_data$environment == 'CAR' & tube_data$species == 'PM25' & !is.na(tube_data$platform_depth),], aes(x = date_time, y = -platform_depth)) +
    geom_line(data=london_background, aes(x = date, y = pm25)) +
    ggtitle(tube_line)
  
  ggsave(plot, file=paste("pm25_timelines/", unlist(strsplit(tube_line, " "))[1], "_pm25_depth.pdf", sep=""))
}

############

















## Scatter plots with best fit lines
pdf("depth_pm25_scatter.pdf", width=11.67, height=8.27)
ggplot(tube_data[tube_data$species == 'PM25' & !is.na(line),], aes(x = platform_depth, y = scaled_concentration)) +
         geom_point() +
        stat_smooth() +
  facet_grid(. ~ line) +
  ggtitle("PM25")
dev.off()

pdf("depth_carbon_black_scatter.pdf", width=11.67, height=8.27)
ggplot(tube_data[tube_data$species == 'CBLK' & !is.na(line),], aes(x = platform_depth, y = scaled_concentration)) +
  geom_point() +
  stat_smooth() +
  facet_grid(. ~ line) +
  ggtitle("Carbon Black")
dev.off()

pdf("depth_particle_number_scatter.pdf", width=11.67, height=8.27)
ggplot(tube_data[tube_data$species == 'PCNT' & !is.na(line),], aes(x = platform_depth, y = scaled_concentration)) +
  geom_point() +
  stat_smooth() +
  facet_grid(. ~ line) +
  ggtitle("Particle Number")
dev.off()





pdf("depth_carbon_black_summary.pdf", width=11.67, height=8.27)
ggplot(aggregated[aggregated$Group.2 == 'CBLK',], aes(x = platform_depth, y = scaled_concentration, colour = Group.1)) +
  geom_point() +
  ggtitle("Carbon Black")
dev.off()

pdf("depth_particle_number_summary.pdf", width=11.67, height=8.27)
ggplot(aggregated[aggregated$Group.2 == 'PCNT',], aes(x = platform_depth, y = scaled_concentration, colour = Group.1)) +
  geom_point() +
  ggtitle("Particle Number")
dev.off()


