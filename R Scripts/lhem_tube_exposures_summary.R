## Focusing on the tube for COMEAP query

rm(list=ls())

library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("scales")
library("gridExtra")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="james_traffic", user="james", password="brianclough", host="localhost")

tube_exposures <- dbGetQuery(con, "with   tube_people_filter AS (
	SELECT		ppid
	FROM 		dynamic_exposure_stops_no_monthly
	WHERE		mode = 17
	AND		ppid IN (SELECT ppid FROM person WHERE ppiwt::numeric > 0 AND bad_flag IS NULL)
	GROUP BY	ppid
)	
SELECT		dynamic_exposure_stops_no_monthly.ppid,
		dynamic_exposure_stops_no_monthly.expansion_factor,
		dynamic_exposure_stops_no_monthly.mode,
		dynamic_exposure_stops_no_monthly.page,
		SUM(dynamic_exposure_stops_no_monthly.microenv_pm25),
    COUNT(*)
FROM		dynamic_exposure_stops_no_monthly
WHERE		ppid IN (SELECT ppid FROM tube_people_filter)
GROUP BY	ppid,
		expansion_factor,
		mode,
		page")

tube_exposures <- tube_exposures[order(tube_exposures$ppid),]

total_exposures <- aggregate(sum ~ ppid, data = tube_exposures, FUN=sum)

tube_exposures <- merge(tube_exposures, total_exposures, by = 'ppid', all.x=T)

tube_exposures$percent_of_total <- 100 * (tube_exposures$sum.x / tube_exposures$sum.y)

tube_exposures$percent_of_time <- 100 * (tube_exposures$count / (60*24))

tube_exposures$age_category <- ''

tube_exposures[tube_exposures$page > 0 & tube_exposures$page < 18,'age_category'] <- 'Children'
tube_exposures[tube_exposures$page > 17 & tube_exposures$page < 30,'age_category'] <- 'Youg adult'
tube_exposures[tube_exposures$page > 29 & tube_exposures$page < 60,'age_category'] <- 'Adult'
tube_exposures[tube_exposures$page >= 60,'age_category'] <- 'Elderly'

names(tube_exposures)[5] <- 'total_microenv_pm25'
names(tube_exposures)[6] <- 'total_microenv_mins'
names(tube_exposures)[7] <- 'total_daily_pm25'
names(tube_exposures)[8] <- 'percent_daily_microenv_pm25'
names(tube_exposures)[9] <- 'percent_daily_microenv_time'

expanded_tube_exposures <- data.frame(
  ppid                          =   rep(tube_exposures$ppid, tube_exposures$expansion_factor),
  mode                          =   rep(tube_exposures$mode, tube_exposures$expansion_factor),
  total_microenv_pm25           =   rep(tube_exposures$total_microenv_pm25, tube_exposures$expansion_factor),
  total_microenv_mins           =   rep(tube_exposures$total_microenv_mins, tube_exposures$expansion_factor),
  total_daily_pm25              =   rep(tube_exposures$total_daily_pm25, tube_exposures$expansion_factor),
  percent_daily_microenv_pm25   =   rep(tube_exposures$percent_daily_microenv_pm25, tube_exposures$expansion_factor),
  percent_daily_microenv_time   =   rep(tube_exposures$percent_daily_microenv_time, tube_exposures$expansion_factor),
  age_category                  =   rep(tube_exposures$age_category, tube_exposures$expansion_factor),
  stringsAsFactors=FALSE
)

## Percent of time on tube by people who use the tube
aggregate(percent_daily_microenv_time ~ age_category, 
          data = expanded_tube_exposures[expanded_tube_exposures$mode == 17,], 
          FUN=mean)

## Percent of daily pm2.5 exposure from the tube by people who use the tube
aggregate(percent_daily_microenv_pm25 ~ age_category, 
          data = expanded_tube_exposures[expanded_tube_exposures$mode == 17,], 
          FUN=mean)

















melted <- melt(tube_exposures[,c('age_category', 'percent_of_total')], id.var = c('age_category'), variable.name = 'percent_of_total')

names(melted) <- c('age_category', 'variable', 'percent_of_daily_pm25_from_tube')

ggplot(melted, aes(x = percent_of_daily_pm25_from_tube, group = age_category, fill = age_category)) +
         geom_histogram(binwidth=1, alpha=0.6) +
  theme_bw() +
  theme(axis.text = element_text(size = 16, colour = 'black'),
        axis.title = element_text(size = 16, colour = 'black'))


