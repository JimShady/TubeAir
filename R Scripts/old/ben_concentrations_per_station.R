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
library('gridExtra')

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="brianclough", host="10.0.4.240")

## THIS IS VERY DRAFT> MOST OF THE CODE IS FROM ANOTGHER SCRIPT AND NEEDS RE-WRITING
## Get daily background averages for London from OpenAir

## Note the CASE statement below. it's because want the raw PM25 data to do our own scaling, but the scaled version of other pollutants.
tube_data <- dbGetQuery(con, "
                        SELECT  	tube_diary_stop,
		species,
		round(AVG(concentration),0) 		AS concentration,
		round(AVG(scaled_concentration),0) 	AS scaled_concentration
FROM		tube_pollution_mapping
WHERE		environment = 'CAR'
AND		tube_diary_stop IS NOT NULL
AND		line <> 'Docklands Light Railway'
GROUP BY	tube_diary_stop,
		species
                        ")

write.csv(tube_data, file = 'concentrations_per_station_for_ben.csv', col.names = T)
