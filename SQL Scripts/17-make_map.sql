-- There's some bits of data that we aren't sure where they are. Typically when equipment turned on, on the way to do monitoring. Going to remove it manually here.
DELETE FROM tube_air WHERE date_time BETWEEN '2016-01-27 14:03:00' AND '2016-01-27 14:13:00'; -- Martha walking to Waterloo station
DELETE FROM tube_air WHERE date_time BETWEEN '2016-01-27 16:48:00' AND '2016-01-27 16:59:00'; -- Martha coming back to FWB from Waterloo station
DELETE FROM tube_air WHERE date_time BETWEEN '2016-02-01 11:03:00' AND '2016-02-01 11:39:00'; -- Rashim going to Hammersmith to start tube run
DELETE FROM tube_air WHERE date_time BETWEEN '2016-02-01 14:07:00' AND '2016-02-01 14:39:00';-- Rashim coming back to Waterloo from Hammersmith after monitoring

DROP TABLE IF EXISTS tube_pollution_mapping;

CREATE TABLE tube_pollution_mapping AS (
WITH	a_table AS (
			SELECT		tube_air.species,
					tube_air.environment,
					tube_air.date_time,
					tube_air.value 		AS concentration,
					tube_air.scaled_value	AS scaled_concentration,
					tube_diary.line,
					tube_diary.station_name AS tube_diary_stop,
					CASE 	WHEN	tube_diary.platform_or_train IS NULL	THEN	'line'
						WHEN	tube_diary.platform_or_train = 'tube' 	THEN	'station'
					ELSE	tube_diary.platform_or_train
					END AS	tube_diary_platform_or_train
			FROM		tube_air
			LEFT JOIN	tube_diary
			ON		tube_air.date_time >= arrived_time AND tube_air.date_time <= departed_time
			ORDER BY	tube_air.date_time
		),
	b_table AS (
			SELECT		date_time,
					tube_diary_stop,
					line,
					RANK () OVER (ORDER BY date_time ASC) AS a_rank
			FROM		a_table
			WHERE		tube_diary_stop IS NOT NULL
			GROUP BY	date_time,
					tube_diary_stop,
					line
			),
	c_table AS (
			SELECT		date_time,
					tube_diary_stop,
					line,
					RANK () OVER (ORDER BY date_time ASC) AS a_rank
			FROM		a_table
			WHERE		tube_diary_stop IS NOT NULL
			GROUP BY	date_time,
					tube_diary_stop,
					line
			),
	d_table AS (
			SELECT		b_table.date_time 	AS start_time,
					b_table.tube_diary_stop AS start_station,
					b_table.line		AS start_line,
					c_table.date_time 	AS end_time,
					c_table.tube_diary_stop AS end_station,
					c_table.line		AS end_line
			FROM		b_table
			LEFT JOIN	c_table
			ON		b_table.a_rank = c_table.a_rank -1
			),
	e_table AS (
			SELECT	start_time,
				start_station,
				start_line,
				end_time,
				end_station,
				end_line
			FROM	d_table
			WHERE	start_station 	<> 	end_station
			AND	start_line 	= 	end_line
			),
	f_table AS (
			SELECT		a_table.species,
					a_table.environment,
					a_table.date_time,
					a_table.concentration,
					a_table.scaled_concentration,
					a_table.tube_diary_stop,
					CASE	WHEN	a_table.line IS NULL 	THEN	start_line
					ELSE		a_table.line
					END AS	line,
					CASE	WHEN	tube_diary_stop IS NULL THEN 	start_station
					END AS	previous_tube_stop,
					CASE	WHEN	tube_diary_stop IS NULL THEN 	end_station
					END AS	next_tube_stop
			FROM		a_table
			LEFT JOIN	e_table
			ON		a_table.date_time > e_table.start_time AND a_table.date_time < e_table.end_time
			)
	SELECT		f_table.species,
			f_table.environment,
			f_table.date_time,
			f_table.concentration,
			f_table.scaled_concentration,
			f_table.tube_diary_stop,
			f_table.line,
			f_table.previous_tube_stop,
			f_table.next_tube_stop,
			network_stations.start_node_geom as station_geometry,
			network_edges.edge as line_geometry
	FROM		f_table
	LEFT JOIN	(SELECT	line, start_station, start_node_geom FROM tube_network GROUP BY line, start_station, start_node_geom) network_stations
	ON		f_table.line = network_stations.line
	AND		f_table.tube_diary_stop = network_stations.start_station
	LEFT JOIN	(SELECT	line, start_station, end_station, edge FROM tube_network GROUP BY line, start_station, end_station, edge) network_edges
	ON		f_table.line = network_edges.line
	AND		f_table.previous_tube_stop = network_edges.start_station
	AND		f_table.next_tube_stop = network_edges.end_station
	ORDER BY	date_time, environment, species
)

SELECT		*
FROM		tube_pollution_mapping
ORDER BY 	date_time
-------------

DELETE FROM tube_pollution_mapping WHERE species = 'PCNT' AND environment = 'CAR' AND date_time = '2015-02-17 13:55:00';
DELETE FROM tube_pollution_mapping WHERE species = 'CBLK' AND environment = 'CAB' AND date_time = '2015-03-03 13:30:00';
DELETE FROM tube_pollution_mapping WHERE species = 'CBLK' AND environment = 'CAR' AND date_time = '2015-04-22 13:38:00';
DELETE FROM tube_pollution_mapping WHERE species = 'PM25' AND environment = 'CAR' AND date_time = '2015-04-22 13:38:00';
DELETE FROM tube_pollution_mapping WHERE species = 'CBLK' AND environment = 'CAR' AND date_time = '2016-02-01 14:40:00';

---- Now need to add the depth data
-- Think I need to re-make the depth data into the table called 'underground_routes' (maybe calling it something better in the process)
-- Script 16a sorts it out. Have processed tube data. Can't go back to original input data now. Just so you know. YOu have to use database info. Not orignal CVS

-- So get tube stations info, join to depths, then join to the concentrations.

WITH	station_depths AS (
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
		tube_pollution_mapping.concentration,
		tube_pollution_mapping.scaled_concentration,
		tube_pollution_mapping.tube_diary_stop,
		tube_pollution_mapping.line,
		station_depths.platform_depth
FROM		tube_pollution_mapping
LEFT JOIN	station_depths
ON		tube_pollution_mapping.tube_diary_stop 	= 	station_depths.station_name
AND		tube_pollution_mapping.line		=	station_depths.line
WHERE		tube_diary_stop IS NOT NULL
ORDER BY	tube_pollution_mapping.date_time,
		tube_pollution_mapping.environment,
		tube_pollution_mapping.species


















SELECT	*
FROM	

-- getting data for 3D R plots
CREATE TABLE temp_stations_depth_stuff AS 
(SELECT		line,
		tube_diary_stop as station,
		round(avg(tube_air_pm25),2) as pm25,
		st_x(station_location) as x,
		st_y(station_location) as y,
		depth as z
FROM		tube_pollution_mapping
WHERE		tube_diary_platform_or_train = 'station'
GROUP BY	tube_diary_line,
		tube_diary_stop,
		station_location,
		depth
ORDER BY	tube_diary_line,
		tube_diary_stop)

COPY temp_stations_depth_stuff TO '/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results/station_depth_with_conc.csv' CSV HEADER;

-- Making a grid, and calculating the mean PM2.5 for each grid square
SELECT		start_station,
		end_station,
		st_line_interpolate_point(edge, percents::numeric/10)
		percents
FROM		tube_network,
		generate_series(2,9,1) as percents
WHERE	start_station 	= 'Kennington'
AND	end_station	= 'Oval'

-- make a map of the stations
DROP TABLE IF EXISTS temp_northern_stations;

CREATE TABLE temp_northern_stations AS (
SELECT		tube_diary_line AS line,
		tube_diary_stop as station,
		AVG(tube_air_pm25) as pm25,
		st_transform(station_location,4326) AS location
FROM		tube_pollution_mapping
WHERE		tube_diary_platform_or_train = 'station'
AND		tube_diary_line = 'Northern'
GROUP BY	tube_diary_line,
		tube_diary_stop,
		location
);

DROP TABLE IF EXISTS temp_central_stations;

CREATE TABLE temp_central_stations AS (
SELECT		tube_diary_line AS line,
		tube_diary_stop as station,
		AVG(tube_air_pm25) as pm25,=s
		st_transform(station_location,4326) AS location
FROM		tube_pollution_mapping
WHERE		tube_diary_platform_or_train = 'station'
AND		tube_diary_line = 'Central'
GROUP BY	tube_diary_line,
		tube_diary_stop,
		location
);

-- make a map of the lines
DROP TABLE IF EXISTS temp_northern_lines;

CREATE TABLE temp_northern_lines AS (
SELECT		tube_diary_line,
		round(AVG(tube_air_pm25),2) as pm25,
		st_transform(edge,4326) AS line
FROM		tube_pollution_mapping
WHERE		tube_diary_platform_or_train = 'line'
AND		tube_diary_line = 'Northern'
GROUP BY	tube_diary_line,
		line
);

DROP TABLE IF EXISTS temp_central_lines;

CREATE TABLE temp_central_lines AS (
SELECT		tube_diary_line,
		round(AVG(tube_air_pm25),2) as pm25,
		st_transform(edge,4326) AS line
FROM		tube_pollution_mapping
WHERE		tube_diary_platform_or_train = 'line'
AND		tube_diary_line = 'Central'
GROUP BY	tube_diary_line,
		line
);

-- Now go to command line and output these as shapefiles to import to CartoDB
pgsql2shp -f "/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results/temp_northern_stations" -h localhost -u james -P brianclough tube_monitoring temp_northern_stations
pgsql2shp -f "/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results/temp_central_stations" -h localhost -u james -P brianclough tube_monitoring temp_central_stations

pgsql2shp -f "/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results/temp_northern_lines" -h localhost -u james -P brianclough tube_monitoring temp_northern_lines
pgsql2shp -f "/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results/temp_central_lines" -h localhost -u james -P brianclough tube_monitoring temp_central_lines