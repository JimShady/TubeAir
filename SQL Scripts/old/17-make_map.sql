-- There's some bits of data that we aren't sure where they are. Typically when equipment turned on, on the way to do monitoring. Going to remove it manually here.
DELETE FROM tube_air WHERE date_time BETWEEN '2016-01-27 14:03:00' AND '2016-01-27 14:13:00'; -- Martha walking to Waterloo station
DELETE FROM tube_air WHERE date_time BETWEEN '2016-01-27 16:48:00' AND '2016-01-27 16:59:00'; -- Martha coming back to FWB from Waterloo station
DELETE FROM tube_air WHERE date_time BETWEEN '2016-02-01 11:03:00' AND '2016-02-01 11:39:00'; -- Rashim going to Hammersmith to start tube run
DELETE FROM tube_air WHERE date_time BETWEEN '2016-02-01 14:07:00' AND '2016-02-01 14:39:00';-- Rashim coming back to Waterloo from Hammersmith after monitoring

DROP SEQUENCE serial;

CREATE SEQUENCE serial START 1;

DROP TABLE IF EXISTS tube_pollution_mapping;

CREATE TABLE tube_pollution_mapping AS (
-- join the tube air data to the tube diaries for Ben to do graphs from
-- This sorts out the station station stuff.
WITH start_information AS (
with a_table AS
(SELECT		tube_air.date_time 	AS tube_air_date_time,
		tube_air.black_carbon 	AS tube_air_black_carbon,
		tube_air.particle_num 	AS tube_air_particle_num,
		tube_air.scaled_pm25	AS tube_air_pm25,
		tube_diary.line 	AS tube_diary_line,
		tube_diary.station_name AS tube_diary_stop,
		CASE 	WHEN	tube_diary.platform_or_train IS NULL	THEN	'line'
			WHEN	tube_diary.platform_or_train = 'tube' 	THEN	'station'
			ELSE	tube_diary.platform_or_train
		END AS	tube_diary_platform_or_train,
		a.station_name 		AS start_station_name,
		a.line 			AS rest_of_line,
		a.arrived_time,
		a.departed_time
FROM		tube_air
LEFT JOIN	tube_diary
ON		tube_air.date_time >= arrived_time AND tube_air.date_time <= departed_time
LEFT JOIN	(SELECT * FROM tube_diary WHERE station_name IS NOT NULL) a
ON		date_trunc('DAY', a.arrived_time) = date_trunc('DAY', tube_air.date_time) AND a.arrived_time < tube_air.date_time
ORDER BY	tube_air.date_time,
		a.arrived_time
),
b_table AS (
SELECT		tube_air_date_time,
		tube_air_black_carbon,
		tube_air_particle_num,
		tube_air_pm25,
		arrived_time,
		tube_diary_line,
		tube_diary_stop,
		tube_diary_platform_or_train,
		start_station_name,
		arrived_time,
		departed_time,
		rest_of_line	,	
		RANK () OVER (PARTITION BY tube_air_date_time ORDER BY tube_air_date_time DESC, arrived_time DESC) AS a_rank
FROM		a_table
ORDER BY 	tube_air_date_time,
		arrived_time)
SELECT	tube_air_date_time,
	tube_air_black_carbon,
	tube_air_particle_num,
	tube_air_pm25,
	CASE	WHEN	tube_diary_line IS NULL THEN rest_of_line
	ELSE		tube_diary_line
	END AS		tube_diary_line,
	tube_diary_stop,
	tube_diary_platform_or_train,
	CASE	WHEN	tube_diary_stop IS NULL THEN	start_station_name
	ELSE	NULL
	END AS	start_station_name
FROM	b_table
WHERE	a_rank = 1),
end_information AS (
-------------------------------
-- Now do the end station stuff
with a_table AS
(SELECT		tube_air.date_time 	AS tube_air_date_time,
		tube_air.black_carbon 	AS tube_air_black_carbon,
		tube_air.particle_num 	AS tube_air_particle_num,
		tube_air.scaled_pm25	AS tube_air_pm25,
		tube_diary.line 	AS tube_diary_line,
		tube_diary.station_name AS tube_diary_stop,
		CASE 	WHEN	tube_diary.platform_or_train IS NULL	THEN	'line'
			WHEN	tube_diary.platform_or_train = 'tube' 	THEN	'station'
			ELSE	tube_diary.platform_or_train
		END AS	tube_diary_platform_or_train,
		a.station_name 		AS end_station_name,
		a.arrived_time,
		a.departed_time
FROM		tube_air
LEFT JOIN	tube_diary
ON		tube_air.date_time >= arrived_time AND tube_air.date_time <= departed_time
LEFT JOIN	(SELECT * FROM tube_diary WHERE station_name IS NOT NULL) a
ON		date_trunc('DAY', a.arrived_time) = date_trunc('DAY', tube_air.date_time) AND a.arrived_time > tube_air.date_time
ORDER BY	tube_air.date_time,
		a.arrived_time
),
b_table AS (
SELECT		tube_air_date_time,
		tube_air_black_carbon,
		tube_air_particle_num,
		tube_air_pm25,
		arrived_time,
		tube_diary_line,
		tube_diary_stop,
		tube_diary_platform_or_train,
		end_station_name,
		arrived_time,
		departed_time,		
		RANK () OVER (PARTITION BY tube_air_date_time ORDER BY tube_air_date_time ASC, departed_time ASC) AS a_rank
FROM		a_table
ORDER BY 	tube_air_date_time,
		departed_time)
SELECT	tube_air_date_time,
	tube_air_black_carbon,
	tube_air_particle_num,
	tube_air_pm25,
	tube_diary_line,
	tube_diary_stop,
	tube_diary_platform_or_train,
	CASE	WHEN	tube_diary_stop IS NULL THEN	end_station_name
	ELSE	NULL
	END AS	end_station_name
FROM	b_table
WHERE	a_rank = 1),
c_table AS (
SELECT		start_information.*,
		end_information.end_station_name
FROM		start_information
LEFT JOIN	end_information
ON		start_information.tube_air_date_time = end_information.tube_air_date_time),
d_table AS (
SELECT		ROUND(AVG(tube_air_black_carbon),1) 	as tunnel_section_average_black_carbon,
		ROUND(AVG(tube_air_particle_num),1) 	as tunnel_section_average_particle_num,
		ROUND(AVG(tube_air_pm25),1) 		as tunnel_section_average_pm25,
		start_station_name,
		end_station_name
FROM		c_table
GROUP BY	start_station_name,
		end_station_name
ORDER BY	start_station_name,
		end_station_name
	),
e_table AS (
SELECT		c_table.tube_air_date_time,
		CASE	WHEN	c_table.start_station_name IS NOT NULL THEN tunnel_section_average_black_carbon
		ELSE	tube_air_black_carbon
		END AS	tube_air_black_carbon,
		CASE	WHEN	c_table.start_station_name IS NOT NULL THEN tunnel_section_average_particle_num
		ELSE	tube_air_particle_num
		END AS	tube_air_particle_num,
		CASE	WHEN	c_table.start_station_name IS NOT NULL THEN tunnel_section_average_pm25
		ELSE	tube_air_pm25
		END AS	tube_air_pm25,
		c_table.tube_diary_line,
		c_table.tube_diary_stop,
		c_table.tube_diary_platform_or_train,
		c_table.start_station_name,
		c_table.end_station_name
FROM		c_table
LEFT JOIN	d_table
ON		c_table.start_station_name = d_table.start_station_name
AND		c_table.end_station_name = d_table.end_station_name
ORDER BY	tube_air_date_time
)
SELECT		nextval('serial') as id,
		e_table.tube_air_date_time,
		depth_data.depth,
		e_table.tube_air_black_carbon,
		e_table.tube_air_particle_num,
		e_table.tube_air_pm25,
		e_table.tube_diary_stop,
		e_table.tube_diary_line,
		e_table.tube_diary_platform_or_train,
		e_table.start_station_name,
		e_table.end_station_name,
		station_location.start_node_geom as station_location,
		line_location.edge
FROM		e_table
LEFT OUTER JOIN	(SELECT start_station,
			start_node_geom,
			line
		FROM 	tube_network
		GROUP BY start_station,
			start_node_geom,
			line) AS station_location
ON		e_table.tube_diary_stop = station_location.start_station
AND		e_table.tube_diary_line = station_location.line
LEFT OUTER JOIN	tube_network AS line_location
ON		e_table.start_station_name = line_location.start_station
AND		e_table.end_station_name = line_location.end_station
AND		e_table.tube_diary_line = line_location.line
LEFT JOIN	(	SELECT		underground_routes.line,
					underground_routes.shortname,
					CASE WHEN	underground_routes.northbound IS NULL THEN	round(underground_routes.ground_level - (((underground_routes.eastbound + underground_routes.westbound)/2)-100),2)
					ELSE		round(underground_routes.ground_level - (((underground_routes.northbound + underground_routes.southbound)/2)-100),2)
					END AS		depth
			FROM		underground_routes
			GROUP BY	line,
					shortname,
					depth) AS depth_data
ON		e_table.tube_diary_line = depth_data.line
AND		e_table.tube_diary_stop = depth_data.shortname
ORDER BY	e_table.tube_air_date_time
);

-------------

-- had to do some manual fixing of station names, lines, that sort of thing here. Have mostly corrected the original data.
-- Note that there's some 9999 in the air quality data, which represents no data.

UPDATE tube_pollution_mapping SET tube_air_black_carbon = NULL where tube_air_black_carbon = 9999;
UPDATE tube_pollution_mapping SET tube_air_particle_num = NULL where tube_air_black_carbon = 9999;
UPDATE tube_pollution_mapping SET tube_air_pm25 	= NULL where tube_air_black_carbon = 9999;

-- getting data for 3D R plots
CREATE TABLE temp_stations_depth_stuff AS 
(SELECT		tube_diary_line as line,
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