DROP SEQUENCE serial;

CREATE SEQUENCE serial START 1;

DROP TABLE IF EXISTS test_table;

CREATE TABLE test_table AS (
WITH	a_table as (
	SELECT		line,
			station_name,
			arrived_time,
			departed_time,
			AVG(black_carbon) as black_carbon,
			AVG(particle_num) as particle_num,
			AVG(pm25) as pm25
	FROM		tube_diary
	LEFT JOIN	tube_air
	ON		tube_air.date_time >= arrived_time AND tube_air.date_time <= departed_time
	GROUP BY	line,
			station_name,
			arrived_time,
			departed_time
	ORDER BY	arrived_time
	),
	b_table AS (
	SELECT		line,
			station_name,
			AVG(black_carbon) as black_carbon,
			AVG(particle_num) as particle_num,
			AVG(pm25) as pm25
	FROM		a_table
	GROUP BY	line,
			station_name
	ORDER BY	line
	)
SELECT		nextval('serial') as id,
		b_table.line,
		b_table.station_name,
		round(b_table.black_carbon,0) as black_carbon,
		round(b_table.particle_num,0) as particle_num,
		round(b_table.pm25, 0) as pm25,
		depth_data.platform_depth,
		tube_network.start_node_geom
FROM		b_table
LEFT JOIN	(SELECT line, start_station, start_node_geom FROM tube_network GROUP BY line, start_station, start_node_geom) as tube_network
ON		b_table.line = tube_network.line
AND		b_table.station_name = tube_network.start_station
LEFT JOIN	(SELECT	line,
			shortname,
			CASE WHEN	underground_routes.northbound IS NULL THEN	round(underground_routes.ground_level - (((underground_routes.eastbound + underground_routes.westbound)/2)-100),2)
			ELSE		round(underground_routes.ground_level - (((underground_routes.northbound + underground_routes.southbound)/2)-100),2)
			END AS		depth
		FROM	underground_routes) AS depth_data
ON		b_table.station_name = depth_data.shortname);



SELECT * FROM test_table ORDER BY station_name;

