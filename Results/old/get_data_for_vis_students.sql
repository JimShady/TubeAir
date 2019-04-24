-- Data for Ben data vis students

COPY (
SELECT		tube_diary_line 				AS line,
		tube_diary_stop 				AS station,
		round(AVG(tube_air_pm25),2)			AS pm25,
		round(AVG(tube_air_particle_num),2)		AS particle_number,
		round(AVG(tube_air_black_carbon),2)		AS black_carbon,
		st_x(st_transform(station_location,4326)) 	AS x,
		st_y(st_transform(station_location,4326)) 	AS y,
		depth*-1					AS z
FROM		tube_pollution_mapping
WHERE		tube_diary_platform_or_train = 'station'
GROUP BY	line,
		station,
		x,
		y,
		z

) TO '/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results/student_vis_data.csv' CSV HEADER;