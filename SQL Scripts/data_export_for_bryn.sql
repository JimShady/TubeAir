SELECT		tube_pollution_mapping.date_time,
		tube_pollution_mapping.concentration,
		tube_pollution_mapping.line,
		tube_pollution_mapping.previous_tube_stop,
		tube_pollution_mapping.next_tube_stop,
		previous.platform_depth AS previous_tube_stop_depth,
		next.platform_depth AS next_tube_stop_depth
FROM 		tube_pollution_mapping
LEFT JOIN	station_depths_import AS previous
ON		tube_pollution_mapping.line = previous.line
AND		tube_pollution_mapping.previous_tube_stop = previous.station_name
LEFT JOIN	station_depths_import AS next
ON		tube_pollution_mapping.line = next.line
AND		tube_pollution_mapping.next_tube_stop = next.station_name
WHERE		species = 'PM25'
AND 		tube_diary_stop IS NULL