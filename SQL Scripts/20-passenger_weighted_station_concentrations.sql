SELECT		date_time,
		tube_diary_stop,
		concentration,
		station_name,
		saturday_entry
FROM		tube_pollution_mapping
LEFT JOIN	passenger_numbers
ON		tube_diary_stop = station_name
WHERE		species = 'PM25'
AND		environment = 'CAR'
AND		tube_diary_stop IS NOT NULL
AND		line <> 'Docklands Light Railway'
AND		tube_diary_stop <> 'Hammersmith'
UNION ALL
SELECT		date_time,
		tube_diary_stop,
		concentration,
		station_name,
		saturday_entry
FROM		tube_pollution_mapping
LEFT JOIN	passenger_numbers
ON		tube_diary_stop = station_name
AND		line = line_if_present
WHERE		species = 'PM25'
AND		environment = 'CAR'
AND		tube_diary_stop IS NOT NULL
AND		line <> 'Docklands Light Railway'
AND		tube_diary_stop = 'Hammersmith'
ORDER BY	date_time