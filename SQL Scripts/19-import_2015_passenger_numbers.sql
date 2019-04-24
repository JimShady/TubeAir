-- DROP TABLE IF ALREADY EXISTS
DROP TABLE IF EXISTS passenger_numbers;

-- CREATE TABLE

CREATE TABLE passenger_numbers (
nlc_code NUMERIC,
station_name TEXT,
station_borough TEXT,
note TEXT,
weekday_entry NUMERIC,
weekday_entry_weight NUMERIC,
saturday_entry NUMERIC,
saturday_entry_weight NUMERIC,
sunday_entry NUMERIC,
sunday_entry_weight NUMERIC,
weekday_exit NUMERIC,
weekday_exit_weight NUMERIC,
saturday_exit NUMERIC,
saturday_exit_weight NUMERIC,
sunday_exit NUMERIC,
sunday_exit_weight NUMERIC,
line_if_present TEXT
	);

-- IMPORT DATA FROM CSV
COPY passenger_numbers FROM '/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Passgenger Number Data/2015_entry_exit_by_station.csv' DELIMITERS ',' HEADER CSV;

-- Data seperates out Hammersmith station to two stations. District, Hammersmith & City. Should be four. Piccadilly and Circle are missing. Think they use the same gates.
-- So going to halve current numbers and make two new entries.

UPDATE	passenger_numbers
SET	weekday_entry 		= weekday_entry/2,
	saturday_entry		= saturday_entry/2,
	sunday_entry		= sunday_entry/2,
	weekday_exit		= weekday_exit/2,
	saturday_exit		= saturday_exit/2,
	sunday_exit		= sunday_exit/2
WHERE	line_if_present 	= 'District'
AND	station_name 		= 'Hammersmith';

UPDATE	passenger_numbers
SET	weekday_entry 		= weekday_entry/2,
	saturday_entry		= saturday_entry/2,
	sunday_entry		= sunday_entry/2,
	weekday_exit		= weekday_exit/2,
	saturday_exit		= saturday_exit/2,
	sunday_exit		= sunday_exit/2
WHERE	line_if_present 	= 'Hammersmith & City'
AND	station_name 		= 'Hammersmith';

INSERT INTO passenger_numbers (
SELECT 	nlc_code,
	station_name,
	station_borough,
	note,
	weekday_entry,
	weekday_entry_weight,
	saturday_entry,
	saturday_entry_weight,
	sunday_entry,
	sunday_entry_weight,
	weekday_exit,
	weekday_exit_weight,
	saturday_exit,
	saturday_exit_weight,
	sunday_exit,
	sunday_exit_weight,
	'Piccadilly'
FROM	passenger_numbers
WHERE	line_if_present = 'District'
AND	station_name = 'Hammersmith');

INSERT INTO passenger_numbers (
SELECT 	nlc_code,
	station_name,
	station_borough,
	note,
	weekday_entry,
	weekday_entry_weight,
	saturday_entry,
	saturday_entry_weight,
	sunday_entry,
	sunday_entry_weight,
	weekday_exit,
	weekday_exit_weight,
	saturday_exit,
	saturday_exit_weight,
	sunday_exit,
	sunday_exit_weight,
	'Circle'
FROM	passenger_numbers
WHERE	line_if_present = 'Hammersmith & City'
AND	station_name = 'Hammersmith');

-- The passenger number data groups Monument and Bank. But my AQ data gives them seperately. So going to split the passenger numbers in half for each

UPDATE	passenger_numbers
SET	weekday_entry 		= weekday_entry/2,
	saturday_entry		= saturday_entry/2,
	sunday_entry		= sunday_entry/2,
	weekday_exit		= weekday_exit/2,
	saturday_exit		= saturday_exit/2,
	sunday_exit		= sunday_exit/2,
	station_name		= 'Bank'
WHERE	station_name 		= 'Bank & Monument';

INSERT INTO passenger_numbers (
SELECT 	nlc_code,
	'Monument',
	station_borough,
	note,
	weekday_entry,
	weekday_entry_weight,
	saturday_entry,
	saturday_entry_weight,
	sunday_entry,
	sunday_entry_weight,
	weekday_exit,
	weekday_exit_weight,
	saturday_exit,
	saturday_exit_weight,
	sunday_exit,
	sunday_exit_weight,
	''
FROM	passenger_numbers
WHERE	station_name = 'Bank');