-- DROP TABLE IF ALREADY EXISTS
DROP TABLE IF EXISTS tube_air;

-- CREATE TABLE
CREATE TABLE tube_air (
	species TEXT,
	environment TEXT,
	date_time TIMESTAMP,
	value NUMERIC,
	scaled_value NUMERIC
	);

-- IMPORT DATA FROM CSV
COPY tube_air(species, environment, date_time, value, scaled_value) FROM '/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/LU AQ Data/LUexportMay2016.csv' DELIMITERS ',' HEADER CSV;

