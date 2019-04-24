DROP TABLE IF EXISTS tube_air;

CREATE TABLE tube_air (
	date_time TIMESTAMP,
	black_carbon NUMERIC,
	pm25 NUMERIC,
	particle_num NUMERIC
	);

-- Now go to psql at command line and import the csv of the data from phd/10 monitoring chapter

COPY tube_air(date_time, black_carbon, particle_num) FROM '/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/LU AQ Data/LUExportCarriage_Apr2015_reformatted.csv' DELIMITERS ',' HEADER CSV;

-- But now need to get the PM2.5 data in.
-- First import it to a temp table
DROP TABLE IF EXISTS temp_import;

CREATE TABLE temp_import(
device_location TEXT,
date_time 	TIMESTAMP,
raw_pm25	numeric
);

COPY temp_import FROM '/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/LU AQ Data/RawTubePM25_reformatted.csv' CSV HEADER;

UPDATE	tube_air SET pm25 = raw_pm25 FROM temp_import WHERE tube_air.date_time = temp_import.date_time;

------
COPY tube_air(date_time, black_carbon, pm25, particle_num) FROM '/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/LU AQ Data/extra_by_martha_kashim/ExtraLUExport_reformatted.csv' DELIMITERS ',' HEADER CSV;

-- Now need to scale the data using factors provided by Dave Green and Ben
-- Factor is divide by 1.8

ALTER TABLE tube_air ADD COLUMN scaled_pm25 numeric;

UPDATE tube_air SET scaled_pm25 = pm25 * 1.8

-------

