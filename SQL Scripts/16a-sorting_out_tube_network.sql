-- Been various versions of tube networks in my database now
-- Believe that this is the best

CREATE TABLE	station_geom_depth AS (
SELECT		shortname,
		longname,
		line,
		in_order,
		section,
		the_geom
FROM		underground_routes
);

SELECT	*
FROM	station_geom_depth

DROP TABLE station_depths_import;

CREATE TABLE	station_depths_import (
		station_name text,
		ground_level numeric,
		line text,
		raw_platform_depth numeric,
		platform_depth numeric);

COPY station_depths_import FROM '/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Line Depth Data/station_depths_edited_v4.csv' CSV HEADER;

SELECT	*
FROM	station_depths_import
WHERE	line = 'District'



