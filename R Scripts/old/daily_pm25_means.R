library(openair)

## Get daily background averages for London from OpenAir

background_pm25 <- importKCL(site = "kc1", year = c(2015,2016), pollutant = "pm25", met = FALSE,
          units = "mass", extra = FALSE)

background_pm25 <- data.frame(background_pm25, day = as.Date(format(background_pm25$date)))

background_pm25 <- aggregate(pm25 ~ day, background_pm25, mean)