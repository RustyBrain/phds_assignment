library(dplyr)
library(fingertipsR)
library(fingertipscharts)



df <- fingertips_data(20101) %>%
  filter(Sex == "Persons" &
           AreaType == "County & UA" &
           Timeperiod == "2017")

# map code - need to change this to the proper data
ons_api <- "https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson"
map1 <- fingertipscharts::map(df,
                              ons_api = ons_api,
                              area_code = AreaCode,
                              value = value,
                              fill = ComparedtoEnglandvalueorpercentiles,
                              title = "Low birth weight",
                              subtitle = "Across England")
map1
