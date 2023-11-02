### FREDR Loop Function

#Example model - to base off 

list <- list(
  series_id = c("CPIAUCSL", "CPIAUCNS", "CPIUFDSL", "CPIUFDNS", 
                "CPIENGSL", "CPIENGNS"),
  frequency = c("m")
)

uscpi_head <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100) %>%
  ungroup() %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "CPIAUCSL" ~ "cpi_headline_sa", 
                          series_id == "CPIAUCNS" ~ "cpi_headline_nsa",
                          series_id == "CPIUFDSL" ~ "cpi_food_sa",
                          series_id == "CPIUFDNS" ~ "cpi_food_nsa",
                          series_id == "CPIENGSL" ~ "cpi_energy_sa",
                          series_id == "CPIENGNS" ~ "cpi_energy_nsa"))


# From Github 

pause <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
} # use ~ value slightly greater than 0.5 seconds


seriesIDs <- c("CPIAUCSL")

# Define function to iterate through the series ID variables and return one long data.frame .
IDs.toDF = function(ids){
  bigdata = data.frame()
  for (i in ids){
    singleObs = fredr(i)
    bigdata = rbind(bigdata, singleObs)
    pause(0.53) # avoid hitting rate limit and incurring 20 sec. pause
  }
  bigdata = dcast(bigdata, date~series_id, fill = NA)
  return(bigdata)
}

#Run list of series and get results
df = IDs.toDF(seriesIDs)




#fredr Function 



fredr_function <- function(series_id, )


prop_metros_dist <- function(prop_long_lat, metro_data){
  output_distances <- c()
  
  for(metro in 1:nrow(metro_data)){
    # Pull out our longitude and latitude values for the metro station
    metro_long_lat <- c(metro_data$Longitude[metro],
                        metro_data$Latitude[metro])
    # Calculate the distance to our property
    distance <- distHaversine(p1 = prop_long_lat,
                              p2 = metro_long_lat)
    # append that distance to our distances vector
    output_distances <- c(output_distances, distance)
  }
  min(output_distances)/1609.344
}


distance_function <- function(LONG, LAT, metro_df){
  if(!require(geosphere)){ # what does require do?
    warning("geosphere package is not installed")
  } else{
    # We need to turn our LONG and LAT columns into a vector
    prop_long_lat <- c(LONG, LAT)
    # Now call our prop_metros_dist function from above
    out <- prop_metros_dist(prop_long_lat, metro_df)
    return(out)
  }
}


distance_function(joined_data$LONGITUDE[3],
                  joined_data$LATITUDE[3],
                  metro_data)
