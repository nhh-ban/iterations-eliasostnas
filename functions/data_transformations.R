library(lubridate)
library(anytime)
library(purrr)

# Problem 2 - transform stations_metadata to a tibble, converting latestData 
# to UTC format

transform_metadata_to_df <- function(data){
  df <- data[[1]] %>% 
  map(as_tibble) %>% 
  list_rbind() %>% 
  mutate(latestData = map_chr(latestData, 1, .default=NA_character_)) %>% 
  mutate(latestData = as_datetime(latestData, tz="UTC")) %>% 
  unnest_wider(location) %>% 
  unnest_wider(latLon)
  return(df)
}

# Problem 4 - Add a function called iso8601. The function should take *two* arguments: 
# a date time variable and an offset measured in days.
# The function should *return* the date time variable in ISO8601 format, with the offset added. 
# There should be a letter "Z" appended to the end of the date string, to indicate the the time zone is UTC. 


to_iso8601 <- function(datetime, offset_in_days) {
  adjusted_datetime <- datetime + days(offset_in_days) # Adjust the datetime by the offset
  iso_str <- iso8601(adjusted_datetime)  # Convert to ISO 8601 format
  return(paste0(iso_str, "Z")) # Append the 'Z' to indicate UTC timezone and return
}

# Test the function
print(to_iso8601(Sys.time(), 5))  # Adds 5 days to the current date-time and prints in ISO 8601 format

to_iso8601(as_datetime("2016-09-01 10:11:12"),0) #as_datetime part of lubridate for right date format
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)


# Problem 5. After completing task number 5 you should be able to run the entire iterations.r-script.
# In order to do that you must add a function transform_volumes() that transforms the json-return from 
# the API to a data frame that can be used for plotting.

transform_volumes <- function(api_response, station_name = NULL) {
  edges <- api_response$trafficData$volume$byHour$edges
  
  df <- map_dfr(edges, function(edge) {
    tibble( # Create a tibble for each edge
      from = as_datetime(edge$node$from), # Convert 'from' timestamp to datetime format
      to = as_datetime(edge$node$to), # Convert 'to' timestamp to datetime format
      volume = edge$node$total$volumeNumbers$volume, # Extract the volume data
      station = station_name # Attach the  station name to each row
    )
  })
  
  return(df)
}






