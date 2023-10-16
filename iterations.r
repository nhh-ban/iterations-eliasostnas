library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml") # reads the api link from the assignment


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata to a tibble, converting latestData to UTC format

source("functions/data_transformations.r") # calling function from data_transformations.R

stations_metadata_df <- 
  stations_metadata %>% # retrieve data from stations_metadata and call it stations_metadata_df
  transform_metadata_to_df() # using this function for stations_metadata to create stations_metadata_df

head(stations_metadata_df) # to show tibble



#### 3: Testing metadata
source("functions/data_tests.r") # retrieving functions from fie data_sets.r in the functions folder
test_stations_metadata(stations_metadata_df) # runs the function test_stations_metadata
# from the stations_metadata_df dataset

### 4.b: call vol_cry function

source("gql-queries/vol_qry.r") 

GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

### 5: Final volume query: 


stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% # showing results from one random station
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% #line 62
  ggplot(aes(x=from, y=volume)) + 
  geom_line() + 
  theme_classic()

# Problem 6 - making the plot prettier


stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) -> selected_station

data_for_plot <- selected_station %$% vol_qry(
  id = id,
  from = to_iso8601(latestData, -4),
  to = to_iso8601(latestData, 0)
) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes(station_name = selected_station$name[1])

data_for_plot %>%
  ggplot(aes(x=from, y=volume, color=station)) + 
  geom_line() + 
  labs(color="Traffic Station") + # adds legends
  theme_classic()








