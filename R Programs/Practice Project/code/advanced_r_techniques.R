# R program practice - advanced 

#Walks through example data with Redfin and incorporates additional packages 

#################################################################################################################################
# SET UP                                                                                           
#################################################################################################################################

# Clean Session -----------------------------------------------------------------------------------------------------------------#

rm(list = ls())

# Set Working Directory ---------------------------------------------------------------------------------------------------------#

# to see current directory

getwd()

# Assign Working Directory - example below
setwd()
setwd("C:/Users/username/data/")


# Install packages and load libraries -------------------------------------------------------------------------------------------#

#Default packages can be found under the packages tab to the right side of the R window. 

# Install - comment out if already installed
install.packages("stargazer")
install.packages('tidyverse')
install.packages('broom')
install.packageS('scales')
install.packageS("xlsx")
install.packages("geosphere")

# Load
library(tidyverse)
library(stargazer)
library(broom)
library(scales)
library(xlsx)
library(geosphere)


#################################################################################################################################
# Automation with New Data                                                                                 
#################################################################################################################################

# Import data and Combine ------------------------------------------------------------------------------------------------------#

# Using Redfin data

# Import Redfin csv file 01
property_data <- read.csv("data/property_redfin_01.csv")

property_data2 <- read_csv("data/property_redfin_02.csv")

# Identical(names(df)): To see if datasets belong together
identical(names(property_data2), names(property_data))

# Check the number of rows (obs) in each dataset
nrow(property_data)
nrow(property_data2)

# Combining data with the same columns the old fashioned way

property_data_bound <- bind_rows(property_data, property_data2)

nrow(property_data_bound)


# General Structure: Loops  ----------------------------------------------------------------------------------------------------#

# Often when conducting analysis, we have to repeat a task or caluclation several times. It's not always practical to do these 
# steps manually, so 'for loops' help automate these tasks


# Loops often use function, defined as such:

# function_name <- function(arguments) {
#   Code that does your task
# }

# General structure of a for loop:

# for(*variable* in *index*) {
#   *code to be executed*
# }  


# Example: Squaring a set of numbers

for(num in seq(1,8)) {
  print(num^2)
}

# Example: Iteration over lists 

months <- c("January","February", "March", "April", "May", "June", "July", "August", "September", "October","November","December")

for(mon in months){
  print(mon)
}

for(mon in months){
  print(paste(mon, "is my favorite month"))
}

#Example: Using indices

pets <- c('dog', 'cat', 'mouse')

length(pets)

#print indices
for(pet in seq(1, length(pets))){
  print(pet)
}

#print elements
for(pet in seq(1, length(pets))){
  print(pets[pet])
}


# Advanced Loops  -------------------------------------------------------------------------------------------------------------#

# Save and update variables using loops

#Example: Saving cumulative sum results by creating a new object to store values

num_vec <- seq(1,5) 

cumulSum <- numeric()

for(currentNum in seq(1, length(num_vec))){
  
  #Calculate at current iteration
  currentCumulSum <- sum(num_vec[1:currentNum])
  
  #Write object during loop to record progress
  cumulsum <- c(cumulSum, currentCumulSum)
}

print(cumulSum)


# if/else loops 
# if(logical argument){
#   ## code to be executed
# } else{
#   ## code to be executed
# }

#Example: Using modulo operator (%%) to characterize numbers as even or odd and return remainder after division

testNum <- 12

if(testNum %% 2 == 0){
  print(paste(testNum, "is EVEN"))
} else{
  print(paste(testNum, "is ODD"))
}



# Back to Redfin Data ----------------------------------------------------------------------------------------------------------#


# Get list of all data set names using paste0 which takes any strings or vectors that can be coerced into a character and makes
# one string

property_files <- paste0("data/property_redfin_0",
                         1:8, ".csv")

location_files <- paste0("data/location_redfin_0",
                         1:8, ".csv")

# combine them into a single vector
files <- c(property_files, location_files)
files

# str_detect() adds another layer to help determine how the loop should be when combining all the data sets together
# In this case, we need to know whether a dataset is a property or location file. Str_detect() tells us whether a 
# string contains a certain word/phrase 

str_detect("property_redfin_01.csv", "property") # TRUE
str_detect("location_redfin_01.csv", "property") # FALSE
str_detect(c("property_redfin_01.csv",
             "location_redfin_01.csv"),
           "location") # c(FALSE, TRUE)


# Reading in Redfin data with a loop -------------------------------------------------------------------------------------------#

property_data <- data.frame() # initialize empty data.frame
location_data <- data.frame() # initialize empty data.frame

for(file in files){
  
  if(str_detect(file, "property")){ # this is a property file, add to property_data
    data <- read.csv(file)
    property_data <- bind_rows(property_data, data)
  } else if(str_detect(file, "location")){ # location file
    data <- read.csv(file)
    location_data <- bind_rows(location_data, data)
  }
}

dim(location_data)
dim(property_data) # same number of rows as property data



# Joining the Data -------------------------------------------------------------------------------------------------------------#

# To join data, need at least one column of the same variable. The operator %in% tells us which elements in one object are in 
# another

property_names <- names(property_data)
location_names <- names(location_data)

overlap <- property_names[property_names %in% location_names]
overlap

# There are several types of joins: enter ?jplyr::join in the console to see the description of the others
joined_data <- full_join(property_data,
                         location_data,
                         by = overlap)


# Clean up the data and then show relationship between sq feet and price

joined_data <- joined_data %>% 
  mutate(PRICE = str_replace(PRICE, "USD", "")) %>%
  filter(!is.na(PRICE)) %>%
  mutate(PRICE = as.numeric(PRICE))

joined_data %>%
  ggplot(aes(x = SQUARE.FEET, y = PRICE)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relationship between price and home size",
       y = "USD",
       x = "Sq ft",
       caption = "Data from Redfin") +
  scale_y_continuous("USD", labels = dollar) 

# Some more cleaning steps - gsub() to replace words, str_split_fixed() to separate columns, and str_trim() to remove excess space
# str_sub() and str_length() can also be used to clean up text, see the line with zip codes below

joined_data <- joined_data %>%
  mutate(CITY = str_trim(str_split_fixed(CITY_STATE, ",", n = 2)[,1]),
         STATE = str_to_upper(str_trim(str_split_fixed(CITY_STATE, ",", n = 2)[, 2])), 
         LATITUDE = as.numeric(str_split_fixed(LAT_LON, "&", n = 2)[,1]), #important for a later step
         LONGITUDE = as.numeric(str_split_fixed(LAT_LON, "&", n = 2)[,2]),
         PROPERTY.TYPE = gsub("propertytype:", "", PROPERTY.TYPE),  #We know the property type because its the name of the column, so we don't need to value in the row
         ZIP_CODE = str_sub(ZIP_CODE, 2, str_length(ZIP_CODE) - 4)) #the zip codes have 4 trailing zeroes and we don't need those


state_average <- joined_data %>% 
  filter(STATE %in% c("VA", "DC", "MD")) %>%
  group_by(PROPERTY.TYPE, STATE) %>%
  summarise(price = mean(PRICE/SQUARE.FEET, na.rm = T))

state_average_plot <- state_average %>%
  ggplot(aes(x = STATE, y = price, fill = STATE)) +
  geom_bar(stat = "identity") +
  facet_wrap("PROPERTY.TYPE") +
  labs(title = "Price by property type",
       x = NULL,
       y = "USD/Sq ft.",
       caption = "Data from Redfin") +
  theme(plot.title = element_text(face = "italic", size = 14, color = "Blue"),
        axis.text.x = element_text(face = "bold", color = "green4"),
        plot.background = element_rect(fill = "beige", color = "red"),
        panel.background = element_rect(fill = "cadetblue1",
                                        color = "black", linetype = 3))
state_average_plot

##### PRACTICE ##### 
# 1) Taken joined_data and select price, square feet, and lot size
# 2) Recode any values that are missing in lot size by replacing the lot size with square feet (hint: use is.na and iselse in mutate)
# 3) Summarise mean price per lot size by property type and state
# 4) Create another data frame that looks at how price changes by zipcode


# Joining the Data: Metro Distance, creating functions, and function safety ----------------------------------------------------#

#Install metro data. We will merge this with the Redfin data and determine distance from the house to the metro

metro_data <- read.xlsx("data/Metro_lat_lon.xlsx")

# Combining property data and metro data requires 4 arguments: property long and lat values, metro long and lat values. Then we 
# need to caluclate the distance between the property and metro using the pythaogrean theorem 

# Below is a new function that takes the metro data and pulls lat and lon for the station, calculates distance to property
# and then appends that distance to the vector and converts output from meters to miles

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


# warning() and require() are useful for flagging when a package fails to load and alerting the user to that failure. This helps
# to understand why functions are not working and thus make appropriate changes


#First, create a distance function

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

# Simple distance function 

distance_function(joined_data$LONGITUDE[3],
                  joined_data$LATITUDE[3],
                  metro_data)

# Improved functionality with for loop

for(i in 1:nrow(joined_data)){
  joined_data$metro_distance[i] <- distance_function(joined_data$LONGITUDE[i],
                                                     joined_data$LATITUDE[i],
                                                     metro_data)
}

head(joined_data$metro_distance)


#Create a plot showing distance property distance to metro

joined_data %>% mutate(distance_tenth = round(metro_distance, 1)) %>%
  group_by(distance_tenth) %>%
  summarise(price = mean(PRICE/SQUARE.FEET, na.rm = T)) %>%
  ggplot(aes(x = distance_tenth, y = price)) +
  geom_line() +
  theme_light() +
  labs(title = "Price vs distance from metro station",
       y = "USD/Sq ft.",
       x = "Miles",
       caption = "Data from Redfin and WMATA")
