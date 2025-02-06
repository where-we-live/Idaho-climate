#extract_climate_data.R
#who is the author
#date
#this is function which extracts climate data from the UI THREDDS climate server
#the inputs to the fuction are as follows: 
#coordinate - these are the point coordinates for the location to extract
#var_type - is the climate variable, which includes:


# Load necessary libraries
library(ncdf4)
library(dplyr)
library(lubridate)

# Function to extract climate data for specified coordinates
extract_climate_data <- function(opendap_url, coordinates, var_type) {

  # Initialize an empty data frame to store data for all coordinates
  all_climate_data <- data.frame()
  
  # Loop over each set of coordinates
  for (i in 1:nrow(coordinates)) {
    target_lon <- coordinates$lon[i]
    target_lat <- coordinates$lat[i]
    coord_label <- coordinates$label[i]  # Custom label for each point
    
    # Open the NetCDF file
    nc_data <- nc_open(opendap_url)
    
    # Identify variable name automatically
    var_name <- names(nc_data$var)[1]
    
    # Extract longitude, latitude, and time
    lon <- ncvar_get(nc_data, "lon")
    lat <- ncvar_get(nc_data, "lat")
    time <- ncvar_get(nc_data, "day")
    
    # Find the closest indices for target longitude and latitude
    lon_ind <- which.min(abs(lon - target_lon))
    lat_ind <- which.min(abs(lat - target_lat))
    
    # Set up start and count for data extraction
    start <- c(lon_ind, lat_ind, 1)
    count <- c(1, 1, -1)  # -1 retrieves all time steps
    
    # Extract data
    var_array <- ncvar_get(nc_data, var_name, start = start, count = count)
    nc_close(nc_data)  # Close the NetCDF file
    
    # Convert time to Date format
    start_date <- as.Date("1900-01-01")
    dates <- start_date + time
    
    # Create a data frame for the current coordinate point
    climate_df <- data.frame(date = dates, value = as.numeric(var_array), coord = coord_label)
    
    # Convert temperature from Kelvin to Celsius if var_type is temperature
    if (var_name == "daily_minimum_temperature"|var_name=="daily_maximum_temperature") {
      climate_df$value <- climate_df$value - 273.15
    }
   
    # Rename the value column based on the var_name
    colnames(climate_df)[which(names(climate_df) == "value")] <- var_name
    
    # Add day, month, and year columns
    climate_df <- climate_df %>%
      mutate(day = day(date), month = month(date), year = year(date))
    
    # Append the data to the main data frame
    all_climate_data <- bind_rows(all_climate_data, climate_df)
  }
  
  # Return the extracted climate data
  return(all_climate_data)
}
    
# Example coordinates
df_coordinates <- data.frame(
  lon = c(-116.394, -116.647, -116.556, -116.77),
  lat = c(46.859, 46.614, 46.799, 46.737),
  label = c("Bovill", "Kendrick", "Deary", "Troy")
)

# Example usage
opendap_url_temp <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc"
extracted_data <- extract_climate_data(opendap_url_temp, df_coordinates, "var_name")
View(extracted_data)

opendap_url_temp <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmn_1979_CurrentYear_CONUS.nc"
extracted_data <- extract_climate_data(opendap_url_temp, df_coordinates, "var_name")
View(extracted_data)

head(extracted_data)
summary(extracted_data)









extracted_data$coord<-as.factor(extracted_data$coord)

# Compute average monthly minimum temperature for each coordinate
monthly_avg_temp <- extracted_data %>%
  group_by(coord, month) %>%
  summarise(avg_min_temp = mean(daily_minimum_temperature, na.rm = TRUE), .groups = "drop")

# Create the climograph with all coordinates in one graph
ggplot(monthly_avg_temp, aes(x = month, y = avg_min_temp, color = coord, group = coord)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Climograph of Average Monthly Minimum Temperature",
       x = "Month",
       y = "Average Minimum Temperature (°C)",
       color = "Coordinate") +
  theme_minimal() +
  theme(legend.position = "bottom")


