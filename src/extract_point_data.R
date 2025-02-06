
## LOAD THE REQUIRED LIBRARY
library(ncdf4)
library(dplyr)
library(lubridate)

# Define the coordinates matrix (longitude, latitude)
coordinates <- matrix(c(-116.394, 46.859,  # Point 1 Bovill
                        -116.647, 46.614,  # Point 2 Kendrick 
                        -116.556, 46.799,  # Point 3 Deary
                        -116.77, 46.737),  # Point 4 Troy
                      ncol = 2, byrow = TRUE)

urltotal <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmx_1979_CurrentYear_CONUS.nc#fillmismatch"




extract_single_point_data <- function(urltotal, coordinates) {

### DEFINE THE URL
#urltotal <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmx_1979_CurrentYear_CONUS.nc#fillmismatch"

## OPEN THE FILE
nc <- nc_open(urltotal)
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")

lat_target = 45.5
lon_target =  - 117.5   

diff_lat <-  abs(lat - lat_target)
index_lat =which(diff_lat == min(diff_lat))
diff_lon<-  abs(lon- lon_target)
index_lon =which(diff_lon == min(diff_lon))

v3 <- nc$var[[1]]
lonsize <- v3$varsize[1]
latsize <- v3$varsize[2]
endcount <- v3$varsize[3]

## DEFINE OUR VARIABLE NAME
#var="precipitation_amount"
var="daily_maximum_temperature"

## READ DATA VARIABLE
data <- ncvar_get(nc, var, start=c(index_lon,index_lat,1),count=c(1,1,endcount))

## READ THE TIME VARIABLE
time <- ncvar_get(nc, "day", start=c(1),count=c(endcount))

## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html

# PUT EVERYTHING INTO A DATA FRAME
c <- data.frame(time,data)

df <- c %>% 
  mutate(month = lubridate::month(time),  #Month
         dow = lubridate::wday(time, week_start = 1), #Day of the week
         dom = lubridate::day(time),  #Day of the month
         year = lubridate::year(time))

df2 <- df %>%
  group_by(month, year) %>%
  summarise(total = mean(data))

df3 <- arrange(df2,year,month)
## CLOSE THE FILE
nc_close(nc)
return(df3)
}


output <- extract_single_point_data(urltotal, coordinates)


plot(output$total)
lines(output$total, col="blue")
