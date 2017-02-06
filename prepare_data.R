library(cism)
library(tidyverse)
library(rgdal)
library(sp)
library(maptools)
library(readxl)
library(tidyr)
library(leaflet)
library(RColorBrewer)

# Define a connection object
co <- credentials_connect(credentials_extract())

# # Get census data
# individual <- get_data('SELECT * FROM individual',
#                    dbname = 'openhds')

# Get some spatial data for manhica
moz3 <- moz3
man <- moz3[moz3@data$NAME_2 == 'Manhiça',]

# Get locations data (sent by Aura via email)
coordenadas <- read_csv('spatial/Coordenadas.csv')
# Convert to lat/long
coordinates(coordenadas) <- ~LongUTM+LatUTM
new_pro <- CRS("+init=epsg:3036")
proj4string(coordenadas) <- new_pro
coordenadas <- spTransform(coordenadas, CRS("+init=epsg:4326"))
# Create lng, lat columns
coordenadas$lng <- coordinates(coordenadas)[,1]
coordenadas$lat <- coordinates(coordenadas)[,2]

# Read in AFEPI data
afepi <- read_csv('data/afepi_X.csv')
# Make a family id
afepi$Family_id <- substr(afepi$perm_id, 1, 8)

# Read in a shapefile for bairros (sent by Faustion)
bairros <- readOGR('spatial/', 'region')

# Define the projection
proj4string(bairros) <- new_pro

# Convert to lat/long
bairros <- spTransform(bairros, CRS("+init=epsg:4326"))

# Convert bairros to zonas
# define a zone_number variable
bairros@data$zone_number <- substr(bairros@data$Zona, 1, 2)

# Dissolve by zone_number
zonas <- maptools::unionSpatialPolygons(bairros, IDs = bairros@data$zone_number)

# Make a dataframe
zonas_df <- data.frame(zone_number = sapply(slot(zonas, "polygons"), function(i) slot(i, "ID"))
 )
row.names(zonas_df) <- as.character(zonas_df$zone_number)
zonas_df$zone_number <- as.numeric(as.character(zonas_df$zone_number))
zonas <- SpatialPolygonsDataFrame(zonas,
                                zonas_df)

# Get locations for afepi participants
afepi <-
  left_join(x = afepi,
            y = coordenadas@data,
            by = 'Family_id')

# Get zone_numbers for afepi participants
afepi_sp <- afepi[!is.na(afepi$lng),]
coordinates(afepi_sp) <- ~lng+lat
proj4string(afepi_sp) <- proj4string(zonas)
afepi_sp$zone_overlay <- over(afepi_sp, polygons(zonas))

afepi <- left_join(x = afepi,
                   y = afepi_sp@data %>%
                     dplyr::select(perm_id, zone_overlay),
                   by = 'perm_id')

# Use alternative method for zone number (from permid)
afepi$zone_number <- as.numeric(substr(afepi$perm_id, 1, 2))

# Read in the mortality data sent by Charfudin
read_mortality <- function(year){
  
  # Read in the data as are
  mortality <- read_excel('data/Mortes de adultos_2010_2017.xlsx')
  
  # Identify the index where our year begins
  start_here <- which(grepl(year, names(mortality)))
  
  # Get end points
  end_points <- which(grepl('Ano', names(mortality)))
  end_point <- end_points[end_points > start_here][1] - 1
  if(length(end_point) == 0){
    end_point <- ncol(mortality)
  }
  
  # Select columns
  # Read again, selecting only for our year
  this_year <- read_excel('data/Mortes de adultos_2010_2017.xlsx',
                          skip = 3)
  this_year <- this_year[,start_here:end_point]
  this_year <- this_year[,2:ncol(this_year)]
  
  # Give explicit names
  names(this_year) <- c('zone_number',
                        'age_15_17',
                        'age_18_50',
                        'age_51_69',
                        'total')
  
  # Remove the total and below
  total_in <- any(grepl('Total', this_year$zone_number))
  if(total_in){
    this_year <-
      this_year[1:(which(grepl('Total', this_year$zone_number)) - 1),]
  }
  
  
  # Make zone number numeric
  this_year$zone_number <- as.numeric(as.character(this_year$zone_number))
  
  # Add a year column
  this_year$year <- year
  
  # Gather
  this_year <- gather(this_year, age_group, deaths, age_15_17:total)
  
  # Clean up deaths column
  this_year$n_deaths <- 
    as.numeric(unlist(lapply(strsplit(this_year$deaths, ' '), function(x){x[1]})))
  this_year$p_deaths <- 
    as.numeric(unlist(lapply(strsplit(this_year$deaths, ' '), function(x){
      y <- x[2]
      y <- gsub('[(]|[)]|%', '', y)
      y
      })))
  
  # Remove unecessary columns
  this_year$deaths <- NULL
  
  # Return this year's data
  return(this_year)
}

years <- 2010:2016
out <- list()
for (i in 1:length(years)){
  out[[i]] <- read_mortality(year = years[i])
}
mortality <- bind_rows(out)

# Define function for joining mortality to zonas shp
join_data <- function(year = 2010,
                      age_group = 'age_15_17'){
  the_year <- year
  the_age_group <- age_group
  sub_mortality <- mortality %>%
    filter(year == the_year,
           age_group == the_age_group)
  spatial_data <- zonas
  spatial_data@data <-
    left_join(x = spatial_data@data,
              y= sub_mortality,
              by = 'zone_number')
  return(spatial_data)
}

# Define function for plotting joined data
plot_joined_data <- function(joined_object){
  
  out_df <- joined_object
  
  out <- tidy(out_df, id = out@data$zone_number)
  out$id_numeric <- as.numeric(as.character(out$id))
  out <- left_join(x = out,
                   y = out_df@data %>%
                     mutate(id_numeric = zone_number),
                   by = 'id_numeric')
  palette <- brewer.pal(n = 9, 'Oranges')
  g <- ggplot(data = out,
         aes(x = long,
             y = lat,
             group = group)) +
    geom_polygon(aes(fill = p_deaths)) +
    coord_map() +
    scale_fill_gradient2(name = 'Mortality\nrate',
                         low=palette[1],mid=palette[5],high=palette[9]) +
    theme_cism() +
    labs(x = 'Longitude',
         y = 'Latitude')
  return(g)
}

# Define function for plotting via leaflet
plot_joined_data_leaflet <- function(joined_object){
  
  # Define a palette
  palette <- colorNumeric("Oranges", NULL, n = 4)
  
  # Define a popup
  popup <- paste0("<strong>Zone number: </strong>", 
                        joined_object@data$zone_number, 
                        "<br><strong>Mortality rate: </strong>", 
                        joined_object@data$p_deaths,
                  "<br><strong>Number of deaths: </strong>", 
                  joined_object@data$n_deaths)

  leaflet(data = joined_object) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~palette(joined_object@data$p_deaths),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.8,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup) %>%
    addLegend("bottomright", 
              pal = palette, 
              values = ~joined_object@data$p_deaths,
              title = "Mortality rate",
              opacity = 0.8)
}

# Define function for getting hiv data
