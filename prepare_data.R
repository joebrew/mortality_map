library(cism)
library(tidyverse)
library(rgdal)
library(sp)
library(maptools)
library(readxl)
library(tidyr)
library(leaflet)
library(RColorBrewer)

# # Get census data
# Load in census data
if('census_data.RData' %in% dir('data')){
  load('data/census_data.RData')
} else {
  residency <- cism::get_data(tab = 'residency',
                              dbname = 'openhds',
                              port = 3306)
  individual <- cism::get_data(tab = 'individual',
                               dbname = 'openhds',
                               port = 3306)
  location <- cism::get_data(tab = 'location',
                             dbname = 'openhds',
                             port = 3306)
  save(residency,
       individual,
       location,
       file = 'data/census_data.RData')
}

# Create a time at risk dataset for the opd period
if('cleaned_time_at_risk.RData' %in% dir('data')){
  load('data/cleaned_time_at_risk.RData')
} else {
  
  # Create time at risk
  rr <- residency %>%
    filter(startDate >= '2010-01-01' &
             startDate <= '2012-12-31')
  ii <- individual %>%
    filter(dob <= '1994-01-01',
           dob >= '1963-01-01')
  tar <- cism::create_time_at_risk(residency = rr,
                                   individual = ii,
                                   location = location)
  
  # Keep only those who are in the non-expanded zone
  # (ie, bairro <= 3499) (need to confirm with charfudin)
  expansion <- read_excel('data/Bairros de area de Expansao_Demo.xls',
                          skip = 3)$Bairro
  tar$bairro <- as.numeric(as.character(substr(tar$locationName, 1, 4)))
  tar <- tar %>%
    filter(!bairro %in% expansion)
  
  # Get n at risk for June 1, 2010 and June 1, 2012
  etar <- 
    tar %>%
    mutate(at_risk_2010 = 
             startDate <= '2010-06-01' &
             endDate >= '2010-06-01',
           at_risk_2012 = 
             startDate <= '2012-06-01' &
             endDate >= '2012-06-01')
  
  # Since 2010 is not trustable, let's just use 2012
  etar <- etar %>%
    mutate(at_risk_2010 = at_risk_2012) %>%
    filter(at_risk_2010) %>%
    filter(!duplicated(individual_uuid))
  
  # Join to individuals
  ii <- ii %>%
    left_join(etar %>%
                dplyr::select(at_risk_2010,
                              at_risk_2012,
                              individual_extId),
              by = c('extId' = 'individual_extId'))
  
  # Keep only those who were 18-50 at the time
  ii <- ii %>%
    filter(!is.na(dob)) %>%
    mutate(dob = as.Date(dob)) %>%
    mutate(age_in_2010 = as.numeric(as.Date('2010-06-01') - dob) / 365.25,
           age_in_2012 = as.numeric(as.Date('2012-06-01') - dob) / 365.25) %>%
    mutate(at_risk_2010 = 
             at_risk_2010 &
             age_in_2010 >= 18 &
             age_in_2010 <= 50,
           at_risk_2012 = 
             at_risk_2012 &
             age_in_2012 >= 18 &
             age_in_2012 <= 50)
  
  # Save a copy for faster later use
  save(ii,
       file = 'data/cleaned_time_at_risk.RData')
}

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
# Get a test year
afepi$Testdate <- as.Date(afepi$Testdate,
                          format = '%m/%d/%Y')
afepi$Birthdate <- as.Date(afepi$Birthdate,
                           format = '%m/%d/%Y')
afepi$test_year <- as.numeric(format(afepi$Testdate, '%Y'))

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

# FUNCTIONS

# Define function for getting figure numbers
figure_number <- 
  function (caption_text = NULL, make_grey = TRUE, make_italic = TRUE, 
            make_small = TRUE, make_centered = TRUE) {
    if(!exists('i')){
      i <<- 0
    }
    i <<- i + 1
    if (is.null(caption_text)) {
      caption_text <- ""
    }
    else {
      caption_text <- paste0(": ", caption_text)
    }
    if (make_grey) {
      text_out <- paste0("<span style=\"color:grey\">", paste("Figure ", 
                                                              i, caption_text, sep = ""), "</span>", collapse = "")
    }
    else {
      text_out <- paste("Figure ", a, caption_text, sep = "")
    }
    if (make_italic) {
      text_out <- paste0("<i>", text_out, "</i>")
    }
    if (make_small) {
      text_out <- paste0("<small>", text_out, "</small>")
    }
    if (make_centered) {
      text_out <- paste0("<center>", text_out, "</center>")
    }
    return(text_out)
  }


# Define function for nice data tables
make_pretty <-
  function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE, 
            cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y", 
            round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE, 
            data_table = TRUE, nrows = 5) {
    require(Hmisc)
    require(DT)
    require(ggplot2)
    require(scales)
    column_names <- names(the_table)
    the_table <- data.frame(the_table)
    names(the_table) <- column_names
    classes <- lapply(the_table, function(x) {
      unlist(class(x))[1]
    })
    if (cap_columns) {
      names(the_table) <- Hmisc::capitalize(names(the_table))
    }
    if (remove_underscores_columns) {
      names(the_table) <- gsub("_", " ", names(the_table))
    }
    for (j in 1:ncol(the_table)) {
      the_column <- the_table[, j]
      the_class <- classes[j][1]
      if (the_class %in% c("character", "factor")) {
        if (cap_characters) {
          the_column <- as.character(the_column)
          the_column <- Hmisc::capitalize(the_column)
        }
        if (remove_line_breaks) {
          the_column <- gsub("\n", " ", the_column)
        }
      }
      else if (the_class %in% c("POSIXct", "Date")) {
        the_column <- format(the_column, format = date_format)
      }
      else if (the_class %in% c("numeric", "integer")) {
        the_column <- round(the_column, digits = round_digits)
        if (comma_numbers) {
          the_column <- scales::comma(the_column)
        }
      }
      the_table[, j] <- the_column
    }
    if (remove_row_names) {
      row.names(the_table) <- NULL
    }
    if (data_table) {
      the_table <- DT::datatable(the_table, options = list(pageLength = nrows), 
                                 rownames = FALSE)
    }
    return(the_table)
  }


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
plot_joined_data <- function(joined_object,
                             fill_var = NULL,
                             legend_title = 'Mortality\nrate'){
  require(tidyr)
  require(broom)
  if(is.null(fill_var)){
    fill_var <- joined_object@data$p_deaths
  } 
  joined_object@data[,'var'] <- fill_var
  
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
    geom_polygon(aes(fill = var)) +
    coord_map() +
    scale_fill_gradient2(name = legend_title,
                         low=palette[1],mid=palette[5],high=palette[9]) +
    theme_cism() +
    labs(x = 'Longitude',
         y = 'Latitude')
  return(g)
}

# Define function for plotting via leaflet
plot_joined_data_leaflet <- function(joined_object,
                                     fill_var = NULL,
                                     popup_text1 = NULL,
                                     popup_var1 = NULL,
                                     popup_text2 = NULL,
                                     popup_var2 = NULL,
                                     legend_title = 'Mortality\nrate'){
  
  if(is.null(fill_var)){
    fill_var <- joined_object@data$p_deaths
  } 
  joined_object@data[,'var'] <- fill_var
  
  if(is.null(popup_text1)){
    popup_text1 <- 'Mortality rate'
  }
  
  if(is.null(popup_var1)){
    popup_var1 <- joined_object@data$p_deaths
  }
  
  if(is.null(popup_text2)){
    popup_text2 <- 'Number of deaths'
  }
  
  if(is.null(popup_var2)){
    popup_var2 <- joined_object@data$n_deaths
  }
  
  # Define a palette
  palette <- colorNumeric("Oranges", NULL, n = 4)
  
  # Define a popup
  popup <- paste0("<strong>Zone number: </strong>", 
                  joined_object@data$zone_number, 
                  "<br><strong>",
                  popup_text1,
                  ": </strong>", 
                  popup_var1,
                  "<br><strong>",
                  popup_text2,
                  ": </strong>",
                  popup_var2)
  
  leaflet(data = joined_object) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~palette(joined_object@data$var),  ## we want the polygon filled with 
                ## one of the palette-colors
                ## according to the value in student1$Anteil
                fillOpacity = 0.8,         ## how transparent do you want the polygon to be?
                color = "darkgrey",       ## color of borders between districts
                weight = 1.5,            ## width of borders
                popup = popup) %>%
    addLegend("bottomright", 
              pal = palette, 
              values = ~joined_object@data$var,
              title = legend_title,
              opacity = 0.8)
}

# Define function for joining hiv_data to spatial data
join_data_hiv <- function(year = 2010,
                          condition = 'Age >= 18 & Age <=50'){
  the_year <- year
  sub_afepi <- afepi %>%
    filter(test_year == the_year) %>%
    filter_(condition) %>%
    filter(Resulttest != 'Indeterminado') %>%
    group_by(zone_number) %>%
    summarise(positives = length(which(Resulttest == 'Positivo')),
              negatives = length(which(Resulttest == 'Negativo')),
              denom = n()) %>%
    mutate(hiv_rate = positives / denom * 100)
  spatial_data <- zonas
  spatial_data@data <-
    left_join(x = spatial_data@data,
              y= sub_afepi,
              by = 'zone_number')
  return(spatial_data)
}
