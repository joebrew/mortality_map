library(cism)
library(tidyverse)
library(rgdal)
library(sp)
library(maptools)
library(readxl)
library(tidyr)
library(leaflet)
library(RColorBrewer)

# # # Get census data
# # Load in census data
# if('census_data.RData' %in% dir('data')){
#   load('data/census_data.RData')
# } else {
#   residency <- cism::get_data(tab = 'residency',
#                               dbname = 'openhds',
#                               port = 3306)
#   individual <- cism::get_data(tab = 'individual',
#                                dbname = 'openhds',
#                                port = 3306)
#   location <- cism::get_data(tab = 'location',
#                              dbname = 'openhds',
#                              port = 3306)
#   save(residency,
#        individual,
#        location,
#        file = 'data/census_data.RData')
# }
# 
# # Correct variable names
# individual <- individual %>%
#   mutate(permid = lastName)
# 
# # Remove those not in study area
# individual <- individual %>%
#   filter(!grepl('HH', permid),
#          permid != '9999-999-99') %>%
#   filter(!duplicated(permid),
#          !is.na(permid),
#          permid != '')
# 
# # Create a time at risk dataset for the opd period
# if('cleaned_time_at_risk.RData' %in% dir('data')){
#   load('data/cleaned_time_at_risk.RData')
# } else {
#   
#   # Create time at risk
#   rr <- residency %>%
#     filter(startDate >= '2010-01-01' &
#              startDate <= '2012-12-31')
#   ii <- individual %>%
#     filter(dob <= '1994-01-01',
#            dob >= '1963-01-01')
#   tar <- cism::create_time_at_risk(residency = rr,
#                                    individual = ii,
#                                    location = location)
#   
#   # Keep only those who are in the non-expanded zone
#   # (ie, bairro <= 3499) (need to confirm with charfudin)
#   expansion <- read_excel('data/Bairros de area de Expansao_Demo.xls',
#                           skip = 3)$Bairro
#   tar$bairro <- as.numeric(as.character(substr(tar$locationName, 1, 4)))
#   tar <- tar %>%
#     filter(!bairro %in% expansion)
#   
#   # Get n at risk for June 1, 2010 and June 1, 2012
#   etar <- 
#     tar %>%
#     mutate(at_risk_2010 = 
#              startDate <= '2010-06-01' &
#              endDate >= '2010-06-01',
#            at_risk_2012 = 
#              startDate <= '2012-06-01' &
#              endDate >= '2012-06-01')
#   
#   # Since 2010 is not trustable, let's just use 2012
#   etar <- etar %>%
#     mutate(at_risk_2010 = at_risk_2012) %>%
#     filter(at_risk_2010) %>%
#     filter(!duplicated(individual_uuid))
#   
#   # Join to individuals
#   ii <- ii %>%
#     left_join(etar %>%
#                 dplyr::select(at_risk_2010,
#                               at_risk_2012,
#                               individual_extId),
#               by = c('extId' = 'individual_extId'))
#   
#   # Keep only those who were 18-50 at the time
#   ii <- ii %>%
#     filter(!is.na(dob)) %>%
#     mutate(dob = as.Date(dob)) %>%
#     mutate(age_in_2010 = as.numeric(as.Date('2010-06-01') - dob) / 365.25,
#            age_in_2012 = as.numeric(as.Date('2012-06-01') - dob) / 365.25) %>%
#     mutate(at_risk_2010 = 
#              at_risk_2010 &
#              age_in_2010 >= 18 &
#              age_in_2010 <= 50,
#            at_risk_2012 = 
#              at_risk_2012 &
#              age_in_2012 >= 18 &
#              age_in_2012 <= 50)
#   
#   # Save a copy for faster later use
#   save(ii,
#        file = 'data/cleaned_time_at_risk.RData')
# }

credentials <- credentials_extract()
credentials$dbname <- 'dss'
co <- credentials_connect(credentials)

# Get data for census
if('dss_data.RData' %in% dir('data')){
  load('data/dss_data.RData')
} else {
  tables <- c('av_adult',
              'household',
              'household_details',
              'member',
              'member_details',
              'migration_history')
  for (i in 1:length(tables)){
    x <- get_data(tab = tables[i],
                  connection_object = co)
    assign(tables[i],
           x)
    rm(x)
    save(av_adult,
         household,
         household_details,
         member,
         member_details,
         migration_history,
         file = 'data/dss_data.RData')
    
}
}


# # Read in coordinates from Aura
# ccords <- read_csv('spatial/Coordenadas.csv')

# Clean up member
member <- member %>%
  filter(!perm_id %in% c('9999-999-99'),
         !is.na(perm_id)) %>%
  filter(!duplicated(perm_id)) %>%
  filter(!grepl('h', tolower(perm_id)))
member$dob <- as.Date(substr(member$dob, 1, 10))
member$start_date <- as.Date(member$start_date)
member$end_date <- as.Date(member$end_date, format = '%Y-%m-%d')
# Get zone
member$zone_number <- as.numeric(substr(member$house_number, 1, 2))

# Get a death date
member$death_date <-
  ifelse(member$end_type == 'DTH',
         as.character(member$end_date),
         NA)
member$death_date <- as.Date(member$death_date)
member$death_year <- as.numeric(format(member$death_date, '%Y'))

# Create a multi-year copy of member
years <- 2010:2016
for (i in 1:length(years)){
  x <- member %>%
    mutate(year = years[i])
  if(i == 1){
    member_expanded <- x
  } else {
    member_expanded <- bind_rows(member_expanded,
                                 x)
  }
}

# Clean out from member_expanded those who hadn't yet entered or already exited
member_expanded <-
  member_expanded %>%
  filter(start_date <= 
           as.Date(paste0(year, '-01-01')),
         is.na(end_date) | 
           end_date >= 
           as.Date(paste0(year, '-01-01')))

# Get a boolean died this year or not
member_expanded$died_this_year <-
  member_expanded$death_year == member_expanded$year

# Get age
member_expanded <-
  member_expanded %>%
  mutate(years_of_age = 
           as.numeric(as.Date(paste0(year, '-01-01')) -
                        dob) / 365.25) %>%
  filter(years_of_age >= 0) %>%
  filter(years_of_age <= 120)


# Get a death rate by age group
member_expanded <-
  member_expanded %>%
  mutate(age_group = 
           ifelse(age >= 18 & age <= 27, '18-27',
                  ifelse(age > 27 &
                           age <= 37, '28-37',
                         ifelse(age > 37 & age <= 47,
                                '38-47',
                                ifelse(age > 47 &
                                         age <=50,
                                       '48-50',
                                       NA)))))


# Get death rate by age group
age_specific_death_rate <-
  member_expanded %>%
  group_by(age_group) %>%
  summarise(expected_death_rate = length(which(died_this_year)) / n(),
            n = n()) %>%
  mutate(p = n / sum(n)) %>%
  dplyr::select(-n)

# Get population and deaths per zone per year
pop <-
  member_expanded %>%
  filter(years_of_age >= 18,
         years_of_age <= 50) %>%
  group_by(zone_number,
           year) %>%
  summarise(pop = n(),
            deaths = length(which(died_this_year)))

# Get all pop too
all_pop <-
  member_expanded %>%
  group_by(zone_number,
           year) %>%
  summarise(all_pop = n(),
            all_deaths = length(which(died_this_year)))

# Get by age too
pop_age <-
  member_expanded %>%
  filter(years_of_age >= 18,
         years_of_age <= 50) %>%
  group_by(zone_number,
           year,
           age_group) %>%
  summarise(pop = n(),
            deaths = length(which(died_this_year))) %>%
  left_join(age_specific_death_rate) %>%
  mutate(real_death_rate = deaths / pop) %>%
  filter(!is.na(age_group)) %>%
  group_by(zone_number,
           year) %>%
  mutate(fake_pop = sum(pop) * p) %>%
  ungroup %>%
  mutate(fake_deaths = real_death_rate * fake_pop) %>%
  group_by(zone_number,
           year) %>%
  summarise(adjusted_death_rate = sum(fake_deaths) / sum(pop))

# Join to pop
pop <- left_join(pop, pop_age)
pop$death_rate <- pop$deaths / pop$pop

# Create an object called "mortality"
mortality <-
  member_expanded %>%
  filter(years_of_age >= 18,
         years_of_age <= 50) %>%
  group_by(zone_number,
           year,
           age_group) %>%
  summarise(pop = n(),
            deaths = length(which(died_this_year))) %>%
  # left_join(age_specific_death_rate) %>%
  mutate(real_death_rate = deaths / pop) %>%
  rename(n_deaths = deaths) %>%
         mutate(p_deaths = n_deaths / pop) %>%
  filter(!is.na(age_group))
  
# create a frow for agg
mort_two <-
  member_expanded %>%
  filter(years_of_age >= 18,
         years_of_age <= 50) %>%
  group_by(zone_number,
           year) %>%
  summarise(pop = n(),
            deaths = length(which(died_this_year))) %>%
  mutate(real_death_rate = deaths / pop) %>%
  rename(n_deaths = deaths) %>%
  mutate(p_deaths = n_deaths / pop) %>%
  mutate(age_group = '18-50')
mortality <-
  bind_rows(mortality,
            mort_two)


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

# # Exclude some zones
# exclude_these <- c(28, 29, 31, 32)
# 
# # Also exclude 34 and 33 since they're far
# exclude_these <- c(exclude_these, c(33, 34))
# 
# zonas <- zonas[!zonas$zone_number %in% exclude_these,]

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

# years <- 2010:2016
# out <- list()
# for (i in 1:length(years)){
#   out[[i]] <- read_mortality(year = years[i])
# }
# mortality <- bind_rows(out)

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
                      age_group = '18-50'){
  the_year <- year
  the_age_group <- age_group
  sub_mortality <- mortality %>%
    filter(year == the_year)
  sub_mortality <- sub_mortality %>%
    filter(age_group == the_age_group)
  spatial_data <- zonas
  spatial_data@data <-
    left_join(x = spatial_data@data,
              y= sub_mortality,
              by = 'zone_number') %>%
    dplyr::select(-real_death_rate)
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


# Define function for hotspots
hotspot <- function(joined_object = x,
                    cases,
                    population){
  
  # require(rsatscan)
  # invisible(ss.options(reset=TRUE))
  
  # # Join to hiv too
  # hiv <- afepi %>% 
  #   filter(test_year == joined_object@data$year[1]) %>%
  #   group_by(zone_number) %>%
  #   summarise(n_positive = length(which(Resulttest == 'Positivo')),
  #             n = n())
  # joined_object@data <-
  #   left_join(joined_object@data,
  #             y = hiv)
  
  require(SpatialEpi)
  
  # Create centroids
  centroids <- as.matrix(coordinates(zonas))
  # centroids <- centroids[!is.na(population)]
  # cases <- cases[!is.na(population)]
  # zonas <- zonas[!is.na(population),]
  # population <- population[!is.na(population)]
  
  # Define cases
  cases[is.na(cases)] <- mean(cases, na.rm = TRUE)
  population[is.na(population)] <- mean(population, na.rm = TRUE)
  
  n_strata <- nrow(zonas@data)
  # expected_cases <- expected(population, cases, n_strata)
  expected_cases <- sum(cases, na.rm = TRUE)
  expected_cases <- expected_cases * (population / sum(population, na.rm = TRUE))
  expected_cases[is.na(expected_cases)] <- mean(expected_cases, na.rm = TRUE)
  
  # Set paramaters
  pop.upper.bound <- 0.5
  n.simulations <- 999
  alpha.level <- 0.05
  plot <- FALSE
  
  
  # Run satscan
  # binomial <- kulldorff(geo = centroids,
  #                 cases = cases,
  #                 population = population,
  #                 pop.upper.bound = pop.upper.bound,
  #                 n.simulations = n.simulations,
  #                 alpha.level = 0.05,
  #                 plot = FALSE)
  poisson <- kulldorff(geo = centroids,
                        cases = cases,
                        population = population,
                        expected.cases = expected_cases,
                        pop.upper.bound = pop.upper.bound,
                        n.simulations = n.simulations,
                        alpha.level = 0.05,
                        plot = FALSE)
  
  # # Run bayes cluster
  # out <- bayes_cluster(y = cases, 
  #                      E = expected_cases, 
  #                      population = population, 
  #                      sp.obj = zonas, 
  #                      centroids = centroids, 
  #                      max.prop = 0.5, 
  #                      shape = c(0,1), 
  #                      rate = c(0,1), 
  #                      J = 10, 
  #                      pi0 = 0.95,
  #               n.sim.lambda = 10^4, 
  #               n.sim.prior = 10^5, 
  #               n.sim.post = 10^5, 
  #               burnin.prop = 0.1,
  #               theta.init = vector(mode="numeric", length=0))
  
  # # Besag-newell
  # out <- besag_newell(geo = centroids, 
  #                     population, cases, 
  #                     expected.cases= expected_cases, 
  #                     k = sum(cases), 
  #                     alpha.level = alpha.level)
  
  # get clusters
  cluster <- poisson$most.likely.cluster$location.IDs.included
  
  # Plot
  plot(zonas,axes=TRUE)
  plot(zonas[cluster,],add=TRUE,col="red")
}
