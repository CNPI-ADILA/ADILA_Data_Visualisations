# directories
data_dir <- "O:/formatted_data/amu_data"  #MIGHT NEED TO USE RELATIVE PATH WHEN DEPLOYING APP ON THE CLOUD (i.e., shinyapps.io) OR AN INTERNAL SERVER???

# load data
d <- paste0(data_dir,"/row_level_ddd.csv") #NB: also have data filtered /by_administration and /by_aware categories
data_original <- read.csv(d)

# reformat/clean
  # renaming columns
  data_reformatted <- data_original %>% 
    select(source_title, who_region, country, antimicrobials, aware_category, route_of_administration, sector, effective_from, standard_units, ddd, did, di) %>%
    mutate(year = as.numeric(gsub("-01-01", "", effective_from))) %>% select(-effective_from)
  # reshaping wide to long
  data_reformatted <- gather(data_reformatted, metric, value, standard_units, ddd, did, di, factor_key = TRUE)
  # collapsing stratification in other categories (e.g., age_appropriate (true/false), manufacturer, etc.)
  data_reformatted <- data_reformatted %>% 
    group_by(source_title, who_region, country, antimicrobials, aware_category, route_of_administration, sector, year, metric) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
    ungroup()

# create summed cols for "All" antimicrobials/routes/countries/aware_categories/who_region/sector
source("collapse_by_category.R")
  
# change some country names that don't match the names in the shape file
data_for_visualisations <- data_for_visualisations %>%
  mutate(country = ifelse(country == "Russian Federation", "Russia", country)) %>%
  mutate(country = ifelse(country == "U.K. of Great Britain and Northern Ireland", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "United States of America", "United States", country)) %>%
  mutate(country = ifelse(country == "Viet Nam", "Vietnam", country)) %>%
  mutate(country = ifelse(country == "All", "Global", country))

# make values more manageable 
data_for_visualisations <- data_for_visualisations %>%
  mutate(value = case_when(metric == "ddd" ~ value/1000, metric == "standard_units" ~ value/1000, .default = value))

# aesthetics, functions etc.
  # country polygons 
  shape_file <- st_read("shapefile/GBD2020_simplified.shp")
  #shape_file <- rgdal::readOGR("shapefile/GBD2020_simplified.shp")
  shape_file <- shape_file[shape_file$loc_name %in% data_for_visualisations$country,]
  # colours
    # for choropleth (spatial)
    #colour_palette set within server code so that it adapts to range of values for selected inputs
    # for line plots (temporal)
    colours_country <- fread("colours/colour_scheme_country.csv")
    colours_antimicrobial <- fread("colours/colour_scheme_antimicrobial.csv")
    colours_route <- fread("colours/colour_scheme_route.csv")
    colours_aware <- fread("colours/colour_scheme_aware.csv")
  # multiple line-breaks
  linebreaks <- function(n){HTML(strrep(br(), n))}