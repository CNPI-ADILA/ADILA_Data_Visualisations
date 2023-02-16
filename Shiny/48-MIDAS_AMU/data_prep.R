# directories
#app_dir <- "H:/Shiny/MIDAS_AMU"
data_dir <- "O:/formatted_data/amu_data"  #MIGHT NEED TO USE RELATIVE PATH WHEN DEPLOYING APP ON THE CLOUD (i.e., shinyapps.io) OR AN INTERNAL SERVER???

# load data
d <- paste0(data_dir,"/all_ddd.csv") #NB: also have data filtered /by_administration and /by_aware categories
data_original <- read.csv(d)

# reformat/clean
  # renaming columns
  data_reformatted <- data_original %>% 
    select(fdef_short_name, antimicrobials, aware_category, route_of_administration, effective_from, ddd, did, di) %>%
    mutate(year = as.numeric(gsub("-01-01", "", effective_from))) %>% select(-effective_from) %>%
    rename(country = fdef_short_name) #CAN DELETE THIS IF IT CHANGES BACK TO BEING CALLED "country" (and replace fdef_short_name above)!!!
  # reshaping wide to long
  data_reformatted <- gather(data_reformatted, metric, value, ddd, did, di, factor_key = TRUE)
  # collapsing age_appropriate (true/false) stratification
  data_reformatted <- data_reformatted %>% 
    group_by(country, antimicrobials, aware_category, route_of_administration, year, metric) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
    ungroup()

# create summed cols for "All" antimicrobials/routes/countries/aware_categories
  # "All" in all four groups
  subset_all_4 <- data_reformatted %>% group_by(year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(country = "All", antimicrobials = "All", aware_category = "All", route_of_administration = "All") %>%
    ungroup()
  # "All" in any three of the four groups
  subset_route_3 <- data_reformatted %>% group_by(route_of_administration, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(country = "All", antimicrobials = "All", aware_category = "All") %>%
    ungroup()
  subset_aware_3 <- data_reformatted %>% group_by(aware_category, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(country = "All", antimicrobials = "All", route_of_administration = "All") %>%
    ungroup()
  subset_antimicrobial_3 <- data_reformatted %>% group_by(antimicrobials, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(country = "All", route_of_administration = "All", aware_category = "All") %>%
    ungroup()
  subset_country_3 <- data_reformatted %>% group_by(country, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(route_of_administration = "All", antimicrobials = "All", aware_category = "All") %>%
    ungroup()
  # "All" in any two of the four groups
  subset_aware_route_2 <- data_reformatted %>% group_by(aware_category, route_of_administration, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(country = "All", antimicrobials = "All") %>%
    ungroup()
  subset_antimicrobial_route_2 <- data_reformatted %>% group_by(antimicrobials, route_of_administration, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(country = "All", aware_category = "All") %>%
    ungroup()
  subset_country_route_2 <- data_reformatted %>% group_by(country, route_of_administration, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(aware_category = "All", antimicrobials = "All") %>%
    ungroup()
  subset_antimicrobial_aware_2 <- data_reformatted %>% group_by(antimicrobials, aware_category, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(country = "All", route_of_administration = "All") %>%
    ungroup()
  subset_country_aware_2 <- data_reformatted %>% group_by(country, aware_category, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(route_of_administration = "All", antimicrobials = "All") %>%
    ungroup()
  subset_country_antimicrobial_2 <- data_reformatted %>% group_by(country, antimicrobials, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(route_of_administration = "All", aware_category = "All") %>%
    ungroup()
  # "All" in only one of the four groups
  subset_route_1 <- data_reformatted %>% group_by(country, antimicrobials, aware_category, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(route_of_administration = "All") %>%
    ungroup()
  subset_aware_1 <- data_reformatted %>% group_by(country, antimicrobials, route_of_administration, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(aware_category = "All") %>%
    ungroup()
  subset_antimicrobial_1 <- data_reformatted %>% group_by(country, route_of_administration, aware_category, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(antimicrobials = "All") %>%
    ungroup()
  subset_country_1 <- data_reformatted %>% group_by(route_of_administration, antimicrobials, aware_category, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
    mutate(country = "All") %>%
    ungroup()
  # append each of these subsets to the main dataset (15 in total)  #may need to subset into smaller datasets if the app is too slow (e.g., ddd/did/di)???
  data_for_visualisations <- rbind(data_reformatted, 
                                   subset_all_4, 
                                   subset_route_3, subset_aware_3, subset_antimicrobial_3, subset_country_3, 
                                   subset_aware_route_2, subset_antimicrobial_route_2, subset_country_route_2, subset_antimicrobial_aware_2, subset_country_aware_2, subset_country_antimicrobial_2, 
                                   subset_country_1, subset_antimicrobial_1, subset_aware_1, subset_route_1)
  rm(list = ls(pattern = "subset_")) 

# change some country names that don't match the names in the shape file
data_for_visualisations <- data_for_visualisations %>%
  mutate(country = ifelse(country == "Russian Federation", "Russia", country)) %>%
  mutate(country = ifelse(country == "U.K. of Great Britain and Northern Ireland", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "United States of America", "United States", country)) %>%
  mutate(country = ifelse(country == "Viet Nam", "Vietnam", country)) %>%
  mutate(country = ifelse(country == "All", "Global", country))

# make values more manageable 
data_for_visualisations <- data_for_visualisations %>%
  mutate(value = case_when(metric == "ddd" ~ value/1000, .default = value))

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