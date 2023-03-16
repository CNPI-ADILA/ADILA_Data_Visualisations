# "All" in all six groups
subset_all_6 <- data_reformatted %>% group_by(source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", aware_category = "All", route_of_administration = "All", who_region = "All", sector = "All") %>%
  ungroup()
# "All" in any five of the six groups (6 combinations)
subset_sector_5 <- data_reformatted %>% group_by(sector, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", aware_category = "All", route_of_administration = "All", who_region = "All") %>%
  ungroup()
subset_whoregion_5 <- data_reformatted %>% group_by(who_region, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", aware_category = "All", route_of_administration = "All", sector = "All") %>%
  ungroup()
subset_route_5 <- data_reformatted %>% group_by(route_of_administration, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", aware_category = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_aware_5 <- data_reformatted %>% group_by(aware_category, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", route_of_administration = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_antimicrobial_5 <- data_reformatted %>% group_by(antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", aware_category = "All", route_of_administration = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_country_5 <- data_reformatted %>% group_by(country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", aware_category = "All", route_of_administration = "All", who_region = "All", sector = "All") %>%
  ungroup()
# "All" in any four of the six groups (15 combinations)
subset_sector_whoregion_4 <- data_reformatted %>% group_by(sector, who_region, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", aware_category = "All", route_of_administration = "All") %>%
  ungroup()
subset_sector_route_4 <- data_reformatted %>% group_by(sector, route_of_administration, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", aware_category = "All", who_region = "All") %>%
  ungroup()
subset_sector_aware_4 <- data_reformatted %>% group_by(sector, aware_category, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", route_of_administration = "All", who_region = "All") %>%
  ungroup()
subset_sector_antimicrobial_4 <- data_reformatted %>% group_by(sector, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", aware_category = "All", route_of_administration = "All", who_region = "All") %>%
  ungroup()
subset_sector_country_4 <- data_reformatted %>% group_by(sector, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", aware_category = "All", route_of_administration = "All", who_region = "All") %>%
  ungroup()
subset_whoregion_route_4 <- data_reformatted %>% group_by(who_region, route_of_administration, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", aware_category = "All", sector = "All") %>%
  ungroup()
subset_whoregion_aware_4 <- data_reformatted %>% group_by(who_region, aware_category, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", route_of_administration = "All", sector = "All") %>%
  ungroup()
subset_whoregion_antimicrobial_4 <- data_reformatted %>% group_by(who_region, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", aware_category = "All", route_of_administration = "All", sector = "All") %>%
  ungroup()
subset_whoregion_country_4 <- data_reformatted %>% group_by(who_region, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", aware_category = "All", route_of_administration = "All", sector = "All") %>%
  ungroup()
subset_route_aware_4 <- data_reformatted %>% group_by(route_of_administration, aware_category, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_route_antimicrobial_4 <- data_reformatted %>% group_by(route_of_administration, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", aware_category = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_route_country_4 <- data_reformatted %>% group_by(route_of_administration, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", aware_category = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_aware_antimicrobial_4 <- data_reformatted %>% group_by(aware_category, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", route_of_administration = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_aware_country_4 <- data_reformatted %>% group_by(aware_category, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", route_of_administration = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_antimicrobial_country_4 <- data_reformatted %>% group_by(antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(aware_category = "All", route_of_administration = "All", who_region = "All", sector = "All") %>%
  ungroup()
# "All" in any three of the six groups (20 combinations)
subset_sector_whoregion_route_3 <- data_reformatted %>% group_by(sector, who_region, route_of_administration, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", aware_category = "All") %>%
  ungroup()
subset_sector_whoregion_aware_3 <- data_reformatted %>% group_by(sector, who_region, aware_category, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", route_of_administration = "All") %>%
  ungroup()
subset_sector_whoregion_antimicrobial_3 <- data_reformatted %>% group_by(sector, who_region, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", aware_category = "All", route_of_administration = "All") %>%
  ungroup()
subset_sector_whoregion_country_3 <- data_reformatted %>% group_by(sector, who_region, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", aware_category = "All", route_of_administration = "All") %>%
  ungroup()
subset_sector_route_aware_3 <- data_reformatted %>% group_by(sector, route_of_administration, aware_category, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", who_region = "All") %>%
  ungroup()
subset_sector_route_antimicrobial_3 <- data_reformatted %>% group_by(sector, route_of_administration, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", aware_category = "All", who_region = "All") %>%
  ungroup()
subset_sector_route_country_3 <- data_reformatted %>% group_by(sector, route_of_administration, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", aware_category = "All", who_region = "All") %>%
  ungroup()
subset_sector_aware_antimicrobial_3 <- data_reformatted %>% group_by(sector, aware_category, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", route_of_administration = "All", who_region = "All") %>%
  ungroup()
subset_sector_aware_country_3 <- data_reformatted %>% group_by(sector, aware_category, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", route_of_administration = "All", who_region = "All") %>%
  ungroup()
subset_sector_antimicrobial_country_3 <- data_reformatted %>% group_by(sector, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(aware_category = "All", route_of_administration = "All", who_region = "All") %>%
  ungroup()
subset_whoregion_route_aware_3 <- data_reformatted %>% group_by(who_region, route_of_administration, aware_category, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All", sector = "All") %>%
  ungroup()
subset_whoregion_route_antimicrobial_3 <- data_reformatted %>% group_by(who_region, route_of_administration, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", aware_category = "All", sector = "All") %>%
  ungroup()
subset_whoregion_route_country_3 <- data_reformatted %>% group_by(who_region, route_of_administration, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", aware_category = "All", sector = "All") %>%
  ungroup()
subset_whoregion_antimicrobial_country_3 <- data_reformatted %>% group_by(who_region, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(aware_category = "All", route_of_administration = "All", sector = "All") %>%
  ungroup()
subset_whoregion_aware_antimicrobial_3 <- data_reformatted %>% group_by(who_region, aware_category, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", route_of_administration = "All", sector = "All") %>%
  ungroup()
subset_whoregion_aware_country_3 <- data_reformatted %>% group_by(who_region, aware_category, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", route_of_administration = "All", sector = "All") %>%
  ungroup()
subset_route_aware_antimicrobial_3 <- data_reformatted %>% group_by(route_of_administration, aware_category, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_route_aware_country_3 <- data_reformatted %>% group_by(route_of_administration, aware_category, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_route_antimicrobial_country_3 <- data_reformatted %>% group_by(route_of_administration, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(aware_category = "All", who_region = "All", sector = "All") %>%
  ungroup()
subset_aware_antimicrobial_country_3 <- data_reformatted %>% group_by(aware_category, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(route_of_administration = "All", who_region = "All", sector = "All") %>%
  ungroup()
# "All" in any two of the six groups (15 combinations)
subset_sector_whoregion_route_aware_2 <- data_reformatted %>% group_by(sector, who_region, route_of_administration, aware_category, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", antimicrobials = "All") %>%
  ungroup()
subset_sector_whoregion_route_antimicrobial_2 <- data_reformatted %>% group_by(sector, who_region, route_of_administration, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", aware_category = "All") %>%
  ungroup()
subset_sector_whoregion_route_country_2 <- data_reformatted %>% group_by(sector, who_region, route_of_administration, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", aware_category = "All") %>%
  ungroup()
subset_sector_whoregion_aware_antimicrobial_2 <- data_reformatted %>% group_by(sector, who_region, aware_category, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", route_of_administration = "All") %>%
  ungroup()
subset_sector_whoregion_aware_country_2 <- data_reformatted %>% group_by(sector, who_region, aware_category, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(route_of_administration = "All", antimicrobials = "All") %>%
  ungroup()
subset_sector_whoregion_antimicrobial_country_2 <- data_reformatted %>% group_by(sector, who_region, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(aware_category = "All", route_of_administration = "All") %>%
  ungroup()
subset_sector_route_aware_antimicrobial_2 <- data_reformatted %>% group_by(sector, route_of_administration, aware_category, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", who_region = "All") %>%
  ungroup()
subset_sector_route_aware_country_2 <- data_reformatted %>% group_by(sector, route_of_administration, aware_category, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", who_region = "All") %>%
  ungroup()
subset_sector_route_antimicrobial_country_2 <- data_reformatted %>% group_by(sector, route_of_administration, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(aware_category = "All", who_region = "All") %>%
  ungroup()
subset_route_aware_antimicrobial_country_2 <- data_reformatted %>% group_by(route_of_administration, aware_category, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(who_region = "All", sector = "All") %>%
  ungroup()
subset_whoregion_route_aware_antimicrobial_2 <- data_reformatted %>% group_by(who_region, route_of_administration, aware_category, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All", sector = "All") %>%
  ungroup()
subset_whoregion_route_aware_country_2 <- data_reformatted %>% group_by(who_region, route_of_administration, aware_category, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All", sector = "All") %>%
  ungroup()
subset_whoregion_route_antimicrobial_country_2 <- data_reformatted %>% group_by(who_region, route_of_administration, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(aware_category = "All", sector = "All") %>%
  ungroup()
subset_whoregion_aware_antimicrobial_country_2 <- data_reformatted %>% group_by(who_region, aware_category, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(route_of_administration = "All", sector = "All") %>%
  ungroup()
subset_sector_aware_antimicrobial_country_2 <- data_reformatted %>% group_by(sector, aware_category, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(route_of_administration = "All", who_region = "All") %>%
  ungroup()
# "All" in only one of the six groups (6 combinations)
subset_sector_1 <- data_reformatted %>% group_by(who_region, route_of_administration, aware_category, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(sector = "All") %>%
  ungroup()
subset_whoregion_1 <- data_reformatted %>% group_by(sector, route_of_administration, aware_category, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(who_region = "All") %>%
  ungroup()
subset_route_1 <- data_reformatted %>% group_by(sector, who_region, aware_category, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(route_of_administration = "All") %>%
  ungroup()
subset_aware_1 <- data_reformatted %>% group_by(sector, who_region, route_of_administration, antimicrobials, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(aware_category = "All") %>%
  ungroup()
subset_antimicrobial_1 <- data_reformatted %>% group_by(sector, who_region, route_of_administration, aware_category, country, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(antimicrobials = "All") %>%
  ungroup()
subset_country_1 <- data_reformatted %>% group_by(sector, who_region, route_of_administration, aware_category, antimicrobials, source_title, year, metric) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>% 
  mutate(country = "All") %>%
  ungroup()
# append each of these subsets to the main dataset (63 in total)  #may need to subset into smaller datasets if the app is too slow (e.g., ddd/did/di)???
data_for_visualisations <- rbind(data_reformatted, 
                                 subset_all_6, 
                                 subset_sector_5, subset_whoregion_5, subset_route_5, subset_aware_5, subset_antimicrobial_5, subset_country_5,
                                 subset_sector_whoregion_4, subset_sector_route_4, subset_sector_aware_4, subset_sector_antimicrobial_4, subset_sector_country_4, subset_whoregion_route_4, subset_whoregion_aware_4, subset_whoregion_antimicrobial_4, subset_whoregion_country_4, subset_route_aware_4, subset_route_antimicrobial_4, subset_route_country_4, subset_aware_antimicrobial_4, subset_aware_country_4, subset_antimicrobial_country_4,
                                 subset_sector_whoregion_route_3, subset_sector_whoregion_aware_3, subset_sector_whoregion_antimicrobial_3, subset_sector_whoregion_country_3, subset_sector_route_aware_3, subset_sector_route_antimicrobial_3, subset_sector_route_country_3, subset_sector_aware_antimicrobial_3, subset_sector_aware_country_3, subset_sector_antimicrobial_country_3, subset_whoregion_route_aware_3, subset_whoregion_route_antimicrobial_3, subset_whoregion_route_country_3, subset_whoregion_antimicrobial_country_3, subset_whoregion_aware_antimicrobial_3, subset_whoregion_aware_country_3, subset_route_aware_antimicrobial_3, subset_route_aware_country_3, subset_route_antimicrobial_country_3, subset_aware_antimicrobial_country_3,
                                 subset_sector_whoregion_route_aware_2, subset_sector_whoregion_route_antimicrobial_2, subset_sector_whoregion_route_country_2, subset_sector_whoregion_aware_antimicrobial_2, subset_sector_whoregion_aware_country_2, subset_sector_whoregion_antimicrobial_country_2, subset_sector_route_aware_antimicrobial_2, subset_sector_route_aware_country_2, subset_sector_route_antimicrobial_country_2, subset_route_aware_antimicrobial_country_2, subset_whoregion_route_aware_antimicrobial_2, subset_whoregion_route_aware_country_2, subset_whoregion_route_antimicrobial_country_2, subset_whoregion_aware_antimicrobial_country_2, subset_sector_aware_antimicrobial_country_2,
                                 subset_sector_1, subset_whoregion_1, subset_route_1, subset_aware_1, subset_antimicrobial_1, subset_country_1)
rm(list = ls(pattern = "subset_")) 
