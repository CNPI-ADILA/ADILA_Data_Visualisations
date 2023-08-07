#                                                                                        #
# Assess which countries have both "retail" and "hospital" data, and which only have one #
#                                                                                        #

# drop duplicates
coverage <- data_for_visualisations %>% 
  filter(who_regional_office == "All" & antimicrobials == "All" & aware_category == "All" & route_of_administration == "All" & metric == "ddd" & sector != "All" & country != "All") %>%
  select(country, sector, year, value)

# frequency table (with %) of how many countries have one or both sectors
total = length(unique(coverage$country))
sector_by_country <- coverage %>% group_by(country, year) %>% mutate(coverage = ifelse(length(sector) == 2, "Retail & Hospital", sector))
summary <- sector_by_country %>% group_by(coverage) %>% summarise(num_of_countries = length(unique(country)), perc_of_countries = round((length(unique(country))/total)*100,2)) %>% ungroup()

# table listing countries that change categories over time
changes_over_time <- sector_by_country %>% group_by(country) %>% filter(length(unique(coverage)) > 1) %>% arrange(country, year, sector) %>% select(-value, -sector) %>% unique()

# for those with both, what percentage of their total AMC is in each sector (and does this change over time)
prop_AMC <- sector_by_country %>% filter(coverage == "Retail & Hospital") %>% select(-coverage) %>% spread(sector, value) %>% 
  group_by(country, year) %>%
  summarise(prop_hospital = round((Hospital/(Hospital+Retail))*100,2), prop_retail = round((Retail/(Hospital+Retail))*100,2)) %>%
  mutate(absolute_diff = abs(prop_hospital - prop_retail)) %>%
  arrange(country, year) %>%
  ungroup()
prop_AMC <- gather(prop_AMC, "metric", "perc", prop_hospital, prop_retail, absolute_diff)
prop_AMC <- prop_AMC %>% arrange(desc(metric), year, perc, country) %>% mutate(order = seq(1:length(prop_AMC$perc)))
ggplot(prop_AMC, aes(x = reorder(interaction(country, year), order), y = perc, fill = metric, color = year)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "percentage", fill = NULL) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
ggsave("./prop_DDD_by_sector.png", 
       scale = 1, width = 30, height = 20, dpi = 1000, units = "cm")         
