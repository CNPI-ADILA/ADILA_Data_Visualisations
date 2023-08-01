# NOTE: the values in each of these fields may have changed (e.g., been deleted/replaced), such as the WHO region classification

country <- data_for_visualisations %>% select(country)
country <- country[!duplicated(country), ]
country_match <- merge(colours_country, country, by.x = "location", by.y = "country", all = T)
write.csv(country_match, file = './colours/new/country_match.csv', na = "ADD A COLOUR", row.names=FALSE)

antimicrobial <- data_for_visualisations %>% select(antimicrobials)
antimicrobial <- antimicrobial[!duplicated(antimicrobial), ]
ant_match <- merge(colours_antimicrobial, antimicrobial, by.x = "antimicrobial", by.y = "antimicrobials", all = T)
write.csv(ant_match, file = './colours/new/antmicrobial_match.csv', na = "ADD A COLOUR", row.names=FALSE)

route <- data_for_visualisations %>% select(route_of_administration)
route <- route[!duplicated(route), ]
route_match <- merge(colours_route, route, by.x = "route", by.y = "route_of_administration", all = T)
write.csv(route_match, file = './colours/new/route_match.csv', na = "ADD A COLOUR", row.names=FALSE)

aware <- data_for_visualisations %>% select(aware_category)
aware <- aware[!duplicated(aware), ]
aware_match <- merge(colours_aware, aware, by.x = "aware_category", by.y = "aware_category", all = T)
write.csv(aware_match, file = './colours/new/aware_match.csv', na = "ADD A COLOUR", row.names=FALSE)

sector <- data_for_visualisations %>% select(sector)
sector <- sector[!duplicated(sector), ]
sector_match <- merge(colours_sector, sector, by.x = "sector", by.y = "sector", all = T)
write.csv(sector_match, file = './colours/new/sector_match.csv', na = "ADD A COLOUR", row.names=FALSE)

whoregion <- data_for_visualisations %>% select(who_regional_office)
whoregion <- whoregion[!duplicated(whoregion), ]
whoreg_match <- merge(colours_whoregion, whoregion, by.x = "region", by.y = "who_regional_office", all = T)
write.csv(whoreg_match, file = './colours/new/whoreg_match.csv', na = "ADD A COLOUR", row.names=FALSE)

rm(country, antimicrobial, route, aware, sector, whoregion)
rm(list = ls(pattern = "_match"))