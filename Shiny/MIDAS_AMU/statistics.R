# Integrate as new tab ("Statistics") in Shiny app:
library(DT)
library(writexl)
colours <- c("#000000", "#E69F00", "#56B4E9", "#009E73","#F0E442","#CC79A7") #if needed: "#0072B2", "#D55E00"

########################
# Data sources summary # could also add 95% CIs if necessary
########################
  # values to put in a data table
  num_data_sources <- length(unique(data_for_visualisations$source_title))
  
  total_num_countries <- length(unique(data_for_visualisations$country))
  
  avg_num_countries_per_data_source <- data_for_visualisations %>%
    group_by(source_title) %>%
    mutate(count = n_distinct(country)) %>%
    ungroup()
  avg_num_countries_per_data_source <- round(sum(unique(avg_num_countries_per_data_source$count)) / num_data_sources, 2)
  
  total_num_antimicrobials <- length(unique(data_for_visualisations$antimicrobials))
    
  avg_num_antimicrobials_per_data_source <- data_for_visualisations %>%
    group_by(source_title) %>%
    mutate(count = n_distinct(antimicrobials)) %>%
    ungroup()
  avg_num_antimicrobials_per_data_source <- round(sum(unique(avg_num_antimicrobials_per_data_source$count)) / num_data_sources, 2)
  
  total_year_range_min <- min(data_for_visualisations$year, na.rm = T)
  total_year_range_max <- max(data_for_visualisations$year, na.rm = T)
  total_year_range <- paste0(total_year_range_min,"-",total_year_range_max)
  rm(total_year_range_min, total_year_range_max)
  
  avg_year_range_per_data_source <- data_for_visualisations %>%
    group_by(source_title) %>%
    mutate(year_range = (max(data_for_visualisations$year, na.rm = T) - min(data_for_visualisations$year, na.rm = T))) %>%
    ungroup()
  avg_year_range_per_data_source <- round(sum(unique(avg_year_range_per_data_source$year_range)) / num_data_sources, 2)
  
  avg_perc_aware <- data_for_visualisations %>%
    filter(metric == "ddd" & who_region == "All" & country == "All" & antimicrobials == "All" & route_of_administration == "All" & sector == "All" & aware_category != "All") %>%
    group_by(source_title, aware_category) %>%
    mutate(AMC_aware = sum(value)) %>%
    ungroup()
  avg_perc_aware <- avg_perc_aware %>%
    group_by(source_title) %>%
    mutate(AMC_total = sum(value), perc = (AMC_aware/AMC_total)*100) %>%
    ungroup()   
  avg_perc_access <- avg_perc_aware %>% filter(aware_category == "Access")
  avg_perc_access <- round(sum(unique(avg_perc_access$perc)) / num_data_sources, 2)
  avg_perc_watch <- avg_perc_aware %>% filter(aware_category == "Watch")
  avg_perc_watch <- round(sum(unique(avg_perc_watch$perc)) / num_data_sources, 2)
  avg_perc_reserve <- avg_perc_aware %>% filter(aware_category == "Reserve")
  avg_perc_reserve <- round(sum(unique(avg_perc_reserve$perc)) / num_data_sources, 2)
  rm(avg_perc_aware)

  avg_perc_route <- data_for_visualisations %>%
    filter(metric == "ddd" & who_region == "All" & country == "All" & antimicrobials == "All" & aware_category == "All" & sector == "All" & route_of_administration != "All") %>%
    group_by(source_title, route_of_administration) %>%
    mutate(AMC_route = sum(value)) %>%
    ungroup()
  avg_perc_route <- avg_perc_route %>%
    group_by(source_title) %>%
    mutate(AMC_total = sum(value), perc = (AMC_route/AMC_total)*100) %>%
    ungroup()   
  avg_perc_oral <- avg_perc_route %>% filter(route_of_administration == "Oral")
  avg_perc_oral <- round(sum(unique(avg_perc_oral$perc)) / num_data_sources, 2)
  avg_perc_parenteral <- avg_perc_route %>% filter(route_of_administration == "Parenteral")
  avg_perc_parenteral <- round(sum(unique(avg_perc_parenteral$perc)) / num_data_sources, 2)
  rm(avg_perc_route)
  
  # combine into data table
  row_names <- c("Number of data sources", "Number of countries", "Avg. number of countries (per data source)", "Number of antimicrobials",
                 "Avg. number of antimicrobials (per data source)", "Year range", "Avg. number of years (per data source)",
                 "Avg. percent of DDD that are Access antimicrobials (per data source)", "Avg. percent of DDD that are Watch antimicrobials (per data source)", 
                 "Avg. percent of DDD that are Reserve antimicrobials (per data source)", "Avg. percent of DDD that are oral antimicrobials (per data source)", 
                 "Avg. percent of DDD that are parenteral antimicrobials (per data source)")
  values <- c(num_data_sources, total_num_countries, avg_num_countries_per_data_source, total_num_antimicrobials,
              avg_num_antimicrobials_per_data_source, total_year_range, avg_year_range_per_data_source,
              avg_perc_access, avg_perc_watch, avg_perc_reserve, avg_perc_oral, avg_perc_parenteral)
  data_source_summary_table <- data.table("metric" = row_names, "value" = values)
  write_xlsx(data_source_summary_table, "./outputs/data_source_summary_table.xlsx")
  #datatable(data_source_summary_table)
  
#------------------------------------------------------------------------------#   
#------------------------------------------------------------------------------#   
#------------------------------------------------------------------------------#   
######################
# Statistical models #
######################
  # DU90
    # copy over / adapt from Pharma DU90 corporation report (except do for antimicrobials, inputs: who_region/country/aware/route/sector/year/metric)

#------------------------------------------------------------------------------#   
  # Correlations
    # by countries (and antimicrobials)
      # univariable (i.e., one group at a time)
        # Is AMC in administration route categories correlated?
          country_route_univ <- data_for_visualisations %>%
            filter(metric == "su" & (route_of_administration == "Oral" | route_of_administration == "Parenteral")) %>%
            filter(value > 0) %>%
            mutate(value = log(value)) %>%
            group_by(who_region, country, antimicrobials, aware_category, route_of_administration, sector) %>%
            summarize(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
            ungroup()
          country_route_univ <- country_route_univ %>%
            pivot_wider(
              names_from = route_of_administration,
              values_from = value
            ) %>%
            filter((aware_category == "Access" | aware_category == "Watch" | aware_category == "Reserve") & sector != "All" & antimicrobials != "All") 
          cor_labels <- country_route_univ %>%
            group_by(who_region) %>%
            mutate(label = round(cor(Oral, Parenteral, use = "complete.obs"), 2)) %>%
            distinct(label) %>% mutate(aware_category = NA, sector = NA, group = NA)
          cor_labels$label <- paste0("corr=", cor_labels$label)
          country_route_univ <- country_route_univ %>% mutate(group = paste0(aware_category, "_", sector)) %>% select(-c(aware_category, sector))
          country_route_univ_plot <- 
            ggplot(country_route_univ, aes(x = Oral, y = Parenteral, color = group)) +
            geom_point(alpha = 0.25) +
            geom_smooth(method = "lm", se = FALSE) +
            scale_colour_manual(values = colours) +
            labs(x = "log(Oral SU)", y = "log(Parenteral SU)", fill = NULL) +
            facet_wrap(~who_region)
          country_route_univ_plot <- country_route_univ_plot + geom_text(x = Inf, y = -Inf, hjust = 1, vjust = .001, aes(label = label), data = cor_labels, show.legend = FALSE)
          country_route_univ_plot
          ggsave("./outputs/parenteral_vs_oral.png", 
                 scale = 1, width = 30, height = 20, dpi = 1000, units = "cm")
        # Is AMC in AWaRe categories correlated?
          country_aware_univ <- data_for_visualisations %>%
            filter(metric == "su" & (aware_category == "Access" | aware_category == "Watch" | aware_category == "Reserve")) %>%
            filter(value > 0) %>%
            mutate(value = log(value)) %>%
            group_by(who_region, country, aware_category, route_of_administration, sector) %>%
            summarize(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
            ungroup()
          country_aware_univ <- country_aware_univ %>%
            pivot_wider(
              names_from = aware_category,
              values_from = value
            ) %>%
            filter((route_of_administration == "Oral" | route_of_administration == "Parenteral") & sector != "All") 
          cor_labels1 <- country_aware_univ %>%
            group_by(who_region) %>%
            mutate(label = round(cor(Access, Watch, use = "complete.obs"), 2)) %>%
            distinct(label) %>% mutate(route_of_administration = NA, sector = NA, group = NA)
          cor_labels1$label <- paste0("corr=", cor_labels1$label)
          cor_labels2 <- country_aware_univ %>%
            group_by(who_region) %>%
            mutate(label = round(cor(Access, Reserve, use = "complete.obs"), 2)) %>%
            distinct(label) %>% mutate(route_of_administration = NA, sector = NA, group = NA)
          cor_labels2$label <- paste0("corr=", cor_labels2$label)
          cor_labels3 <- country_aware_univ %>%
            group_by(who_region) %>%
            mutate(label = round(cor(Watch, Reserve, use = "complete.obs"), 2)) %>%
            distinct(label) %>% mutate(route_of_administration = NA, sector = NA, group = NA)
          cor_labels3$label <- paste0("corr=", cor_labels3$label)
          country_aware_univ <- country_aware_univ %>% mutate(group = paste0(route_of_administration, "_", sector)) %>% select(-c(route_of_administration, sector))
          country_aware_univ_plot1 <- 
            ggplot(country_aware_univ, aes(x = Access, y = Watch, color = group)) +
            geom_point(alpha = 0.25) +
            geom_smooth(method = "lm", se = FALSE) +
            scale_colour_manual(values = colours) +
            labs(x = "log(Access SU)", y = "log(Watch SU)", fill = NULL) +
            facet_wrap(~who_region)
          country_aware_univ_plot1 <- country_aware_univ_plot1 + geom_text(x = Inf, y = -Inf, hjust = 1, vjust = .001, aes(label = label), data = cor_labels1, show.legend = FALSE)
          country_aware_univ_plot1
          ggsave("./outputs/watch_vs_access.png", 
                 scale = 1, width = 30, height = 20, dpi = 1000, units = "cm")
          country_aware_univ_plot2 <- 
            ggplot(country_aware_univ, aes(x = Access, y = Reserve, color = group)) +
            geom_point(alpha = 0.25) +
            geom_smooth(method = "lm", se = FALSE) +
            scale_colour_manual(values = colours) +
            labs(x = "log(Access SU)", y = "log(Reserve SU)", fill = NULL) +
            facet_wrap(~who_region)
          country_aware_univ_plot2 <- country_aware_univ_plot2 + geom_text(x = Inf, y = -Inf, hjust = 1, vjust = .001, aes(label = label), data = cor_labels2, show.legend = FALSE)
          country_aware_univ_plot2
          ggsave("./outputs/reserve_vs_access.png", 
                 scale = 1, width = 30, height = 20, dpi = 1000, units = "cm")
          country_aware_univ_plot3 <- 
            ggplot(country_aware_univ, aes(x = Watch, y = Reserve, color = group)) +
            geom_point(alpha = 0.25) +
            geom_smooth(method = "lm", se = FALSE) +
            scale_colour_manual(values = colours) +
            labs(x = "log(Watch SU)", y = "log(Reserve SU)", fill = NULL) +
            facet_wrap(~who_region)
          country_aware_univ_plot3 <- country_aware_univ_plot3 + geom_text(x = Inf, y = -Inf, hjust = 1, vjust = .001, aes(label = label), data = cor_labels3, show.legend = FALSE)
          country_aware_univ_plot3
          ggsave("./outputs/reserve_vs_watch.png", 
                 scale = 1, width = 30, height = 20, dpi = 1000, units = "cm")
        # Is AMC in sectors correlated?
          country_sector_univ <- data_for_visualisations %>%
            filter(metric == "su" & (sector == "Retail" | sector == "Hospital")) %>%
            filter(value > 0) %>%
            mutate(value = log(value)) %>%
            group_by(who_region, country, antimicrobials, aware_category, route_of_administration, sector) %>%
            summarize(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
            ungroup()
          country_sector_univ <- country_sector_univ %>%
            pivot_wider(
              names_from = sector,
              values_from = value
            ) %>%
            filter((route_of_administration == "Oral" | route_of_administration == "Parenteral") & (aware_category == "Access" | aware_category == "Reserve" | aware_category == "Watch") & antimicrobials != "All") 
          cor_labels <- country_sector_univ %>%
            group_by(who_region) %>%
            mutate(label = round(cor(Retail, Hospital, use = "complete.obs"), 2)) %>%
            distinct(label) %>% mutate(aware_category = NA, route_of_administration = NA, group = NA)
          cor_labels$label <- paste0("corr=", cor_labels$label)
          country_sector_univ <- country_sector_univ %>% mutate(group = paste0(aware_category, "_", route_of_administration)) %>% select(-c(aware_category, route_of_administration))
          country_sector_univ_plot <- 
            ggplot(country_sector_univ, aes(x = Retail, y = Hospital, color = group)) +
            geom_point(alpha = 0.25) +
            geom_smooth(method = "lm", se = FALSE) +
            scale_colour_manual(values = colours) +
            labs(x = "log(Retail SU)", y = "log(Hospital SU)", fill = NULL) +
            facet_wrap(~who_region)
          country_sector_univ_plot <- country_sector_univ_plot + geom_text(x = Inf, y = -Inf, hjust = 1, vjust = .001, aes(label = label), data = cor_labels, show.legend = FALSE)
          country_sector_univ_plot
          ggsave("./outputs/hospital_vs_retail.png", 
                 scale = 1, width = 30, height = 20, dpi = 1000, units = "cm")
      # multivariable
        #e.g., is route correlated to aware correlated to sector?
        #3D: 
          #http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization
          #https://plotly.com/r/3d-scatter-plots/
          #https://r-graph-gallery.com/3d_scatter_plot.html?utm_content=cmp-true

    # by WHO regions (and antimicrobials)
      # ... same as above but collapse countries
  
    # by HIC vs LMIC (and antimicrobials)
      # ... same as above but need to merge country income status
    
#------------------------------------------------------------------------------#   
  # Principal Component Analysis?
                            
#------------------------------------------------------------------------------#   
  # Predict AMC five years into the future/past
    # https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
    # https://www.pluralsight.com/guides/time-series-forecasting-using-r
    # https://otexts.com/fpp3/
    
#------------------------------------------------------------------------------#   
  # Malaria Atlas Project type heat maps?
    # would have to look at the source code of their malariaAtlas package to copy raster modelling
    
    
    
    