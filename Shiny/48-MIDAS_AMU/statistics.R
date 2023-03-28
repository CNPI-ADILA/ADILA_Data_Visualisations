# Integrate as new tab ("Statistics") in Shiny app:

########################
# Data sources summary #
########################
  # values to put in a data table
  num_data_sources <- length(unique(data_for_visualisations$source_title))
  
  total_num_countries <- data_for_visualisations
    
  avg_num_countries_per_data_source <- data_for_visualisations
  
  total_num_antimicrobials <- data_for_visualisations
    
  avg_num_antimicrobials_per_data_source <- data_for_visualisations
    
  total_year_range <- data_for_visualisations
    
  avg_year_range_per_data_source <- data_for_visualisations
  
  avg_perc_access <- #these ones will require some collapsing, just use one metric (e.g., DDD), etc.
  
  avg_perc_watch <-
    
  avg_per_reserve <-
    
  avg_perc_oral <-
  
  avg_perc_parenteral <-
    
    # combine into data table
    row_names <- c("Number of data sources", etc.)
    values <- c(num_data_sources, etc.)
    data_source_summary_table <- data.table("metric" = row_names, "value" = values)
  
#------------------------------------------------------------------------------#   
#------------------------------------------------------------------------------#   
#------------------------------------------------------------------------------#   
######################
# Statistical models #
######################
  # DU90 (global)
    #with colouring by aware category

#------------------------------------------------------------------------------#   
  # Correlations / variation
    # within...
      # countries
        # globally
          # univariable (i.e., one group at a time)
            # Is AMC in administration route categories correlated?
              #imagine line plots of the correlation between oral/parenteral over time for each country, OR,
              #a scatter plot of oral vs parenteral for all the countries (collapsed by year)
              
            # ... same but in aware categories...
              
            # ... same but in sectors...
    
          # multivariable
            #e.g., is route correllated to aware correlated to sector?
    
        # within WHO regions
          # ... same as above...
          
      # WHO regions
        # ... same as above...
    
      # HIC vs LMIC
        # ... same as above...
    
    # between...
      # countries
        # gloabally
          # univariable (i.e., one group at a time)
            # Is AMC in administration route categories correlated?
              
            # ... same but in aware categories...
            
            # ... same but in sectors...
    
          # multivariable
            # ... same as above...
      
        # within WHO regions
          # ... same as above...
      
      # WHO regions
        # ... same as above...
    
      # HIC vs LMIC
        # ... same as above...

#------------------------------------------------------------------------------#   
  # Predict AMC five years into the future/past
    # https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
    # https://www.pluralsight.com/guides/time-series-forecasting-using-r
    # https://otexts.com/fpp3/
    
#------------------------------------------------------------------------------#   
  # Malaria Atlas Project type heat maps?
    # do they have a package for doing this???
    
    
    
    