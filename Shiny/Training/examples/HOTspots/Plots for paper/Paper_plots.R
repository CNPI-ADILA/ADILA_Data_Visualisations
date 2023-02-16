###**************************###
### Plots for Teresa's paper ###
###**************************###

# Project aim:
#
# Author: Alys Young
#
# Collaborators: Teresa Wozniak
#
# Date: late April 2021
#
# Script aim: Create the 16 plots required for Teresas upcoming paper.
              # All for community onset location only
              # Bugs:
                    # Third-generation cephalosporin (ceftazidime or ceftriaxone) resistant E.coli
                    # Ceftriaxone resistant E.coli
                    # Ceftriaxone resistant K. pneumoniae
                    # Ceftazidime resistant pseumonas aeruginosa
                    # Methicillin resistant S.aureus



# Set up --------------------------------------------------------------------------------------------------------


library(dplyr)
library(leaflet) # for colour palette
library(tmap)
library(sf)



# Regions shapefile
SA3_sf <- st_read("www/data/Australian_regions/Aus_regions.shp")


# Resistance data
HOTspots_yearly <- read.csv("~/Dropbox/HOTspots _data/HOTspots_yearly.csv")


# Filter for Community data only 
data <- HOTspots_yearly %>%
  filter(onset == "Community") %>%
  group_by(sample_type)




# Colour palette
h_palette <- list(
  
  ## For heat map
  `heat`  = c(
    `green`         = "#629c25",
    `yellow orange` = "#FFF100", 
    `orangy yellow` = "#FFD100",
    `orange`        = "#fc8105", 
    `dark orange`   = "#e34f00",
    `red`           = "#ff0000",
    `red2`          = "#db0000",
    `deep red`      = "#C00004",
    `dark red`      = "#810000",
    `almost black`  = "#410000")
)

# for a continuous variable, Change distinct colours into a gradient
pal_num <- leaflet::colorNumeric(h_palette$heat, domain = 0:100)
#pal_num10 <- pal_num(1:10)
pal_num100 <- pal_num(1:100)






# 1. Third-generation cephalosporin (ceftazidime and ceftriaxone) resistant E.coli -----------------------------------------------------------------------

data_1 <- data %>%
  filter(organism ==  "E. coli") %>%
  filter(antimicrobial   == "Ceftazidime")


# 2. Ceftriaxone resistant E.coli --------------------------------------------------------------------------------------------------------------------------------------
data_2 <- data %>%
  filter(organism ==  "E. coli",
         antimicrobial == "Ceftriaxone")



# 3. Ceftriaxone resistant K. pneumoniae -----------------------------------------------------------------------
data_3 <- data %>%
  filter(organism ==  "K. pneumoniae", 
         antimicrobial   == "Ceftriaxone")


# 4. Ceftazidime resistant pseumonas aeruginosa --------------------------------------------------------------

data_4 <- data %>%
  filter(organism ==  "P. aeruginosa", 
         antimicrobial   == "Ceftazidime")

# 5. Methicillin resistant S.aureus --------------------------------------------------------------

data_5 <- data %>%
  filter(organism ==  "S. aureus",
         antimicrobial   == "Methicillin (proxy)")





# Plot  All at once -------------------------------------------------------------
data_subset <- c("data_1", "data_2",  "data_3", "data_4", "data_5")



# loop through the data
for(d in 1:length(data_subset)){
  
  # get each of the 4 data subsets
  data <- get(data_subset[d])

  if (data_subset[d] == "data_3" ) {
    sample_types <- c("All", "Urine", "Blood", "Respiratory")
  } else {
    sample_types <- c("All", "Urine", "Blood")
  }
  
  # For each sample type
  for(s in 1:length(sample_types)){

    # filter the data for the sample type
    data_samp <- data %>%
      filter(sample_type == sample_types[s]) %>%
      group_by(region) %>%
      select(-year) %>%
      summarise(res = (sum(resistant_yearly)/ sum(num_of_tests_yearly))*100)
    
    # Assign to the environment if want to check
    #  assign(x = paste0("data_", sample_types[s]), value = data_samp)
    
    # merge the data with the spatial map of regions
    SA3_sf_merged <- merge(SA3_sf, data_samp, by.x = "SA3_NAME16", by.y = "region", all.x = TRUE)
    
    # Create tmap
    tm_map_samp <- tm_shape(SA3_sf_merged) + 
      tm_polygons("res", breaks = seq(0, 100, 10), palette = pal_num100, style = "cont",
                  lwd = 0.4, textNA = "Not included in study",  title = "% Resistance") +
      tm_layout(paste0(unique(data$antimicrobial), " resistant ", unique(data$organism), " (", sample_types[s],")"),
                legend.title.size = 1.7,
                legend.text.size = 1.3,
                legend.position = c("left","bottom"))
    
    # Assign the tmap file with a unique name 
    assign(x = paste0("tm_", unique(data$organism), "_", unique(data$antimicrobial), "_", sample_types[s]), value = tm_map_samp)
    
    # Save the tmap
    tmap_save(tm_map_samp,  filename = paste0("Outputs_plot/", unique(data$organism), "_", unique(data$antimicrobial), "_", sample_types[s] , ".png"))
  }
  
}


# 3. Ceftriaxone resistant K. pneumoniae -----------------------------------------------------------------------
# data_3 , "K. pneumoniae", "Ceftriaxone"


# 4. Ceftazidime resistant pseumonas aeruginosa --------------------------------------------------------------
# data_4, "P. aeruginosa",  "Ceftazidime"

# 5. Methicillin resistant S.aureus --------------------------------------------------------------
# data_5, "S. aureus", "Methicillin (proxy)"

unique(data_5$sample_type) 
data_5  %>% filter(sample_type == "Blood") %>%  as.data.frame

data_samp <- data_5 %>%
  filter(sample_type == "Blood") %>%
  group_by(region) %>%
  select(-year) %>%
  summarise(res = (sum(resistant_yearly)/ sum(num_of_tests_yearly))*100)

# merge the data with the spatial map of regions
SA3_sf_merged <- merge(SA3_sf, data_samp, by.x = "SA3_NAME16", by.y = "region", all.x = TRUE)

# Create tmap
tm_map_samp  <- tm_shape(SA3_sf_merged) + 
  tm_polygons("res", palette  = pal_num100[53], style = "cont",
              lwd = 0.4, textNA = "Not included in study",  title = "% Resistance") +
  tm_layout(paste0(unique(data$antimicrobial), " resistant ", unique(data$organism), " (", sample_types[s],")"),
            legend.title.size = 1.7,
            legend.text.size = 1.3,
            legend.position = c("left","bottom"))
tmap_save(tm_map_samp,  filename = paste0("Outputs_plot/S. aureus_Methicillin (proxy)_Blood.png"))












unique(data_5$sample_type) 
data_5  %>% filter(sample_type == "Urine") %>%  as.data.frame

data_samp <- data_5 %>%
  filter(sample_type == "Urine") %>%
  group_by(region) %>%
  select(-year) %>%
  summarise(res = (sum(resistant_yearly)/ sum(num_of_tests_yearly))*100)

# merge the data with the spatial map of regions
SA3_sf_merged <- merge(SA3_sf, data_samp, by.x = "SA3_NAME16", by.y = "region", all.x = TRUE)

# Create tmap
tm_map_samp  <- tm_shape(SA3_sf_merged) + 
  tm_polygons("res", palette  = pal_num100[9], style = "cont",
              lwd = 0.4, textNA = "Not included in study",  title = "% Resistance") +
  tm_layout(paste0(unique(data$antimicrobial), " resistant ", unique(data$organism), " (", sample_types[s],")"),
            legend.title.size = 1.7,
            legend.text.size = 1.3,
            legend.position = c("left","bottom"))
tmap_save(tm_map_samp,  filename = paste0("Outputs_plot/S. aureus_Methicillin (proxy)_Urine.png"))
