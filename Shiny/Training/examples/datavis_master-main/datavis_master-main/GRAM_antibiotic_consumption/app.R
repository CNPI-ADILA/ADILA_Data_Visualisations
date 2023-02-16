rm(list = ls())
library(shiny)
library(ggplot2)
library(data.table)
library(plotly)
library(dplyr)
library(stringr)
library(leaflet)
library(RColorBrewer)
library(sf)
library(rgdal)
# setwd('C:/Users/Annie/Documents/GRAM/data_visualisations/GRAM_antibiotic_consumption/')

#Load & prep data for maps ####
abc <- read.csv("data/ABC_map.csv")
ABU_DT0 <- fread('data/ABU_adm0.csv')
ABU_DT1 <- fread('data/ABU_adm1.csv')
ABU_DT2 <- fread('data/ABU_adm2.csv')

ABU_adm0 <- ABU_DT0[,.(location_code, year, mean, lower, upper)]
ABU_adm1 <- ABU_DT1[,.(location_code, year, mean, lower, upper)]
ABU_adm2 <- ABU_DT2[,.(location_code, year, mean, lower, upper)]

shp <- st_read("shapefiles/GBD2020_simplified.shp")
adm0_shp <- st_read('shapefiles/lbd_standard_admin_0_simplified.shp')
adm1_shp <- st_read('shapefiles/lbd_standard_admin_1_simplified.shp')
# adm2_shp <- st_read('shapefiles/lbd_standard_admin_2_simplified.shp')

# Create colour palette and labels
abc_palette <- colorNumeric(palette="YlGnBu", domain=c(0,50), na.color="transparent")
abu_cols <- read.csv('ABU_colour_palette.csv')
abu_cols <- abu_cols$x
abu_palette <- colorNumeric(palette=abu_cols, domain=c(29,91), na.color="transparent")
labs=c('<30', '45', '60','75', '>90')

#Load and prep data for plots ####
all_locs <- fread('data/all_locations_total_ddds.csv')
my_colours <- fread('colour_scheme.csv') #Can change colour scheme here but want it consistant
atc3 <- fread('data/ATC3_total_DDDs.csv')
my_points <- fread('point_types.csv')
AWaRe <- fread('data/AWaRe.csv')

#factorize options to ensure order of colours
all_locs$location <- factor(all_locs$location, levels = unique(all_locs$location))
atc3$location <- factor(atc3$location, levels = unique(atc3$location))
atc3$ATC3 <- factor(atc3$ATC3, levels = unique(atc3$ATC3))
AWaRe$AWARE <- factor(AWaRe$AWARE, levels = c("Other", "Reserve", "Watch", "Access"))

#Function to move legend when faceting ggplotly
layout_ggplotly <- function(gg, x = -0.02, y = -0.08){
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
  gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
  gg
}

# UI ####
ui <- fluidPage(    
  
  # Give the page a title
  #  titlePanel(title = div("Antibiotic usage and consumption", 
  #                         img(height = 40, width = 110, src='MORU.jpg',style="float:right; padding-right:25px"),
  #                         img(height = 40, width = 110, src='IHME.png',style="float:right; padding-right:25px"),
  #                         img(height = 40, width = 90, src='BDI.jpg',style="float:right; padding-right:25px"),
  #                         img(height = 40, width = 120, src='GRAM.png',style="float:right; padding-right:25px"))),
  navbarPage('Data visualisations', id = 'nav',
             tabPanel('Maps',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "map_type", 
                                      label = "Indicator:", 
                                      choices= c('Total antibiotic consumption',
                                                 'Antibiotic usage in children'),
                                      selected = 'Total antibiotic consumption'),
                          sliderInput(inputId = 'year',
                                      label = 'Year',
                                      min = 2000,
                                      max = 2018,
                                      value = 2000,
                                      sep = '',
                                      step = 1),
                          hr(),
                          helpText(
                            tags$p("The maps displayed here present the results from ",
                                    tags$a(href= "http://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00280-1/fulltext", 
                                           "Browne AJ et al 2021 Global antibiotic consumption and usage in humans, 2000 to 2018: a spatial modelling study",
                                           target="_blank")),
                            tags$p(tags$strong("Total antibiotic consumption"),
                                   " displays maps of the rate of total antibiotic consumption in each country,
                                   expressed as defined daily doses per 1,000 population per day (DDD/1,000/day). 
                                   Hovering over the relevant country will display the modelled estimates of antibiotic 
                                   consumption with the accompanying uncertainty intervals. 
                                   Using the slider will display the estimated rates of antibiotic consumption for each year, from 2000 to 2018."),
                            tags$p(tags$strong("Antibiotic usage in children"),
                                   " displays the caregiver reported proportion of children under five years old,
                                   with symptoms of lower respiratory tract infection (LRI), who received antibiotics for this illness. 
                                   Estimates are for low- and middle-income countries only.
                                   Modelled estimates are displayed at the national level with uncertainty
                                   intervals and zooming into the map will show estimates for administrative level one locations (states). 
                                   Using the slider will display the estimated antibiotic usage for each year, from 2000 to 2018."),
                            tags$p(tags$strong("Data sources"), " for modelled estimates of total antibiotic consumption: ",
                                   tags$a(href = "https://www.iqvia.com/insights/the-iqvia-institute/available-iqvia-data", HTML(paste0("IQVIA",tags$sup("TM"), " database, ")), target="_blank"),
                                   tags$a(href = "https://www.ecdc.europa.eu/en/about-us/partnerships-and-networks/disease-and-laboratory-networks/esac-net", "European Center for Disease Control, ", target="_blank"),
                                   tags$a(href = "https://www.who.int/publications-detail-redirect/who-report-on-surveillance-of-antibiotic-consumption", "World Health Organization,", target="_blank"),
                                   " and published literature. Data sources for modelled estimates of antibiotic usage in children under 5 years old: ", 
                                   tags$a(href= "https://dhsprogram.com/", "Demographic Health Surveys, ", target="_blank"),
                                   tags$a(href= "https://mics.unicef.org/", "Multiple Indicator Cluster Surveys. ", target="_blank"),
                                   "All are described in detail in Browne AJ et al.")
                          )
                        ),
                        mainPanel(
                          leafletOutput(outputId = "map"),
                          h6("If you use these modelled estimates on antibiotic consumption and antibiotic usage, please cite this publication as a reference: ",
                             tags$a(href= "http://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00280-1/fulltext",
                                    "Browne AJ, Chipeta MG, Haines-Woodhouse G, et al. Global antibiotic consumption and usage in humans, 2000 to 2018: a spatial modelling study. Lancet Planetary Health 2021", target="_blank")),
                      fillRow(img(height = 40, width = 120, src='MORU.jpg',style="padding-right:25px"),
                                  img(height = 40, width = 110, src='BDI.jpg',style="padding-right:25px"),
                                  img(height = 40, width = 120, src='GRAM.png',style="padding-right:25px"),
                                  img(height = 40, width = 150, src='OCGHR.jpg',style="padding-right:25px"),
                                  img(height = 40, width = 140, src='IHME.png',style="padding-right:25px"), height = 40)
                        )
                        )
                      ),
             tabPanel('Plots',
                      sidebarLayout(
                        # Define the sidebar with one input
                        sidebarPanel(
                          selectInput(inputId = "plot", 
                                      label = "Plot:", 
                                      choices= c('Total antibiotic consumption',
                                                 'ATC level 3 antibiotic consumption',
                                                 'AWaRe antibiotic consumption'),
                                      selected = 'Total antibiotic consumption'),
                          conditionalPanel(condition = "input.plot !='AWaRe antibiotic consumption'",
                            selectInput(inputId = "location", 
                                        label = "Location:", 
                                        choices= unique(all_locs$location),
                                        selected = 'Global',
                                        multiple = TRUE)),
                          conditionalPanel(condition = "input.plot =='AWaRe antibiotic consumption'",
                                           selectInput(inputId = "location2", 
                                                       label = "Location:", 
                                                       choices= unique(AWaRe$location),
                                                       selected = NULL,
                                                       multiple = TRUE)),
                          #only show this panel if ATC3 plot
                          conditionalPanel(condition = "input.plot =='ATC level 3 antibiotic consumption'",
                                           selectInput(inputId = "ATC3", 
                                                       label = "ATC level 3 antibiotic class:", 
                                                       choices= unique(atc3$ATC3),
                                                       selected = 'J01A',
                                                       multiple = TRUE)),
                       
                          hr(),
                          helpText(tags$p("The plots displayed here present the results from ",
                                          tags$a(href= "http://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00280-1/fulltext", 
                                                 "Browne AJ et al 2021 Global antibiotic consumption and usage in humans, 2000 to 2018: a spatial modelling study", target="_blank")),
                                   tags$p(tags$strong("Total antibiotic consumption"),
                                          " shows the rate of total antibiotic consumption expressed as defined daily doses per 1,000 population per day (DDD/1,000/day) 
                                          for each location selected. Select multiple locations to compare antibiotic consumption."),
                                   tags$p(tags$strong("ATC level 3 antibiotic consumption"),
                                          " shows antibiotic consumption rates by ATC level 3 classes for various locations expressed as DDD/1,000/day. Select the locations and ATC3 classes to compare."),
                                   tags$p(tags$strong("AWaRe antibiotic consumption"),
                                           " shows the proportion of antibiotic consumption in each country and year according to the WHO Access, Watch and Reserve categories. 
                                           These estimates are based on the ",                                          tags$a(href = "https://www.iqvia.com/insights/the-iqvia-institute/available-iqvia-data", HTML(paste0("IQVIA",tags$sup("TM"), " database, ")))," and the locations reflect the categorisation within that data. 
                                           Select locations for comparisons"),
                                   tags$p(tags$strong("Data sources"), " for modelled estimates of total antibiotic consumption and ATC level 3 antibiotic consumption: ",
                                          tags$a(href = "https://www.iqvia.com/insights/the-iqvia-institute/available-iqvia-data", HTML(paste0("IQVIA",tags$sup("TM"), " database, ")), target="_blank"),
                                          tags$a(href = "https://www.ecdc.europa.eu/en/about-us/partnerships-and-networks/disease-and-laboratory-networks/esac-net", "European Center for Disease Control, ", target="_blank"),
                                          tags$a(href = "https://www.who.int/publications-detail-redirect/who-report-on-surveillance-of-antibiotic-consumption", "World Health Organization,", target="_blank"),
                                          " and the published literature as described in detail in Browne AJ et al."),
                                   
                          )
                          ),
                        
                        # Create a spot for the barplot
                        mainPanel(
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"),
                          plotlyOutput("DDD_Plot"),
                          h6("If you use these modelled estimates on antibiotic consumption and antibiotic usage, please cite this publication as a reference: ",
                                   tags$a(href= "http://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00280-1/fulltext",
                                          "Browne AJ, Chipeta MG, Haines-Woodhouse G, et al. Global antibiotic consumption and usage in humans, 2000 to 2018: a spatial modelling study. Lancet Planetary Health 2021", target="_blank")),
                          fillRow(img(height = 40, width = 120, src='MORU.jpg',style="padding-right:25px"),
                                  img(height = 40, width = 110, src='BDI.jpg',style="padding-right:25px"),
                                  img(height = 40, width = 120, src='GRAM.png',style="padding-right:25px"),
                                  img(height = 40, width = 150, src='OCGHR.jpg',style="padding-right:25px"),
                                  img(height = 40, width = 140, src='IHME.png',style="padding-right:25px"), height = 40)                          )
                        )),
             tabPanel('Model estimates',
                      sidebarLayout(
                        # Define the sidebar with one input
                        sidebarPanel(
                          selectInput(inputId = "data_type", 
                                      label = "Indicator:", 
                                      choices= c('Antibiotic consumption',
                                                 'Antibiotic usage in children'),
                                      selected = 'Antibiotic consumption'),
                          conditionalPanel(condition = "input.data_type =='Antibiotic consumption'",
                                           selectInput(inputId = "abx_group", 
                                                       label = "Antibiotic grouping:", 
                                                       choices= c('Total antibiotic consumption', 'ATC level 3 antibiotic consumption'),
                                                       selected = 'Total antibiotic consumption',
                                                       multiple = FALSE)),
                          conditionalPanel(condition = "input.data_type =='Antibiotic usage in children'",
                                           selectInput(inputId = "resolution", 
                                                       label = "Resolution:", 
                                                       choices= c('National', 'Admin 1 (State)', 'Admin 2 (District)'),
                                                       selected = 'National',
                                                       multiple = FALSE)),
                          downloadButton("download_output", "Download"),
                          hr(),
                          helpText(
                            tags$p("The data displayed here are the modelled results from ",
                                   tags$a(href= "http://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00280-1/fulltext", 
                                          "Browne AJ et al 2021 Global antibiotic consumption and usage in humans, 2000 to 2018: a spatial modelling study"), target="_blank"),
                            tags$p(tags$strong("Antibiotic consumption"),
                                   " displays the modelled estimates of rate of total antibiotic consumption for each location and year
                                   expressed as defined daily doses per 1,000 population per day (DDD/1,000/day), with accompanying uncertainty intervals."),
                            tags$p(tags$strong("Antibiotic usage in children"),
                                   " displays the modelled estimates of the caregiver reported proportion of children under five years old,
                                   with symptoms of lower respiratory tract infection (LRI), 
                                   who recieved antibiotics for this illness, with accompanying uncertainty intervals. 
                                   Estimates are for low- and middle-income countries only and are availalbe for national, administrative 
                                   level one (states) and administrative level two (districts) locations."),
                            tags$p(tags$strong("Data sources"), " for modelled estimates of antibiotic consumption: ",
                                   tags$a(href = "https://www.iqvia.com/insights/the-iqvia-institute/available-iqvia-data", HTML(paste0("IQVIA",tags$sup("TM"), " database, ")), target="_blank"),
                                   tags$a(href = "https://www.ecdc.europa.eu/en/about-us/partnerships-and-networks/disease-and-laboratory-networks/esac-net", "European Center for Disease Control, ", target="_blank"),
                                   tags$a(href = "https://www.who.int/publications-detail-redirect/who-report-on-surveillance-of-antibiotic-consumption", "World Health Organization,", target="_blank"),
                                   " and published literature. Data sources for modelled estimates of antibiotic usage in children under 5 years old: ", 
                                   tags$a(href= "https://dhsprogram.com/", "Demographic Health Surveys, ", target="_blank"),
                                   tags$a(href= "https://mics.unicef.org/", "Multiple Indicator Cluster Surveys. ", target="_blank"),
                                   "All are described in detail in Browne AJ et al.")
                          )
                        ),
                      mainPanel(
                        dataTableOutput("mytable"),
                        h6("If you use these modelled estimates on antibiotic consumption and antibiotic usage, please cite this publication as a reference: ",
                           tags$a(href= "http://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00280-1/fulltext",
                                  "Browne AJ, Chipeta MG, Haines-Woodhouse G, et al. Global antibiotic consumption and usage in humans, 2000 to 2018: a spatial modelling study. Lancet Planetary Health 2021", target="_blank")),
                        fillRow(img(height = 40, width = 120, src='MORU.jpg',style="padding-right:25px"),
                                img(height = 40, width = 110, src='BDI.jpg',style="padding-right:25px"),
                                img(height = 40, width = 120, src='GRAM.png',style="padding-right:25px"),
                                img(height = 40, width = 150, src='OCGHR.jpg',style="padding-right:25px"),
                                img(height = 40, width = 140, src='IHME.png',style="padding-right:25px"), height = 40)   
                        ) ) 
                      )
             )
  )



# Server ####
server <- function(input, output) {
  
  #Plot the abx consumption map
  output$map <- renderLeaflet({
    if(input$map_type =='Total antibiotic consumption'){
    #filter data depending on selected date
    filtered_data <- abc[abc$year == input$year,]

    #prep shapefile
    filtered_shp <- merge(shp, filtered_data, by.x = 'loc_name', by.y = 'Country')

    #Set up hover text
    mytext <- paste(
      "Country: ", filtered_shp$loc_name,"<br/>",
      "Year: ", filtered_shp$year,"<br/>",
      "Antibiotic consumption: ", round(filtered_shp$measure,1), "<br/>",
      "Uncertainty Interval: ", round(filtered_shp$lower,1),"-", round(filtered_shp$upper,1),
      sep="") %>%
      lapply(htmltools::HTML)

    #Plot the map
    leaflet(filtered_shp) %>%
      addTiles()  %>%
      setView(lat=10, lng=0, zoom=2) %>%
      addPolygons(fillColor = ~abc_palette(measure),
                  stroke=TRUE,
                  fillOpacity = 0.9,
                  color="black",
                  weight=0.3,
                  label = mytext,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"),
                  highlightOptions = highlightOptions(color = "black", weight = 3,
                                                      bringToFront = TRUE)
      ) %>%
      addLegend(pal=abc_palette,
                values=~measure,
                opacity=0.9,
                title = "DDD/1,000/day",
                position = "bottomleft")
    }else{
      #code for abx usage map
      #filter data depending on selected date
      filtered_adm0 <- ABU_adm0[ABU_adm0$year == input$year,]
      filtered_adm1 <- ABU_adm1[ABU_adm1$year == input$year,]
      # filtered_adm2 <- ABU_adm2[ABU_adm2$year == input$year,]

      #prep shapefiles
      adm0 <- merge(adm0_shp, filtered_adm0, by.x = 'lctn_cd', by.y = 'location_code')
      adm1 <- merge(adm1_shp, filtered_adm1, by.x = 'lctn_cd', by.y = 'location_code')
      # adm2 <- merge(adm2_shp, filtered_adm2, by.x = 'lctn_cd', by.y = 'location_code')

      #Set up hover text
      adm0_txt <- paste(
        "Country: ", adm0$locatin,"<br/>",
        "Year: ", adm0$year,"<br/>",
        "Antibiotic usage: ", adm0$mean, "%<br/>",
        "Uncertainty Interval: ", adm0$lower,"-", adm0$upper, "%",
        sep="") %>%
        lapply(htmltools::HTML)

      adm1_txt <- paste(
        "Location: ", adm1$locatin,"<br/>",
        "Country: ", adm1$country,"<br/>",
        "Year: ", adm1$year,"<br/>",
        "Antibiotic usage: ", adm1$mean, "%<br/>",
        "Uncertainty Interval: ", adm1$lower,"-", adm1$upper, "%",
        sep="") %>%
        lapply(htmltools::HTML)

      # adm2_txt <- paste(
      #   "Location: ", adm2$locatin,"<br/>",
      #   "Country: ", adm2$country,"<br/>",
      #   "Year: ", adm2$year,"<br/>",
      #   "Antibiotic usage: ", adm2$mean, "%<br/>",
      #   "Uncertainty Interval: ", adm2$lower,"-", adm2$upper, "%",
      #   sep="") %>%
        # lapply(htmltools::HTML)

      # restrict the range of values to improve plotting
      adm0$mean[adm0$mean<30] <- 29
      adm0$mean[adm0$mean>90] <- 91
      adm1$mean[adm1$mean<30] <- 29
      adm1$mean[adm1$mean>90] <- 91
      
      # Plot map with various resolutions at different zooms
      leaflet() %>%
        addTiles()  %>%
        setView(lat=10, lng=0, zoom=2) %>%
        addPolygons(data = adm0,
                    fillColor = ~abu_palette(mean),
                    stroke=TRUE,
                    smoothFactor = .5,
                    fillOpacity = 0.9,
                    color="black",
                    weight=0.3,
                    highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                    label = adm0_txt,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "13px",
                      direction = "auto"),
                    group = 'adm0')%>%
        addPolygons(data = adm1,
                    fillColor = ~abu_palette(mean),
                    stroke=TRUE,
                    smoothFactor = .5,
                    fillOpacity = 0.9,
                    color="black",
                    weight=0.3,
                    highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                    label = adm1_txt,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "13px",
                      direction = "auto"),
                    group = 'adm1')%>%
        # addPolygons(data = adm2,
        #             fillColor = ~abu_palette(mean),
        #             stroke=TRUE,
        #             smoothFactor = .5,
        #             fillOpacity = 0.9,
        #             color="black",
        #             weight=0.3,
        #             highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
        #             label = adm2_txt,
        #             labelOptions = labelOptions(
        #               style = list("font-weight" = "normal", padding = "3px 8px"),
        #               textsize = "13px",
        #               direction = "auto"),
        #             group = 'adm2')%>%
        groupOptions("adm0", zoomLevels = 1:3)%>%
        groupOptions("adm1", zoomLevels = 4:20)%>%
        # groupOptions("adm2", zoomLevels = 6:10)%>%
        addLegend(pal=abu_palette,
                  values=c(29:91),
                  labFormat = function(type, cuts, p) {  # Here's the trick
                    paste0(labs)},
                  opacity=0.9,
                  title = "Antibiotic usage (%)",
                  position = "bottomleft")

    }
  })

  # Plot of options total abx consumption / ATC3 / AWaRe
  output$DDD_Plot <- renderPlotly({
    
    #Select the right dataset and countries
    if(input$plot == 'Total antibiotic consumption'){
      my_locs <- all_locs[all_locs$location %in% input$location,]
    }else if(input$plot == 'ATC level 3 antibiotic consumption'){
      my_locs <- atc3[atc3$location %in% input$location,]
      my_ATC3 <- my_locs[my_locs$ATC3 %in% input$ATC3,]
    }else if(input$plot == 'AWaRe antibiotic consumption'){
      my_locs <- AWaRe[AWaRe$location %in% input$location2,]
    }  
    
    #Get the colours for the countries
    select_cols <- my_colours$my_colours[my_colours$location %in% input$location]
    #Select point type for ATC3 class
    select_points <- my_points$point_type[my_points$ATC3 %in% input$ATC3]
    
    #Set up the plots
    if(input$plot == 'Total antibiotic consumption'){
      p <-
        ggplot(my_locs, aes(group = location, text = paste0('</br>Location: ', location,
                                                            '</br>Year: ', year,
                                                            '</br>DDD/1,000/day: ', `DDD/1,000/day`,
                                                            '</br>Uncertainty interval: ', lower, '-', upper)))+
        geom_line(aes(x = year, y = `DDD/1,000/day`, colour = location), size = 1)+
        geom_ribbon(aes(x = year, ymin = lower, ymax = upper, fill = location), alpha = 0.2)+
        scale_fill_manual(values = select_cols)+
        scale_colour_manual(values = select_cols)+
        scale_x_continuous('Year', breaks = seq(2000,2018,1), labels = seq(2000,2018,1))+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        guides(fill = 'none')+
        labs(x = 'Year', y = 'Defined Daily Doses per 1000 population per day', colour = NULL, fill = NULL)+
        ylim(0,46)
    }

    if(input$plot == 'ATC level 3 antibiotic consumption'){
      p <-
        ggplot(my_ATC3, aes(group = location, text = paste0('</br>Location: ', location,
                                                            '</br>Year: ', year,
                                                            '</br>ATC Class: ', ATC3,
                                                            '</br>DDD/1,000/day: ', `DDD/1,000/day`)))+
        geom_line(aes(x = year, y = `DDD/1,000/day`, group = ATC3, colour = location), size = 1)+
        geom_point(aes(x = year, y = `DDD/1,000/day`, shape = ATC3))+
        scale_colour_manual(values = select_cols)+
        scale_shape_manual(values = select_points)+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        scale_x_continuous('Year', breaks = seq(2000,2018,1), labels = seq(2000,2018,1))+
        guides(colour = 'none', shape = 'none')+
        labs(x = 'Year', y = 'Defined Daily Doses per 1000 population per day', colour = 'Location', linetype = 'ATC3 Class')
    }
    
    if(input$plot =='AWaRe antibiotic consumption'){
      p <- 
        ggplot(my_locs, aes(x = year, y = proportion, fill = AWARE,
                                 text = paste0('</br>Location: ', location,
                                               '</br>Year: ', year,
                                               '</br>AWaRe category: ', AWARE,
                                               '</br>Percentage of consumption: ', round(proportion*100,1), '%')))+
        geom_bar(position="fill", stat="identity")+
        geom_hline(yintercept = 0.6, linetype = "dashed", color='black', size=1)+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        scale_fill_manual(values = c('#984ea3', '#4daf4a', '#377eb8', '#e41a1c'))+
        scale_y_continuous("Percentage of antibiotics consumed", breaks = c(0, .25, .50, .75, 1), labels = c('0', '25', '50', '75', '100'), expand = c(0, 0))+
        scale_x_continuous('Year', breaks = seq(2000,2018,1), labels = seq(2000,2018,1), expand = c(0, 0))+
        # theme(legend.position = 'bottom',
        #       axis.title=element_text(size=8))+
        guides(fill = 'none')+
        facet_wrap(~location, ncol = 1)
      
    }
    
    #Make plot interactive
    gp <- ggplotly(p, tooltip = "text") 
    
    #Sort out legend (remove () and ,1)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    if(input$plot == 'AWaRe antibiotic consumption'){
      layout_ggplotly(gp, x = -0.15, y = -0.05)
    }else{gp}
    
  })
  
  # Load up the data tables for consumption and usage estimates
  output$mytable <- renderDataTable({
    if(input$data_type == 'Antibiotic consumption' & 
       input$abx_group == 'Total antibiotic consumption'){
      all_locs[,.(Location = location, Year = year, `Antibiotic consumption (DDD/1,000/day)` = `DDD/1,000/day`,
                  `Lower uncertainty interval` = lower, `Upper uncertainty interval` = upper)]
    }else if(input$data_type == 'Antibiotic consumption' & 
             input$abx_group == 'ATC level 3 antibiotic consumption'){
      atc3[,.(Location = location, Year = year, `ATC level 3 class` = ATC3,
              `Antibiotic consumption (DDD/1,000/day)` = `DDD/1,000/day`)]
    }else if(input$data_type == 'Antibiotic usage in children' &
       input$resolution == 'National'){
      ABU_DT0[,.(Country = country, Year = year, `Antibiotic usage (%)` = mean, `Lower uncertainty interval (%)` = lower, `Upper uncertainty interval (%)` = upper)]
    }else if(input$data_type == 'Antibiotic usage in children' &
       input$resolution == 'Admin 1 (State)'){
      ABU_DT1[,.(Country = country, `State (admin 1)` = adm1, Year = year, `Antibiotic usage (%)` = mean, `Lower uncertainty interval (%)` = lower, `Upper uncertainty interval (%)` = upper)]    
      }else if(input$data_type == 'Antibiotic usage in children' &
       input$resolution == 'Admin 2 (District)'){
      ABU_DT2[,.(Country = country, `State (admin 1)` = adm1, `District (admin 2)` = adm2, Year = year, `Antibiotic usage (%)` = mean, `Lower uncertainty interval (%)` = lower, `Upper uncertainty interval (%)` = upper)] 
      }
  }
)
  
  output$download_output <- downloadHandler(
    filename = function(){
      if(input$data_type == 'Antibiotic consumption' & 
         input$abx_group == 'Total antibiotic consumption'){
        'total_antibiotic_consumption_estimates.csv'
      }else if(input$data_type == 'Antibiotic consumption' & 
               input$abx_group == 'ATC level 3 antibiotic consumption'){
        'antibiotic_consumption_by_ATC3.csv'
      }else if(input$data_type == 'Antibiotic usage in children' &
               input$resolution == 'National'){
        'antibiotic_usage_in_children_national_estimates.csv'
      }else if(input$data_type == 'Antibiotic usage in children' &
               input$resolution == 'Admin 1 (State)'){
        'antibiotic_usage_in_children_admin1_estimates.csv'
      }else if(input$data_type == 'Antibiotic usage in children' &
               input$resolution == 'Admin 2 (District)'){
        'antibiotic_usage_in_children_admin2_estimates.csv'
      }
    },
    content = function(file) {
      write.csv( if(input$data_type == 'Antibiotic consumption'& 
                    input$abx_group == 'Total antibiotic consumption'){
        all_locs[,.(Location = location, Year = year, `Antibiotic consumption (DDD/1,000/day)` = `DDD/1,000/day`,
                    `Lower uncertainty interval` = lower, `Upper uncertainty interval` = upper)]
      }else if(input$data_type == 'Antibiotic consumption' & 
               input$abx_group == 'ATC level 3 antibiotic consumption'){
        atc3[,.(Location = location, Year = year, `ATC level 3 class` = ATC3,
                `Antibiotic consumption (DDD/1,000/day)` = `DDD/1,000/day`)]
      }else if(input$data_type == 'Antibiotic usage in children' &
               input$resolution == 'National'){
        ABU_DT0[,.(Country = country, Year = year, `Antibiotic usage (%)` = mean, `Lower uncertainty interval (%)` = lower, `Upper uncertainty interval (%)` = upper)]
      }else if(input$data_type == 'Antibiotic usage in children' &
               input$resolution == 'Admin 1 (State)'){
        ABU_DT1[,.(Country = country, `State (admin 1)` = adm1, Year = year, `Antibiotic usage (%)` = mean, `Lower uncertainty interval (%)` = lower, `Upper uncertainty interval (%)` = upper)]    
      }else if(input$data_type == 'Antibiotic usage in children' &
               input$resolution == 'Admin 2 (District)'){
        ABU_DT2[,.(Country = country, `State (admin 1)` = adm1, `District (admin 2)` = adm2, Year = year, `Antibiotic usage (%)` = mean, `Lower uncertainty interval (%)` = lower, `Upper uncertainty interval (%)` = upper)] 
      }, 
                file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
