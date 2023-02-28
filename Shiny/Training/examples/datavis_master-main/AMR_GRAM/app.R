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
library(viridis)
library(shinyWidgets)

# load in lookup table
lookup <- fread('file_lookup.csv')
my_shp <- st_read('shapefile/GBD2020_simplified.dbf')

#set up colour schemes
my_palette <- colorNumeric(palette="inferno", domain=c(0,100), na.color="transparent", reverse = TRUE)
my_colours <- fread('colour_scheme.csv')

#set up the shiny app
# UI ####
ui <- fluidPage(    
  # Give the page a title
  titlePanel(title = div("Prevalence of antimicrobial resistance (AMR)", 
                         img(height = 40, width = 110, src='MORU.jpg',style="float:right; padding-right:25px"),
                         img(height = 40, width = 110, src='IHME.png',style="float:right; padding-right:25px"),
                         img(height = 40, width = 90, src='BDI.jpg',style="float:right; padding-right:25px"),
                         img(height = 40, width = 120, src='GRAM.png',style="float:right; padding-right:25px"))),
  navbarPage('Data visualisations', id = 'nav',
             tabPanel('Maps',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "pathogen", 
                                      label = "Pathogen:", 
                                      choices= c('', sort(unique(lookup$bug))),
                                      selected = NULL),
                          selectInput(inputId = "drug", 
                                      label = "Antibiotic:", 
                                      choices= "",
                                      selected = NULL),
                          sliderTextInput(inputId = 'year',
                                      label = 'Year',
                                      choices = seq(1990,2018,1),
                                      grid = T),
                          hr(),
                          helpText("Project Description ...")
                        ),
                        mainPanel(
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"),
                          leafletOutput(outputId = "map"))
                      )
             ),
             tabPanel('Plots',
                      sidebarLayout(
                        # Define the sidebar with one input
                        sidebarPanel(
                          selectInput(inputId = "pathogen2", 
                                      label = "Pathogen:", 
                                      choices= c('', sort(unique(lookup$bug))),
                                      selected = 'NULL'),
                          selectInput(inputId = "drug2", 
                                      label = "Antibiotic:", 
                                      choices= "",
                                      selected = 'NULL'),
                          selectInput(inputId = "location", 
                                      label = "Location:", 
                                      choices= "",
                                      selected = 'NULL',
                                      multiple = TRUE),
                          hr(),
                          helpText("Project description")
                        ),
                        mainPanel(
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"), 
                          plotlyOutput("plot")  
                        )
                      )
             ),
             tabPanel('Modelled estimates',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "pathogen3", 
                                      label = "Pathogen:", 
                                      choices= c('', sort(unique(lookup$bug))),
                                      selected = NULL),
                          selectInput(inputId = "drug3", 
                                      label = "Antibiotic:", 
                                      choices= "",
                                      selected = NULL),
                          downloadButton("download_output", "Download"),
                          hr(),
                          helpText("Project description")
                        ),
                        mainPanel(
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"),
                          dataTableOutput("output_data_table")  
                        )
                      )
             ),
             tabPanel('Input data',
                      sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "pathogen4", 
                                        label = "Pathogen:", 
                                        choices=  c('', sort(unique(lookup$bug))),
                                        selected = NULL),
                            selectInput(inputId = "drug4", 
                                        label = "Antibiotic:", 
                                        choices= '',
                                        selected = NULL),
                            downloadButton("download_input", "Download"),
                          hr(),
                          helpText("Project description")
                        ),
                        mainPanel(
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"),
                          dataTableOutput("input_data_table")  
                        )
                      )
             )
  )
)



# Server ####
server <- function(input, output, session) {
  
  #read in the data selected for each tab
  map_data <- reactive({
    temp_data <- fread(paste0('model_estimates/', lookup$filename[lookup$bug == input$pathogen & lookup$drug == input$drug], '.csv'))
    colnames(temp_data)[colnames(temp_data) == 'location_id'] <- 'loc_id'  # some files appear to have location_id and some loc_id so making compatable with both  
    temp_data
  })
  
  plot_data <- reactive({
    temp_data <- fread(paste0('model_estimates/', lookup$filename[lookup$bug == input$pathogen2 & lookup$drug == input$drug2], '.csv'))
    colnames(temp_data)[colnames(temp_data) == 'location_id'] <- 'loc_id'  # some files appear to have location_id and some loc_id so making compatable with both  
    merge(temp_data, my_shp, by = 'loc_id')
  })
  
  output_data <- reactive({
    temp_data <-fread(paste0('model_estimates/', lookup$filename[lookup$bug == input$pathogen3 & lookup$drug == input$drug3], '.csv'))
    colnames(temp_data)[colnames(temp_data) == 'location_id'] <- 'loc_id'  # some files appear to have location_id and some loc_id so making compatable with both  
    merge(temp_data, my_shp, by = 'loc_id')
  })
  
  input_data <- reactive({
    fread(paste0('input_data/', lookup$filename[lookup$bug == input$pathogen4 & lookup$drug == input$drug4], '.csv'))
  })
  
  #Update dropdown options for antibiotics used for the bug selected
  observeEvent(input$pathogen,{
    updateSelectInput(session, 'drug', choices = c('', lookup$drug[lookup$bug == input$pathogen]), selected = NULL)
  })
  
  observeEvent(input$pathogen2,{
    updateSelectInput(session, 'drug2', choices = c('', lookup$drug[lookup$bug == input$pathogen2]), selected = NULL)
    updateSelectInput(session, 'location', choices = '', selected = NULL)
  })
  
  observeEvent(input$pathogen3,{
    updateSelectInput(session, 'drug3', choices = c('', lookup$drug[lookup$bug == input$pathogen3]), selected = NULL)
  })
  
  observeEvent(input$pathogen4,{
    updateSelectInput(session, 'drug4', choices = c('', lookup$drug[lookup$bug == input$pathogen4]), selected = NULL)
  })
  
  observeEvent(input$drug2,{
  if(is.null(input$drug2)|is.na(input$drug2)|input$drug2==""){
  }else{
    updateSelectInput(session, 'location', choices = sort(unique(plot_data()$loc_name)), selected = NULL)
  }})

  #Plot the map of resistance
  output$map <- renderLeaflet({
    
    #Update the slider to only show years for that drug-bug combination
    updateSliderTextInput(session, 'year', choices = seq(min(map_data()$year_id), max(map_data()$year_id),1))

    #select year
    filtered_data <- map_data()[year_id == input$year,]
    #merge onto shapefile
    filtered_data <- merge(my_shp, filtered_data)
    
    #set up hover text
    my_txt <- paste(
      "Country: ", filtered_data$loc_name,"<br/>",
      "Year: ", filtered_data$year_id,"<br/>",
      "Prevalence of resistance: ", round(filtered_data$gpr_mean*100,1), "%<br/>",
      "Uncertainty Interval: ", round(filtered_data$gpr_lower*100,1),"-", round(filtered_data$gpr_upper*100,1), "%",
      sep="") %>%
      lapply(htmltools::HTML)
    
    #plot the map
    leaflet() %>%
      addTiles()  %>%
      setView(lat=20, lng=0, zoom=2) %>%
      addPolygons(data = filtered_data,
                  fillColor = ~my_palette(gpr_mean*100),
                  stroke=TRUE,
                  smoothFactor = .5,
                  fillOpacity = 0.9,
                  color="black",
                  weight=0.3,
                  highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                  label = my_txt,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"))%>%
      addLegend(pal=my_palette,
                values=c(0:100),
                opacity=0.9,
                title = 'Prevalence of AMR (%)',
                position = "bottomleft")
  })

  #Plots of resistance over time time
  output$plot <- renderPlotly({

    #Get the colours for the countries
    select_cols <- my_colours$my_colours[my_colours$location %in% input$location]

    p <-
      ggplot(plot_data()[loc_name%in%input$location], aes(group = loc_name, text = paste0('</br>Location: ', loc_name,
                                                            '</br>Year: ', year_id,
                                                            '</br>Prevalence of resistance: ', round(gpr_mean*100,1), '%',
                                                            '</br>Uncertainty interval: ', round(gpr_lower*100,1), '-', round(gpr_upper*100,1), '%')))+
      geom_line(aes(x = year_id, y = gpr_mean*100, colour = loc_name), size = 1)+
      geom_ribbon(aes(x = year_id, ymin = gpr_lower*100, ymax = gpr_upper*100, fill = loc_name), alpha = 0.2)+
      scale_fill_manual(values = select_cols)+
      scale_colour_manual(values = select_cols)+
      scale_x_continuous('Year', breaks = seq(1990,2018,1), labels = seq(1990,2018,1))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      guides(fill = 'none')+
      labs(x = 'Year', y = 'Prevalence of resistance (%)', colour = NULL, fill = NULL)+
      ylim(0,100)

    #Make plot interactive
    gp <- ggplotly(p, tooltip = "text")

    #Sort out legend (remove () and ,1)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }

    gp

  })

  output$output_data_table <- renderDataTable({
    output_data()[,.(Country = loc_name, Year = year_id, 
                   `Prevalence resistance (%)` = round(gpr_mean*100,1),
                   `Lower uncertainty interval (%)` = round(gpr_lower*100,1),
                   `Upper uncertainty interval (%)` = round(gpr_upper*100,1))]
    
  })
  
  output$input_data_table <- renderDataTable({
    input_data()[,.(`Source name`= citation_short,
                    Citation = citation,
                    Country = country, 
                    Year = year_id, 
                    `Sample size` = sample_size,
                    `Percentage resistance (%)` = round(val*100,1),	
                    Outlier = is_outlier)]
  })
  
  output$download_output <- downloadHandler(
    filename = function() {
      gsub(" ", "_", paste(input$pathogen3, input$drug3, "modelled_estimates.csv", sep = " "))
    },
    content = function(file) {
      write.csv(output_data()[,.(Country = loc_name, Year = year_id, 
                                  `Prevalence resistance (%)` = round(gpr_mean*100,1),
                                  `Lower uncertainty interval (%)` = round(gpr_lower*100,1),
                                  `Upper uncertainty interval (%)` = round(gpr_upper*100,1))], 
                file, row.names = FALSE)
    }
  )
  
  output$download_input <- downloadHandler(
    filename = function() {
      gsub(" ", "_", paste(input$pathogen4, input$drug4, "input_data.csv", sep = " "))
    },
    content = function(file) {
      write.csv(input_data()[,.(`Source name`= citation_short,
                                Citation = citation,
                                Country = country, 
                                Year = year_id, 
                                `Sample size` = sample_size,
                                `Percentage resistance (%)` = round(val*100,1),	
                                Outlier = is_outlier)], 
                file, row.names = FALSE)
    }
  )
}



shinyApp(ui, server)




