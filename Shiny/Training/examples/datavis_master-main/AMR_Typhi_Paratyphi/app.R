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

# load in data
all_data <- fread('data/all_results.csv')
all_data$location <- factor(all_data$location, levels = unique(all_data$location))
input_data <- fread('data/input_data.csv')

#set colour schemes
my_palette <- colorNumeric(palette="inferno", domain=c(0,100), na.color="transparent", reverse = TRUE)
my_colours <- fread('colour_scheme.csv')

#set up the shiny app
# UI ####
ui <- fluidPage(    
  # Give the page a title
  titlePanel(title = div("Prevalence of antimicrobial resistance", 
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
                                      choices= c('Salmonella Typhi',
                                                 'Salmonella Paratyphi A'),
                                      selected = 'Salmonella Typhi'),
                          selectInput(inputId = "drug", 
                                      label = "Antibiotic:", 
                                      choices= c('Multidrug resistance (MDR)',
                                                 'Fluoroquinolone non-susceptibility (FQNS)',
                                                 'Third-generation cephalosporin (3GC)'),
                                      selected = 'Multidrug resistance (MDR)'),
                          sliderInput(inputId = 'year',
                                      label = 'Year',
                                      min = 1990,
                                      max = 2019,
                                      value = 1990,
                                      sep = '',
                                      step = 1),
                          hr(),
                          helpText("These maps show the estimates of antimicrobial resistance (AMR) in
                                   humans with Salmonella Typhi and Paratyphi A infections.
                                   Estimates are for multidrug resistance (defined as concurrent resistance 
                                  to ampicillin, co-trimoxazole and chloramphenicol); fluoroquinolone 
                                  non-susceptibility (defined as either a ciprofloxacin or nalidixic acid non-susceptibility) and
                                  and third-generation cephalosporin resistance (defined as ceftriaxone resistance). 
                                   Estimates are shown at the national level and at the subnational level (administrive level one) when zooming in. 
                                   Use the slider to select a year, estimates are available to 1990 to 2019. Estimates are not provided for countries classified as non-endemic,
                                    or for small island nationals with populations under 2 million persons.")
                          ),
                          mainPanel(
                            leafletOutput(outputId = "map"))
                          )
             ),
             tabPanel('Plots',
                      sidebarLayout(
                        # Define the sidebar with one input
                        sidebarPanel(
                          selectInput(inputId = "pathogen2", 
                                      label = "Pathogen:", 
                                      choices= c('Salmonella Typhi',
                                                 'Salmonella Paratyphi A'),
                                      selected = 'Salmonella Typhi'),
                          selectInput(inputId = "drug2", 
                                      label = "Antibiotic:", 
                                      choices= c('Multidrug resistance (MDR)',
                                                 'Fluoroquinolone non-susceptibility (FQNS)',
                                                 'Third-generation cephalosporin (3GC)'),
                                      selected = 'Multidrug resistance (MDR)'),
                          selectInput(inputId = "location", 
                                      label = "Location:", 
                                      choices= unique(all_data$location[all_data$level!=3]),
                                      selected = 'Global',
                                      multiple = TRUE),
                        hr(),
                        helpText("These plots show the modelled estimates of the prevalence of antimicrobial resistance (AMR) in
                                   human Salmonella Typhi and Paratyphi A infections for the years 1990 to 2019.
                                   Estimates are for multidrug resistance (defined as concurrent resistance 
                                  to ampicillin, co-trimoxazole and chloramphenicol); fluoroquinolone 
                                  non-susceptibility (defined as either a ciprofloxacin or nalidixic acid non-susceptibility) and third-generation cephalosporin resistance (defined as ceftriaxone resistance). 
                                 Select the pathogen and antibitoic of interest, then select locations to compare from the drop-down lists.")
                      ),
                      mainPanel(
                        plotlyOutput("plot")  
                      )
                      )
                      ),
             tabPanel('Model estimates',
                      sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "pathogen3", 
                                               label = "Pathogen:", 
                                               choices= c('Salmonella Typhi',
                                                          'Salmonella Paratyphi A'),
                                               selected = 'Salmonella Typhi'),
                                   selectInput(inputId = "drug3", 
                                               label = "Antibiotic:", 
                                               choices= c('Multidrug resistance (MDR)',
                                                          'Fluoroquinolone non-susceptibility (FQNS)',
                                                          'Third-generation cephalosporin (3GC)'),
                                               selected = 'Multidrug resistance (MDR)'),
                                   hr(),
                                   helpText("These tables show the modelled estimates of the prevalence of antimicrobial resistance (AMR) 
                                            in Salmonella Typhi and Salmonella Paratyphi A infections for key antibiotics. 
                                            Select the pathogen and antimicrobial you wish to investigate from the drop down list. 
                                            Estimates are available at the global, regional, national and subnational (administrative level one) level. 
                                            All estimates are presented with 95% uncertainty intervals.")
                                 ),
                                 mainPanel(
                                   dataTableOutput("data_table")  
                                 )
                               )
                      ),
             tabPanel('Input data',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "pathogen4", 
                                      label = "Pathogen:", 
                                      choices= c('Salmonella Typhi',
                                                 'Salmonella Paratyphi A'),
                                      selected = 'Salmonella Typhi'),
                          selectInput(inputId = "drug4", 
                                      label = "Antibiotic:", 
                                      choices= c('Multidrug resistance (MDR)',
                                                 'Fluoroquinolone non-susceptibility (FQNS)',
                                                 'Third-generation cephalosporin (3GC)'),
                                      selected = 'Multidrug resistance (MDR)'),
                          hr(),
                          helpText("These tables show the input data for the prevalence of antimicrobial resistance (AMR) 
                                            models for Salmonella Typhi and Salmonella Paratyphi A infections for key antibiotics. 
                                            Select the pathogen and antimicrobial you wish to investigate from the drop down list.")
                        ),
                        mainPanel(
                          dataTableOutput("input_data_table")  
                        )
                      )
             )
  )
)



# Server ####
server <- function(input, output, session) {
  #read in shapefiles
  national_shp <- reactive(st_read('shapefiles/typhi_national_simplified.shp'))
  subnat_shp <- reactive(st_read('shapefiles/typhi_admin1_simplified.shp'))
  
  #read in data for the map
  nat_data <- reactive({
    if(input$pathogen == 'Salmonella Typhi' & input$drug == 'Multidrug resistance (MDR)'){
      fread('data/MDR_Typhi_national.csv')
    }else if(input$pathogen == 'Salmonella Typhi' & input$drug == 'Fluoroquinolone non-susceptibility (FQNS)'){
      fread('data/FQNS_Typhi_national.csv')
    }else if(input$pathogen == 'Salmonella Typhi' & input$drug == 'Third-generation cephalosporin (3GC)'){
      fread('data/3GC_Typhi_national.csv')
    }else if(input$pathogen == 'Salmonella Paratyphi A' & input$drug == 'Multidrug resistance (MDR)'){
      fread('data/MDR_Paratyphi_national.csv')
    }else if(input$pathogen == 'Salmonella Paratyphi A' & input$drug == 'Fluoroquinolone non-susceptibility (FQNS)'){
      fread('data/FQNS_Paratyphi_national.csv')
    }else if(input$pathogen == 'Salmonella Paratyphi A' & input$drug == 'Third-generation cephalosporin (3GC)'){
      fread('data/3GC_Paratyphi_national.csv')
    }else{}
  })
  subnat_data <- reactive({
    if(input$pathogen == 'Salmonella Typhi' & input$drug == 'Multidrug resistance (MDR)'){
      fread('data/MDR_Typhi_subnat.csv')
    }else if(input$pathogen == 'Salmonella Typhi' & input$drug == 'Fluoroquinolone non-susceptibility (FQNS)'){
      fread('data/FQNS_Typhi_subnat.csv')
    }else if(input$pathogen == 'Salmonella Typhi' & input$drug == 'Third-generation cephalosporin (3GC)'){
      fread('data/3GC_Typhi_subnat.csv')
    }else if(input$pathogen == 'Salmonella Paratyphi A' & input$drug == 'Multidrug resistance (MDR)'){
      fread('data/MDR_Paratyphi_subnat.csv')
    }else if(input$pathogen == 'Salmonella Paratyphi A' & input$drug == 'Fluoroquinolone non-susceptibility (FQNS)'){
      fread('data/FQNS_Paratyphi_subnat.csv')
    }else if(input$pathogen == 'Salmonella Paratyphi A' & input$drug == 'Third-generation cephalosporin (3GC)'){
      fread('data/3GC_Paratyphi_subnat.csv')    
    }else{}
  })

  #Plot map
  output$map <- renderLeaflet({
    #select year
    filtered_nat <- nat_data()[year == input$year,]
    filtered_subnat <- subnat_data()[year == input$year,]
    
    #merge onto shapefile
    filtered_nat <- merge(national_shp(), filtered_nat, by.x = 'ihme_lc_id', by.y = 'COUNTRY_ID')
    filtered_subnat <- merge(subnat_shp(), filtered_subnat, by = 'adj_id')
    
    #Set up hover text
    nat_txt <- paste(
      "Country: ", filtered_nat$country,"<br/>",
      "Year: ", filtered_nat$year,"<br/>",
      "Prevalence of resistance: ", round(filtered_nat$p.mean*100,1), "%<br/>",
      "Uncertainty Interval: ", round(filtered_nat$p.lower*100,1),"-", round(filtered_nat$p.upper*100,1), "%",
      sep="") %>%
      lapply(htmltools::HTML)
    
    subnat_txt <- paste(
      "Location: ", filtered_subnat$NAME,"<br/>",
      "Country: ", filtered_subnat$country,"<br/>",
      "Year: ", filtered_subnat$year,"<br/>",
      "Prevalence of resistance: ", round(filtered_subnat$p.mean*100,1), "%<br/>",
      "Uncertainty Interval: ", round(filtered_subnat$p.lower*100,1),"-", round(filtered_subnat$p.upper*100,1), "%",
      sep="") %>%
      lapply(htmltools::HTML)
    
    #plot the map
    leaflet() %>%
      addTiles()  %>%
      setView(lat=20, lng=50, zoom=2) %>%
      addPolygons(data = filtered_nat,
                  fillColor = ~my_palette(p.mean*100),
                  stroke=TRUE,
                  smoothFactor = .5,
                  fillOpacity = 0.9,
                  color="black",
                  weight=0.3,
                  highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                  label = nat_txt,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"),
                  group = 'nat')%>%
      addPolygons(data = filtered_subnat,
                  fillColor = ~my_palette(p.mean*100),
                  stroke=TRUE,
                  smoothFactor = .5,
                  fillOpacity = 0.9,
                  color="black",
                  weight=0.3,
                  highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                  label = subnat_txt,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"),
                  group = 'subnat')%>%
    groupOptions("nat", zoomLevels = 1:3)%>%
      groupOptions("subnat", zoomLevels = 4:10)%>%
      addLegend(pal=my_palette,
                values=c(0:100),
                opacity=0.9,
                title = ifelse(grepl('MDR', input$drug),'MDR (%)', 'FQNS (%)'),
                position = "bottomleft")


    })
  
  #update options to only show countries estimated
  observeEvent(input$pathogen2,{
    updateSelectInput(session, 'location', choices = unique(all_data$location[all_data$level != 3 & all_data$pathogen == input$pathogen2]), selected = 'Global')
  })
  
  #select data for plots
  plot_drugbug <- reactive(all_data[pathogen == input$pathogen2 & drug == input$drug2,])
 
  #plots over time
  output$plot <- renderPlotly({
    #select data to plot
    plot_data <- plot_drugbug()[location %in% input$location,]
    
    #Get the colours for the countries
    select_cols <- my_colours$my_colours[my_colours$location %in% input$location]
    
    #plot out the data
    p <-
      ggplot(plot_data, aes(group = location, text = paste0('</br>Location: ', location,
                                                          '</br>Year: ', year,
                                                          '</br>Prevalence of resistance: ', mean,
                                                          '</br>Uncertainty interval: ', lower, '-', upper)))+
      geom_line(aes(x = year, y = mean, colour = location), size = 1)+
      geom_ribbon(aes(x = year, ymin = lower, ymax = upper, fill = location), alpha = 0.2)+
      scale_fill_manual(values = select_cols)+
      scale_colour_manual(values = select_cols)+
      scale_x_continuous('Year', breaks = seq(1990,2019,1), labels = seq(1990,2019,1))+
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
  
  #data tables
  output$data_table <- renderDataTable({
    all_data[pathogen == input$pathogen3 & drug == input$drug3,
             .(Region, Country, `Administrative level 1`, Year = year,
             `Prevalence resistance (%)` = mean, `Lower uncertainty interval` = lower,
             `Upper uncertainty interval` = upper)]
  })
  
  output$input_data_table <- renderDataTable({
    input_data[pathogen == input$pathogen4 & drug == input$drug4,
               .(`Source name`= citation_short,
                 Citation = citation,
                 `Data source` = data_source,
                 Country = country,
                 District = district,
                 Location = location_name,
                 Year = year,
                 `Sample size` = sample_size,
                 `Percentage resistance (%)` = percent_resistant,	
                 Outlier = is_outlier)]
  })
}

shinyApp(ui, server)

    
                          
                            
