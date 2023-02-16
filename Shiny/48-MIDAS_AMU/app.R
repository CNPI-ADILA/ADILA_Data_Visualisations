################################################################################
################################################################################
####                A SHINY APP TO VISUALISE THE MIDAS AMU DATA             ####
####                              data dictionary:                          ####
####  https://sgul365.sharepoint.com/sites/ADILA/SitePages/AMU%20Data.aspx  ####
################################################################################
################################################################################

#===============================================================================
#===============================================================================
# PACKAGES #  (MIGHT NEED TO LOAD PACKAGES EXPLICITLY WITH library() AND COMMENT OUT ANY install.packages() WHEN DEPLOYING APP ON THE CLOUD (i.e., shinyapps.io) OR AN INTERNAL SERVER???)
rm(list = ls()) 
#turn on autoreload (https://github.com/sol-eng/background-jobs/tree/master/shiny-job)???
#consider using the renv package to preserve longevity???
packages <- c("shiny","shinyjs","shinyWidgets","shinycssloaders","dplyr","tidyr","ggplot2","plotly","leaflet","sf","rgdal","data.table","RColorBrewer")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, require, character.only = TRUE)
# PRE-APP DATA FORMATTING / CLEANING / etc. #
source("data_prep.R")
#===============================================================================
#===============================================================================

########
#APP UI#------------------------------------------------------------------------
########
ui <- fluidPage( #or try theme=shinytheme("...") instead of colour settings in data_prep.R???
                  #theme = bslib::bs_theme(bootswatch = "nameoftheme")
                  #https://rstudio.github.io/shinythemes/

  useShinyjs(),
  
  # App title
  titlePanel("ANTIMICROBIAL USE DATA (IQVIA MIDAS)"),

  # Two visualisations, each in a separate tab (one tab containing several sub-tabs)
  navbarPage("", selected = NULL, collapsible = TRUE,
             
    #----------
    # HEAT MAP
    #----------
    tabPanel("Spatial", fluidRow(column(6)), 
             
      sidebarLayout(
        
        # Sidebar inputs
        sidebarPanel(
          
          # Select input *ANTIMICROBIAL*
          selectInput(inputId = "antimicrobial_map", 
                      label = "Antimicrobial:",
                      choices = c("", sort(unique(data_for_visualisations$antimicrobials))),
                      selected = NULL, 
                      multiple = FALSE
          ),
          
          # Select input *ADMINISTRATION ROUTE*
          selectInput(inputId = "route_map", 
                      label = "Administration route:",
                      choices = c("", sort(unique(data_for_visualisations$route_of_administration))),
                      selected = NULL, 
                      multiple = FALSE
          ),
          
          # Select input *AWaRe*
          selectInput(inputId = "aware_map", 
                      label = "AWaRe category:",
                      choices = c("", sort(unique(data_for_visualisations$aware_category))),
                      selected = NULL, 
                      multiple = FALSE
          ),
          
          # Slider input *SINGLE YEAR OR YEAR RANGE*
          sliderInput(inputId = "years",
                      label = "Year(s):",
                      value = c(min(data_for_visualisations$year, na.rm = T), max(data_for_visualisations$year, na.rm = T)),
                      min = min(data_for_visualisations$year, na.rm = T),
                      max = max(data_for_visualisations$year, na.rm = T),
                      step = 1,
                      sep = "",
                      round = TRUE,
                      ticks = FALSE),
        
          # Choose metric (DDD/DID/DI)
          radioButtons(inputId = "metric_map",
                       label = "AMU metric:",
                       selected = character(0),
                       choices = unique(data_for_visualisations$metric)),
          conditionalPanel(
            condition = "input.metric_map == null", 
            helpText("Please select metric")
          ),
          
          # Action button to load map
          linebreaks(5),
          actionButton(inputId  = "load_map", label = "Load map"),
          hidden(p(id = "please_wait_map", "Please wait while the map loads..."))
          
        ),
        
        # Main panel map (could also add a table below map???)
        mainPanel(
          
          h4(uiOutput("map_title")),
          leafletOutput(outputId = "heat_map", height = 700)

        )
        
      )
      
    ),
    
    #------------
    # LINE PLOTS
    #------------
    navbarMenu("Temporal", fluidRow(column(6)), 
               
      #
      # Antimicrobials
      #
      tabPanel("compare antimicrobials",
               
        sidebarLayout(
          
          # Sidebar inputs 
          sidebarPanel(
            
            # Select input *ADMINISTRATION ROUTE*
            selectInput(inputId = "route_line_by_antimicrobial", 
                        label = "Administration route:", 
                        choices = c("", sort(unique(data_for_visualisations$route_of_administration))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Select input *AWaRe*
            selectInput(inputId = "aware_line_by_antimicrobial", 
                        label = "AWaRe category:", 
                        choices= c("", sort(unique(data_for_visualisations$aware_category))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Select input *COUNTRY*
            selectInput(inputId = "country_line_by_antimicrobial", 
                        label = "Country:", 
                        choices= c("", sort(unique(data_for_visualisations$country))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Choose metric (DDD/DID/DI)
            radioButtons(inputId = "metric_line_by_antimicrobial",
                         label = "AMU metric:",
                         selected = character(0),
                         choices = unique(data_for_visualisations$metric)),
            conditionalPanel(
              condition = "input.metric_line_by_antimicrobial == null", 
              helpText("Please select metric")
            ),
            
            # Action button to load plot
            linebreaks(5),
            actionButton(inputId  = "load_plot_by_antimicrobial", label = "Load plot"),
            hidden(p(id = "please_wait_by_antimicrobial", "Please wait while the plot loads..."))
            
          ),
         
          # Main panel plot (could also add a table below plot???)
          mainPanel(
            
            h4(uiOutput("plot_title_by_antimicrobial")),
            plotlyOutput(outputId = "plot_by_antimicrobial", height = 600) %>% withSpinner(type = 6),

            # Option to de-select lines from the plot ##NOT NEEDED - NOW USING PLOTLY##
            # linebreaks(2),
            # disabled(
            #   checkboxGroupInput(inputId = "line_options_by_antimicrobial",
            #                      label = "",
            #                      choices = sort(unique(data_for_visualisations$antimicrobials)),
            #                      inline = TRUE,
            #                      selected = "All")
            # )
            
          )
          
        )
        
      ),
      
      #
      # Administration routes
      #
      tabPanel("compare administration routes",
               
        sidebarLayout(
          
          # Sidebar inputs 
          sidebarPanel(
            
            # Select input *ANTIMICROBIAL*
            selectInput(inputId = "antimicrobial_line_by_route", 
                        label = "Antimicrobial:", 
                        choices = c("", sort(unique(data_for_visualisations$antimicrobials))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Select input *AWaRe*
            selectInput(inputId = "aware_line_by_route", 
                        label = "AWaRe category:", 
                        choices = c("", sort(unique(data_for_visualisations$aware_category))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Select input *COUNTRY*
            selectInput(inputId = "country_line_by_route", 
                        label = "Country:", 
                        choices = c("", sort(unique(data_for_visualisations$country))),
                        selected = NULL,
                        multiple = FALSE),

            # Choose metric (DDD/DID/DI)
            radioButtons(inputId = "metric_line_by_route",
                         label = "AMU metric:",
                         selected = character(0),
                         choices = unique(data_for_visualisations$metric)),
            conditionalPanel(
              condition = "input.metric_line_by_route == null", 
              helpText("Please select metric")
            ),
            
            # Action button to load plot
            linebreaks(5),
            actionButton(inputId  = "load_plot_by_route", label = "Load plot"),
            hidden(p(id="please_wait_by_route", "Please wait while the plot loads..."))
            
          ),
                 
          # Main panel plot (could also add a table below plot???)
          mainPanel(
            
            h4(uiOutput("plot_title_by_route")),
            plotlyOutput(outputId = "plot_by_route", height = 600) %>% withSpinner(type = 6),
            
            # Option to de-select lines from the plot ##NOT NEEDED - NOW USING PLOTLY##
            # linebreaks(2),
            # disabled(
            #   checkboxGroupInput(inputId = "line_options_by_route",
            #                      label = "",
            #                      choices = sort(unique(data_for_visualisations$route_of_administration)),
            #                      inline = TRUE,
            #                      selected = "All")
            # )
            
          )
          
        )
        
      ),
      
      #
      # AWaRe categories
      #
      tabPanel("compare AWaRe categories",
               
        sidebarLayout(
          
          # Sidebar inputs 
          sidebarPanel(
            
            # Select input *ANTIMICROBIAL*
            selectInput(inputId = "antimicrobial_line_by_aware", 
                        label = "Antimicrobial:", 
                        choices = c("", sort(unique(data_for_visualisations$antimicrobials))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Select input *ADMINISTRATION ROUTE*
            selectInput(inputId = "route_line_by_aware", 
                        label = "Administration route:", 
                        choices = c("", sort(unique(data_for_visualisations$route_of_administration))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Select input *COUNTRY*
            selectInput(inputId = "country_line_by_aware", 
                        label = "Country:", 
                        choices = c("", sort(unique(data_for_visualisations$country))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Choose metric (DDD/DID/DI)
            radioButtons(inputId = "metric_line_by_aware",
                         label = "AMU metric:",
                         selected = character(0),
                         choices = unique(data_for_visualisations$metric)),
            conditionalPanel(
              condition = "input.metric_line_by_aware == null", 
              helpText("Please select metric")
            ),
            
            # Action button to load plot
            linebreaks(5),
            actionButton(inputId  = "load_plot_by_aware", label = "Load plot"),
            hidden(p(id="please_wait_by_aware", "Please wait while the plot loads..."))
            
          ),
                 
          # Main panel plot (could also add a table below plot???)
          mainPanel(
            
            h4(uiOutput("plot_title_by_aware")),
            plotlyOutput(outputId = "plot_by_aware", height = 600) %>% withSpinner(type = 6),
            
            # Option to de-select lines from the plot ##NOT NEEDED - NOW USING PLOTLY##
            # linebreaks(2),
            # disabled(
            #   checkboxGroupInput(inputId = "line_options_by_aware",
            #                      label = "",
            #                      choices = sort(unique(data_for_visualisations$aware_category)),
            #                      inline = TRUE,
            #                      selected = "All")
            # )
            
          )
          
        )
        
      ),
      
      #
      # Countries
      #
      tabPanel("compare countries",
               
        sidebarLayout(
          
          # Sidebar inputs 
          sidebarPanel(
            
            # Select input *ANTIMICROBIAL*
            selectInput(inputId = "antimicrobial_line_by_country", 
                        label = "Antimicrobial:", 
                        choices = c("", sort(unique(data_for_visualisations$antimicrobials))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Select input *ADMINISTRATION ROUTE*
            selectInput(inputId = "route_line_by_country", 
                        label = "Administration route:", 
                        choices = c("", sort(unique(data_for_visualisations$route_of_administration))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Select input *AWaRe*
            selectInput(inputId = "aware_line_by_country", 
                        label = "AWaRe category:", 
                        choices = c("", sort(unique(data_for_visualisations$aware_category))),
                        selected = NULL,
                        multiple = FALSE),
            
            # Choose metric (DDD/DID/DI)
            radioButtons(inputId = "metric_line_by_country",
                         label = "AMU metric:",
                         selected = character(0),
                         choices = unique(data_for_visualisations$metric)),
            conditionalPanel(
              condition = "input.metric_line_by_country == null", 
              helpText("Please select metric")
            ),
            
            # Action button to load plot
            linebreaks(5),
            actionButton(inputId  = "load_plot_by_country", label = "Load plot"),
            hidden(p(id="please_wait_by_country", "Please wait while the plot loads..."))
            
          ),
          
          # Main panel plot (could also add a table below plot???)
          mainPanel(
            
            h4(uiOutput("plot_title_by_country")),
            plotlyOutput(outputId = "plot_by_country", height = 600) %>% withSpinner(type = 6),
            
            # Option to de-select lines from the plot ##NOT NEEDED - NOW USING PLOTLY##
            # linebreaks(2),
            # disabled(
            #   checkboxGroupInput(inputId = "line_options_by_country",
            #                      label = "",
            #                      choices = sort(unique(data_for_visualisations$country)),
            #                      inline = TRUE,
            #                      selected = "Global")
            # )
            
          )
          
        )
        
      ),
      
    ),
    
  )
  
)

############
#APP SERVER#--------------------------------------------------------------------
############
server <- function(input, output, session) {
  
  #
  #
  # Functionality for Load button(s)
  #
  #
  observeEvent(input$load_map, {
    disable("load_map")
    show("please_wait_map")
    delay(200, enable("load_map"))
    delay(200, hide("please_wait_map"))
  })
  observeEvent(input$load_plot_by_antimicrobial, {
    disable("load_plot_by_antimicrobial")
    show("please_wait_by_antimicrobial")
    delay(200, enable("load_plot_by_antimicrobial"))
    delay(200, hide("please_wait_by_antimicrobial"))
  })
  observeEvent(input$load_plot_by_route, {
    disable("load_plot_by_route")
    show("please_wait_by_route")
    delay(200, enable("load_plot_by_route"))
    delay(200, hide("please_wait_by_route"))
  })
  observeEvent(input$load_plot_by_aware, {
    disable("load_plot_by_aware")
    show("please_wait_by_aware")
    delay(200, enable("load_plot_by_aware"))
    delay(200, hide("please_wait_by_aware"))
  })
  observeEvent(input$load_plot_by_country, {
    disable("load_plot_by_country")
    show("please_wait_by_country")
    delay(200, enable("load_plot_by_country"))
    delay(200, hide("please_wait_by_country"))
  })
  
  #
  #
  # Filter data according to selections made for the map / each plot
  #
  #
  data <- reactive({
    data_for_visualisations
  })
    
    #
    # Heat map
    #
      # antimicrobial
      data_map_filter1 <- reactive({
        req(input$antimicrobial_map)
        
        data_map1 <- data() %>%
          filter(antimicrobials == input$antimicrobial_map)
        data_map1
      })
    
      # administration route
      data_map_filter2 <- reactive({
        req(input$antimicrobial_map, input$route_map)
        
        data_map1 <- data_map_filter1() %>%
          filter(route_of_administration == input$route_map)
        data_map1
      })

      # AWaRe category
      data_map_filter3 <- reactive({
        req(input$antimicrobial_map, input$route_map, input$aware_map)
        
        data_map1 <- data_map_filter2() %>%
          filter(aware_category == input$aware_map)
        data_map1
      })

      # year(s)
      data_map_filter4 <- reactive({
        req(input$antimicrobial_map, input$route_map, input$aware_map, input$years)
        
        data_map1 <- data_map_filter3() %>%
          filter(year %in%  seq(min(input$years, na.rm = T), max(input$years, na.rm = T)))
        data_map1
      })
      
      # AMU metric
      data_map_filter5 <- reactive({
        req(input$antimicrobial_map, input$route_map, input$aware_map, input$years, input$metric_map)
        
        data_map1 <- data_map_filter4() %>%
          filter(metric == input$metric_map)
        data_map1
      })
     
    # 
    # Line plots
    #
      # Antimicrobials
        # administration route
        data_line_by_antimicrobial_filter1 <- reactive({
          req(input$route_line_by_antimicrobial)
          
          data_line_by_antimicrobial1 <- data() %>%
            filter(route_of_administration == input$route_line_by_antimicrobial)
          data_line_by_antimicrobial1
        })
        
        # AWaRe category
        data_line_by_antimicrobial_filter2 <- reactive({
          req(input$route_line_by_antimicrobial, input$aware_line_by_antimicrobial)
          
          data_line_by_antimicrobial1 <- data_line_by_antimicrobial_filter1() %>%
            filter(aware_category == input$aware_line_by_antimicrobial)
          data_line_by_antimicrobial1
        })
        
        # country
        data_line_by_antimicrobial_filter3 <- reactive({
          req(input$route_line_by_antimicrobial, input$aware_line_by_antimicrobial, input$country_line_by_antimicrobial)
          
          data_line_by_antimicrobial1 <- data_line_by_antimicrobial_filter2() %>%
            filter(country == input$country_line_by_antimicrobial)
          data_line_by_antimicrobial1
        })

        # AMU metric
        data_line_by_antimicrobial_filter4 <- reactive({
          req(input$route_line_by_antimicrobial, input$aware_line_by_antimicrobial, input$country_line_by_antimicrobial, input$metric_line_by_antimicrobial)
          
          data_line_by_antimicrobial1 <- data_line_by_antimicrobial_filter3() %>%
            filter(metric == input$metric_line_by_antimicrobial)
          data_line_by_antimicrobial1
        })
 
      # Administration routes
        # antimicrobial
        data_line_by_route_filter1 <- reactive({
          req(input$antimicrobial_line_by_route)
          
          data_line_by_route1 <- data() %>%
            filter(antimicrobials == input$antimicrobial_line_by_route)
          data_line_by_route1
        })
        
        # AWaRe category
        data_line_by_route_filter2 <- reactive({
          req(input$antimicrobial_line_by_route, input$aware_line_by_route)
          
          data_line_by_route1 <- data_line_by_route_filter1() %>%
            filter(aware_category == input$aware_line_by_route)
          data_line_by_route1
        })
        
        # country
        data_line_by_route_filter3 <- reactive({
          req(input$antimicrobial_line_by_route, input$aware_line_by_route, input$country_line_by_route)
          
          data_line_by_route1 <- data_line_by_route_filter2() %>%
            filter(country == input$country_line_by_route)
          data_line_by_route1
        })
        
        # AMU metric
        data_line_by_route_filter4 <- reactive({
          req(input$antimicrobial_line_by_route, input$aware_line_by_route, input$country_line_by_route, input$metric_line_by_route)
          
          data_line_by_route1 <- data_line_by_route_filter3() %>%
            filter(metric == input$metric_line_by_route)
          data_line_by_route1
        })
 
      # AWaRe categories
        # antimicrobial
        data_line_by_aware_filter1 <- reactive({
          req(input$antimicrobial_line_by_aware)
          
          data_line_by_aware1 <- data() %>%
            filter(antimicrobials == input$antimicrobial_line_by_aware)
          data_line_by_aware1
        })
        
        # administration route
        data_line_by_aware_filter2 <- reactive({
          req(input$antimicrobial_line_by_aware, input$route_line_by_aware)
          
          data_line_by_aware1 <- data_line_by_aware_filter1() %>%
            filter(route_of_administration == input$route_line_by_aware)
          data_line_by_aware1
        })
        
        # country
        data_line_by_aware_filter3 <- reactive({
          req(input$antimicrobial_line_by_aware, input$route_line_by_aware, input$country_line_by_aware)
          
          data_line_by_aware1 <- data_line_by_aware_filter2() %>%
            filter(country == input$country_line_by_aware)
          data_line_by_aware1
        })
        
        # AMU metric
        data_line_by_aware_filter4 <- reactive({
          req(input$antimicrobial_line_by_aware, input$route_line_by_aware, input$country_line_by_aware, input$metric_line_by_aware)
          
          data_line_by_aware1 <- data_line_by_aware_filter3() %>%
            filter(metric == input$metric_line_by_aware)
          data_line_by_aware1
        })

      # Countries
        # antimicrobial
        data_line_by_country_filter1 <- reactive({
          req(input$antimicrobial_line_by_country)
          
          data_line_by_country1 <- data() %>%
            filter(antimicrobials == input$antimicrobial_line_by_country)
          data_line_by_country1
        })
        
        # administration route
        data_line_by_country_filter2 <- reactive({
          req(input$antimicrobial_line_by_country, input$route_line_by_country)
          
          data_line_by_country1 <- data_line_by_country_filter1() %>%
            filter(route_of_administration == input$route_line_by_country)
          data_line_by_country1
        })
        
        # AWaRe category
        data_line_by_country_filter3 <- reactive({
          req(input$antimicrobial_line_by_country, input$route_line_by_country, input$aware_line_by_country)
          
          data_line_by_country1 <- data_line_by_country_filter2() %>%
            filter(aware_category == input$aware_line_by_country)
          data_line_by_country1
        })
        
        # AMU metric
        data_line_by_country_filter4 <- reactive({
          req(input$antimicrobial_line_by_country, input$route_line_by_country, input$aware_line_by_country, input$metric_line_by_country)
          
          data_line_by_country1 <- data_line_by_country_filter3() %>%
            filter(metric == input$metric_line_by_country)
          data_line_by_country1
        })
          
  #
  #
  # Update the inputs based on selections made
  #
  #
          
    #
    # Heat map
    #
      # administration route
      observe({
        req(input$antimicrobial_map)
        
        D <- data_map_filter1() #i.e., already filtered on antimicrobial
        updateSelectInput(session, inputId = "route_map",
                          label = "Administration route:",
                          choices = c("", sort(unique(D$route_of_administration))),
                          selected = NULL)
      })

      # AWaRe category
      observe({
        req(input$antimicrobial_map, input$route_map)
        
        D <- data_map_filter2()
        updateSelectInput(session, inputId = "aware_map",
                          label = "AWaRe category:",
                          choices = c("", sort(unique(D$aware_category))),
                          selected = NULL)
      })
        
      # year(s)
      observe({
        req(input$antimicrobial_map, input$route_map, input$aware_map)
        
        D <- data_map_filter3()
        updateSliderInput(session, inputId = "years",
                          label = "Year(s):",
                          value = c(min(D$year, na.rm = T), max(D$year, na.rm = T)),
                          min = min(D$year, na.rm = T),
                          max = max(D$year, na.rm = T))
      })
      
      # AMU metric
      observe({
        req(input$antimicrobial_map, input$route_map, input$aware_map, input$years)
        
        D <- data_map_filter4()
        updateRadioButtons(session, inputId = "metric_map",
                           label = "AMU metric:",
                           selected = character(0),
                           choices = unique(D$metric))
      })

    # 
    # Line plots
    #
      # Antimicrobials
        # AWaRe category
        observe({
          req(input$route_line_by_antimicrobial)
          
          D <- data_line_by_antimicrobial_filter1() #i.e., already filtered on administration route
          updateSelectInput(session, inputId = "aware_line_by_antimicrobial",
                            label = "AWaRe category:",
                            choices = c("", sort(unique(D$aware_category))),
                            selected = NULL)
        })

        # country
        observe({
          req(input$route_line_by_antimicrobial, input$aware_line_by_antimicrobial)
          
          D <- data_line_by_antimicrobial_filter2()
          updateSelectInput(session, inputId = "country_line_by_antimicrobial",
                            label = "Country:",
                            choices = c("", sort(unique(D$country))),
                            selected = NULL)
        })
        
        # AMU metric
        observe({
          req(input$route_line_by_antimicrobial, input$aware_line_by_antimicrobial, input$country_line_by_antimicrobial)
          
          D <- data_line_by_antimicrobial_filter3()
          updateRadioButtons(session, inputId = "metric_line_by_antimicrobial",
                             label = "AMU metric:",
                             selected = character(0),
                             choices = unique(D$metric))
        })
        
      # Administration routes
        # AWaRe category
        observe({
          req(input$antimicrobial_line_by_route)
          
          D <- data_line_by_route_filter1() #i.e., already filtered on antimicrobial
          updateSelectInput(session, inputId = "aware_line_by_route",
                            label = "AWaRe category:",
                            choices = c("", sort(unique(D$aware_category))),
                            selected = NULL)
        })

        # country
        observe({
          req(input$antimicrobial_line_by_route, input$aware_line_by_route)
          
          D <- data_line_by_route_filter2()
          updateSelectInput(session, inputId = "country_line_by_route",
                            label = "Country:",
                            choices = c("", sort(unique(D$country))),
                            selected = NULL)
        })
        
        # AMU metric
        observe({
          req(input$antimicrobial_line_by_route, input$aware_line_by_route, input$country_line_by_route)
          
          D <- data_line_by_route_filter3()
          updateRadioButtons(session, inputId = "metric_line_by_route",
                             label = "AMU metric:",
                             selected = character(0),
                             choices = unique(D$metric))
        })
        
      # AWaRe categories
        # administration route
        observe({
          req(input$antimicrobial_line_by_aware)
          
          D <- data_line_by_aware_filter1() #i.e., already filtered on antimicrobial
          updateSelectInput(session, inputId = "route_line_by_aware",
                            label = "Administration route:",
                            choices = c("", sort(unique(D$route_of_administration))),
                            selected = NULL)
        })
        
        # country
        observe({
          req(input$antimicrobial_line_by_aware, input$route_line_by_aware)
          
          D <- data_line_by_aware_filter2()
          updateSelectInput(session, inputId = "country_line_by_aware",
                            label = "Country:",
                            choices = c("", sort(unique(D$country))),
                            selected = NULL)
        })
        
        # AMU metric
        observe({
          req(input$antimicrobial_line_by_aware, input$route_line_by_aware, input$country_line_by_aware)
          
          D <- data_line_by_aware_filter3()
          updateRadioButtons(session, inputId = "metric_line_by_aware",
                             label = "AMU metric:",
                             selected = character(0),
                             choices = unique(D$metric))
        })
        
      # Countries
        # administration route
        observe({
          req(input$antimicrobial_line_by_country)
          
          D <- data_line_by_country_filter1() #i.e., already filtered on antimicrobial
          updateSelectInput(session, inputId = "route_line_by_country",
                            label = "Administration route:",
                            choices = c("", sort(unique(D$route_of_administration))),
                            selected = NULL)
        })

        # AWaRe category
        observe({
          req(input$antimicrobial_line_by_country, input$route_line_by_country)
          
          D <- data_line_by_country_filter2()
          updateSelectInput(session, inputId = "aware_line_by_country",
                            label = "AWaRe category:",
                            choices = c("", sort(unique(D$aware_category))),
                            selected = NULL)
        })
        
        # AMU metric
        observe({
          req(input$antimicrobial_line_by_country, input$route_line_by_country, input$aware_line_by_country)
          
          D <- data_line_by_country_filter3()
          updateRadioButtons(session, inputId = "metric_line_by_country",
                             label = "AMU metric:",
                             selected = character(0),
                             choices = unique(D$metric))
        })
        
  #----------
  # HEAT MAP
  #----------
    # Map title
    map_title_reactive <- eventReactive(input$load_map, {
      if (input$metric_map == "ddd") {
        paste0(toupper(input$metric_map), " (x 10^3) of ", tolower(input$route_map), " ", tolower(input$antimicrobial_map), " (AWaRe: ", input$aware_map, ") ", ifelse(min(input$years, na.rm = T) == max(input$years, na.rm = T), min(input$years, na.rm = T), paste0(min(input$years, na.rm = T), "-", max(input$years, na.rm = T))))
      } else {
        paste0(toupper(input$metric_map), " of ", tolower(input$route_map), " ", tolower(input$antimicrobial_map), " (AWaRe: ", input$aware_map, ") ", ifelse(min(input$years, na.rm = T) == max(input$years, na.rm = T), min(input$years, na.rm = T), paste0(min(input$years, na.rm = T), "-", max(input$years, na.rm = T))))
      }
    })
    
    output$map_title <- renderUI(
      HTML(map_title_reactive())
    )
    
    # Landing page / starting position for the map
    output$heat_map <- renderLeaflet({

        leaflet() %>%
          fitBounds(lng1 = 180, lat1 = -55.89, lng2 = -180, lat2 = 83.62) %>%
          addTiles(options = providerTileOptions(minZoom = 2))
      
    })
    
    # If data are selected:
    observeEvent(input$load_map, {

      DATA <- data_map_filter5() %>%
        group_by(country) %>%
        mutate(AMU = sum(value) / length(unique(year))) %>%
        select(country, AMU) %>%
        unique() %>%
        ungroup()
      
      merged_data <- merge(shape_file, DATA, by.x = "loc_name", by.y = "country")
      
      year_range = ifelse(min(input$years, na.rm = T) == max(input$years, na.rm = T), min(input$years, na.rm = T), paste0(min(input$years, na.rm = T), "-", max(input$years, na.rm = T)))
      if (input$metric_map == "ddd") {
        hover_text <- paste0('<strong>', merged_data$loc_name, '</strong>',
                             '</br>', '<strong>', toupper(input$metric_map), " (x 10^3): ", '</strong>', round(merged_data$AMU,1),
                             '</br>', '<strong>', "Year(s): ", '</strong>', year_range)
      } else {
        hover_text <- paste0('<strong>', merged_data$loc_name, '</strong>',
                             '</br>', '<strong>', toupper(input$metric_map), ": ", '</strong>', round(merged_data$AMU,1),
                             '</br>', '<strong>', "Year(s): ", '</strong>', year_range)
      }
      
      colour_palette <- colorNumeric(palette = "magma", domain = c(min(merged_data$AMU, na.rm = T):max(merged_data$AMU, na.rm = T)), na.color = "transparent", reverse = TRUE)
      
      leafletProxy("heat_map", data = merged_data) %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = merged_data,
                    fillColor = ~colour_palette(merged_data$AMU),
                    fillOpacity = ifelse(is.na(merged_data$AMU), 0, 0.7),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    popup = ifelse(is.na(merged_data$AMU),
                                   paste("no data available"),
                                   hover_text),
                    highlight = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)) %>%
        addLegend("bottomright", pal = colour_palette, values = ~c(min(merged_data$AMU, na.rm = T):max(merged_data$AMU, na.rm = T)), title = ifelse(input$metric_map == "ddd", paste0(toupper(input$metric_map), " x 10^3"), toupper(input$metric_map)), opacity = 0.9)
      
    })
    
  #------------
  # LINE PLOTS
  #------------
    # By antimicrobial
      # Plot title
      plot_title_by_antimicrobial_reactive <- eventReactive(input$load_plot_by_antimicrobial, {
        if (input$metric_line_by_antimicrobial == "ddd") {
          paste(toupper(input$metric_line_by_antimicrobial), "(x 10^3) of", tolower(input$route_line_by_antimicrobial), input$aware_line_by_antimicrobial, "(AWaRe) antimicrobials in", input$country_line_by_antimicrobial)
        } else {
          paste(toupper(input$metric_line_by_antimicrobial), "of", tolower(input$route_line_by_antimicrobial), input$aware_line_by_antimicrobial, "(AWaRe) antimicrobials in", input$country_line_by_antimicrobial)
        }
      })
  
      output$plot_title_by_antimicrobial <- renderUI(
        HTML(plot_title_by_antimicrobial_reactive())
      )
      
      # Plot
      observeEvent(input$load_plot_by_antimicrobial, {
        
        output$plot_by_antimicrobial <- renderPlotly({
  
          DATA_by_antimicrobial <- data_line_by_antimicrobial_filter4()
          
          DATA_by_antimicrobial_lines <- DATA_by_antimicrobial %>% 
            group_by(route_of_administration, aware_category, country, metric, antimicrobials) %>%
            filter(length(year) > 1) %>% ungroup()
  
          antimicrobial_colours <- colours_antimicrobial$my_colours[colours_antimicrobial$antimicrobial %in% DATA_by_antimicrobial_lines$antimicrobials]

          if (input$metric_line_by_antimicrobial == "ddd") {
            hover_text_by_antimicrobial <- paste0('</br>', DATA_by_antimicrobial_lines$antimicrobials,
                                                  '</br>', DATA_by_antimicrobial_lines$year,
                                                  '</br>', toupper(DATA_by_antimicrobial_lines$metric), " (x 10^3): ", round(DATA_by_antimicrobial_lines$value,0))
          } else {
            hover_text_by_antimicrobial <- paste0('</br>', DATA_by_antimicrobial_lines$antimicrobials,
                                                  '</br>', DATA_by_antimicrobial_lines$year,
                                                  '</br>', toupper(DATA_by_antimicrobial_lines$metric), ": ", round(DATA_by_antimicrobial_lines$value,0))
          }
          
          DATA_by_antimicrobial_plot <-
            ggplot(DATA_by_antimicrobial_lines, aes(x = year, y = value, text = hover_text_by_antimicrobial, group = antimicrobials)) +
            geom_line(aes(colour = antimicrobials)) +
            #geom_point(aes(colour = antimicrobials)) +
            scale_colour_manual(values = antimicrobial_colours) +
            scale_x_continuous("Year", breaks = seq(2015,2019,1), labels = seq(2015,2019,1)) +
            labs(x = "Year", y = ifelse(input$metric_line_by_antimicrobial == "ddd", paste0(toupper(unique(DATA_by_antimicrobial_lines$metric)), " x 10^3"), toupper(unique(DATA_by_antimicrobial_lines$metric))), fill = NULL) +
            theme_bw() +
            theme(text = element_text(size = 15, angle = 0))
  
          by_antimicrobial_interactive <- ggplotly(DATA_by_antimicrobial_plot, tooltip = "text")

        })
        
        # Line display options ##NOT NEEDED - NOW USING PLOTLY##
        # enable("line_options_by_antimicrobial")
        # tickbox_update_by_antimicrobial <- data_line_by_antimicrobial_filter4() %>%
        #   group_by(route_of_administration, aware_category, country, metric, antimicrobials) %>%
        #   filter(length(year) > 1) %>% ungroup()
        # updateCheckboxGroupInput(session, inputId = "line_options_by_antimicrobial",
        #                          label = "",
        #                          choices = sort(unique(tickbox_update_by_antimicrobial$antimicrobials)),
        #                          inline = TRUE,
        #                          selected = "All")

      })
    
    # By route of administration
      # Plot title
      plot_title_by_route_reactive <- eventReactive(input$load_plot_by_route, {
        if (input$metric_line_by_route == "ddd") {
          paste0(toupper(input$metric_line_by_route), " (x 10^3) of ", tolower(input$antimicrobial_line_by_route), " (AWaRe: ", input$aware_line_by_route, ") in ", input$country_line_by_route)
        } else {
          paste0(toupper(input$metric_line_by_route), " of ", tolower(input$antimicrobial_line_by_route), " (AWaRe: ", input$aware_line_by_route, ") in ", input$country_line_by_route)
        }
      })
      
      output$plot_title_by_route <- renderUI(
        HTML(plot_title_by_route_reactive())
      )
      
      # Plot
      observeEvent(input$load_plot_by_route, {
      
        output$plot_by_route <- renderPlotly({
          
          DATA_by_route <- data_line_by_route_filter4()
          
          DATA_by_route_lines <- DATA_by_route %>% 
            group_by(antimicrobials, aware_category, country, metric, route_of_administration) %>%
            filter(length(year) > 1) %>% ungroup()
  
          route_colours <- colours_route$my_colours[colours_route$route %in% DATA_by_route_lines$route_of_administration]
          
          if (input$metric_line_by_route == "ddd") {
            hover_text_by_route <- paste0('</br>', DATA_by_route_lines$route_of_administration,
                                          '</br>', DATA_by_route_lines$year,
                                           '</br>', toupper(DATA_by_route_lines$metric), " (x 10^3): ", round(DATA_by_route_lines$value,0))
          } else {
            hover_text_by_route <- paste0('</br>', DATA_by_route_lines$route_of_administration,
                                          '</br>', DATA_by_route_lines$year,
                                          '</br>', toupper(DATA_by_route_lines$metric), ": ", round(DATA_by_route_lines$value,0))
          }
  
          DATA_by_route_plot <-
            ggplot(DATA_by_route_lines, aes(x = year, y = value, text = hover_text_by_route, group = route_of_administration)) +
            geom_line(aes(colour = route_of_administration)) +
            #geom_point(aes(colour = route_of_administration)) +
            scale_colour_manual(values = route_colours) +
            scale_x_continuous("Year", breaks = seq(2015,2019,1), labels = seq(2015,2019,1)) +
            labs(x = "Year", y = ifelse(input$metric_line_by_route == "ddd", paste0(toupper(unique(DATA_by_route_lines$metric)), " x 10^3"), toupper(unique(DATA_by_route_lines$metric))), fill = NULL) + 
            theme_bw() + 
            theme(text = element_text(size = 15, angle = 0))
          
          by_route_interactive <- ggplotly(DATA_by_route_plot, tooltip = "text")

        })
        
        # Line display options ##NOT NEEDED - NOW USING PLOTLY##
        # enable("line_options_by_route")
        # tickbox_update_by_route <- data_line_by_route_filter4() %>%
        #   group_by(antimicrobials, aware_category, country, metric, route_of_administration) %>%
        #   filter(length(year) > 1) %>% ungroup()
        # updateCheckboxGroupInput(session, inputId = "line_options_by_route",
        #                          label = "",
        #                          choices = sort(unique(tickbox_update_by_route$route_of_administration)),
        #                          inline = TRUE,
        #                          selected = "All")

      })
      
    # By AWaRe category
      # Plot title
      plot_title_by_aware_reactive <- eventReactive(input$load_plot_by_aware, {
        if (input$metric_line_by_aware == "ddd") {
          paste(toupper(input$metric_line_by_aware), "(x 10^3) of", tolower(input$route_line_by_aware), input$antimicrobial_line_by_aware, "in", input$country_line_by_aware)
        } else {
          paste(toupper(input$metric_line_by_aware), "of", tolower(input$route_line_by_aware), input$antimicrobial_line_by_aware, "in", input$country_line_by_aware)
        }
      })
      
      output$plot_title_by_aware <- renderUI(
        HTML(plot_title_by_aware_reactive())
      )
      
      # Plot
      observeEvent(input$load_plot_by_aware, {
        
        output$plot_by_aware <- renderPlotly({
          
          DATA_by_aware <- data_line_by_aware_filter4()
          
          DATA_by_aware_lines <- DATA_by_aware %>% 
            group_by(antimicrobials, route_of_administration, country, metric, aware_category) %>%
            filter(length(year) > 1) %>% ungroup()
          
          aware_colours <- colours_aware$my_colours[colours_aware$aware_category %in% DATA_by_aware_lines$aware_category]
          
          if (input$metric_line_by_aware == "ddd") {
            hover_text_by_aware <- paste0('</br>', DATA_by_aware_lines$aware_category,
                                          '</br>', DATA_by_aware_lines$year,
                                          '</br>', toupper(DATA_by_aware_lines$metric), " (x 10^3) : ", round(DATA_by_aware_lines$value,0))
          } else {
            hover_text_by_aware <- paste0('</br>', DATA_by_aware_lines$aware_category,
                                          '</br>', DATA_by_aware_lines$year,
                                          '</br>', toupper(DATA_by_aware_lines$metric), ": ", round(DATA_by_aware_lines$value,0))
          }
  
          DATA_by_aware_plot <- 
            ggplot(DATA_by_aware_lines, aes(x = year, y = value, text = hover_text_by_aware, group = aware_category)) +
            geom_line(aes(colour = aware_category)) +
            #geom_point(aes(colour = aware_category)) +
            scale_colour_manual(values = aware_colours) +
            scale_x_continuous("Year", breaks = seq(2015,2019,1), labels = seq(2015,2019,1)) +
            labs(x = "Year", y = ifelse(input$metric_line_by_aware == "ddd", paste0(toupper(unique(DATA_by_aware_lines$metric)), " x 10^3"), toupper(unique(DATA_by_aware_lines$metric))), fill = NULL) + 
            theme_bw() + 
            theme(text = element_text(size = 15, angle = 0))
          
          by_aware_interactive <- ggplotly(DATA_by_aware_plot, tooltip = "text")

        })
        
        # Line display options ##NOT NEEDED - NOW USING PLOTLY##
        # enable("line_options_by_aware")
        # tickbox_update_by_aware <- data_line_by_aware_filter4() %>%
        #   group_by(antimicrobials, route_of_administration, country, metric, aware_category) %>%
        #   filter(length(year) > 1) %>% ungroup()
        # updateCheckboxGroupInput(session, inputId = "line_options_by_aware",
        #                          label = "",
        #                          choices = sort(unique(tickbox_update_by_aware$aware_category)),
        #                          inline = TRUE,
        #                          selected = "All")

      })
      
    # By country
      # Plot title
      plot_title_by_country_reactive <- eventReactive(input$load_plot_by_country, {
        if (input$metric_line_by_country == "ddd") {
          paste(toupper(input$metric_line_by_country), "(x 10^3) of", tolower(input$route_line_by_country), input$antimicrobial_line_by_country, "(AWaRe:", input$aware_line_by_country, ")")
        } else {
          paste(toupper(input$metric_line_by_country), "of", tolower(input$route_line_by_country), input$antimicrobial_line_by_country, "(AWaRe:", input$aware_line_by_country, ")")
        }
      })
      
      output$plot_title_by_country <- renderUI(
        HTML(plot_title_by_country_reactive())
      )
      
      # Plot
      observeEvent(input$load_plot_by_country, {
        
      output$plot_by_country <- renderPlotly({
        
        DATA_by_country <- data_line_by_country_filter4()
        
        DATA_by_country_lines <- DATA_by_country %>% 
          group_by(antimicrobials, route_of_administration, aware_category, metric, country) %>%
          filter(length(year) > 1) %>% ungroup()
        
        country_colours <- colours_country$my_colours[colours_country$location %in% DATA_by_country_lines$country]
        
        if (input$metric_line_by_country == "ddd") {
          hover_text_by_country <- paste0('</br>', DATA_by_country_lines$country,
                                          '</br>', DATA_by_country_lines$year,
                                          '</br>', toupper(DATA_by_country_lines$metric), "(x 10^3): ", round(DATA_by_country_lines$value,0))
        } else {
          hover_text_by_country <- paste0('</br>', DATA_by_country_lines$country,
                                          '</br>', DATA_by_country_lines$year,
                                          '</br>', toupper(DATA_by_country_lines$metric), ": ", round(DATA_by_country_lines$value,0))
        }

        DATA_by_country_plot <- 
          ggplot(DATA_by_country_lines, aes(x = year, y = value, text = hover_text_by_country, group = country)) +
          geom_line(aes(colour = country)) +
          #geom_point(aes(colour = country)) +
          scale_colour_manual(values = country_colours) +
          scale_x_continuous("Year", breaks = seq(2015,2019,1), labels = seq(2015,2019,1)) +
          labs(x = "Year", y = ifelse(input$metric_line_by_country == "ddd", paste0(toupper(unique(DATA_by_country_lines$metric)), " x 10^3"), toupper(unique(DATA_by_country_lines$metric))), fill = NULL) + 
          theme_bw() + 
          theme(text = element_text(size = 15, angle = 0))
        
        by_country_interactive <- ggplotly(DATA_by_country_plot, tooltip = "text")

      })
      
      # Line display options ##NOT NEEDED - NOW USING PLOTLY##
      # enable("line_options_by_country")
      # tickbox_update_by_country <- data_line_by_country_filter4() %>%
      #   group_by(antimicrobials, route_of_administration, aware_category, metric, country) %>%
      #   filter(length(year) > 1) %>% ungroup()
      # updateCheckboxGroupInput(session, inputId = "line_options_by_country",
      #                          label = "",
      #                          choices = sort(unique(tickbox_update_by_country$country)),
      #                          inline = TRUE,
      #                          selected = "Global")

      })
      
}

############
#DEPLOY APP#
############
shinyApp(ui = ui, server = server)
