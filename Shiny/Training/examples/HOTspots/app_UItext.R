#######################################
## Tracking Antimicrobial Resistance ##
#######################################



## TO DO ##
# Hotspots logo on the landing page and plotting page
# title on landing and plotting page the same 

# Dynamic title on the plotting page
  # use an if statement inside a render text
    # if load button is null, text is basic title. Otherwise text is based on inputs

# Antibiogram column max width



# Aesthetics
  # Side bar width smaller
  # map to fill page



## To try if time
# Change the leaflet highlight options so it starts thin and black line and then when you scroll over it becomes bigv and white

# Colour blind 
  # pattern on the leaflet polygon map
    #cloudybay / leaflet-polygon-fillPattern
    # https://leafletjs.com/plugins.html#markers--renderers
  # Opacity different on the leadfet map
    # more see-through for the lower values
  # Hover over text saying name and the percentage with the size indicating the percentage
  # Button to change to a colour blind friendly palette
    # blue to red
  # add shape to male and female 

## Ideas
# in the panels where you have to select a region, have a map showing the extent of the region

## To do future
# date range slider
  # issue - speed of the app


###############
## 1. Set up ##
###############

##########################
## 1.1 Loading packages ##
##########################

if (!require(shiny)) install.packages('shiny')
if (!require(shinythemes)) install.packages('shinythemes')
if (!require(rgdal)) install.packages('rgdal')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(dplyr)) install.packages('dplyr')
if (!require(leaflet)) install.packages('leaflet')
if (!require(reshape2)) install.packages('reshape2')
if (!require(shinydashboard)) install.packages('shinydashboard') # for valuebox
if (!require(shinyWidgets)) install.packages('shinyWidgets')
if (!require(shinycssloaders)) install.packages('shinycssloaders')
#if (!require(gridExtra)) install.packages('gridExtra')
#if (!require(grid)) install.packages('grid')
# if (!require(DT)) install.packages('DT')

library(shiny) # for the shiny app
library(shinythemes) # for the shiny app
library(ggplot2) # to make plots
library(dplyr) # to clean up code and allow piping %>%
library(leaflet) # for interactive maps
library(rgdal) # to open shapefiles of areas to map
library(reshape2)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
#library(gridExtra)
#library(grid)
#library(DT) # for creating nice datatables
# note common convention to not load DT, but instead call it each time with DT::

library(rsconnect)
#deployApp()

#############
## Options ##
#############

## Contact email
# HOTspots@menzies.edu.au 

# Date the data was last updated

# citation

# team members

# message to display when shared to social media
  # type the message as vector, then switch all the spaces to %20 and insert in correct place of url



##############
## 1.2 Data ##
##############

#consider a timer or a sceduler that will run the datamanipulation script every time the data is updated
# source("myScript.r", echo = TRUE)

#rm(list=ls(all=TRUE))

hotspot_monthly_data    <- read.csv("www/data/HOTspots_monthly.csv")
hotspot_yearly_data     <- read.csv("www/data/hotspot_yearly_data.csv")
#hotspot_yearly_split    <- read.csv("www/data/hotspot_yearly_split.csv")
hotspot_yearly_splitage <- read.csv("www/data/hotspot_yearly_splitage.csv")
hotspot_yearly_splitsex <- read.csv("www/data/hotspot_yearly_splitsex.csv")

hotspot_monthly_data$date_dmy <- as.Date(paste("01", hotspot_monthly_data$month_year), format = "%d %b %y")

## Note data missing for WA
  # currently there are 16 regions for monthly data and 14 foryearly data
  # Mid East and Mid West only in monthly and have not been included in the yearly due to lack of data

## Shapefile
SA3 <- rgdal::readOGR("www/data/Australian_regions/Aus_regions.shp")
SA3_data <- SA3[SA3$SA3_NAME16 %in% hotspot_yearly_data$region,] # select the SA3 for which we have data for currently
rm(SA3)

#text_select_inputs <- paste("Select inputs on the left to display plots")

# Make the datatable column names angles
headerCallback2 <- c(
  "function(thead, data, start, end, display){",
  "  var $ths = $(thead).find('th');",
  "  $ths.css({'vertical-align': 'bottom', 'white-space': 'nowrap'});",
  "  var betterCells = [];",
  "  $ths.each(function(){",
  "    var cell = $(this);",
  "    var newDiv = $('<div>', {height: 'auto', width: 'auto'});",
  "    var newInnerDiv = $('<div>', {text: cell.text()});",
  "    newDiv.css({margin: 'auto'});",
  "    newInnerDiv.css({",
  "      transform: 'rotate(200deg)',",
  "      'writing-mode': 'tb-rl',",
  "      'white-space': 'nowrap'",
  "    });",
  "    newDiv.append(newInnerDiv);",
  "    betterCells.push(newDiv);",
  "  });",
  "  $ths.each(function(i){",
  "    $(this).html(betterCells[i]);",
  "  });",
  "}"
)


####################
## Colour palette ##
####################

## Palette options
hotspot_palette <- list(
  
  ## For heat map
  `heat`  = c( # colour palette called heat
    `green`         = "#629c25", ## Check the palette
    #`yellow green`  = "#f5ff38", 
    `yellow orange` = "#FFF100", # "#ffde24",
    `orangy yellow` = "#FFD100", #"#ffb833",
    `orange`        = "#fc8105", 
    #`deep orange`   = "#ff6600",
    `dark orange`   = "#e34f00",
    `red`           = "#ff0000",
    `red2`          = "#db0000",
    `deep red`      = "#C00004",
    `dark red`      = "#810000",
    `almost black`  = "#410000"),
  
  ## For heat map
  `heat_CBfriendly`  = c( # colour palette called heat
    `teal`         = "#57C4AD", ## Check the palette
    `yellow orange` = "#E6E1BC", 
    `orangy yellow` = "#E6BD85",
    `orange`        = "#EDA247", 
    `dark orange`   = "#ED8047",
    `red`           = "#DB4325",
    `red2`          = "#db0000",
    `deep red`      = "#C00004",
    `dark red`      = "#810000",
    `almost black`  = "#410000"),
  
  ## For the reigons
  `regions` = c( 'Cairns and Hinterland' = "#CC6677", # rose
                 'Mackay'                = "#88CCEE", #cyan 
                 'North West'            = "#44AA99", # teal
                 'Torres and Cape'       = "#117733", # green
                 'Townsville'            = "#DDCC77", # sand
                 #'name' = "#332288", # indigo
                 #'name' = "#999933", # olive
                 #'name' = "#882255", # wine
                 #'name' = "#AA4499", # purple
                 
                 ## NT
                  # colours lightened
                 'Alice Springs'         = "#FF8096", # rose
                 'Darwin'                = "#CCEEFF", # cyan
                 'Gove'                  = "#92EFD3", # teal
                 'Katherine'             = "#CCDDAA", # green
                 'Tennant Creek'        = "#FFE57E", # sand FFEC8C
                 #'name' = "#8183E6", # indigo
                 #'name' = "#C5C86E", # olive
                 #'name' = "#", # wine # too similar to rose
                 #'name' = "#F488EE", # purple
                 
                 ## WA 
                  # colours darkened
                 'Kimberley'             = "#78343F", # rose
                 'Mid East'              = "#225555", # cyan 
                 'Mid West'              = "#36877A", # teal
                 'Perth'                 = "#225522", # green
                 'Pilbara'               = "#A59858", # sand
                 'South'                 = "#222255" # indigo
                 #'name' = "#2F345B", # indigo
                 #'name' = "#7B7B29", # olive
                 #'name' = "#781D4A", # wine
                 #'name' = "#803273", # purple
                 
                 ),
  

  ## For the onset locations
  `onset` = c('Overall'  = "#5FC3CE" ,
              'Hospital' = "#ff666a",
              'Community' =  "#009E73"),

  ## For the sample types
  `sample` = c('All' = "#332288" , #indigo
               'Blood' = "#88CCEE", #cyan
               'Other' = "#44AA99", # teal
               'Respiratory' = "#117733", # green
               'Skin & soft tissue' = "#A0515E", # rose
               'Urine' = "#999933"), #olive
  
  ## For the age brackets
  `age` = c('0-5' = "#332288" , #indigo
            '6-15' = "#88CCEE", #cyan
            '16-25' = "#44AA99", # teal
            '26-40' = "#117733", # green
            '41-60' = "#A0515E", # rose
            '61-80' = "#DDCC77", # Sand
            '81+' = "#999933"), #olive
  

  `sex` = c( 'male' = "#44AA99", ## teal
              'female'  = "#332288", ## indigo
              'overall' = "#999933") ## olive
)



# numeric colour scheme
pal_num <- colorNumeric(hotspot_palette$heat, domain = 0:100)
pal_num_CBfriendly <- colorNumeric(hotspot_palette$heat_CBfriendly, domain = 0:100)

##  to check the colour palletes
# par(mar=c(0,0,0,0))
# pie(rep(1, length(hotspot_palette$heat)), col = hotspot_palette$heat)
# 
# pie(rep(1, 100), col = pal_num_CBfriendly(1:100))



# save.image(file = "www/data/app_setup_Jan31.RData")
# load("www/data/app_setup_Jan31.RData")


#####################
## User Interface ##
#####################

ui <- fluidPage(

  useShinydashboard(),
  #options(shiny.sanitize.errors = TRUE),
  
  tags$head(tags$style(HTML("
                           .navbar-nav {float: none !important;}
                           .navbar {font-size: 15px;}
                           .navbar-nav > li:nth-child(8) {float: right;}
                           .small-box {height: 85px; margin-bottom: 0px;}
                           
                           .irs-bar { background: none; border-top: none; border-bottom: none;}
                           .irs-bar-edge {background: none; border: none;}
                            #microbe_name+ div>.selectize-input{font-style: italic;}
                            #microbe_name+ div>.selectize-dropdown{font-style: italic;}
                            #microbe_name_spec+ div>.selectize-input{font-style: italic;}
                            #microbe_name_spec+ div>.selectize-dropdown{font-style: italic;}
                            "))),

  #     {margin-top: 0; margin-bottom: 0;}
  #.h3 {margin-top: 0; margin-bottom: 0;}
  #.navbar {font-size: 16px;}
  
  ## The navigation bar at the top of the screen
  navbarPage(title ="", # title for the title bar, could add the logo or symbols
             id="nav", selected = NULL, collapsible = TRUE, 
             theme = shinytheme("flatly"),
             #tags$style(type='text/css', '.navbar {font-size: 13px;}'),
             
             
             # Tab 1 - Map, landing page -----------------------------------------------------------------------
             tabPanel("Map", icon = icon("map"), # Name on the navigation bar
                      
                      fluidRow( #class = "header", tags$head(tags$style(".header {height:50px;}")),
                        #column(2, imageOutput("hotspots_logo", width="100", height="100")),
                        
                        column(2, img(src='HOTspots_logo copy 2.png', align = "left", width = '100%', height = 'auto')), #, width = "250px"
                        valueBoxOutput("VBox_organism", width = 2), # , width = 2
                        valueBoxOutput("VBox_antibiotic", width = 2),
                        valueBoxOutput("VBox_regions", width = 2),
                        valueBoxOutput("VBox_year", width = 2),
                        valueBoxOutput("VBox_tests", width = 2)
                        #
                      ),
                      br(),
                      
                      # Sidebar layout with input and output definitions
                      sidebarLayout(
                        
                        # Sidebar panel for inputs on the left ---------------------------------------------------
                        sidebarPanel(
                          
                          radioButtons(inputId = "source",
                                       label = "Select dataset:",
                                       choices = c("Human", "Animal"), #change to sort(unique(hotspot_yearly_data$source) post display
                                       selected = character(0)), # also can change choice to be unique(hotspot_yearly_data$sample_oranism) which should be human and animal
                          
                          helpText("Note: Samples may currently be only available for humans"),
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.source == null ", 
                            helpText("Please select dataset") 
                          ),
                          
                          # Select the location where the infection was identified
                          conditionalPanel(
                            condition = "input.source != null ", # change to != null when animal or the column is added
                            radioButtons(inputId = "onset",
                                         label = "Select healthcare setting:",
                                         selected = character(0),
                                         choices = rev(unique(hotspot_yearly_data$onset))) # change this when animal data is added rev(unique(hotspot_yearly_data$onset))
                          ),
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.onset == null && input.source != null", 
                            helpText("Please select healthcare setting")
                          ),
                          
                          # Select the site of the human infection
                          conditionalPanel(
                            condition = "input.onset != null",
                            selectInput(
                              "isolatetype",
                              label = "Select specimen type:",
                              choices = sort(unique(hotspot_yearly_data$sample_type))) ## unique(hotspot_yearly_data$sample_type[hotspot_yearly_data$sample_oranism == "Human"])
                          ),
                          
                          
                          br(),
                          
                          
                          #tags$style(type='text/css', ".selectize-input {font-style: italic; } .selectize-dropdown { font-style: italic; }"),
                          
                          # Select the microbe name
                          selectInput(inputId = "microbe_name", 
                                      label = "Select organism:",
                                      choices = sort(unique(hotspot_yearly_data$organism)),
                                      selected = "E.coli", 
                                      multiple = FALSE), # cannot select multiple
                          
                          # Select the antibiotic name
                          selectInput(inputId = "antibiotic_name",
                                      label = "Select antibiotic:",
                                      choices = sort(unique(hotspot_yearly_data$antimicrobial)),
                                      selected = "Cefazolin", 
                                      multiple = FALSE),
                          
                          br(),
                          # A slider to select the year
                          #tags$style(type = "text/css", ".irs-bar { background: none; border-top: none; border-bottom: none;}
                          #           .irs-bar-edge {background: none; border: none;}"), # setting the blue bar to be nothing on the slider
                          
                          sliderInput(inputId = "year",
                                      label = "Select a single year:",
                                      value = max(hotspot_yearly_data$year), # default value
                                      min = min(hotspot_yearly_data$year),
                                      max = max(hotspot_yearly_data$year),
                                      step = 1,
                                      sep = "",
                                      round = TRUE,
                                      ticks = FALSE),
                          
                          br(),
                          
                          # A check box to change to colour blind friendly
                          checkboxInput("load_CB_friendly_map", "Change to colour-blind friendly", FALSE),
                          
                          # A button to load the map
                          actionButton(inputId  = "load_map", label = "Load map"),
                          
                          
                          helpText("Please wait for the areas to load on the map")
                          
                          ## TO show or hide the help text, possibly use:
                          
                          # Delay https://rdrr.io/cran/shinyjs/man/delay.html
                          # observeEvent(input$load_map, {
                          #   delay(500, hide())
                          # })
                          
                          # or
                          
                          # shinyjs show and hide with timer
                          
                          # or
                          
                          # pop up notification
                          
                          # or 
                          
                          # disable the actionbutton for 10 seconds
                          
                        ), # close side panel
                        

                        
                        
                        # Main panel for displaying outputs ---------------------------------------------------
                        mainPanel(
                          #div(h3(textOutput("map_title"), style= "margin-top: 0; margin-bottom: 0;")),
                          h3(uiOutput("map_title")),
                          #{margin-top: 0; margin-bottom: 0;}
                          #h3(textOutput("map_title")),
                          leafletOutput("leaflet_map", height=700), # plot the leaflet map
                          tags$div(id="Citation",
                                   'How to cite: Menzies School of Health Research. HOTspots [computer software]. Darwin, Northern Territory. Retrieved from [inset web address when done]') # TO DO ## update this
                        ) # close main panel
                      ), # close sidebar layout
                      
                      # fluidRow( 
                      #   column(2, img(src='Mezies_logo_white.png', align = "left", width = '70%', height = 'auto')), #, width = "250px"
                      #   column(2, img(src='HotNorth logo.png', align = "left", width = '70%', height = 'auto')) #, width = "250px"
                      # ),
                      # 
                      
                      ## Consider here outside of the sidebar layout
                      # The logos for Hot North and Menzies underneath
             ), # close tabPanel
             
             
             
             
             
             
             
             
             
             
             
             
             # Tab 2 - Plots -----------------------------------------------------------------------
             
             tabPanel(("Plots"), # name in nav bar
                      icon = icon("chart-bar"), # icon in nav bar
                      #titlePanel(tags$h4("Tracking antimicrobial resistance")), # title on the page
                      #tags$br(),    
                      
                      sidebarLayout(
                        
                        # Side panel for displaying inputs ---------------------------------------------------
                        sidebarPanel(
                          
                          
                          radioButtons(inputId = "source_spec",
                                       label = "Select dataset:",
                                       choices = sort(unique(hotspot_yearly_data$source)),
                                       selected = character(0)), # also can change choice to be unique(hotspot_yearly_data$sample_oranism) which should be human and animal
                          
                          helpText("Note: Samples may currently be only available for humans"),
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.source_spec == null ", 
                            helpText("Please select dataset") 
                          ),
                          
                          # Select the location where the infection was identified
                          conditionalPanel(
                            condition = "input.source_spec != null ", # change to != null when animal or the column is added
                            radioButtons(inputId = "onset_spec",
                                         label = "Select healthcare setting:",
                                         selected = character(0),
                                         choices = rev(unique(hotspot_yearly_data$onset))) # change this when animal data is added rev(unique(hotspot_yearly_data$onset))
                          ),
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.onset_spec == null && input.source_spec != null", 
                            helpText("Please select healthcare setting")
                          ),
                          
                          # Select the site of the human infection
                          conditionalPanel(
                            condition = "input.onset_spec != null",
                            selectInput(
                              "isolatetype_spec",
                              label = "Select specimen type:",
                              choices = sort(unique(hotspot_yearly_data$sample_type))) ## unique(hotspot_yearly_data$sample_type[hotspot_yearly_data$sample_oranism == "Human"])
                          ),
                          
                          
                          # Select the microbe name
                          selectInput(inputId = "microbe_name_spec", 
                                      label = "Select organism:",
                                      choices = sort(unique(hotspot_yearly_data$organism)),
                                      selected = NULL, # none selected as the default when the app opens
                                      multiple = FALSE), # cannot select multiple
                          
                          
                          
                          
                          ## Only allow the selection of one variable for the non region or antimicrobe names
                          conditionalPanel(
                            condition = "input.tab_plot != 'antimicrobe'",
                            
                            selectInput(inputId = "antibiotic_name_spec1",
                                        label = "Select an antibiotic:",
                                        choices = sort(unique(hotspot_yearly_data$antimicrobial)),
                                        selected = sort(unique(hotspot_yearly_data$antimicrobial))[1],
                                        multiple = FALSE)
                          ),
                          ## If the tab is antimicrobe, allow multiple antimicrobes to be selected
                          conditionalPanel(
                            condition = "input.tab_plot == 'antimicrobe'",
                            
                            #helpText("You may now select mutliple antimicrobials"),
                            
                            selectInput(inputId = "antibiotic_name_spec2",
                                        label = "Select multiple antimicrobials:",
                                        choices = sort(unique(hotspot_yearly_data$antimicrobial)),
                                        selected = sort(unique(hotspot_yearly_data$antimicrobial))[16:30],
                                        multiple = TRUE)
                          ),
                          
                          
                          ## Only allow the selection of one variable for the non region or antimicrobe names
                          conditionalPanel(
                            condition = "input.tab_plot != 'region'",
                            
                            selectInput(inputId = "region_spec1",
                                        label = "Select one region:",
                                        choices = sort(unique(hotspot_yearly_data$region)),
                                        selected = sort(unique(hotspot_yearly_data$region))[1],
                                        multiple = FALSE)
                          ),
                          
                          ## If the tab is region, allow multiple regions to be selected
                          conditionalPanel(
                            condition = "input.tab_plot == 'region' ",
                            
                            #helpText("You may now select mutliple regions"),
                            
                            selectInput(inputId = "region_spec2",
                                        label = "Select multiple regions:",
                                        choices = sort(unique(hotspot_yearly_data$region)),
                                        selected = sort(unique(hotspot_yearly_data$region))[1:10],
                                        multiple = TRUE)
                          ),
                          
                          br(),
                          
                          # A slider to select the year
                          # Alter the style so there in no tail behind
                          #tags$style(type = "text/css", ".irs-bar { background: none; border-top: none; border-bottom: none;}
                          #           .irs-bar-edge {background: none; border: none;}"), # setting the blue bar to be nothing on the slider
                          
                          
                          
                          conditionalPanel(  # conditional to display the year input only when the tab is antimicrobe
                            condition = "input.tab_plot == 'antimicrobe'",
                            
                            sliderInput(inputId = "year_spec",
                                        label = "Select a year:",
                                        value = max(hotspot_yearly_data$year), # default value
                                        min = min(hotspot_yearly_data$year),
                                        max = max(hotspot_yearly_data$year),
                                        step = 1,
                                        sep = "",
                                        round = TRUE,
                                        ticks = FALSE)
                          )
                          
                          
                          # sliderInput(inputId = "year_spec",
                          #             label = "Select a year:",
                          #             value = max(hotspot_yearly_data$year), # default value
                          #             min = min(hotspot_yearly_data$year),
                          #             max = max(hotspot_yearly_data$year),
                          #             step = 1,
                          #             sep = "",
                          #             round = TRUE,
                          #             ticks = FALSE)
                          
                        ),
                        
                        
                        
                        # Main panel for displaying outputs ---------------------------------------------------
                        mainPanel(
                          
                          # tabs on the main panel
                          tabsetPanel(id = "tab_plot",
                                      
                                      tabPanel(("By location"),
                                               value="region",
                                               h4(uiOutput("text_compare_reg")),
                                               #h4(textOutput("text_compare_reg")),
                                               plotOutput("plot_compare_reg") %>% withSpinner(type=5)
                                      ),
                                      
                                      
                                      
                                      tabPanel(("By antibiotic"),
                                               value="antimicrobe",
                                               h4(uiOutput("text_compare_anti")),
                                               #h4(textOutput("text_compare_anti")),
                                               plotOutput("plot_compare_anti") %>% withSpinner(type=5),
                                               
                                               br(),
                                               br(),
                                               
                                               h4(uiOutput("text_compare_anti2")),
                                               #h4(textOutput("text_compare_anti2")),
                                               plotOutput("plot_compare_anti2") %>% withSpinner(type=5),
                                               br(),
                                               br()
                                               #h4(textOutput("text_compare_anti3")),
                                               #plotOutput("plot_compare_anti3") %>% withSpinner(type=5)
                                      ),
                                      
                                      
                                      
                                      tabPanel(("By sample"),
                                               value="sample type",
                                               br(),
                                               h2("Currently being updated"),
                                               br(),
                                               h4("Comparison of the sample types"),
                                               br(),
                                               h4("Comparison of the onset locations"),
                                               br(),
                                               h4("Comparison of the animal and human samples")
                                               # p("The total number of tests (grey) and the number of resistant samples (coloured) by either the sample type (first graph) or by onset location (second graph)"),
                                               # br(),
                                               # h4("Sample types compared to total number of tests"), # change this to a text output
                                               # p("Need to add grey box to the legend"),
                                               # plotOutput("plot_compare_sample") %>% withSpinner(type=5),
                                               # br(),
                                               # br(),
                                               # plotOutput("plot_compare_sample1") %>% withSpinner(type=5),
                                               # br(),
                                               # br(),
                                               # h4("Onset locations"), # change this to a text output
                                               # plotOutput("plot_compare_onset") %>% withSpinner(type=5),
                                               # br(),
                                               # br(),
                                               # h4("comparison of animal and human samples"), # change this to a text output
                                               # p("Possible third graph of human vs animal samples")
                                      ),
                                      
                                      tabPanel(("By age"), # name of tab
                                               value = "age",
                                               
                                               h4(uiOutput("text_age")),
                                               #h4(textOutput("text_age")),

                                               plotOutput("plot_compare_age", height=500) %>% withSpinner(type=5)),
                                      
                                      tabPanel(("By sex"), # name of tab
                                               value = "sex",
                                               
                                               h4(uiOutput("text_sex")),
                                               #h4(textOutput("text_sex")),
                                               #h4("Comparisons of the two sexes: Male and Female "),

                                               plotOutput("plot_compare_sex", height=500) %>% withSpinner(type=5)),
                                      
                                      
                                      tabPanel(("By month"), # name of tab
                                               value = "monthly",
                                               
                                               h4(uiOutput("text_spec")),
                                               plotOutput("plot_spec") %>% withSpinner(type=5),
                                               br(),
                                               
                                               h4(uiOutput("text_spec2")),
                                               #h4(textOutput("text_spec2")),
                                               
                                               plotOutput("plot_spec2") %>% withSpinner(type=5) # , color = "#5FC3CE"
                                               
                                      )
                          ) # close tabset panel
                        )# close main panel
                      )# close sidebar layout
             ), # close tabpanel
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             # Tab 3 - Antibiogram  -----------------------------------------------------------------------------------------------------
             
             tabPanel(("Antibiogram"), # name in nav bar
                      icon = icon("table"), # icon in nav bar
                      value="antibiogram",
                      
                      
                      
                      # Sidebar layout with input and output definitions
                      sidebarLayout(
                        
                        # Sidebar panel for inputs on the left -----------------------------------------------------------------------
                        sidebarPanel(
                          
                          
                          radioButtons(inputId = "source_table",
                                       label = "Select dataset:",
                                       choices = c("Human", "Animal"), #change to sort(unique(hotspot_yearly_data$source) post display
                                       selected = character(0)), # also can change choice to be unique(hotspot_yearly_data$sample_oranism) which should be human and animal
                          
                          helpText("Note: Samples may currently be only available for humans"),
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.source_table == null ", 
                            helpText("Please select dataset") 
                          ),
                          
                          # Select the location where the infection was identified
                          conditionalPanel(
                            condition = "input.source_table != null ", # change to != null when animal or the column is added
                            radioButtons(inputId = "onset_table",
                                         label = "Select healthcare setting:",
                                         selected = character(0),
                                         choices = rev(unique(hotspot_yearly_data$onset))) # change this when animal data is added rev(unique(hotspot_yearly_data$onset))
                          ),
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.onset_table == null && input.source_table != null", 
                            helpText("Please select healthcare setting")
                          ),
                          
                          # Select the site of the human infection
                          conditionalPanel(
                            condition = "input.onset_table != null",
                            selectInput(
                              "isolatetype_table",
                              label = "Select specimen type:",
                              choices = sort(unique(hotspot_yearly_data$sample_type))) ## unique(hotspot_yearly_data$sample_type[hotspot_yearly_data$sample_oranism == "Human"])
                          ),
                          
                          selectInput(inputId = "region_table",
                                      label = "Select regions:",
                                      choices = sort(unique(hotspot_yearly_data$region)),
                                      selected = "Townsville",
                                      multiple = FALSE),
                          
                          # A slider to select the year
                          #tags$style(type = "text/css", ".irs-bar { background: none; border-top: none; border-bottom: none;}
                          #           .irs-bar-edge {background: none; border: none;}"), # setting the blue bar to be nothing on the slider
                          
                          sliderInput(inputId = "year_table",
                                      label = "Select a year:",
                                      value = max(hotspot_yearly_data$year), # default value
                                      min = min(hotspot_yearly_data$year),
                                      max = max(hotspot_yearly_data$year),
                                      step = 1,
                                      sep = "",
                                      round = TRUE,
                                      ticks = FALSE),
                          
                          br(),
                          br(),
                          # actionButton(inputId  = "load_table",
                          #              label = "Load antibiogram"),
                          helpText("Please wait for the antibiogram to load")
                          
                          
                        ), # close side panel
                        
                        
                        
                        # Main panel for displaying outputs ---------------------------------------------------
                        mainPanel(
                          h4(textOutput("antibiogram_text")),
                          DT::dataTableOutput("antibiogram_table") %>% withSpinner(type=5),
                          tags$div(id="Citation",
                                   'How to cite: Menzies School of Health Research. HOTspots [computer software]. Darwin, Northern Territory. Retrieved from [inset web address when done]' # TO DO ## update this
                                   
                          ) # close citation
                        ) # close main panel
                      ) # close sidebar layout
                      
             ), # close tab panel
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             # Tab 4 - Data -----------------------------------------------------------------------------------------------------
             tabPanel(("Data and Methods"), # name in nav bar
                      icon = icon("list-ul"), # icon in nav bar

                      h3("Data disclaimer"),
                      
                      p("Unless otherwise stated, the information contained in the dataset is provided by the laboratories Territory Pathology (Northern Territory), PathWest (Western Australia), Pathology Queensland (Queensland) and Western Diagnostics (WA and NT data). 
                        With respect to the HOTspots dataset provided by Menzies School of Health Research (‘Menzies), and to the extent permitted by law, neither Menzies nor or any of its employees, makes any warranty, express or implied, or assumes any legal liability or responsibility for the accuracy, completeness, or usefulness of any information (either isolated or in the aggregate) contained, or represents that its use would not infringe privately owned rights. 
                        While the data is provided in good faith and to the best of Menzies knowledge, Menzies does not commit to it being updated. While every effort is made to ensure the data quality, the data is provided 'as is'. Menzies or HOTspots investigators are not responsible for data management after extraction and transmission to the recipient. 
                        The data and information in the dataset provided here are intended for use by persons possessing some technical skill and knowledge in epidemiology, surveillance or data management."),
                      
                      br(), 
                      
                      p("In order to use the HOTspots extracted datasets provided users must adhere to the following guidelines:"),
                      p("•	consider whether a breach of confidentiality is likely due to a low cell count and make no use of the identity of any person discovered inadvertently;"),
                      p("•	not to distribute or sell the datasets to any other individual, institution, or organization without the prior written consent of Menzies and HOTspots investigators."),
                      br(),
                      p("The accuracy of the users' statistical analysis and the findings they report are not the responsibility of Menzies or HOTspots investigators. Menzies or HOTspots investigators shall not be held liable for improper or incorrect use of the data. 
                        In no event shall Menzies or HOTspots be liable for any incidental, indirect, consequential or special damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of profit, loss of contracts, goodwill, or business relationships, arising out of or in connection with the use of the data. 
                        Menzies do not warrant that the files, the servers and the databases used for data storage, extraction, management and transmission are error, virus or bug free and the recipient accepts that it is its responsibility to make adequate provision for protection against such threats."),
                      p("For more information, please see the Menzies Information and Privacy Policy at https://www.menzies.edu.au/page/Policies/."),
                      
                      
                      
                      h3("Methodology"),
                      p("Antibiotic susceptibility data have been contributed by four main pathology service providers across three jurisdictions in northern Australia. These are Territory Pathology, Queensland Pathology, Western Diagnostics and PathWest."),
                      p("Between laboratories there were variations in the content and format that the data were supplied in, requiring a process of data cleaning and standardisation. This is in part due to the variation in AST guidelines used by jurisdiction (Northern Territory and Western Australia use CLSI while Queensland used CLSI to 30 June 2012 and then moved to EUCAST), however it is also due to individual laboratory policies and processes and the availability of antimicrobial agents for testing. For example, Territory Pathology and PathWest both use CLSI, which recommends agents that are important for routine testing against various organisms or organism groups (an antimicrobial panel), however other agents may be tested or reported selectively based on the institution's formulary or substituted with a more practical alternative where their activity is similar. Therefore, the number and type of antibiotics used against the same microbes varies between laboratories. The microbes reported also varied, however common pathogens were identified and these microbes are available to select from the dropdown menu."),
                      p("Data were harmonised across the three jurisdictions by standardising antibiotic, microbe and sample type nomenclature. Duplicates were removed from the data by selecting the first isolate (per person, per calendar year). The percentage of resistant isolates was calculated by dividing the number resistant by the total number of isolates tested.  For years with <30 isolates collected and tested, these data (within the region of interest) were added to the following or previous year (or excluded if all 3 years had <30 isolates). Finally, a three-year rolling average of percentage of resistant isolates was calculated, due to the small numbers of isolates tested in some years."),
                      
                     
                      
                      h3("Explore the data"),
                      
                      # Sidebar layout with input and output definitions
                      sidebarLayout(
                        
                        # Sidebar panel for inputs on the left
                        sidebarPanel(
                          
                          # title
                          h4("Filter the data"), # bold
                          
                          
                          
                          radioButtons(inputId = "source_filt",
                                       label = "Select dataset:",
                                       choices = c("All", sort(unique(hotspot_yearly_data$source))),
                                       selected = "All"), # also can change choice to be unique(hotspot_yearly_data$sample_oranism) which should be human and animal
                          
                        
                          # Select the location where the infection was identified
                          radioButtons(inputId = "onset_filt",
                                         label = "Select healthcare setting:",
                                         selected = "Overall",
                                         choices = rev(unique(hotspot_yearly_data$onset))), # change this when animal data is added rev(unique(hotspot_yearly_data$onset))
                          
                          selectInput(inputId = "isolatetype_filt",
                                      label = "Select specimen type:",
                                      choices = sort(unique(hotspot_yearly_data$sample_type))), ## unique(hotspot_yearly_data$sample_type[hotspot_yearly_data$sample_oranism == "Human"])
                          br(),
                          
                          
                          # Select the microbe name
                          selectInput(inputId = "microbe_name_filt", 
                                      label = "Select organism:",
                                      choices = c("All", sort(unique(hotspot_yearly_data$organism))),
                                      selected = "All", # none selected as the default when the app opens
                                      multiple = FALSE), # cannot select multiple
                          
                          
                          
                            selectInput(inputId = "antibiotic_name_filt",
                                        label = "Select antibiotic:",
                                        choices = c("All",sort(unique(hotspot_yearly_data$antimicrobial))),
                                        selected = "All",
                                        multiple = FALSE),
                         
                            selectInput(inputId = "region_filt",
                                        label = "Select a region:",
                                        choices = c("All", sort(unique(hotspot_yearly_data$region))),
                                        selected = "All",
                                        multiple = FALSE),
                          
                          br(),
                          
                          # A slider to select the year
                          # Alter the style so there in no tail behind
                          #tags$style(type = "text/css", ".irs-bar { background: none; border-top: none; border-bottom: none;}
                          #           .irs-bar-edge {background: none; border: none;}"), # setting the blue bar to be nothing on the slider
                          
                          radioButtons(inputId = "year_select_filt",
                                       label = "Which years to display?",
                                       selected = "All",
                                       choices = c("All" = "All", 
                                                   "Single year" = "single")), # add in a range?
                          
                          
                          conditionalPanel(  # conditional to display the year input only when the tab is antimicrobe
                            condition = "input.year_select_filt == 'single'",
                            
                            sliderInput(inputId = "year_filt",
                                        label = "Select a year:",
                                        value = max(hotspot_yearly_data$year), # default value
                                        min = min(hotspot_yearly_data$year),
                                        max = max(hotspot_yearly_data$year),
                                        step = 1,
                                        sep = "",
                                        round = TRUE,
                                        ticks = FALSE)
                          ),
                          
                        
                          
                          
                          checkboxGroupInput(inputId = "data_investigate", label ="Timeframe:",
                                             choices = c("Yearly data" = "yearly",
                                                         "Monthly data" = "monthly"),
                                             selected = "yearly"),
                          br(),
                          
                          
                          h4("Download the data"),
                          downloadButton(outputId = "downloadData_yearly", label = "Full dataset"),
                          downloadButton(outputId = "downloadData_yearly_subset", label = "Subset")
                          
                          
                        ), # close the side bar
                        
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          conditionalPanel(
                            #condition = "input.data_investigate == 'yearly'",
                            condition = "input.data_investigate.indexOf('yearly') > -1",
                            h4("The yearly data"),
                            br(),
                            DT::dataTableOutput("table_data_year"),
                            br(),
                            br(),
                            
                          ),
                          
                          conditionalPanel(
                            #condition = "input.data_investigate == 'monthly'",
                            condition = "input.data_investigate.indexOf('monthly') > -1",
                            h4("The monthly data"),
                            br(),
                            DT::dataTableOutput("table_data_month"),
                            br(),
                            br(),
                            br()
                          ) # close conditional panel
                        )# close main panel
                      ), # close side bar layout
                      
                     br(),
                     br(),
                     
                      h3("Terms of use"),
                      
                      p("Users must read and adhere to the terms of the HOTspots Data Disclaimer. Users must not use the datasets in any way which is inconsistent with the Privacy Act 1988 (Cth), the Information Act 2002 (NT), the HOTSpots Data Disclaimer or the HOTspots Terms of Use."),
                      p("The data and information in the dataset downloaded are intended for use by persons possessing technical skill and knowledge in epidemiology, surveillance and data management. Commercial use of the HOTspots data is not permitted without prior written consent from Menzies."),
                      p("Except where otherwise stated, downloading and reproduction of published (in paper or electronically) HOTspots data for personal use or for further non-commercial dissemination, are authorised provided appropriate acknowledgement is given to HOTspots investigators as the source. Any publication arising from the dataset provided should credit Menzies and HOTspots investigators in the relevant parts of the publication. Please contact the lead investigator Teresa.wozniak@menzies.edu.au to discuss further."), # TO DO ## add link to the email
                      
                      br(),           
                      p("The citation should read as follows:"),
                      strong("Menzies School of Health Research. HOTspots [computer software]. Darwin, Northern Territory. Retrieved from [inset web address when done]"),
                      
                      fluidRow( 
                        column(3, img(src='HOTspots_logo copy 2.png', align = "left", width = '100%', height = 'auto')), #, width = "250px"
                        column(2, img(src='Mezies_logo_white.png', align = "left", width = '100%', height = 'auto')), #, width = "250px"
                        column(2, img(src='HotNorth logo.png', align = "left", width = '100%', height = 'auto')) #, width = "250px"
                      )
             ),
             
             tabPanel("Economic Burden", icon = icon("calculator"),
                      h4("The economic loss frmo AMR calculator"),
                      p("Add in the code from the other shiny app here"),
                      br(),
                      br(),
                      p("Citation for the economic shiny to Prof Adrian Barnett")
                      
             ),
             
             
             
             
             
             
             ## Fifth tab, drop down menu for links and news  ----------------------------------------------------------------
             
             navbarMenu(("More information"), icon = icon("info"), # change icon
                        
                        ## Info tab, Contact us  ----------------------------------------------------------------
                        tabPanel(("Contact us"), 
                                 titlePanel(h1("Contact the HOTspots team")),
                                 br(),
                                 br(),
                                 p("For general information regarding the data, use of the HOTspots tool and suggestions of further aspects to implement,"),
                                 p("please contact Teresa Wozniak from the Menzie's School of Health Research on teresa.wozniak@menzies.edu.au"),
                                 
                                 br(),
                                 p("For specific questions regarding the website or to report an issue,"),
                                 p("please email HOTspots@menzies.edu.au")),
                        
                        
                        ## therapeutic guidelines
                        tabPanel(("Therapeutic Guidelines"),
                                 titlePanel(h1("Links to the therapeutic guidelines")),
                                 h3("Australian guides"),
                                 p("links"),
                                 br(),
                                 h3("Local Guides"),
                                 h4("NT"),
                                 p("links"),
                                 h4("QLD"),
                                 p("links"),
                                 h4("WA"),
                                 p("links"),
                                 p("ect...")
                                 
                                 ),     
                        
                        
                        ## About tab, Publications  ----------------------------------------------------------------
                        tabPanel(("Publications"), 
                                 titlePanel(h1("List of relevant publications"))),
                        
                        ## About tab, News  ----------------------------------------------------------------
                        tabPanel(("News"), 
                                 titlePanel(h1("News"))),                        
                        
                        
                        ## About tab, Other  ----------------------------------------------------------------
                        tabPanel(("Other resources"), 
                                 titlePanel(h1("Links to the News articles or blogs")))
                        
                        
             ),
             
             tabPanel("About Us", icon = icon("user-circle"),
                      h3("Meet the team"),
                      p("Images and blurb for the team members"),
                      br(),
                      br(),
                      h3("Contact us"),
                      p("Email: HOTspots@menzies.edu.au "),
                      
             ),
             
             
             
             ## Eight tab, Share  ----------------------------------------------------------------
             
             navbarMenu(("Share"), icon = icon("share-alt"), # consider adding a hashtag
                        
                        ## Twitter  ----------------------------------------------------------------
                        tabPanel(tags$a(href = 'https://twitter.com/intent/tweet?url=http%3A%2F%2Famrhotspots.com.au%2Faci%2Fadminpanel%2Fmanage%2Flogin &text=Check%20out%20this%20tool%20to%20track%20antibiotic%20resistance', icon("twitter"), "Twitter" )),
                        
                        ## LinkedIn  ----------------------------------------------------------------
                        tabPanel(tags$a(href = 'http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Famrhotspots.com.au%2Faci%2Fadminpanel%2Fmanage%2Flogin &title=Check%20out%20this%20tool%20to%20track%20antibiotic%20resistance', icon("linkedin"), "LinkedIn" )),
                        
                        ## Facebook  ----------------------------------------------------------------
                        tabPanel(tags$a(href = 'https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Famrhotspots.com.au%2Faci%2Fadminpanel%2Fmanage%2Flogin', icon("facebook"), "Facebook" ))
                        
                        
             )# close navbar2 
  )# close navbar
) # Close fluidpage



























































############
## Server ##
############

server <- function(input,output, session){
  
  
  
  
  
  
  #### Reactive values -------------------------------------------------------------------------------
  # Saves computational time by only calculating the values once and then using them many times
  
  
  #### Reactive values -------------------------------------------------------------------------------
  # Saves computational time by only calculating the values once and then using them many times
  
  Data_yearly <- reactive({
    hotspot_yearly_data
  })
  
  ## For the main landing page -----------------
  
  # Filtered by the sample attributes
  hotspot_yearly_filter1 <- reactive({
    req(input$source, input$onset, input$isolatetype, input$microbe_name)
    
    data1 <- Data_yearly() %>%
      filter(source == input$source, onset == input$onset, sample_type == input$isolatetype, organism ==  input$microbe_name)
    data1
  })
  
  # # Filtered by the organism
  # hotspot_yearly_filter2 <- reactive({
  #   req(input$microbe_name, input$source, input$isolatetype, input$onset)
  #   
  #   data1 <- hotspot_yearly_filter1() %>%
  #     filter(organism ==  input$microbe_name)
  #   data1
  # })
  # 
  
  # Filtered by the antimicrobial
  hotspot_yearly_filter2 <- reactive({
    req(input$source, input$onset, input$isolatetype, input$microbe_name, input$antibiotic_name)
    
    data1 <- hotspot_yearly_filter1() %>%
      filter(antimicrobial ==  input$antibiotic_name)
    data1
  })
  
  # Filtered by the year
  hotspot_yearly_filter3 <- reactive({
    req(input$year, input$antibiotic_name, input$microbe_name, input$source, input$isolatetype, input$onset)
    
    data1 <- hotspot_yearly_filter2() %>%
      filter(year ==  input$year)
    data1
  })
  
  
  
  
  
  ## For the specific combinations ----------------- 
  
  ## Use this in the plots to compare regions, drugs and specific combos
  # data_microbe_spec <- reactive({
  #   req(input$isolatetype_spec, input$onset_spec )
  #   data1 <- hotspot_monthly_data %>%
  #     filter(source == input$source_spec, sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec) # add another filter to be human/animal
  #   data1
  # })
  
  ## Use this in the plots to compare regions, drugs and specific combos
  data_monthly_spec <- reactive({
    req(input$source_spec, input$isolatetype_spec, input$onset_spec, input$microbe_name_spec , input$region_spec1, input$antibiotic_name_spec1)
    
    data1 <- hotspot_monthly_data %>% 
      filter( source == input$source_spec, sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec, region == input$region_spec1, antimicrobial == input$antibiotic_name_spec1) # add another filter to be human/animal
    data1
  })
  
    
  ## yearly ##
  data_yearly_spec <- reactive({
    req(input$microbe_name_spec,  input$isolatetype_spec, input$onset_spec )
    yearly_data1 <- Data_yearly()%>%
      filter(source == input$source_spec, sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec) 
    yearly_data1
  })
  
  # antimicrobial
  data_yearly_spec2 <- reactive({
    req(input$microbe_name_spec,  input$isolatetype_spec, input$onset_spec, input$antibiotic_name_spec1 )
    yearly_data1 <- data_yearly_spec()%>%
      filter(antimicrobial == input$antibiotic_name_spec1)
    yearly_data1
  })
  
  # antibiotics
  data_yearly_spec3 <- reactive({
    req(input$microbe_name_spec,  input$isolatetype_spec, input$onset_spec, input$antibiotic_name_spec2 )
    yearly_data1 <- data_yearly_spec() %>%
      filter(antimicrobial %in% input$antibiotic_name_spec2, region == input$region_spec1 )
    yearly_data1 
  })
  

  
  # ## Use this in the plots to compare the sample type
  # ## yearly
  # data_yearly_sample_type <- reactive({
  #   req(input$isolatetype_spec, input$onset_spec )
  #   data1 <- Data_yearly()%>%
  #     filter(onset == input$onset_spec, organism ==  input$microbe_name_spec, antimicrobial == input$antibiotic_name_spec1, region == input$region_spec1) # add another filter to be human/animal
  #   data1
  # })
  # ## monthly
  # data_compare_sample_type <- reactive({
  #   req(input$isolatetype_spec, input$onset_spec )
  #   data1 <- hotspot_monthly_data %>%
  #     filter(onset == input$onset_spec, organism ==  input$microbe_name_spec, antimicrobial == input$antibiotic_name_spec1, region == input$region_spec1) # add another filter to be human/animal
  #   data1
  # })
  # 
  # ## Use this in the plots to compare the onset location
  # data_compare_onset_location <- reactive({
  #   req(input$isolatetype_spec, input$onset_spec )
  #   data1 <- hotspot_monthly_data %>%
  #     filter(sample_type == input$isolatetype_spec, organism ==  input$microbe_name_spec, antimicrobial == input$antibiotic_name_spec1, region == input$region_spec1) # add another filter to be human/animal
  #   data1
  # })
  # 
  
  
  
  data_antibiogram <- reactive({
    req( input$source_table, input$isolatetype_table, input$onset_table, input$region_table)
      
    data <-  Data_yearly() %>%
      filter(source == input$source_table, sample_type == input$isolatetype_table, onset == input$onset_table, region == input$region_table) %>%
      # select(organism, antimicrobial, percent_resistant_yearly_overall) %>%
      distinct
    
  })

  
  
  
  
  #### Update the UI inputs  -----------------------------------------------------------------
  
  
  ## For the landingpage map ##
  
  # Update the list of antimicrobials
  observe({
    req(input$source, input$isolatetype, input$onset, input$microbe_name)
    data <- hotspot_yearly_filter1() # filtered by everything expect the antibiotic name and  year
    
    updateSelectInput(session, inputId = "antibiotic_name",
                      label = "Select antimicrobial:",
                      selected = "Cefazolin", 
                      choices = sort(unique(data$antimicrobial)))
  })
  
  # Upate the years available
  observe({
    req(input$source, input$isolatetype, input$onset, input$microbe_name, input$antibiotic_name)
    data <- hotspot_yearly_filter2() # filtered by everything expect the antibiotic name and  year
    
    updateSliderInput(session, inputId = "year",
                      value = max(data$year),
                      min = min(data$year),
                      max = max(data$year))
  })
  
  
  
  
  ## For the plotting page ##
  # Update the list of antimicrobials
  observe({ 
    req(input$isolatetype, input$onset, input$microbe_name)
    data <- hotspot_yearly_filter1() %>%
      filter(source == input$source, sample_type == input$isolatetype, onset == input$onset, organism ==  input$microbe_name)
    
    updateSelectInput(session, inputId = "antibiotic_name_spec1",
                      label = "Select antimicrobial:",
                      choices = sort(unique(data$antimicrobial)),
                      selected = NULL)
  })
  

  
  ## TO DO
  observe({ 
    req(input$antibiotic_name_spec1)
    
    data <- data_yearly_spec2()
    
    updateSelectInput(session, inputId = "region_spec1",
                      label = "Select region:",
                      choices = sort(unique(data$region)),
                      selected = "Townsville")
  })
  
  
  ## TO DO 
  observe({ 
    req(input$region_spec1, input$antibiotic_name_spec1, )
    
    data <- data_yearly_spec3()
      
    updateSliderInput(session, inputId = "year_spec",
                      label = "Select the year:",
                      value = max(data$year), # default value
                      min = min(data$year),
                      max = max(data$year))
  })
  
  
  ## Antibiogram year selector
  
  
  observe({
    req( input$source_table, input$isolatetype_table, input$onset_table, input$region_table)
    
    data <- data_antibiogram()
    
    updateSliderInput(session, inputId = "year_table",
                      label = "Select the year:",
                      value= max(data$year),
                      min = min(data$year),
                      max = max(data$year))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #### Outputs to display -----------------------------------------------------------------
  
  # output$map_title <- renderText({
  #   # if(is.null(input$load_map) & is.null(input$load_CB_friendly_map)){
  #   #   paste("Tracking Antimicrobial Resistance")
  #   # } else {
  #     paste("Resistance of", input$microbe_name, "to", input$antibiotic_name, "in", input$year )
  #   # }
  #  
  # })
  
  # reactive expression
  map_title_RV <- eventReactive( input$load_map, {
    paste("Resistance of", em(input$microbe_name), "to", tolower(input$antibiotic_name), "in", input$year )
  })
  
  # text output
  # output$map_title <- renderText({
  #   map_title <- map_title_RV()
  # })
  
  output$map_title <- renderUI(
    HTML(map_title_RV())
  )
  
  ## For the landing page map -----------------
  output$leaflet_map <- renderLeaflet({
    #data <- hotspot_yearly_filter3()
    
    SA3_data %>% # using the shapefile data
      leaflet() %>% # create a leaflet map
      fitBounds(lng1 = summary(SA3_data)$bbox[1], lat1 = summary(SA3_data)$bbox[2], lng2 = summary(SA3_data)$bbox[3], lat2 = summary(SA3_data)$bbox[4]) %>% # the starting position of the map
      addTiles(options = providerTileOptions(minZoom = 2)) %>% # sets the furtherst that can be zoomed out
      addLegend("bottomright", pal = pal_num, values = ~c(0:100), 
                title = "% resistance", # legend title
                opacity = 1)
    
  })
  
  
  # If data is selected, then add the shapefiles
  observeEvent(input$load_map, {
    
    #req(input$onset, input$isolatetype, input$microbe_name, input$antibiotic_name, input$year)
    
    if(input$load_CB_friendly_map == FALSE){
      col_palette <- pal_num
    } else if (input$load_CB_friendly_map == TRUE){
      col_palette <- pal_num_CBfriendly
    }
    
    data <- hotspot_yearly_filter3()
    merged_data <- merge(SA3_data, data, by.x="SA3_NAME16", by.y="region")
    
    
    
    
    
    leafletProxy("leaflet_map", data = merged_data ) %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(  fillColor = ~col_palette(merged_data$percent_resistant_yearly_overall),
                    fillOpacity = ifelse(is.na(merged_data$percent_resistant_yearly_overall), 0, 0.7),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    popup = ifelse(is.na(merged_data$percent_resistant_yearly_overall), # consider the option of adding a little 
                                   paste("No data available"),
                                   paste0('<strong>', merged_data$SA3_NAME16, '</strong>',
                                          '<br/>', '<strong>',"Resistance: ", '</strong>',  round(merged_data$percent_resistant_yearly_overall,1), "%",
                                          '<br/>', '<strong>', "No. of isolates: ", '</strong>', format(merged_data$num_of_tests_yearly_overall, big.mark = ",") )),
                    highlight = highlightOptions(
                      weight = 5,
                      color = "black",
                      bringToFront = TRUE))  %>%
      addLegend("bottomright", pal = col_palette, values = ~c(0:100), # also add values as ~percent_resistant_yearly
                title = "% resistance",
                opacity = 1)
    
  })
  
  
  
  # TO DO - turn on and off with the
  # If data is selected, then add the shapefiles
  # observeEvent(input$load_CB_friendly_map, {
  # 
  #   isolate(
  #   data <- hotspot_yearly_filter3()
  #   )
  # 
  #   merged_data <- merge(SA3_data, data, by.x="SA3_NAME16", by.y="region")
  # 
  #   leafletProxy("leaflet_map", data = merged_data ) %>%
  #     clearControls () %>%
  #     addPolygons(  fillColor = ~pal_num_CBfriendly(merged_data$percent_resistant_yearly_overall),
  #                   fillOpacity = ifelse(is.na(merged_data$percent_resistant_yearly_overall), 0, 0.7),
  #                   weight = 2,
  #                   opacity = 1,
  #                   color = "white",
  #                   dashArray = "3",
  #                   popup = ifelse(is.na(merged_data$percent_resistant_yearly_overall), # consider the option of adding a little
  #                                  paste("No data available"),
  #                                  paste0('<strong>', merged_data$SA3_NAME16, '</strong>',
  #                                         '<br/>', '<strong>',"Resistance: ", '</strong>',  round(merged_data$percent_resistant_yearly_overall,1), "%",
  #                                         '<br/>', '<strong>', "No. of isolates: ", '</strong>', format(merged_data$num_of_tests_yearly_overall, big.mark = ",") )),
  #                   highlight = highlightOptions(
  #                     weight = 5,
  #                     color = "black",
  #                     bringToFront = TRUE)) %>%
  #     addLegend("bottomright", pal = pal_num_CBfriendly, values = ~c(0:100), # also add values as ~percent_resistant_yearly
  #               title = "% resistance",
  #               opacity = 1)
  # 
  # 
  # })
  # 
  
  
  
  
  
  
  
  
  
  
  ## Main plots  -----------------------------------------------------------------
  # to use on the first page maybe under the heat map
  
  output$text_monthly <- renderText({
    paste("Measured at monthly intervals")
  })
  output$text_yearly <- renderText({
    paste("Measured at yearly invervals with >15 samples per month/year")
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ## Compare regions  -----------------------------------------------------------------
  
  ##### HERE #######
  # output$text_compare_reg <- renderText({
  #   # if(is.null(input$isolatetype_spec)){
  #   #   text_select_inputs
  #   # } else{
  #   #   paste("Resistance of", input$antibiotic_name_spec1, "to", input$microbe_name_spec)
  #   # }
  #   req(input$source_spec, input$onset_spec, input$isolatetype_spec, input$region_spec2)
  #   paste("Resistance of", input$microbe_name_spec, "to", input$antibiotic_name_spec1)
  # })

  output$text_compare_reg <- renderUI(
    req(input$source_spec, input$onset_spec, input$isolatetype_spec, input$region_spec2),
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1)))
  )
  

  
  output$plot_compare_reg <- renderPlot( {
    data <- data_yearly_spec()
    
    
    data %>%
      filter(antimicrobial == input$antibiotic_name_spec1) %>%
      filter(region %in% input$region_spec2) %>%
      arrange(region) %>%
      ggplot(aes(x=as.factor(year),  y=percent_resistant_yearly_overall, fill=region)) + 
      geom_bar(stat="identity", position=position_dodge(width=0.7, preserve = 'single'), width=0.7) + 
      theme_bw() +
      labs(x="Year", y="Percentage resistance  (%)", fill="Regions") + 
      theme(text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +  #text = element_text(size=15)
      scale_fill_manual(values = hotspot_palette$regions) + 
      facet_wrap(~jurisdiction, ncol=1, labeller = labeller(jurisdiction = 
                                                              c("FNQ" = "Far North Queensland",
                                                                "WA" = "Western Australia",
                                                                "NT" = "Northern Territory")))
    
    
  }, height = function(){
    session$clientData$output_plot_compare_reg_width
  })
  
  
  
  
  
  
  
  
  
  
  ## Compare antimicrobials  -----------------------------------------------------------------
  # output$text_compare_anti <- renderText({
  #   req(input$source_spec, input$onset_spec, input$isolatetype_spec)
  #   paste("Resistance of", input$microbe_name_spec, "in", input$region_spec1, "in", input$year_spec)
  # })
  
  output$text_compare_anti <- renderUI(
    req(input$source_spec, input$onset_spec, input$isolatetype_spec),
    HTML(paste("Resistance of", em(input$microbe_name_spec), "in", input$region_spec1, "in", input$year_spec))
  )
  
  output$plot_compare_anti <- renderPlot( {
    #data <- data_yearly_spec()
    
    data_yearly_spec3() %>%
      filter(year== input$year_spec)  %>%
      arrange(percent_resistant_yearly_overall) %>%
      ggplot() + 
      geom_point( aes(x=reorder(antimicrobial, percent_resistant_yearly_overall),  y=percent_resistant_yearly_overall), size=5) + # colour factor year
      coord_flip() +
      labs(x=NULL, y="Percentage resistance  (%)") + 
      theme_bw() +
      theme(legend.position = "bottom",text = element_text(size=15) ) #text = element_text(size=15)
    
  })
  
  
  ## plot yearly resistance as bar plot, facet wrap by the antimicrobial
  # output$text_compare_anti2 <- renderText({
  #   req(input$source_spec, input$onset_spec, input$isolatetype_spec)
  #   paste("Resistance of", input$microbe_name_spec, "in", input$region_spec1)
  # })
  
  output$text_compare_anti2 <- renderUI(
    req(input$source_spec, input$onset_spec, input$isolatetype_spec),
    HTML(paste("Resistance of", em(input$microbe_name_spec), "in", input$region_spec1))
  )
  
  output$plot_compare_anti2 <- renderPlot( {
    #data <- data_yearly_spec()
    req(input$region_spec1, input$antibiotic_name_spec2)
    
    validate(
      need(data_yearly_spec3() != "", "Data is empty")
    )
    
    validate(
      need(!is.null(data_yearly_spec3()), "Data is null")
    )
    
    data_yearly_spec3() %>%
      arrange(antimicrobial) %>%
      ggplot(aes(x=as.factor(year),  y=percent_resistant_yearly_overall)) + 
      geom_bar(stat="identity", position=position_dodge(width=0.7, preserve = 'single'), width=0.7) + 
      labs(x="Year", y="Percentage resistance  (%)") + 
      theme_bw() +
      theme(legend.position = "bottom", text = element_text(size=15) ) + # text = element_text(size=15)
      facet_wrap(~antimicrobial, ncol=1)
    
  }, height = function(){
    session$clientData$output_plot_compare_anti2_width
  })
  
  
  
  
  
  
  
  
  
  
  
  ## Compare samples  -----------------------------------------------------------------
  
  # output$plot_compare_sample <- renderPlot( {
  #   data <- data_compare_sample_type()
  #   
  #   data %>%
  #     ggplot(aes(x = date_dmy) ) + 
  #     geom_path(aes( y = percent_resistant_monthly_raw, colour = sample_type), position=position_dodge) +
  #     scale_x_date(date_labels = "%b-%Y", name = "Date") +
  #     scale_colour_manual(values = hotspot_palette$sample) + 
  #     xlab("Percent resistant") +
  #     theme_bw() 
  # })
  
  # ## By year
  # ## Polygon edge not found?? 
  # output$plot_compare_sample <- renderPlot( {
  #   data <- data_yearly_sample_type()
  #   
  #   data  %>%
  #     filter(sample_type != "All") %>%
  #     ggplot(aes(x = as.factor(year)) ) + 
  #     geom_bar(aes(y = num_of_tests_yearly_overall), stat="identity", fill="lightgrey") +
  #     geom_bar(aes(y = resistant_yearly, fill = sample_type), stat="identity", position = "stack") +
  #     scale_fill_manual(values = hotspot_palette$sample) + 
  #     labs(x=NULL, y="Number of tests", fill="Type of resistant sample") + 
  #     theme_bw()  +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #           text = element_text(size=15))
  #   
  # })
  # 
  # 
  # ## by month
  # output$plot_compare_sample1 <- renderPlot( {
  #   data <- data_compare_sample_type()
  #   
  #   data  %>%
  #     filter(sample_type != "All") %>%
  #     ggplot(aes(x = date_dmy) ) + 
  #     geom_bar(aes(y = num_of_tests_monthly_raw), stat="identity", fill="lightgrey") +
  #     geom_bar(aes(y = resistant_monthly_raw, fill = sample_type), stat="identity", position = "stack") +
  #     scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month", date_labels = "%b-%Y") +
  #     scale_fill_manual(values = hotspot_palette$sample) + 
  #     labs(x=NULL, y="Number of tests", fill="Type of resistant sample") + 
  #     theme_bw() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #           text = element_text(size=15),
  #           legend.position = "top")
  # })
  # 
  
  
  
  ## Compare onset -----------------------------------------------------------------
  ## Polygon edge not found?? 
  ## consider making this a donut 
  # output$plot_compare_onset <- renderPlot( {
  #   data <- data_compare_onset_location() # consider removing overall
  #   
  #   data %>%
  #     filter(onset != "Overall") %>%
  #     ggplot(aes(x = date_dmy) ) + 
  #     geom_bar(aes(y = num_of_tests_monthly_raw), stat="identity", fill="lightgrey") +
  #     geom_bar(aes(y = resistant_monthly_raw, fill = onset), stat="identity", position = "stack") +
  #     scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month", date_labels = "%b-%Y") +
  #     scale_colour_manual(values = hotspot_palette$onset) + 
  #     labs(x=NULL, y="Number of tests", fill="Onset location of the resistant samples") + 
  #     theme_bw()  +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #           text = element_text(size=15),
  #           legend.position = "bottom")
  #   
  # })
  
  
  
  ## Compare Age group  --------------------
  # output$text_age <- renderText({
  #   req(input$source_spec, input$onset_spec, input$isolatetype_spec)
  #   paste("Resistance of", input$microbe_name_spec, "to", input$antibiotic_name_spec1, "in", input$region_spec1, "by age groups")
  # })
  # 
  output$text_age <- renderUI(
    req(input$source_spec, input$onset_spec, input$isolatetype_spec),
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1), "in", input$region_spec1, "by age groups"))
  )
  
  output$plot_compare_age <- renderPlot({
    
    data_age <- as.data.frame(hotspot_yearly_splitage) %>%
      filter(source == input$source_spec, sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec) %>%
      filter(region == input$region_spec1, antimicrobial == input$antibiotic_name_spec1)
    
    data_age %>%
      ggplot(aes(x=as.factor(year),  y=percent_resistant_yearly_overall, fill=age_group)) + 
      geom_bar(stat="identity", position=position_dodge(width=0.7, preserve = 'single'), width=0.7) +
      theme_bw() +
      scale_fill_manual(values = hotspot_palette$age) + 
      labs(x="Year", y="Percentage resistance  (%)", fill="Age Group") + 
      theme(legend.position = "top", text = element_text(size=15))
    
  })
  
  
  
  
  
  ## Compare Sex --------------------
  ## plot yearly resistance as bar plot, facet wrap by the antimicrobial
  # output$text_sex <- renderText({
  #   req(input$source_spec, input$onset_spec, input$isolatetype_spec)
  #   paste("Resistance of", input$microbe_name_spec, "to", input$antibiotic_name_spec1, "in", input$region_spec1, "for the two sexes of female and male")
  # })
  
  output$text_sex <- renderUI({
    req(input$source_spec, input$onset_spec, input$isolatetype_spec)
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1), "in", input$region_spec1, "for the two sexes of female and male"))
  })
  
  
  output$plot_compare_sex <- renderPlot({
    
    data <-  data_yearly_spec2() %>%
      filter(region == input$region_spec1)
    
    data_sex <- hotspot_yearly_splitsex %>%
      filter(source == input$source_spec, sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec) %>%
      filter(region == input$region_spec1, antimicrobial == input$antibiotic_name_spec1)
    
    ggplot() +
      #overall
      geom_path(data = data, aes(x = year, y=percent_resistant_yearly_overall), size = 2, colour="grey") + 
      geom_point(data = data, aes(x = year, y=percent_resistant_yearly_overall), size = 3, colour="grey") +
      # sex 
      geom_path(data = data_sex, aes(x = year, y=percent_resistant_yearly_overall, color = sex, linetype = sex), size=1.5) + 
      geom_point(data = data_sex, aes(x = year, y=percent_resistant_yearly_overall, color = sex, shape = sex), size=2) +
      scale_x_continuous(breaks=seq(min(data$year), max(data$year))) +
      scale_fill_manual( values = hotspot_palette$sex, breaks=c("F", "M"), labels=c("Female", "Male"), name = "Sex") + #add here name = "sex"
      labs(x=NULL, y="Percentage resistance (%)") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size=15),
            legend.position = "top") #text = element_text(size=15)
  })
  
  
  
  

  
  
  
  
  
  
  
  
  ## Specific plots ----------
  # Original
  output$text_spec <- renderUI({
    req(input$microbe_name_spec, input$region_spec1, input$antibiotic_name_spec1, input$isolatetype_spec , input$onset_spec)
   
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", input$antibiotic_name_spec1 , "in the", input$region_spec1, "by month"))
  })
  
  output$plot_spec <- renderPlot( {
    req(input$microbe_name_spec, input$region_spec1, input$antibiotic_name_spec1, input$isolatetype_spec , input$onset_spec)
    
    data_monthly_spec() %>%
      ggplot(aes(x = date_dmy, y=percent_resistant_monthly_raw) ) +
      geom_path() + 
      geom_point() +
      scale_x_date(date_breaks = "6 months",
                   date_minor_breaks = "1 month",
                   date_labels = "%b-%Y") + #6 months when text size  increase
      labs(x=NULL, y="Percentage resistance (%)") + # check if this should be colour or color
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size=15))
  })
  
  
  # output$text_spec2 <- renderText({
  #   req(input$microbe_name_spec, input$region_spec1, input$antibiotic_name_spec1, input$isolatetype_spec , input$onset_spec)
  #   
  #   paste("Number of isolates testing the resistance of", input$microbe_name_spec, "to", input$antibiotic_name_spec1 , "in the", input$region_spec1, "by month")
  # })
  
  output$text_spec2 <- renderUI({
    req(input$microbe_name_spec, input$region_spec1, input$antibiotic_name_spec1, input$isolatetype_spec , input$onset_spec)
    HTML(paste("Number of isolates testing the resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1) , "in the", input$region_spec1, "by month"))
  })
  

  
  output$plot_spec2 <- renderPlot( {
    req(input$microbe_name_spec, input$region_spec1, input$antibiotic_name_spec1, input$isolatetype_spec , input$onset_spec)
    
   
    
    data_monthly_spec() %>%
      ggplot(aes(x = date_dmy) ) +
      geom_bar(aes(y = resistant_monthly_raw), stat = "identity", fill="lightgrey") +
      scale_x_date(date_breaks = "6 months",
                   date_minor_breaks = "1 month",
                   date_labels = "%b-%Y") + #6 months when text size  increase
      labs(x=NULL, y="Number of isolates") + # check if this should be colour or color
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size=15))
    
  })
  
  
  
  
  
  
  
  
  ## Antibiogram -----------------------------------------------------------------
  ## Disable the search bar 
  ## Check the colour palette
  
  output$antibiogram_table <- DT::renderDataTable({
    req(input$source_table, input$isolatetype_table, input$onset_table,  input$region_table, input$year_table)
    
    # input$load_table
    # isolate(data <-  data_antibiogram() %>%
    #   filter(year == input$year_table))
    
    data <-  data_antibiogram() %>%
      filter(year == input$year_table)
    
    # validate(
    #   need(data != "", "data is empty")
    # )
    # 
    # validate(
    #   need(!is.null(data), "Data is null")
    # )
    # 
    # validate(
    #   need(!exists("Data"), "Data does not exist")
    # )
    
    data["percent_susceptible_yearly_overall"] <- round(data["percent_susceptible_yearly_overall"], 1)
    data <- reshape2::dcast(data = data, antimicrobial ~ organism, value.var = "percent_susceptible_yearly_overall")
    rownames(data) <- data[,1]
    data <- data[,-1]
    
    
    # default is not for NAs to be displayed
    # if you do want NAs, use
    options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
    
    # Set the colours and the breaks at which to set the colours ## TO DO
    brks <- seq(10, 100, 10) # values to break
    clrs <- rev(c("#fffeed", pal_num(seq(0,90,10)))) # 11 colours, # turn the heat map into 10 colours
    
    
    dat <-  DT::datatable(data = data, 
                          class = 'cell-border hover', #compact
                          extensions = c('Buttons', 'FixedColumns', 'KeyTable'),
                          #extensions = "FixedColumns",
                          options = list(scrollX=TRUE,
                                         keys = TRUE,
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         headerCallback = JS(headerCallback2),
                                         columnDefs = list(
                                           list(targets = "_all", className = "dt-center")),
                                         fixedColumns = list(leftColumns = 1),
                                         dom = 'tB')) %>%
      DT::formatStyle(names(data), # all the columns
                      backgroundColor = DT::styleInterval(cuts = brks, values = clrs),
                      color = DT::styleInterval( 50, c('white', 'black'))) # any text with a value below 50 will be black, and above 50 will be white
    #DT::formatStyle(1,  # hopefully the row names
    #               fontWeight = 'bold') # Doesnt work -first column of data
    
    # Add option of displaying percentage in each cell
    # %>% formatPercentage(names(data), number of decimal places)
    
    return(dat)
    
    
    # 
    # dat <-  DT::datatable(data = data, 
    #                       class = 'cell-border hover', #compact
    #                       extensions = c('Buttons', 'FixedColumns', 'KeyTable'),
    #                       #extensions = "FixedColumns",
    #                       options = list(scrollX=TRUE,
    #                                      keys = TRUE,
    #                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    #                                      headerCallback = JS(headerCallback),
    #                                      columnDefs = list(
    #                                        list(targets = "_all", className = "dt-center")),
    #                                      fixedColumns = list(leftColumns = 1),
    #                                      dom = 'tB')) %>%
    #   DT::formatStyle(names(data), # all the columns
    #                   backgroundColor = DT::styleInterval(cuts = brks, values = clrs),
    #                   color = DT::styleInterval( 50, c('white', 'black'))) # any text with a value below 50 will be black, and above 50 will be white
    # #DT::formatStyle(1,  # hopefully the row names
    # #               fontWeight = 'bold') # Doesnt work -first column of data
    # 
    # # Add option of displaying percentage in each cell
    # # %>% formatPercentage(names(data), number of decimal places)
    # 
    # return(dat)
    # 
  })

  output$antibiogram_table2 <- DT::renderDataTable({
    # req(input$source_table, input$isolatetype_table, input$onset_table,  input$region_table, input$year_table)

    # input$load_table
    # isolate(data <-  data_antibiogram() %>%
    #   filter(year == input$year_table))

    data <-  data_antibiogram() %>%
      filter(year == input$year_table)

    dat <-  DT::datatable(data = data,
                          class = 'cell-border hover', #compact
                          extensions = c('Buttons', 'FixedColumns', 'KeyTable'),
                          #extensions = "FixedColumns",
                          options = list(scrollX=TRUE,
                                         keys = TRUE,
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         headerCallback = JS(headerCallback2),
                                         columnDefs = list(
                                           list(targets = "_all", className = "dt-center")),
                                         fixedColumns = list(leftColumns = 1),
                                         dom = 'tB'))

    return(dat)


  })


  output$antibiogram_table3 <- DT::renderDataTable({
    # req(input$source_table, input$isolatetype_table, input$onset_table,  input$region_table, input$year_table)

    # input$load_table
    # isolate(data <-  data_antibiogram() %>%
    #   filter(year == input$year_table))

    data <-  data_antibiogram() %>%
      filter(year == input$year_table)

    dat <-  DT::datatable(data = data,
                          options = list(fixedColumns = list(leftColumns = 1),
                                         dom = 't'))

    return(dat)


  })

  output$antibiogram_table_source <- DT::renderDataTable({

    
    data <-  data_antibiogram_source()
    
    dat <-  DT::datatable(data = data, 
                          options = list(fixedColumns = list(leftColumns = 1),
                                         dom = 'tB'))
    
    return(dat)
    
  })
  
  output$antibiogram_table_onset <- DT::renderDataTable({
    
    data <-  data_antibiogram_onset()
    dat <-  DT::datatable(data = data, 
                          options = list(fixedColumns = list(leftColumns = 1),
                                         dom = 'tB'))
    return(dat)
  })
  

  output$antibiogram_table_region <- DT::renderDataTable({
    # req(input$source_table, input$isolatetype_table, input$onset_table,  input$region_table, input$year_table)
    
    # input$load_table
    # isolate(data <-  data_antibiogram() %>%
    #   filter(year == input$year_table))
    
    data <-  data_antibiogram_region() 
    
    dat <-  DT::datatable(data = data, 
                          options = list(fixedColumns = list(leftColumns = 1),
                                         dom = 'tB'))
    
    return(dat)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  output$antibiogram_text <- renderText({
    
    # add an if statement for saying community or hospital onset location in the title
    if(input$onset_table == "Overall") {
      paste("Percentage susceptible in ", input$region_table, "in", input$year_table)
    } else if(input$onset_table == "Community") {
      paste("Percentage susceptible in the", input$region_table, "community in", input$year_table)
    } else if(input$onset_table == "Hospital") {
      paste("Percentage susceptible in the", input$region_table, "hospitals in", input$year_table)
    }
    
    #paste("Percentage susceptible in the ", input$region_table, "for", input$year_table)
  })
  
  
  data_to_download <- reactive({
    data <- Data_yearly()
    # filter by source
    if(input$source_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(source == input$source_filt)
    }
    
    # filter by onset
    if(input$onset_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(onset == input$onset_filt)
    }
    
    #filter by isolate type
    if(input$isolatetype_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(sample_type == input$isolatetype_filt)
    }
    
    # filter by microbe
    if(input$microbe_name_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(organism == input$microbe_name_filt)
    }
    
    # filter by antibiotic
    if(input$antibiotic_name_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(antimicrobial == input$antibiotic_name_filt)
    }
    
    
    # filter by region
    if(input$region_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(region == input$region_filt)
    }
    
    
    # filter by year
    if(input$year_select_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(year == input$year_filt)
    }
    return(data)
  })
  
  
  
  
  ## Tables of data -----------------------
  
  output$table_data_year <- DT::renderDataTable({
    data <- data_to_download() %>%
      select(source, region, sample_type, onset, organism, antimicrobial, year, num_of_tests_yearly_overall,  percent_resistant_yearly_overall) 
    data$percent_resistant_yearly_overall  <- round(data$percent_resistant_yearly_overall , 2)
    
    data <- data %>%
      rename("number of tests" = num_of_tests_yearly_overall,
             "percentage resistance" = percent_resistant_yearly_overall)
    
    dat <- DT::datatable(data, rownames = FALSE, #colnames =  c("Region", "Sample type", "Onset location", "Organism", "Antimicrobial", "Year", "Number of tests", "Percent resistant"),
                         class = 'compact stripe hover',
                         options = list(headerCallback = JS(headerCallback2)))
    return(dat)
    
  })
  
  
  output$table_data_month <- DT::renderDataTable({
    data <- data_to_download() %>%
      select(region, sample_type, onset, organism, antimicrobial, month_year, num_of_tests_monthly_raw, percent_resistant_monthly_raw)
    data$percent_resistant_monthly_raw  <- round(data$percent_resistant_monthly_raw , 2)
    
    data <- data  %>%
      rename("number of tests" = num_of_tests_monthly_raw,
             "percentage resistance" = percent_resistant_monthly_raw)
    dat <- DT::datatable(data, rownames = FALSE, #colnames =  c("Region", "Sample type", "Onset location", "Organism", "Antimicrobial", "Date", "Number of tests", "Percent resistant"),
                         class = 'compact stripe hover',
                         options = list(headerCallback = JS(headerCallback2)))
    
    return(dat)
    
  })
  
  
  
  output$downloadData_yearly <- downloadHandler(
      filename = function() {
        paste('HOTspots_AMRdata-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(Data_yearly(), con)
      }
    )

  output$downloadData_yearly_subset <- downloadHandler(
    filename = function() {
      paste('HOTspots_AMRdata-subset-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data_to_download(), con)
    }
  )
  
  
  
  
  
  
  
  
  
  ## Header logo
  # image2 sends pre-rendered images
  output$hotspots_logo <- renderImage({
      return(list(
        src = "www/HOTspots_logo.png",
        filetype = "image/png",
        alt = "The HOTspots logo"
      ))
    }, deleteFile = FALSE)

  
  ## Header of summary stats -------------------------------------------
  # output$summary_stats <- renderPlot({
  #   data <- Data_yearly()
  #   
  #   data_num_isolates  <- data %>% 
  #     filter(onset == "Overall",
  #            sample_type == "All")
  #     
  #   hotspot_yearly_data
  #   sum(data$num_of_tests_yearly_overall)
  #   
  #   ## Summary stats
  #   df <- data.frame(
  #     x = seq(0, 65, 13), # where the boxes start horizontally
  #     y = rep(2, 6), # where the boxes start vertically
  #     w = rep(12,6),# the width of the boxes - note the first is wider
  #     
  #     # the number value to display
  #     value = c("1",
  #               length(unique(data$organism)),
  #               length(unique(data$antimicrobial)),
  #               (format(sum(data_num_isolates$num_of_tests_yearly_overall), big.mark=",")),
  #               length(unique(data$year)),
  #               length(unique(data$region))),
  #     
  #     # the description to go with the numbers
  #     sum_stats = c("Logo placeholder",
  #                   "organisms",
  #                   "antibacterials",
  #                   "susceptibility tests",
  #                   "years of data",
  #                   "regions"),
  #     
  #     # the icons
  #     icon = fontawesome(c("fa-rss",
  #                          "fa-certificate",
  #                          "fa-crosshairs",
  #                          "fa-eyedropper",
  #                          "fa-clock-o",
  #                          "fa-map-marker"))
  #   )
  #   
  #   
  #   gg_stats <- ggplot( df, aes(x, y, height = 4, width = w, label = sum_stats)) +
  #     geom_tile( fill = "white", colour = "#2B3E51", size =2) +
  #     geom_text(aes(label = value, x = x - rep(5,6), y = y + 0.7), hjust = 0, color = "#2B3E51",  size =8) +
  #     geom_text(aes(label = sum_stats, x = x - rep(5,6), y = y - 0.7), hjust = 0, color ="#2B3E51", fontface = "bold", size = 5) +
  #     geom_text(size = 18, aes(label = icon, family = "fontawesome-webfont", x = x + 4, y = y + 0.5), alpha = 0.20) +
  #     coord_fixed() + 
  #     theme_void() +
  #     guides(fill = F)
  #   
  #   return(gg_stats)
  # })
  

  
  
  
  
  
  ## Header of summary stats -------------------------------------------
  output$VBox_organism <- renderValueBox({
    valueBox(
      tags$p(paste0(length(unique(hotspot_yearly_data$organism))),style = "font-size: 80%;"),
      "organisms", 
      icon= tags$i(icon("virus"), style="font-size: 60px"),
      color = "light-blue"
    )
  })
  
  output$VBox_antibiotic <- renderValueBox({
    valueBox(
      tags$p(paste0(length(unique(hotspot_yearly_data$antimicrobial))),style = "font-size: 80%;"),
      "antibiotics", 
      icon= tags$i(icon("pills"), style="font-size: 60px"),
      color = "light-blue"
    )
  })
  
  output$VBox_regions <- renderValueBox({
    valueBox(
      tags$p(paste0(length(unique(hotspot_yearly_data$region))),style = "font-size: 80%;"),
      "regions", 
      icon= tags$i(icon("globe-asia"), style="font-size: 60px"),
      color = "light-blue"
    )
  })
  
  output$VBox_year<- renderValueBox({
    valueBox(
      tags$p(paste0(length(unique(hotspot_yearly_data$year))),style = "font-size: 80%;"),
      "years of data", 
      icon= tags$i(icon("calendar-alt"), style="font-size: 60px"),
      color = "light-blue"
    )
  })
  
  output$VBox_tests <- renderValueBox({
    data_num_isolates  <- hotspot_yearly_data %>% 
      filter(onset == "Overall",
             sample_type == "All") 
    
    valueBox(
      tags$p(format(sum(data_num_isolates$num_of_tests_yearly_overall), big.mark=","), style = "font-size: 80%;"),
      "susceptibility tests", 
      icon= tags$i(icon("vial"), style="font-size: 60px"),
      color = "light-blue"
    )
  })
  
} # close server function







###############
## Shiny App ##
###############
shinyApp(server=server, ui=ui)





