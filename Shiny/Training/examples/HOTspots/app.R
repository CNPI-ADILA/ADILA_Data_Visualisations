###***********************************###
### Tracking Antimicrobial Resistance ###
###***********************************###
# Project aim: Create a shiny app to map antimicrobial resistance 
#
# Author: Alys Young
#
# Collaborators: Saras Windecker and Nick Golding
#
#
# Script aim: The shiny app





##***********##
## 1. Set up ## ------------------------------------------------------------------------------------------------------------------------------
##***********##

# clear the environment
rm(list = ls())


##**********************##
## 1.1 Loading packages ## ------------------------------------------------------------------------------------------------------------------------------
##**********************##

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
if (!require(DT)) install.packages('DT')

library(shiny) # for the shiny app
library(shinythemes) # for the shiny app
library(ggplot2) # to make plots
library(dplyr) # to clean up code and allow piping %>%
library(leaflet) # for interactive maps
library(rgdal) # to open shapefiles of areas to map
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(tidyr)

# try without these, I think I got rid of the code 
library(reshape2) #

library(rsconnect)
#deployApp()

##*************##
## 1.2 Options ## ------------------------------------------------------------------------------------------------------------------------------
##*************##

# Contact email
contact_email <- "HOTspots@menzies.edu.au" 

# Date the data was last updated
date_updated <- format(as.Date(substr(file.info("www/data/HOTspots_monthly.csv")$mtime, 0,10)), "%d-%b-%Y")
year_updated <- substr(file.info("www/data/HOTspots_monthly.csv")$mtime, 0,4)

date_retrieved <-  format(Sys.time(), '%d %B, %Y')

# Date the methods were updated
date_method_update <- "13-March-2021"

# Citation
how_to_cite <- paste0('How to cite: Menzies School of Health Research. ', year_updated, '. HOTspots [computer software]. Darwin, Northern Territory. Retrieved from [web address] on ', date_retrieved, '.')


# Other options of things that might be changed
# Social media message
# Team members


##**********##
## 1.3 Data ## ------------------------------------------------------------------------------------------------------------------------------
##**********##

#consider a timer or a sceduler that will run the datamanipulation script every time the data is updated
# source("myScript.r", echo = TRUE)

# If the data changes:
# Need to add more colours to the 


# Resistance data ---------------------------------------------------------

hotspot_monthly_data    <- read.csv("www/data/HOTspots_monthly.csv")
hotspot_yearly_data     <- read.csv("www/data/hotspot_yearly_data.csv")
hotspot_yearly_split    <- read.csv("www/data/hotspot_yearly_split.csv")
hotspot_yearly_splitage <- read.csv("www/data/hotspot_yearly_splitage.csv")
hotspot_yearly_splitsex <- read.csv("www/data/hotspot_yearly_splitsex.csv")


hotspot_monthly_data    <- read.csv("~/Dropbox/HOTspots _data/HOTspots_monthly.csv")
hotspot_yearly_data     <- read.csv("~/Dropbox/HOTspots _data/HOTspots_yearly.csv")
#hotspot_yearly_split    <- read.csv("~/Dropbox/HOTspots _data/HOTspots_yearly_age&sex.csv")
hotspot_yearly_splitage <- read.csv("~/Dropbox/HOTspots _data/HOTspots_yearly_age.csv")
hotspot_yearly_splitsex <- read.csv("~/Dropbox/HOTspots _data/HOTspots_yearly_sex.csv")

hotspot_monthly_data$date_dmy <- as.Date(paste("01", hotspot_monthly_data$month_year), format = "%d %b %y")


# Regions -----------------------------------------------

## Shapefile
SA3 <- rgdal::readOGR("www/data/Australian_regions/Aus_regions.shp")
SA3_data <- SA3[SA3$SA3_NAME16 %in% hotspot_yearly_data$region,] # select the SA3 for which we have data for currently
rm(SA3)


## Cities
# To be displayed on the map 
cities_names <- read.csv("www/data/Australian_regions/Cities.csv")








# Code modifications ------------------------------------------------------

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




##********************##
## 1.4 Colour palette ## ------------------------------------------------------------------------------------------------------------------------------
##********************##

hotspot_palette <- list(
  
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
    `almost black`  = "#410000"),
  
  ## For heat map - colour-blind friendly
  `heat_CBfriendly` = c(
    `teal`          = "#57C4AD",
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
  `regions` = c( ## QLD
    'Cairns and Hinterland' = "#CC6677", # rose
    'Mackay'                = "#88CCEE", #cyan 
    'North West'            = "#44AA99", # teal
    'Torres and Cape'       = "#117733", # green
    'Townsville'            = "#DDCC77", # sand
    #'Outback - South' = "#332288", # indigo
    #'Central Highlands (Qld)' = "#999933", # olive
    #'Rockhampton' = "#882255", # wine
    #'Biloela' = "#AA4499", # purple
    
    ## NT
    # colours lightened
    'Alice Springs'         = "#FF8096", # rose
    'Darwin'                = "#CCEEFF", # cyan
    'Gove'                  = "#92EFD3", # teal
    'Katherine'             = "#CCDDAA", # green
    'Tennant Creek'         = "#FFE57E", # sand FFEC8C
    
    ## WA 
    # colours darkened
    'Kimberley'             = "#78343F", # rose
    'Mid East'              = "#225555", # cyan 
    'Mid West'              = "#36877A", # teal
    'Perth'                 = "#225522", # green
    'Pilbara'               = "#A59858", # sand
    'South'                 = "#222255" # indigo
    
    ## Other colours availabe to use
    
    #'region name' = "#8183E6", # indigo
    #'region name' = "#C5C86E", # olive
    #'region name' = "#", # wine # too similar to rose
    #'region name' = "#F488EE", # purple
    
    #'region name' = "#2F345B", # indigo
    #'region name' = "#7B7B29", # olive
    #'region name' = "#781D4A", # wine
    #'region name' = "#803273", # purple
    
  ),
  
  
  ## For the onset locations
  `onset` = c('Overall'   = "#332288" , # indigo
              'Hospital'  = "#88CCEE", # cyan
              'Community' = "#44AA99"), # teal
  
  ## For the sample types
  `sample` = c('All'                = "#332288" , # indigo
               'Blood'              = "#88CCEE", # cyan
               'Other'              = "#44AA99", # teal
               'Respiratory'        = "#117733", # green
               'Skin & soft tissue' = "#A0515E", # rose
               'Urine'              = "#999933"), #olive
  
  ## For the age brackets
  `age` = c('0-5'   = "#332288" , # indigo
            '6-15'  = "#88CCEE", # cyan
            '16-25' = "#44AA99", # teal
            '26-40' = "#117733", # green
            '41-60' = "#A0515E", # rose
            '61-80' = "#DDCC77", # sand
            '81+'   = "#999933", # olive
            ' '     = "#803273" ),  # purple - not needed but incase
  
  # For the sexes
  `sex` = c( 'male'    = "#44AA99", # teal
             'female'  = "#332288", # indigo
             'overall' = "#999933") # olive
)



# Change heat colour palette into a gradient
pal_num <- colorNumeric(hotspot_palette$heat, domain = 0:100)
pal_num_CBfriendly <- colorNumeric(hotspot_palette$heat_CBfriendly, domain = 0:100)


## To see the colour palletes:
# Uncomment the lines below and run them, changing $heat to the $name of the colour palette you want to see
# par(mar=c(0,0,0,0))
# pie(rep(1, length(hotspot_palette$heat)), col = hotspot_palette$heat)
# 
# pie(rep(1, 100), col = pal_num_CBfriendly(1:100))



# Check all the data categories have a corresponding colour in the palettes
if(length(unique(hotspot_monthly_data$region))  != length(hotspot_palette$regions)) print("Check all the regions in the data have a colour in the regions colour palette")
if(length(unique(hotspot_monthly_data$onset))  != length(hotspot_palette$onset)) print("Check all the onser locatoins in the data have a colour in the onset colour palette")
if(length(unique(hotspot_monthly_data$sample_type))  != length(hotspot_palette$sample)) print("Check all the sample types in the data have a colour in the sample colour palette")
if(length(unique(hotspot_yearly_splitage$age))  != length(hotspot_palette$age)) print("Check all the age brackets in the data have a colour in the age colour palette")




# save.image(file = "www/data/app_setup_Jan31.RData")
# load("www/data/app_setup_Jan31.RData")


source('Options.R')





















##*******************##
## 2. User Interface ## -------------------------------------------------------------------------------------------------------------------------------
##*******************##

ui <- fluidPage(
  
  useShinydashboard(),
  useShinyjs(),
  #options(shiny.sanitize.errors = TRUE), # use this to change error messages to something generic
  tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
  tags$head(tags$style(".shiny-output-error:after{content: 'An error has occurred. Please ensure all inputs are selected or try different inputs. Contact the website administrators if it persists.'; visibility: visible}")),
  
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
  
  #.navbar {font-size: 16px;}
  
  ## Nav Bar ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  navbarPage(title ="", # title for the title bar, could add the logo or symbols
             id="nav", selected = NULL, collapsible = TRUE, 
             theme = shinytheme("flatly"),
             #tags$style(type='text/css', '.navbar {font-size: 13px;}'),
             
             
             ## Tab 1 - Map --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
             # landing page 
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
                        
                        # Sidebar ---------------------------------------------------
                        sidebarPanel(
                          
                          
                          
                          # Select the location where the infection was identified
                          radioButtons(inputId = "onset",
                                       label = "Select healthcare setting:",
                                       selected = character(0),
                                       choices = rev(unique(hotspot_yearly_data$onset))), # change this when animal data is added rev(unique(hotspot_yearly_data$onset))
                          
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.onset == null", 
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
                          
                          # sliderInput(inputId = "year",
                          #             label = "Select a single year:",
                          #             value = max(hotspot_yearly_data$year), # default value
                          #             min = min(hotspot_yearly_data$year),
                          #             max = max(hotspot_yearly_data$year),
                          #             step = 1,
                          #             sep = "",
                          #             round = TRUE,
                          #             ticks = FALSE),
                          
                          sliderInput(inputId = "range_year",
                                      label = "Select a time period:",
                                      value = c(max(hotspot_yearly_data$year) - 5, max(hotspot_yearly_data$year)), # default value
                                      min = min(hotspot_yearly_data$year),
                                      max = max(hotspot_yearly_data$year),
                                      step = 1,
                                      sep = "",
                                      round = TRUE,
                                      ticks = FALSE),
                          br(),
                          
                          
                          # A check box to change to colour blind friendly
                          checkboxInput("load_CB_friendly_map", "Colour-blind friendly palette", FALSE),
                          
                          # A check box to change to colour blind friendly
                          checkboxInput("load_show_cities", "Show cities", FALSE),
                          
                          
                          ## Optional update ##
                          # Nicer way than checkboxes, but couldnt make them be inline with text nicely 
                          # use instead of the checkboxes above
                          # switchInput( inputId = "load_CB_friendly_map",
                          #              value = FALSE,
                          #              inline = TRUE,
                          #              size = "mini"),
                          # switchInput( inputId = "load_show_cities",
                          #              value = FALSE,
                          #              inline = TRUE,
                          #              size = "mini"),
                          # 
                          
                          
                          
                          # A button to load the map
                          actionButton(inputId  = "load_map", label = "Load map"),
                          
                          
                          hidden(p(id="please_wait", "Please wait while the map loads.")),
                          br(),
                          p(paste0("Data last updated ", date_updated)),
                          
                          downloadButton("report", "Generate report")
                          
                        ), # close side panel
                        
                        
                        
                        
                        # Main panel ---------------------------------------------------
                        mainPanel(
                          #div(h3(textOutput("map_title"), style= "margin-top: 0; margin-bottom: 0;")),
                          h4(uiOutput("map_title")),
                          #br(),
                          #{margin-top: 0; margin-bottom: 0;}
                          #h3(textOutput("map_title")),
                          leafletOutput("leaflet_map", height=700), # plot the leaflet map
                          p(how_to_cite)
                          # tags$div(id="Citation", 'How to cite: Menzies School of Health Research. HOTspots [computer software]. Darwin, Northern Territory. Retrieved from [inset web address when done]') # TO DO ## update this
                        ) # close main panel
                      ) # close sidebar layout
             ), # close tabPanel
             
             
             
             
             
             
             
             
             
             
             
             
             ## Tab 2 - Plots -----------------------------------------------------------------------
             
             tabPanel(("Plots"), # name in nav bar
                      icon = icon("chart-bar"), # icon in nav bar
                      #titlePanel(tags$h4("Tracking antimicrobial resistance")), # title on the page
                      #tags$br(),    
                      
                      sidebarLayout(
                        
                        # Side panel for displaying inputs ---------------------------------------------------
                        sidebarPanel(
                          
                          # Select the location where the infection was identified
                          radioButtons(inputId = "onset_spec",
                                       label = "Select healthcare setting:",
                                       selected = character(0),
                                       choices = rev(unique(hotspot_yearly_data$onset))), # change this when animal data is added rev(unique(hotspot_yearly_data$onset))
                          
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.onset_spec == null", 
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
                                               plotOutput("plot_compare_reg") %>% withSpinner(type = 5)
                                      ),
                                      
                                      
                                      
                                      tabPanel(("By antibiotic"),
                                               value="antimicrobe",
                                               h4(uiOutput("text_compare_anti")),
                                               #h4(textOutput("text_compare_anti")),
                                               plotOutput("plot_compare_anti") %>% withSpinner(type = 5),
                                               
                                               br(),
                                               br(),
                                               
                                               h4(uiOutput("text_compare_anti2")),
                                               #h4(textOutput("text_compare_anti2")),
                                               plotOutput("plot_compare_anti2") %>% withSpinner(type = 5),
                                               br(),
                                               br()
                                               #h4(textOutput("text_compare_anti3")),
                                               #plotOutput("plot_compare_anti3") %>% withSpinner(type = 5)
                                      ),
                                      
                                      
                                      
                                      tabPanel(("By sample"),
                                               value="sample type",
                                               br(),
                                               h2("Currently being updated"),
                                               br(),
                                               h4("Comparison of the sample types"),  # change this text to uioutput # UPDATE TO DO 
                                               plotOutput("plot_compare_sample") %>% withSpinner(type = 5),
                                               br(),
                                               br(),
                                               plotOutput("plot_compare_sample_tests") %>% withSpinner(type = 5),
                                               br(),
                                               br(),
                                               h4("Onset locations"), # change this text to uioutput # UPDATE TO DO 
                                               plotOutput("plot_compare_onset") %>% withSpinner(type = 5),
                                               br(),
                                               br(),
                                               plotOutput("plot_compare_onset_tests") %>% withSpinner(type = 5),
                                               br(),
                                               br(),
                                      ),
                                      
                                      tabPanel(("By age"), # name of tab
                                               value = "age",
                                               
                                               h4(uiOutput("text_age")),
                                               #h4(textOutput("text_age")),
                                               
                                               plotOutput("plot_compare_age", height=500) %>% withSpinner(type =5)),
                                      
                                      tabPanel(("By sex"), # name of tab
                                               value = "sex",
                                               
                                               h4(uiOutput("text_sex")),
                                               
                                               plotOutput("plot_compare_sex", height=500) %>% withSpinner(type = 5)),
                                      
                                      
                                      tabPanel(("By month"), # name of tab
                                               value = "monthly",
                                               
                                               h4(uiOutput("text_spec")),
                                               plotOutput("plot_spec") %>% withSpinner(type = 5),
                                               br(),
                                               
                                               h4(uiOutput("text_spec2")),
                                               plotOutput("plot_spec2") %>% withSpinner(type = 5),
                                               uiOutput("text_monthly_low_isolates")
                                               
                                      ) # close Tabpanel
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
                          
                          # Select the location where the infection was identified
                          radioButtons(inputId = "onset_table",
                                       label = "Select healthcare setting:",
                                       selected = character(0),
                                       choices = rev(unique(hotspot_yearly_data$onset))),
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.onset_table == null", 
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
                                      selected = "Darwin",
                                      multiple = FALSE),
                          
                          # A slider to select the year
                          #tags$style(type = "text/css", ".irs-bar { background: none; border-top: none; border-bottom: none;}
                          #           .irs-bar-edge {background: none; border: none;}"), # setting the blue bar to be nothing on the slider
                          
                          # sliderInput(inputId = "year_table",
                          #             label = "Select a year:",
                          #             value = max(hotspot_yearly_data$year), # default value
                          #             min = min(hotspot_yearly_data$year),
                          #             max = max(hotspot_yearly_data$year),
                          #             step = 1,
                          #             sep = "",
                          #             round = TRUE,
                          #             ticks = FALSE),
                          
                          sliderInput(inputId = "range_table",
                                      label = "Select a time period:",
                                      value = c(max(hotspot_yearly_data$year) - 5, max(hotspot_yearly_data$year)), # default value
                                      min = min(hotspot_yearly_data$year),
                                      max = max(hotspot_yearly_data$year),
                                      step = 1,
                                      sep = "",
                                      round = TRUE,
                                      ticks = FALSE)
                          
                          
                        ), # close side panel
                        
                        
                        
                        # Main panel for displaying outputs ---------------------------------------------------
                        mainPanel(
                          h4(textOutput("antibiogram_text")),
                          
                          DT::dataTableOutput("antibiogram_table") %>% withSpinner(type = 5),
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
                      p("For more information, please see the", tags$a(href="https://www.menzies.edu.au/page/Policies/", "Menzies policy website"), ", including the", tags$a(href="https://www.menzies.edu.au/icms_docs/307159_Information_and_Privacy_Policy_-_2019.pdf", "Information and Privacy policy")),
                      
                      
                      h3("Methodology"),
                      p("Antibiotic susceptibility data have been contributed by four main pathology service providers across three jurisdictions in northern Australia. These are Territory Pathology (Northern Territory), Pathology Queensland, Western Diagnostics (Western Australia and Northern Territory) and PathWest (Western Australia)."),
                      p("Between pathology providers there were variations in the content and format of the supplied data, requiring a process of data cleaning and standardisation. This is in part due to the variation in antimicrobial susceptibility testing (AST) guidelines used (Northern Territory and Western Australia use CLSI while Queensland used CLSI to 30th  June 2012 and then moved to EUCAST), however it is also due to individual laboratory policies and processes and the availability of antimicrobial agents for testing. For example, Western Diagnostics, PathWest and Territory Pathology all use CLSI, which recommends agents that are important for routine testing against various organisms or organism groups (an antimicrobial panel), however other agents may be tested or reported selectively based on the institution's formulary or substituted with a more practical alternative where their activity is similar. Therefore, the number and type of antimicrobials tested against the same microbes varies between laboratories. The microbes reported also varied, however common pathogens were identified and these microbes are available to select from the dropdown menu."),
                      p("Data were harmonised across the three jurisdictions by standardising antimicrobial, microbe and sample type nomenclature. Regions within jurisdictions were based on classification by the Australian Bureau of Statistics, Statistical Area Level 3. The healthcare setting was determined by the type of facility at which the sample was collected. Duplicates were removed from the data by selecting the first isolate per person, per calendar year. The percentage of resistant isolates was calculated by dividing the number resistant by the total number of isolates tested. For years with <15 isolates collected and tested, these data (within the region of interest) were added to the following or previous year (or excluded if all 3 years had <15 isolates)."),
                      p("Territory Pathology provided minimum inhibitory concentrations, to which we applied the 2017 CLSI M100-S27 Performance Standards for AST (27th Edition). All other data were supplied as interpreted values: susceptible (including intermediate) and resistant. Data on age and sex was not available from Territory Pathology and PathWest, and PathWest data was only available by year."),
                      p(paste("These methods were last updates on", date_method_update)),
                      br(),
                      
                      h3("Explore the data"),
                      
                      # Sidebar layout with input and output definitions
                      sidebarLayout(
                        
                        # Sidebar panel for inputs on the left
                        sidebarPanel(
                          
                          # title
                          h4("Filter the data"), # bold
                          
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
             
             
             
             
             
             
             
             # ResImpact Shiny app  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
             
             
             tabPanel("Economic Burden", icon = icon("calculator"),
                      
                      h3("ResImpact"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("bug", "Organism:",
                                      c("ceftriaxone-resistant E. coli" = "ecoli",
                                        "ceftriaxone-resistant K. pneumoniae" = "Klebsiella",
                                        "ceftazidime-resistant P. aeruginosa" = "Pseudomonas",
                                        "MRSA" = "MRSA",
                                        "VRE" = "VRE"), selected='ecoli'),
                          
                          # BSI resistance
                          numericInput(inputId = "pDrugResBSI",
                                       label = "Probability BSI drug resistance:",
                                       min = 0,
                                       max = 1,
                                       step = 0.01,
                                       value=0.05),
                          
                          
                          # Respiratory resistance
                          conditionalPanel(
                            condition = "input.bug == 'Pseudomonas' | input.bug == 'MRSA'",
                            numericInput(inputId = "pDrugResResp",
                                         label = "Probability respiratory drug resistance:",
                                         min = 0,
                                         max = 1,
                                         step = 0.01,
                                         value=0.05)),
                          
                          # UTI resistance
                          conditionalPanel(
                            condition = "input.bug == 'VRE'",
                            numericInput(inputId = "pDrugResUTI",
                                         label = "Probability UTI drug resistance:",
                                         min = 0,
                                         max = 1,
                                         step = 0.01,
                                         value=0.05))
                        ), # close sidebar
                        
                        mainPanel(h4('Additional cost of treatment', tags$a(href="http://www.cre-rhai.org.au/projects/antibiotic-management-of-drug-resistant-infections-a-survey-of-clinical-practice", "(bloodstream infections only)")),
                                  textOutput(outputId = 'cost_text'),
                                  br(),
                                  h4('Total accounting cost of bed days and treatment'),
                                  textOutput(outputId = 'account_text'),
                                  br(),
                                  h4('Total opportunity cost of bed days and treatment'),
                                  textOutput(outputId = 'opp_text')
                        ) # close main panel
                      ),
                      br(),
                      
                      p(tags$a(href="https://aushsi.shinyapps.io/costresistantecoli/", "ResImpact"), ", is an open-access tool based on a validated and transparent model developed as part of the Health and Economic Modelling of Antimicrobial resistance in Australia (HEMAA) project.", tags$a(href="http://www.cre-rhai.org.au/projects/health-economic-modelling-of-antimicrobial-resistance-in-australia---hemaa", "Click here"), "to read about the HEMAA project."),
                      p("Created by Prof Adrian Barnett"),
                      br(),
                      br(),
                      
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
                                 p(paste("please email", contact_email))),
                        
                        
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
                                 p("ect..."),
                                 
                                 br(),
                                 h4("View guidelines:"),
                                 tags$div("The", tags$a(href="https://www.tg.org.au", "Therapeutic Guidelines website"), ", and the", tags$a(href="https://tgldcdp.tg.org.au/fulltext/tglcontent/quicklinks/GPSummary_v11.pdf", "pdf")),
                                 tags$div("The", tags$a(href="https://www.carpa.com.au", "Central Australia Rural Practioners Association (CARPA) website"), ", and the", tags$a(href="https://healthinfonet.ecu.edu.au/healthinfonet/getContent.php?linkid=592687&title=CARPA+standard+treatment+manual%3A+a+clinic+manual+for+primary+health+care+practitioners+in+remote+and+Indigenous+health+services+in+central+and+northern+Australia", "treatment manual pdf"))
                                 
                        ),     
                        
                        
                        ## About tab, Publications  ----------------------------------------------------------------
                        tabPanel(("Publications"), 
                                 titlePanel(h1("Relevant publications")),
                                 
                                 # To add more publications, use the format below
                                 #p(tags$a(href="link", "Name")),
                                 
                                 #p(tags$a(href="https://bmchealthservres.biomedcentral.com/articles/10.1186/s12913-017-2079-5", " Page, et al., 2017."), ". What is a hospital bed day worth? A contingent valuation study of hospital Chief Executive Officers. BMC Health Services Research 2017;17:137."),
                                 p(tags$a(href="https://www.nature.com/articles/s41598-020-69312-4", "TM Wozniak, W Cuningham, S Buchanan, et al., (2020)."), "Geospatial epidemiology of Staphylococcus aureus in a tropical setting: an enabling digital surveillance platform. Scientific Reports 10, 13169."),
                                 p(tags$a(href="https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciaa1228/5895480", "X Lee, A Stewardson, L Worth, N Graves, TM Wozniak (2020)."), "Attributable length of stay, mortality risk and costs of bacterial healthcare-associated infections in Australia: a retrospective case-cohort study."),
                                 p(tags$a(href="https://peerj.com/articles/9409/", "W Cuningham, et al., (2020)."), "Antimicrobial stewardship in remote primary healthcare across northern Australia. PeerJ, 8."),
                                 p(tags$a(href="https://onlinelibrary.wiley.com/doi/full/10.1111/1753-6405.12876", "W Cuningham, et al., (2019)."), "High burden of infectious disease and antibiotic use in early life in Australian Aboriginal communities."),
                                 p(tags$a(href="https://www.cambridge.org/core/journals/infection-control-and-hospital-epidemiology/article/abs/health-and-economic-burden-of-antimicrobialresistant-infections-in-australian-hospitals-a-populationbased-model/A51CA4B0F6181C891F0B406823460C30", "TM Wozniak, E Bailey, N Graves (2019)."), "Health and economic burden of antimicrobial-resistant infections in Australian hospitals: a population-based model. Infection Control & Hospital Epi 40(3)320-7."),
                                 p(tags$a(href="https://aricjournal.biomedcentral.com/articles/10.1186/s13756-019-0472-z", "TM Wozniak, L Barnsbee, X Lee, R Pacella (2019)."), "Using the best available data to estimate the cost of antimicrobial resistance: a systematic review. Antimicrobial Resistance and Infection Control 8:26."),
                                 p(tags$a(href="https://aricjournal.biomedcentral.com/articles/10.1186/s13756-018-0379-0", "TM Wozniak (2018)."), "Estimating the burden of antimicrobial resistance. Antimicrobial Resistance and Infection Control 7: 91."),
                                 p(tags$a(href="https://www.sciencedirect.com/science/article/pii/S2468045117302286", "TM Wozniak (2018)."), "Clinical management of drug-resistant bacteria in Australian hospitals: an online survey of doctors' opinions. Infection, Disease & Health 23(1); 41-48."),
                                 p(tags$a(href="https://pubmed.ncbi.nlm.nih.gov/30479305/", "TM Wozniak, N Graves, A Barnett (2018)."), "How much do superbugs cost? An evidence-based open- access tool. Infection, Disease & Health 23 (1); 54-56."),
                                 p(tags$a(href="https://idhjournal.com/article/S2468-0451(17)30067-6/fulltext", "10.	TM Wozniak, D Paterson, K Halton (2017)."), "Review of the epidemiological data regarding antimicrobial resistance in gram(-) bacteria in Australia. Infection, Disease & Health 22(4); 210-218."),
                                 p(tags$a(href="https://www.idhjournal.com.au/article/S2468-0451(16)30192-4/fulltext", "11.	J Cameron, L Hall, TM Wozniak, K Halton (2016)."), "The burden of community onset MRSA in Australia Infection, Disease & Health. 21 (3). 140.")
                        ),
                        
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
                      p(paste0("Email:", contact_email)),
                      
             ),
             
             
             
             ## Eight tab, Share  ----------------------------------------------------------------
             
             navbarMenu(("Share"), icon = icon("share-alt"), # consider adding a hashtag
                        
                        # Twitter
                        tabPanel(tags$a(href = 'https://twitter.com/intent/tweet?url=http%3A%2F%2Famrhotspots.com.au%2Faci%2Fadminpanel%2Fmanage%2Flogin &text=Check%20out%20this%20tool%20to%20track%20antibiotic%20resistance', icon("twitter"), "Twitter" )),
                        
                        # LinkedIn 
                        tabPanel(tags$a(href = 'http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Famrhotspots.com.au%2Faci%2Fadminpanel%2Fmanage%2Flogin &title=Check%20out%20this%20tool%20to%20track%20antibiotic%20resistance', icon("linkedin"), "LinkedIn" )),
                        
                        # Facebook
                        tabPanel(tags$a(href = 'https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Famrhotspots.com.au%2Faci%2Fadminpanel%2Fmanage%2Flogin', icon("facebook"), "Facebook" ))
                        
                        
             )# close navbar2 
  )# close navbar
) # Close fluidpage















































##***********##
## 3. Server ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##***********##

server <- function(input,output, session){
  
  
  # Functionality on the landing page map
  output$CB_text <- renderText({ "Colour-blind friendly palette" })
  output$Show_cities_text <- renderText({ "Show cities" })
  
  observeEvent(input$load_map, { 
    disable("load_map")
    show("please_wait")
    
    delay(500, enable("load_map"))
    delay(500, hide("please_wait")) })
  
  
  
  
  ## Reactive values -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Saves computational time by only calculating the values once and then using them many times
  
  Data_yearly <- reactive({
    hotspot_yearly_data
  })
  
  ## For the main landing page -----------------
  
  # Filtered by the sample attributes
  hotspot_yearly_filter1 <- reactive({
    req( input$onset, input$isolatetype, input$microbe_name)
    
    data1 <- Data_yearly() %>%
      filter( onset == input$onset, sample_type == input$isolatetype, organism ==  input$microbe_name)
    data1
  })
  
  # # Filtered by the organism
  # hotspot_yearly_filter2 <- reactive({
  #   req(input$microbe_name,  input$isolatetype, input$onset)
  #   
  #   data1 <- hotspot_yearly_filter1() %>%
  #     filter(organism ==  input$microbe_name)
  #   data1
  # })
  # 
  
  # Filtered by the antimicrobial
  hotspot_yearly_filter2 <- reactive({
    req( input$onset, input$isolatetype, input$microbe_name, input$antibiotic_name)
    
    data1 <- hotspot_yearly_filter1() %>%
      filter(antimicrobial ==  input$antibiotic_name)
    data1
  })
  
  # Filtered by the year
  # hotspot_yearly_filter3 <- reactive({
  #   req(input$year, input$antibiotic_name, input$microbe_name,  input$isolatetype, input$onset)
  #   
  #   data1 <- hotspot_yearly_filter2() %>%
  #     filter(year ==  input$year)
  #   data1
  # })
  # 
  
  # Filtered by the year range
  hotspot_yearly_filter4 <- reactive({
    req(input$range_year, input$antibiotic_name, input$microbe_name,  input$isolatetype, input$onset)
    
    data1 <- hotspot_yearly_filter2() %>%
      filter(year %in%  seq(min(input$range_year), max(input$range_year)))
    data1
  })
  
  
  ## For the specific combinations ----------------- 
  
  
  
  ## Use this in the plots to compare regions, drugs and specific combos
  data_monthly_spec <- reactive({
    req( input$isolatetype_spec, input$onset_spec, input$microbe_name_spec , input$region_spec1, input$antibiotic_name_spec1)
    
    data1 <- hotspot_monthly_data %>% 
      filter(  sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec, region == input$region_spec1, antimicrobial == input$antibiotic_name_spec1)
    data1
  })
  
  
  ## yearly ##
  data_yearly_spec <- reactive({
    req(input$microbe_name_spec,  input$isolatetype_spec, input$onset_spec )
    yearly_data1 <- Data_yearly()%>%
      filter( sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec) 
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
  
  
  
  ## Use this in the plots to compare the sample type
  data_yearly_sampleonset <- reactive({
    req(input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1 )
    data1 <- Data_yearly()%>%
      filter(organism ==  input$microbe_name_spec, antimicrobial == input$antibiotic_name_spec1, region == input$region_spec1) # add another filter to be human/animal
    data1
  })
  data_yearly_compsample <- reactive({
    req(input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1, input$onset_spec )
    data1 <- data_yearly_sampleonset() %>%
      filter(onset ==  input$onset_spec)
    data1
  })
  
  data_yearly_componset <- reactive({
    req(input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1, input$isolatetype_spec )
    data1 <- data_yearly_sampleonset() %>%
      filter(sample_type ==  input$isolatetype_spec)
    data1
  })
  
  data_antibiogram <- reactive({
    
    
    req(  input$isolatetype_table, input$onset_table, input$region_table)
    
    
    data <-  Data_yearly() %>%
      filter( sample_type == input$isolatetype_table, onset == input$onset_table, region == input$region_table) %>%
      # select(organism, antimicrobial, percent_resistant_yearly_overall) %>%
      distinct
    
  })
  
  
  
  
  
  #### Update the UI inputs  -----------------------------------------------------------------
  
  
  ## For the landingpage map ##
  
  # Update the list of antimicrobials
  observe({
    req( input$isolatetype, input$onset, input$microbe_name)
    data <- hotspot_yearly_filter1() # filtered by everything expect the antibiotic name and  year
    
    updateSelectInput(session, inputId = "antibiotic_name",
                      label = "Select antimicrobial:",
                      selected = "Cefazolin", 
                      choices = sort(unique(data$antimicrobial)))
  })
  
  # Upate the years available
  observe({
    req( input$isolatetype, input$onset, input$microbe_name, input$antibiotic_name)
    data <- hotspot_yearly_filter2() # filtered by everything expect the antibiotic name and  year
    
    updateSliderInput(session, inputId = "year",
                      value = max(data$year),
                      min = min(data$year),
                      max = max(data$year))
  })
  
  # Upate the years available
  observe({
    req( input$isolatetype, input$onset, input$microbe_name, input$antibiotic_name)
    data <- hotspot_yearly_filter2() # filtered by everything expect the antibiotic name and  year
    
    updateSliderInput(session, inputId = "range_year",
                      value = c(max(data$year) - 5, max(data$year) ),
                      min = min(data$year),
                      max = max(data$year))
  })
  
  
  ## For the plotting page ##
  # Update the list of antimicrobials
  observe({ 
    req(input$isolatetype, input$onset, input$microbe_name)
    data <- hotspot_yearly_filter1() %>%
      filter( sample_type == input$isolatetype, onset == input$onset, organism ==  input$microbe_name)
    
    updateSelectInput(session, inputId = "antibiotic_name_spec1",
                      label = "Select antimicrobial:",
                      choices = sort(unique(data$antimicrobial)),
                      selected = NULL)
  })
  
  
  
  ## TO DO
  observe({ 
    req(input$antibiotic_name_spec1)
    
    data <- data_yearly_spec2()
    
    # start with this list of unique regions
    sort(unique(data$region))
    
    # maybe need another CSV file to switch the name
    # for each option unique region, find the corresponding region with clearer name
    
    # display the clearer name in the list 
    # but keep letting the value be the unique region name
    
    # end up like this;
    # choices = c("Cylinders" = "cyl",
    #             "Transmission" = "am",
    #             "Gears" = "gear")
    
    # to give it subheadings too use:
    
    # list(`East Coast` = list("NY", "NJ", "CT"),
    #      `West Coast` = list("WA", "OR", "CA"),
    #      `Midwest` = list("MN", "WI", "IA"))
    # 
    # # OR 
    # 
    # list(`State 1` = c("Region A" = "regA",
    #                    "Region B" = "regB",
    #                    "Region C" = "regC"),
    #      `State 2` = c("Region D" = "regD"),
    #      `State 3` = c("Region E" = "regE",
    #                    "Region F" = "regF"))
    
    
    # so find this overall category (jurisdiction)
    # get only the unique
    # paste the unique 
    
    updateSelectInput(session, inputId = "region_spec1",
                      label = "Select region:",
                      choices = sort(unique(data$region)),
                      selected = "Darwin")
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
    req(  input$isolatetype_table, input$onset_table, input$region_table)
    
    data <- data_antibiogram()
    
    updateSliderInput(session, inputId = "year_table",
                      label = "Select the year:",
                      value= max(data$year),
                      min = min(data$year),
                      max = max(data$year))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #### Outputs to display -----------------------------------------------------------------
  
  # Map title
  # written as a reactive expression
  map_title_RV <- eventReactive( input$load_map, {
    paste("Resistance of", em(input$microbe_name), "to", tolower(input$antibiotic_name), "in", input$year )
  })
  
  output$map_title <- renderUI(
    HTML(map_title_RV())
  )
  
  ## For the landing page map -----------------
  output$leaflet_map <- renderLeaflet({
    #data <- hotspot_yearly_filter3()
    
    # SA3_data %>% # using the shapefile data
    #   leaflet() %>% # create a leaflet map
    #   fitBounds(lng1 = summary(SA3_data)$bbox[1], lat1 = summary(SA3_data)$bbox[2], lng2 = summary(SA3_data)$bbox[3], lat2 = summary(SA3_data)$bbox[4]) %>% # the starting position of the map
    #   addTiles(options = providerTileOptions(minZoom = 2)) %>% # sets the furtherst that can be zoomed out
    #   addLegend("bottomright", pal = pal_num, values = ~c(0:100), 
    #             title = "% resistance", # legend title
    #             opacity = 1)
    
    
    if(input$load_show_cities == FALSE){
      
      leaflet() %>% # create a leaflet map
        fitBounds(lng1 = summary(SA3_data)$bbox[1], lat1 = summary(SA3_data)$bbox[2], lng2 = summary(SA3_data)$bbox[3], lat2 = summary(SA3_data)$bbox[4]) %>% # the starting position of the map
        addTiles(options = providerTileOptions(minZoom = 2))
      
    } else if (input$load_show_cities == TRUE){
      
      leaflet() %>% # create a leaflet map
        fitBounds(lng1 = summary(SA3_data)$bbox[1], lat1 = summary(SA3_data)$bbox[2], lng2 = summary(SA3_data)$bbox[3], lat2 = summary(SA3_data)$bbox[4]) %>% # the starting position of the map
        addTiles(options = providerTileOptions(minZoom = 2))  %>%
        addMarkers(data=cities_names, ~long, ~lat, popup = ~as.character(name), label = ~as.character(name))
      
    }
    
    # 
    # leaflet() %>% # create a leaflet map
    #   fitBounds(lng1 = summary(SA3_data)$bbox[1], lat1 = summary(SA3_data)$bbox[2], lng2 = summary(SA3_data)$bbox[3], lat2 = summary(SA3_data)$bbox[4]) %>% # the starting position of the map
    #   addTiles(options = providerTileOptions(minZoom = 2))  %>%
    #   addMarkers(data=cities_names, ~long, ~lat, popup = ~as.character(name), label = ~as.character(name))
    
    
  })
  
  
  # # If data is selected, then add the shapefiles
  # observeEvent(input$load_map, {
  #   
  #   #req(input$onset, input$isolatetype, input$microbe_name, input$antibiotic_name, input$year)
  #   
  #   if(input$load_CB_friendly_map == FALSE){
  #     col_palette <- pal_num
  #   } else if (input$load_CB_friendly_map == TRUE){
  #     col_palette <- pal_num_CBfriendly
  #   }
  #   
  #   data <- hotspot_yearly_filter3()
  #   merged_data <- merge(SA3_data, data, by.x="SA3_NAME16", by.y="region")
  #   
  #   
  #   
  #   
  #   
  #   leafletProxy("leaflet_map", data = merged_data ) %>%
  #     clearControls() %>%
  #     clearShapes() %>%
  #     addPolygons(  fillColor = ~col_palette(merged_data$percent_resistant_yearly_overall),
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
  #                     bringToFront = TRUE))  %>%
  #     addLegend("bottomright", pal = col_palette, values = ~c(0:100), # also add values as ~percent_resistant_yearly
  #               title = "% resistance",
  #               opacity = 1)
  #   
  # })
  
  
  
  # If data is selected, then add the shapefiles
  observeEvent(input$load_map, {
    
    if(input$load_CB_friendly_map == FALSE){
      col_palette <- pal_num
    } else if (input$load_CB_friendly_map == TRUE){
      col_palette <- pal_num_CBfriendly
    }
    
    data <- hotspot_yearly_filter4() %>%
      group_by(region) %>%
      mutate( per_res_overall = (sum(num_of_resistant_tests_yearly_overall) / sum(num_of_tests_yearly_overall))*100,
              num_test_overall = sum(num_of_tests_yearly_overall)) %>%
      select(-year, -percent_resistant_yearly_overall, -percent_susceptible_yearly_overall,  -num_of_tests_yearly_overall, -num_of_resistant_tests_yearly_overall, -num_of_susceptible_tests_yearly_overall  ) %>%
      unique()
    merged_data <- merge(SA3_data, data, by.x="SA3_NAME16", by.y="region")
    
    
    
    leafletProxy("leaflet_map", data = merged_data ) %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(  fillColor = ~col_palette(merged_data$per_res_overall),
                    fillOpacity = ifelse(is.na(merged_data$per_res_overall), 0, 0.7),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    popup = ifelse(is.na(merged_data$per_res_overall), # consider the option of adding a little 
                                   paste("No data available"),
                                   paste0('<strong>', merged_data$SA3_NAME16, '</strong>',
                                          '<br/>', '<strong>',"Resistance: ", '</strong>',  round(merged_data$per_res_overall,1), "%",
                                          '<br/>', '<strong>', "No. of isolates: ", '</strong>', format(merged_data$num_test_overall, big.mark = ",") )),
                    highlight = highlightOptions( weight = 5, 
                                                  color = "black", 
                                                  bringToFront = TRUE))  %>%
      addLegend("bottomright", pal = col_palette, values = ~c(0:100), # also add values as ~percent_resistant_yearly
                title = "% resistance",
                opacity = 1)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Main plots  -----------------------------------------------------------------
  # to use on the first page maybe under the heat map
  
  output$text_monthly <- renderText({
    paste("Measured at monthly intervals")
  })
  output$text_yearly <- renderText({
    paste("Measured at yearly invervals with >15 samples per month/year")
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ## Compare regions  -----------------------------------------------------------------
  
  output$text_compare_reg <- renderUI({
    req( input$onset_spec, input$isolatetype_spec, input$region_spec2)
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1)))
  })
  
  
  
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
  
  
  output$text_compare_anti <- renderUI({
    req( input$onset_spec, input$isolatetype_spec)
    HTML(paste("Resistance of", em(input$microbe_name_spec), "in", input$region_spec1, "in", input$year_spec))
  })
  
  
  
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
  
  
  output$text_compare_anti2 <- renderUI({
    req( input$onset_spec, input$isolatetype_spec)
    HTML(paste("Resistance of", em(input$microbe_name_spec), "in", input$region_spec1))
  })
  
  
  
  output$plot_compare_anti2 <- renderPlot( {
    #data <- data_yearly_spec()
    req(input$region_spec1, input$antibiotic_name_spec2)
    
    
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
  
  output$plot_compare_sample <- renderPlot({
    data <- data_yearly_compsample()
    
    data %>%
      ggplot(aes(x = year, y = percent_resistant_yearly_overall) ) +
      geom_line(aes(colour = sample_type)) + # position = dodge
      geom_point(aes(colour = sample_type)) +
      scale_colour_manual(values = hotspot_palette$sample) +
      labs(x="Year", y="Percentage resistance  (%)", fill="Sample type") + 
      theme_bw() + 
      theme(text = element_text(size=15))
    
  })
  
  # Number of tests
  output$plot_compare_sample_tests <- renderPlot({
    data <- data_yearly_compsample()
    
    data %>%
      ggplot(aes(x = year, y = num_of_tests_yearly_overall, fill = sample_type) ) +
      geom_bar(stat="identity", position=position_dodge(width=0.7, preserve = 'single'), width=0.7) +
      scale_fill_manual(values = hotspot_palette$sample) +
      labs(x="Year", y="Number of isolates", fill="Sample type") + 
      theme_bw() + 
      theme(text = element_text(size=15))
    
  })
  
  
  
  ## Compare onset -----------------------------------------------------------------
  
  output$plot_compare_onset <- renderPlot({
    data <- data_yearly_componset()
    
    data %>%
      ggplot(aes(x = year, y = percent_resistant_yearly_overall) ) +
      geom_line(aes(colour = onset)) + # position = dodge
      geom_point(aes(colour = onset)) +
      scale_colour_manual(values = hotspot_palette$onset) +
      labs(x="Year", y="Percentage resistance  (%)", fill="Healthcare setting") + 
      theme_bw() + 
      theme(text = element_text(size=15))
    
  })
  
  
  
  # Number of tests
  output$plot_compare_onset_tests <- renderPlot({
    data <- data_yearly_componset()
    
    data %>%
      ggplot(aes(x = year, y = num_of_tests_yearly_overall, fill = onset) ) +
      geom_bar(stat="identity", position=position_dodge(width=0.7, preserve = 'single'), width=0.7) +
      scale_fill_manual(values = hotspot_palette$onset) +
      labs(x="Year", y="Number of isolates", fill="Healthcare setting") + 
      theme_bw() + 
      theme(text = element_text(size=15))
    
  })
  
  
  
  
  ## Compare Age group  --------------------
  
  output$text_age <- renderUI({
    req( input$onset_spec, input$isolatetype_spec)
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1), "in", input$region_spec1, "by age groups"))
  })
  
  
  
  
  output$plot_compare_age <- renderPlot({
    
    data_age <- as.data.frame(hotspot_yearly_splitage) %>%
      filter( sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec) %>%
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
  #   req( input$onset_spec, input$isolatetype_spec)
  #   paste("Resistance of", input$microbe_name_spec, "to", input$antibiotic_name_spec1, "in", input$region_spec1, "for the two sexes of female and male")
  # })
  
  output$text_sex <- renderUI({
    req( input$onset_spec, input$isolatetype_spec)
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1), "in", input$region_spec1, "for the two sexes of female and male"))
  })
  
  
  output$plot_compare_sex <- renderPlot({
    
    data <-  data_yearly_spec2() %>%
      filter(region == input$region_spec1)
    
    data_sex <- hotspot_yearly_splitsex %>%
      filter( sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec) %>%
      filter(region == input$region_spec1, antimicrobial == input$antibiotic_name_spec1)
    
    ggplot() +
      #overall
      geom_path(data = data, aes(x = year, y=percent_resistant_yearly_overall), size = 2, colour="grey") + 
      geom_point(data = data, aes(x = year, y=percent_resistant_yearly_overall), size = 3, colour="grey") +
      # sex 
      geom_path(data = data_sex, aes(x = year, y=percent_resistant_yearly_overall, color = sex, linetype = sex), size=1.5) + 
      geom_point(data = data_sex, aes(x = year, y=percent_resistant_yearly_overall, color = sex, shape = sex), size=2) +
      scale_x_continuous(breaks=seq(min(data$year), max(data$year))) +
      scale_fill_manual( values = hotspot_palette$sex, breaks=c("F", "M"), labels=c("Female", "Male")) + #add here name = "sex"
      labs(x=NULL, y="Percentage resistance (%)", fill = "Sex") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size=15),
            legend.position = "top", 
            legend.spacing.x = unit(1.0, 'cm')) #text = element_text(size=15)
  })
  
  
  
  
  
  
  
  
  
  
  ## Specific plots ----------
  # Original
  output$text_spec <- renderUI({
    req(input$microbe_name_spec, input$region_spec1, input$antibiotic_name_spec1, input$isolatetype_spec , input$onset_spec)
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1) , "in the", input$region_spec1, "by month"))
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
      geom_bar(aes(y = num_of_tests_monthly_raw), stat = "identity", fill="lightgrey") +
      scale_x_date(date_breaks = "6 months",
                   date_minor_breaks = "1 month",
                   date_labels = "%b-%Y") + #6 months when text size  increase
      labs(x=NULL, y="Number of isolates") + # check if this should be colour or color
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size=15))
    
  })
  
  output$text_monthly_low_isolates <- renderUI({
    req(input$microbe_name_spec, input$region_spec1, input$antibiotic_name_spec1, input$isolatetype_spec , input$onset_spec)
    
    data <- data_monthly_spec()
    if(any(data$num_of_tests_monthly_raw < 15)){
      text <- "Please be aware that antimicrobial resistance is hard to interpret when the number of isolates is low as the number of isolates strongly impacts the resistance. In the plot being displayed, there are months with few isolates. Too few isolates can cause sharp spikes in the resistance which is not necessarily representative of the overall trend."
    } else {
      text <- ""
    }
    
  })
  
  
  
  
  
  
  ## Antibiogram -----------------------------------------------------------------
  ## Disable the search bar 
  ## Check the colour palette
  
  output$antibiogram_table <- DT::renderDataTable({
    
    req( input$isolatetype_table, input$onset_table,  input$region_table, input$year_table)
    
    data_antibiogram2 <- data_antibiogram() %>%
      filter(year %in% input$range_table) %>%
      group_by(organism, antimicrobial ) %>%
      mutate(suscept = round((sum(num_of_susceptible_tests_yearly_overall) / sum(num_of_tests_yearly_overall))*100, 1)) %>%
      select(organism, antimicrobial,  suscept) %>%
      unique()
    
    data <- data_antibiogram2 %>%
      tidyr::spread(organism, suscept) %>%
      as.data.frame() %>%
      `rownames<-`(.[,1]) %>%
      select(-antimicrobial)
    
    
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
    
    # Add option of displaying percentage in each cell
    # %>% formatPercentage(names(data), number of decimal places)
    
    return(dat)
    
    
  })
  
  
  output$antibiogram_text <- renderText({
    
    ## UPDATE TO DO
    if ( length(input$year_table) == 1){
      year_text <- paste("in", input$year_table)
    } else if (length(input$year_table) == 2){
      year_text <- paste("between", input$year_table[1], "and", input$year_table[2])
    }
    
    # add an if statement for saying community or hospital onset location in the title
    if(input$onset_table == "Overall") {
      paste("Percentage susceptible in ", input$region_table, year_text)
    } else if(input$onset_table == "Community") {
      paste("Percentage susceptible in the", input$region_table, "community", year_text)
    } else if(input$onset_table == "Hospital") {
      paste("Percentage susceptible in the", input$region_table, "hospitals", year_text)
    }
    
    #paste("Percentage susceptible in the ", input$region_table, "for", input$year_table)
  })
  
  
  ## Tables of data ---------------------------------------------------------------------------------------------------------------
  # To explore the data in the methods page
  
  ## yearly data to download
  data_to_download <- reactive({
    data <- Data_yearly()
    
    
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
  
  
  ## Table
  output$table_data_year <- DT::renderDataTable({
    data <- data_to_download() %>%
      select(region, sample_type, onset, organism, antimicrobial, year, num_of_tests_yearly_overall,  percent_resistant_yearly_overall) 
    data$percent_resistant_yearly_overall  <- round(data$percent_resistant_yearly_overall , 2)
    
    data <- data %>%
      rename("number of tests" = num_of_tests_yearly_overall,
             "percentage resistance" = percent_resistant_yearly_overall)
    
    dat <- DT::datatable(data, rownames = FALSE, #colnames =  c("Region", "Sample type", "Onset location", "Organism", "Antimicrobial", "Year", "Number of tests", "Percent resistant"),
                         class = 'compact stripe hover',
                         options = list(headerCallback = JS(headerCallback2)))
    return(dat)
    
  })
  
  
  
  ## Monthly data
  # to explore
  data_to_download_monthly <- reactive({
    data <- hotspot_monthly_data
    
    
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
  
  
  output$table_data_month <- DT::renderDataTable({
    data <- data_to_download_monthly() %>%
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
  
  
  ## Download -----------------------------------------------------------------------------------------------------------------------
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
  
  
  
  ## Rmarkdown reports
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        onset = input$onset,
        isolatetype = input$isolatetype,
        microbe_name = input$microbe_name,
        antibiotic_name = input$antibiotic_name,
        range_year_min = min(input$range_year),
        range_year_max = max(input$range_year),
        load_CB_friendly_map = input$load_CB_friendly_map,
        load_show_cities = input$load_show_cities
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  
  
  
  
  ## Headers -------------------------------------------
  
  ## Summary statistics
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
  
  
  ## Logo 
  # image2 sends pre-rendered images
  output$hotspots_logo <- renderImage({
    return(list(
      src = "www/HOTspots_logo.png",
      filetype = "image/png",
      alt = "The HOTspots logo"
    ))
  }, deleteFile = FALSE)
  
  
  
  
  ##*****************##
  ## 1.1 ResImpact shiny app ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ##*****************##
  
  
  source('ResImpact_app/simulate.R')
  
  # reactive function
  results = reactive({
    res = simulate(inbug=input$bug, inresUTI=input$pDrugResUTI,
                   inresResp=input$pDrugResResp, inresBSI=input$pDrugResBSI)
    res
  })
  
  output$cost_text <- renderText({
    meanc = round(mean(results()$cTreatment))
    ci = round(quantile(results()$cTreatment, probs=c(0.025, 0.975)))
    paste('Using a treatment cost of $',results()$tCost, ' per infection. Mean cost in 2017 = $', format(meanc, big.mark = ','), 
          ' per year, 95% confidence interval = $', format(ci[1], big.mark = ','), 
          ' to $', format(ci[2], big.mark = ','), '.', sep='')
  })
  
  output$account_text <- renderText({
    meanc = round(mean(results()$cBedAccount))
    ci = round(quantile(results()$cBedAccount, probs=c(0.025, 0.975)))
    paste('Using a bed day cost of $', results()$cAccount, ' per day obtained from the Australian Independent Hospital Pricing Authority. Mean cost in 2014 = $', format(meanc, big.mark = ','), 
          ' per year, 95% confidence interval = $', format(ci[1], big.mark = ','), 
          ' to $', format(ci[2], big.mark = ','), '.', sep='')
  })
  
  output$opp_text <- renderText({
    meanc = round(mean(results()$cBedOppCost))
    ci = round(quantile(results()$cBedOppCost, probs=c(0.025, 0.975)))
    paste('Using a bed day cost of $', results()$cOppCost, ' per day calculated as the opportunity cost of a bed-day obtained by contingent valuation (Page et al., 2017). Mean cost = $', format(meanc, big.mark = ','), 
          ' per year, 95% confidence interval = $', format(ci[1], big.mark = ','), 
          ' to $', format(ci[2], big.mark = ','), '.', sep='')
  })
  
} # close server function







##**************##
## 4. Shiny App ## ---------------------------------------------------------------------------------------------------
##**************##
shinyApp(server=server, ui=ui)


