## A password protected app
  # Which display is based on which user is logged in 

# 1. Load libraries
library(shiny)
library(shinymanager)
library(dplyr)



hotspot_yearly_data     <- read.csv("www/data/hotspot_yearly_data.csv")


# 2. Set the log in information
credentials <- data.frame(
  user = c("FNQ", "NT", "WA", "Admin"), # mandatory
  password = c("Queensland", "Northern", "Western", "p_admin"), # mandatory
  admin = c(FALSE, FALSE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

# 3. User Interface
ui <- fluidPage(
  
  # Application title
  titlePanel("HOTspots Data subsetted by user"),
  h4("Password protected to only show the region that was used to log in"),
  p("This could be used to protect the animal data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textOutput("text")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("table")
    )
  )
)

# Wrap your UI with secure_app
ui <- secure_app(ui)




# 4. Server
server <- function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  
  # The usual server
  hotspot_data <- reactive({
    if(res_auth$admin == TRUE) {
      data <- hotspot_yearly_data
    } else if(res_auth$admin != TRUE) {
      data <- hotspot_yearly_data %>% filter(jurisdiction == res_auth$user)
    }
  })
  
  # The species being shown
  output$text <- renderText({
    paste("The location signed in is", res_auth$user)
  })
  
  # table
  output$table <- renderTable({
    hotspot_data()
  })
  
}



# 5. Run app
# Run the application 
shinyApp(ui = ui, server = server)
