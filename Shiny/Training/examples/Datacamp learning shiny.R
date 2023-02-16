################################
## Interactive plots in shiny ##
################################

## Using the gapminder dataset
library(gapminder)
library(ggplot2)

## User interface
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      radioButtons("color", "Point color",
                   choices = c("blue", "red", "green", "black")),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      # Add a slider selector for years to filter
      sliderInput("years", "Years", value = 1980, min = min(gapminder$year), max = max(gapminder$year))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)


## Server
server <- function(input, output) {
  output$plot <- renderPlot({
    # Subset the gapminder data by the chosen years
    data <- subset(gapminder,
                   continent %in% input$continents &
                     year >= input$years[1] & year <= input$years[2])
    
    p <- ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size, col = input$color) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

## App
shinyApp(ui = ui, server = server)



## PLOTLY ##
# Load the plotly package
library(plotly)
library(colourpicker)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      colourInput("color", "Point color", value = "blue"),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      sliderInput("years", "Years",
                  min(gapminder$year), max(gapminder$year),
                  value = c(1977, 2002))
    ),
    mainPanel(
      # Replace the `plotOutput()` with the plotly version
      plotlyOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Replace the `renderPlot()` with the plotly version
  output$plot <- renderPlotly({
    # Convert the existing ggplot2 to a plotly plot
    ggplotly({
      data <- subset(gapminder,
                     continent %in% input$continents &
                       year >= input$years[1] & year <= input$years[2])
      
      p <- ggplot(data, aes(gdpPercap, lifeExp)) +
        geom_point(size = input$size, col = input$color) +
        scale_x_log10() +
        ggtitle(input$title)
      
      if (input$fit) {
        p <- p + geom_smooth(method = "lm")
      }
      p
    })
  })
}

shinyApp(ui = ui, server = server)



### Subsetting data ###
ui <- fluidPage(
  h1("Gapminder"),
  # Add a slider for life expectancy filter 
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      # Use the life expectancy input to filter the data
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    data
  })
}

shinyApp(ui, server)



### If statements when subsetting data
ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  # Add an "All" value to the continent list
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    # Don't subset the data if "All" continent are chosen
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
}

shinyApp(ui, server)





#### Download button
## For csv file
ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  # Add a download button
  downloadButton(outputId = "download_data", label = "Download"),
  plotOutput("plot"),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  # Create a download handler
  output$download_data <- downloadHandler(
    # The downloaded file is named "gapminder_data.csv"
    filename = "gapminder_data.csv",
    content = function(file) {
      # The code for filtering the data is copied from the
      # renderTable() function
      data <- gapminder
      data <- subset(
        data,
        lifeExp >= input$life[1] & lifeExp <= input$life[2]
      )
      if (input$continent != "All") {
        data <- subset(
          data,
          continent == input$continent
        )
      }
      
      # Write the filtered data into a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)



#### Reactive value of the data to reduce computional effort and clean code
ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  downloadButton(outputId = "download_data", label = "Download"),
  plotOutput("plot"),
  tableOutput("table")
)

server <- function(input, output) {
  # Create a reactive variable named "filtered_data"
  filtered_data <- reactive({
    # Filter the data (copied from previous exercise)
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  output$table <- renderTable({
    # Use the filtered_data variable to render the table output
    data <- filtered_data()
    data
  })
  
  output$download_data <- downloadHandler(
    filename = "gapminder_data.csv",
    content = function(file) {
      # Use the filtered_data variable to create the data for
      # the downloaded file
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    # Use the filtered_data variable to create the data for
    # the plot
    data <- filtered_data()
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)







## USING DT for tables
# dont load DT, simply call it each time using DT::
ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  downloadButton("download_data"),
  # Replace the tableOutput() with DT's version
  DT::dataTableOutput("table")
)







server <- function(input, output) {
  
  
  filtered_data <- reactive({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  # Replace the renderTable() with DT's version
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
  
  
  
  output$download_data <- downloadHandler(
    filename = "gapminder_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  

}

shinyApp(ui, server)





