library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

# Loading data ----------------------------------------------

homes <- read.csv("house.csv")
#View(homes)

# Make knitting faster with the NUll within pdf()----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Seattle Homes Prices")



# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    id = "tabs",

# Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),

#Inputs   

  #Decide the preferred zipcode within Seattle
    selectInput("zip",
                "Select your preferred zipcode in Seattle",
                choices = sort(unique(homes$zipcode)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c(98125)),

    radioButtons("c",
                 "Visualization color?",
                  choices = c("Maroon", "Blue", "Green", "Black", "Yellow"),
                  selected = "Maroon"),
    
  #Decide the alpha level of plots
    sliderInput(inputId= "alpha", 
                label = "Alpha level ?", 
                min = 0, 
                max = 1, 
                value = 0.4)
    

    

  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(

  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("bedrooms", plotlyOutput("plot_bedrooms" )),
                   tabPanel("floors", plotlyOutput("plot_floors")),
                   tabPanel("year", plotlyOutput("plot_year")))
          ), 
          
          # Value Boxes ----------------------------------------------
          fluidRow(
            valueBoxOutput("price"),
            valueBoxOutput("bedrooms"),
            valueBoxOutput("build")
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Zipcode", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Defining server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  houseTemp <- reactive({
    
    
    if (length(input$zip) > 0 ) {
      homes <- subset(homes, zipcode %in% input$zip)
    }
    
    # Return dataframe ----------------------------------------------
    return(homes)
  })

  # Reactive melted data ----------------------------------------------
  houseNew <- reactive({
    houseTemp() %>%
      melt(id = "zipcode")
  })
  
  # plot for range of prices based on bedrooms -----------------------------
  output$plot_bedrooms <- renderPlotly({
    dat <- subset(houseTemp())
    
    ggplot(data = dat, aes(x = bedrooms , y = price)) + geom_bar(stat = "identity", alpha = input$alpha, colour = input$c)
  })
  
  
  # plot for range of prices based on number of floors in the house -----------------------------------
  output$plot_floors <- renderPlotly({
    dat <- subset(houseTemp())
    
    ggplot(data = dat, aes(x = floors, y = price)) + geom_bar(stat = "identity", alpha = input$alpha, colour = input$c)
  })
  
  # plot for range of prices based on year it was built -----------------------------------
  output$plot_year <- renderPlotly({
    dat <- subset(houseTemp())
    
    ggplot(data = dat, aes(x = yr_built, y = price)) + geom_bar(stat = "identity", alpha = input$alpha, colour = input$c)
  })
  
  

  
  # Data table of zipcodes ----------------------------------------------
  output$table <- DT::renderDataTable(
    {subset(houseTemp(), select = c(price, bedrooms, bathrooms, yr_built, floors, grade, zipcode, sqft_living, sqft_lot))
  })
  
  # Price max value box ----------------------------------------------
  output$price <- renderValueBox({
    #house <- houseTemp()
    num <- round(max(homes$price, na.rm = T), 2)
    
    valueBox("Highest house price in Seattle", value = num, icon = icon("sort-numeric-asc"), color = "maroon")
  })
  
  # Average house size value box ----------------------------------------------
  output$bedrooms <- renderValueBox({
    #house <- houseTemp()
    num <- round(mean(homes$bedrooms, na.rm = T), 2)
    
    valueBox(subtitle = "Avg size of homes in Seattle", value = num, icon = icon("home"), color = "green")
  })
  
  # Oldest Year value box ----------------------------------------------
  output$build <- renderValueBox({
    #house <- houseTemp()
    num <- round(min(homes$yr_built, na.rm = T), 2)
    
    valueBox(subtitle = "Oldest House  build year in Seattle", value = num, icon = icon("hourglass-half"), color = "purple")
  })
  
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)