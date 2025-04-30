#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(here)
library(sf)
library(leaflet)
library(mapview)
library(tidyverse)

mapviewOptions(basemaps = c("CartoDB.DarkMatter", "Esri.WorldShadedRelief", "OpenStreetMap.DE"),
               layers.control.pos = "topright")

data <- readRDS(here::here("www", "2024_GoM_fishing_events.rds")) |>
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

months <- sort(unique(data$month))

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Explore fishing effort in the Gulf of Mexico"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      shiny::selectInput(inputId = "month",
                         label = "Month of fishing",
                         choices = months,
                         selected = "January",
                         multiple = F)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map"),
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$map <- renderLeaflet({

    # print(input$flag)
    # generate bins based on input$bins from ui.R
    # plot_data <- filter(data, vessel_flag %in% input$flag)

    final <- mapview(sample_frac(data[data$month == input$month, ], 0.1),
                     col.regions = "red",
                     cex = 2,
                     layer.name = paste("fishing during", input$month),
                     alpha = 1)
    final@map
  })

  output$plot <- renderPlot({

    data |>
      filter(month == input$month) |>
      mutate(date = date(start)) |>
      count(date) |>
      ggplot(aes(x = date, y = n)) +
      geom_line(linewidth = 1, color = "red") +
      labs(x = "Date",
           y = "Daily number of fishinge vents") +
      ggthemes::theme_economist()

  })
}

# Run the application
shinyApp(ui = ui, server = server)
