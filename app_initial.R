library(shiny)
library(ggplot2)
library(rnaturalearth)
library(dplyr)

last_week <- readRDS("data/last_week.rds")
last_year <- readRDS("data/last_year.rds")
world <- ne_countries(scale = "medium", returnclass = "sf")

ui <- navbarPage(
  title = "BIG BUD PRESS",
  tabPanel(
    "Google Trends",
    p("This page captures popularity of the search term", span("Big Bud Press"), "from Google Trends. Numbers are all relative to the highest data point for a given region or time - 100 is peak popularity, 50 means half as popular. Data was obtained via the R package", a("gtrendsR", href = "https://cran.r-project.org/web/packages/gtrendsR/index.html", target = "_blank"), "on March 11, 2021, so only goes up til then!"),
    sidebarLayout(
      sidebarPanel(
        selectInput("time", label = "Select a time period", choices = c("Past 12 Months", "Last 7 Days"), selected = "Past 12 Months")
      ),
      mainPanel(
        h3("Interest over time"),
        plotOutput("over_time"),
        column(
          width = 6,
          h3("Interest by region"),
          plotOutput("region")
        ),
        column(
          width = 6,
          h3("Interest by US city"),
          tableOutput("us_cities")
        )
      )
    )
  ),
  tabPanel(
    "About"
  )
)


server <- function(input, output, session) {
  time_data <- reactive({
    switch(input$time,
      "Past 12 Months" = last_year,
      "Last 7 Days" = last_week
    )
  })

  output$over_time <- renderPlot({
    ggplot(time_data()[["interest_over_time"]], aes(x = date, y = hits)) +
      geom_line() +
      theme_minimal()
  })

  output$region <- renderPlot({
    bb_world <- world %>%
      select(name, geometry) %>%
      left_join(time_data()[["interest_by_country"]], by = c("name" = "location"))

    ggplot(data = bb_world) +
      geom_sf(aes(fill = hits)) +
      theme_minimal()
  })

  output$us_cities <- renderTable({
    time_data()[["interest_by_dma"]] %>%
      filter(!is.na(hits)) %>%
      select(location, hits)
  })
}

shinyApp(ui = ui, server = server)
