library(shiny)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(shinycssloaders)
library(reactable)
library(showtext)

font_add_google("Corben")
showtext_auto()

last_week <- readRDS("data/last_week.rds")
last_year <- readRDS("data/last_year.rds")
world <- ne_countries(scale = "medium", returnclass = "sf")

logo <- a(href = "/", img(src = "logo.webp", alt = "BIG BUD PRESS", height = "100px"))
footer <- div(class = "bb-footer", p("Made with love and #rstats by", a("Sharla Gelfand", href = "https://sharla.party", target = "_blank")))

ui <-
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(rel = "shortcut icon", href = "favicon.webp")
    ),
    navbarPage(
      title = logo,
      windowTitle = "BIG BUD PRESS",
      footer = footer,
      tabPanel(
        "Hello!",
        column(
          width = 12,
          h3("Hello!"),
          span(style = "font-size: 20px", "This is an exploration of branding a web app, inspired by (ripping off?) the wonderful", a("Big Bud Press", href = "https://bigbudpress.com", target = "_blank"), ", a Los Angeles based clothing brand specializing in unisex, everyday, and ethical goods. It's just for fun! You can read more about the making of in", a("this blog post", href = "", target = "_blank"), "and see", a("the code here.", href = "", target = "_blank")),
          br(),
          br(),
          img(src = "about-bb.webp", style = "display: block; margin-left: auto; margin-right: auto;")
        )
      ),
      tabPanel(
        "Google Trends",
        column(
          width = 9,
          p("This page captures popularity of the search term", span("Big Bud Press"), "from Google Trends. Numbers are all relative to the highest data point for a given region or time - 100 is peak popularity, 50 means half as popular. Data was obtained via the R package", a("gtrendsR", href = "https://cran.r-project.org/web/packages/gtrendsR/index.html", target = "_blank"), "on March 11, 2021, so only goes up til then!")
        ),
        column(
          width = 3,
          selectInput("time", label = "Time period", choices = c("Past 12 Months", "Last 7 Days"), selected = "Past 12 Months")
        ),
        column(
          width = 12,
          wellPanel(
            h3("Interest over time"),
            withSpinner(
              image = "loading.gif", image.width = 100, image.height = 100,
              plotOutput("over_time")
            )
          )
        ),
        column(
          width = 7,
          wellPanel(
            h3("Interest by region"),
            withSpinner(
              image = "loading.gif", image.width = 100, image.height = 100,
              plotOutput("region")
            )
          )
        ),
        column(
          width = 5,
          wellPanel(
            style = "min-height: 500px",
            h3("Interest by US city"),
            withSpinner(
              image = "loading.gif", image.width = 100, image.height = 100,
              reactableOutput("us_cities")
            )
          )
        )
      )
    )
  )


server <- function(input, output, session) {
  time_data <- reactive({
    switch(input$time,
      "Past 12 Months" = last_year,
      "Last 7 Days" = last_week
    )
  })

  output$over_time <- renderPlot(res = 96, {
    ggplot(time_data()[["interest_over_time"]], aes(x = date, y = hits)) +
      geom_line(colour = "#C585E6", size = 2) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_family = "Corben") +
      theme(axis.text = element_text(face = "bold"))
  })

  output$region <- renderPlot(res = 96, {
    bb_world <- world %>%
      select(name, geometry) %>%
      left_join(time_data()[["interest_by_country"]], by = c("name" = "location"))

    ggplot(data = bb_world) +
      geom_sf(aes(fill = hits), lwd = 0.25) +
      scale_fill_gradient(na.value = "white", low = "#C585E6", high = "#4D0268", limits = c(0, 100), breaks = c(0, 50, 100), name = NULL) +
      guides(fill = guide_colourbar(ticks = FALSE)) +
      theme_minimal(base_family = "Corben") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(face = "bold")
      )
  })

  output$us_cities <- renderReactable({
    time_data()[["interest_by_dma"]] %>%
      filter(!is.na(hits)) %>%
      select(City = location, Interest = hits) %>%
      reactable(defaultPageSize = 5)
  })
}

shinyApp(ui = ui, server = server)
