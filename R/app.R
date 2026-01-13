library(shiny)
library(bslib)
library(tidyverse)
library(scales)

# systemfonts::get_from_google_fonts("Montserrat")
# systemfonts::get_from_google_fonts("Fira Code")

app_siwmStats <- function() {
  df_swims <- load_swim_data()

  ui <- page_sidebar(
    title = "Kaustav's Swim Stats",
    theme = bs_theme(
      preset = "lux",
      bg = "#fff",
      fg = "#B40F20",
      base_font = font_google("Montserrat"),
      heading_font = font_google("Montserrat Alternates"),
      `enable-rounded` = TRUE
    ),
    sidebar = sidebar(
      open = "closed",
      selectInput("year", "Select year", choices = c(2025, 2026)),
      selectInput(
        "month",
        "Select month",
        choices = c(
          "Jan" = 1,
          "Feb" = 2,
          "Mar" = 3,
          "Apr" = 4,
          "May" = 5,
          "Jun" = 6,
          "Jul" = 7,
          "Aug" = 8,
          "Sep" = 9,
          "Oct" = 10,
          "Nov" = 11,
          "Dec" = 12
        ),
        selected = 11
      )
    ),
    layout_column_wrap(
      width = 1 / 4,
      value_box(
        title = "Number of Sessions",
        value = uiOutput("num_sessions", inline = TRUE),
        showcase = icon("water-ladder")
      ),
      value_box(
        title = "Hours swam",
        value = "to add",
        showcase = icon("water")
      )
    ),
    layout_columns(
      col_widths = 1 / 2,
      card(
        card_header("Calendar view"),
        card_body(plotOutput("month_swim_calendar"))
      ),
      card(
        card_header("Benchmark to previous month"),
        card_body("Plot showing the progress compared to previous month")
      )
    ),
    fluidRow(column(
      12,
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Individual swim details",
          "Include a table with dates, swim times and length"
        )
      )
    ))
  )

  server <- function(input, output, session) {
    output$month_swim_calendar <- renderPlot(
      {
        plot_monthly_swim_calendar(
          df_swims,
          year = as.numeric(input$year),
          month = as.numeric(input$month)
        )
      },
      res = 96
    )

    output$num_sessions <- renderUI({
      num_sessions <- df_swims |>
        filter(
          year(activity_date) == as.numeric(input$year),
          month(activity_date) == as.numeric(input$month)
        ) |>
        nrow()
      num_sessions
    })
  }

  shinyApp(ui, server)
}
