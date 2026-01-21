library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(scales)
library(systemfonts)

options(shiny.useragg = TRUE)

app_swim_stats <- function() {
  # Register fonts ---------------------------------------------------------
  clear_registry()

  num_font_path_plain <- system.file(
    "fonts/FiraCode-Regular.ttf",
    package = "swimStats"
  )

  num_font_path_bold <- system.file(
    "fonts/FiraCode-Bold.ttf",
    package = "swimStats"
  )
  register_font(
    "Num Font",
    plain = num_font_path_plain,
    bold = num_font_path_bold
  )

  app_theme <- bs_theme(
    preset = "lux",
    bg = "#fff",
    fg = "#B40F20",
    base_font = font_google("Montserrat"),
    heading_font = font_google("Montserrat Alternates"),
    `enable-rounded` = TRUE
  ) |>
    bs_add_rules(sass::sass_file(system.file(
      "app/www/styles.scss",
      package = "swimStats"
    )))

  df_swims <- load_swim_data()
  choices_year <- unique(year(df_swims$activity_date))
  choices_month <- filter_months(df_swims, year(Sys.Date()))

  ui <- page_sidebar(
    title = "Kaustav's Swim Stats",
    theme = app_theme,
    sidebar = sidebar(
      open = "closed",
      pickerInput(
        "year",
        "Select year",
        choices = choices_year,
        selected = year(Sys.Date())
      ),
      pickerInput(
        "month",
        "Select month",
        choices = choices_month,
        selected = month(Sys.Date())
      ),
    ),
    div(
      id = "nav-control",
      actionButton(
        "btn_prev_month",
        "← Prev",
        class = "btn-sm",
        inline = TRUE
      ),
      actionButton(
        "btn_next_month",
        "Next →",
        class = "btn-sm",
        inline = TRUE
      )
    ),
    h3("Jan 26", class = "month-title"),
    layout_column_wrap(
      width = 1 / 4,
      value_box(
        title = "Number of Sessions",
        value = uiOutput("num_sessions", inline = TRUE),
        showcase = icon("water-ladder")
      ),
      value_box(
        title = "Distance Covered",
        value = uiOutput("dist_swam", inline = TRUE),
        showcase = icon("water")
      ),
      value_box(
        title = "Average pace (mins per 100m)",
        value = uiOutput("avg_pace", inline = TRUE),
        showcase = icon("clock")
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
        # card_body("Plot showing the progress compared to previous month")
        card_body(plotOutput("month_swim_compare"))
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
    observeEvent(input$year, {
      freezeReactiveValue(input, "month")
      choices_month <- filter_months(df_swims, input$year)
      latest_month <- choices_month[length(choices_month)]

      updatePickerInput(
        inputId = "month",
        choices = choices_month,
        selected = latest_month
      )
    })

    output$month_swim_calendar <- renderPlot(
      {
        plot_monthly_swim_calendar(
          df_swims,
          year = as.numeric(input$year),
          month = input$month
        )
      },
      res = 96
    )

    output$month_swim_compare <- renderPlot(
      {
        plot_monthly_compare(
          df_swims,
          year = as.numeric(input$year),
          month = input$month
        )
      },
      res = 96
    )

    output$num_sessions <- renderUI({
      req(input$year, input$month)
      get_num_sessions(df_swims, input$year, input$month)
    })

    output$dist_swam <- renderUI({
      get_dist_swam(df_swims, input$year, input$month)
    })

    output$avg_pace <- renderUI({
      get_avg_pace(df_swims, input$year, input$month)
    })
  }

  shinyApp(ui, server)
}
