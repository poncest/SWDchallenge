# 01_before_app/app.R
# baseline app

# bad (before) baseline using shinydashboard with clutter
#' Why this qualifies as “bad”
#' Too many global controls; filters affect everything (confusing).
#' Multiple tabs and redundant views.
#' KPIs aren’t tied to decisions.
#' Plotly everywhere adds noise without purpose.
#' Color palette and legends are inconsistent.

library(shiny)
library(shinydashboard)

# source(here::here(("2025/09_Sep/01_before_app/global.R")))
source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "A64 Traffic — Before"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Volume", tabName = "volume", icon = icon("chart-line")),
      menuItem("Speed", tabName = "speed", icon = icon("gauge-high")),
      menuItem("Vehicles", tabName = "vehicles", icon = icon("truck")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    ),
    # Too many global filters (intentionally noisy)
    pickerInput("sensor_pick", "Sensors",
      choices = levels(traffic$sensor), multiple = TRUE,
      options = list(`actions-box` = TRUE), selected = levels(traffic$sensor)
    ),
    dateRangeInput("date_range", "Date range",
      start = min(traffic$report_date), end = max(traffic$report_date)
    ),
    pickerInput("daytype_pick", "Day type",
      choices = c("Weekday", "Weekend"), multiple = TRUE,
      selected = c("Weekday", "Weekend")
    ),
    sliderInput("hour_range", "Hour of day", min = 0, max = 23, value = c(0, 23)),
    checkboxInput("show_plotly", "Interactive (Plotly)", value = TRUE),
    checkboxGroupInput("show_boxes", "Show KPI boxes",
      choices = c("Total Volume", "Avg Speed", "% Large Vehicles"),
      selected = c("Total Volume", "Avg Speed", "% Large Vehicles")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "overview",
        fluidRow(
          valueBoxOutput("kpi1", width = 4),
          valueBoxOutput("kpi2", width = 4),
          valueBoxOutput("kpi3", width = 4)
        ),
        fluidRow(
          box(
            width = 6, title = "Daily Volume by Sensor",
            if (isTRUE(getOption("bad_plotly", TRUE))) plotlyOutput("p_daily") else plotOutput("p_daily")
          ),
          box(
            width = 6, title = "Weekday vs Weekend Volume",
            if (isTRUE(getOption("bad_plotly", TRUE))) plotlyOutput("p_daytype") else plotOutput("p_daytype")
          )
        )
      ),
      tabItem(
        "volume",
        fluidRow(
          box(width = 12, title = "Hourly Volume (table)", DTOutput("tbl_vol"))
        )
      ),
      tabItem(
        "speed",
        fluidRow(
          box(
            width = 12, title = "Hourly Avg Speed (faceted)",
            if (isTRUE(getOption("bad_plotly", TRUE))) plotlyOutput("p_speed") else plotOutput("p_speed")
          )
        )
      ),
      tabItem(
        "vehicles",
        fluidRow(
          box(
            width = 12, title = "Vehicle Size Composition",
            if (isTRUE(getOption("bad_plotly", TRUE))) plotlyOutput("p_mix") else plotOutput("p_mix")
          )
        )
      ),
      tabItem(
        "data",
        fluidRow(box(width = 12, DTOutput("tbl_raw")))
      )
    )
  )
)

server <- function(input, output, session) {
  # noisy reactive filter
  filt <- reactive({
    traffic |>
      filter(
        sensor %in% input$sensor_pick,
        report_date >= input$date_range[1],
        report_date <= input$date_range[2],
        day_type %in% input$daytype_pick
      ) |>
      mutate(hh = hour(hms::as_hms(hour))) |>
      filter(hh >= input$hour_range[1], hh <= input$hour_range[2])
  })

  # KPIs (not very meaningful on purpose)
  output$kpi1 <- renderValueBox({
    valueBox(
      comma(sum(filt()$total_volume, na.rm = TRUE)),
      "Total Volume (selected)",
      icon = icon("car"), color = "blue"
    )
  })
  output$kpi2 <- renderValueBox({
    valueBox(
      round(mean(filt()$avg_mph, na.rm = TRUE), 1),
      "Avg Speed (mph)",
      icon = icon("gauge-high"), color = "orange"
    )
  })
  output$kpi3 <- renderValueBox({
    large <- sum(filt()$x1160_cm, na.rm = TRUE)
    share <- large / sum(filt()$total_volume, na.rm = TRUE)
    valueBox(scales::percent(share, accuracy = 0.1),
      "% Large Vehicles",
      icon = icon("truck"), color = "purple"
    )
  })

  # Plots (wrapped in Plotly if requested)
  render_maybe_plotly <- function(p) {
    if (isTRUE(input$show_plotly)) plotly::ggplotly(p) else p
  }

  output$p_daily <- renderPlotly({
    render_maybe_plotly(plot_daily_volume(filt()))
  })
  output$p_daytype <- renderPlotly({
    render_maybe_plotly(plot_daytype_volume(filt()))
  })
  output$p_speed <- renderPlotly({
    render_maybe_plotly(plot_hourly_speed(filt()))
  })
  output$p_mix <- renderPlotly({
    render_maybe_plotly(plot_vehicle_mix(filt()))
  })

  # Tables
  output$tbl_vol <- renderDT({
    filt() |>
      group_by(sensor, hour) |>
      summarise(hourly_vol = sum(total_volume, na.rm = TRUE), .groups = "drop") |>
      datatable(extensions = "Buttons", options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
  })

  output$tbl_raw <- renderDT({
    datatable(filt(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)
