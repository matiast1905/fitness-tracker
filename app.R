library(shiny)
library(tidyverse)
library(shinythemes)
library(zoo)
library(plotly)
library(tidymetrics)
library(shinyjs)

theme_set(theme_minimal())

gym_df <- read_csv("data.csv") |>
  cross_by_dimensions(exercise, category)


graph_options <- c(
  "max_weight", "max_reps", "max_volume",
  "workout_volume", "workout_reps"
)
names(graph_options) <- str_to_title(str_replace(graph_options, "_", " "))

group_period <- c("date","week")
names(group_period) <- c("Daily","Weekly")

ui <- fluidPage(
  theme = shinytheme("superhero"),
  useShinyjs(),
  # Application title
  titlePanel("Visualizing gym workout"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "category",
        label = h4("Select the category"),
        choices = unique(gym_df$category) |> sort(),
        selected = "All",
        multiple = TRUE
      ),
      selectInput(
        inputId = "exercise",
        label = h4("Select the exercise"),
        choices = unique(gym_df$exercise) |> sort(),
        selected = "All",
        multiple = TRUE
      ),
      selectInput(
        inputId = "graph_type",
        label = h4("Select graph"),
        choices = graph_options,
        selected = graph_options[4]
      ),
      selectInput(
        inputId = "group_per",
        label = h4("Group period"),
        choices = group_period,
        selected = group_period[1]
      ),
      dateRangeInput(
        inputId = "date_range",
        label = h4("Date range to display"),
        min = min(gym_df$date),
        max = max(gym_df$date),
        start = min(gym_df$date),
        end = max(gym_df$date),
        weekstart = 1
      ),
      sliderInput(
        inputId = "moving_average",
        label = h4("Moving average"),
        min = 1,
        max = 10,
        value = 1
      ),
      checkboxInput(
        inputId = "trend_line",
        label = "Trend Line"
      ),
      checkboxInput(
        inputId = "y_axis_0",
        label = "Y axis from 0",
        value = TRUE
      ),
      checkboxInput(
        inputId = "facet",
        label = "Facet graph",
        value = FALSE
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("exercise_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$exercise,{
    if (length(input$exercise) > 1){
      enable("facet")
    }else{
      disable("facet")
    }
  })

  onclick("category", {
    updateSelectInput(session = session, inputId = "category", selected = "")
  })

  onclick("exercise", {
    updateSelectInput(session = session, inputId = "exercise", selected = "")
  })

  observeEvent(input$category, {
    options <- gym_df |>
      filter(category %in% input$category) |>
      pull(exercise) |>
      unique() |>
      sort()
    updateSelectInput(
      inputId = "exercise",
      choices = options,
      selected = options[1]
    )
  })

  output$exercise_plot <- renderPlotly({
    validate(need(input$category, 'Choose a category'))
    validate(need(input$exercise, 'Choose an exercise'))
    cat_to_plot <- input$graph_type
    name_cat_to_plot <- str_to_title(str_replace(input$graph_type, "_", " "))

    g <- gym_df |>
      filter(category %in% input$category,
             exercise %in% input$exercise,
             between(date, input$date_range[1], input$date_range[2])) |>
      group_by(exercise, !!sym(input$group_per)) |>
      dplyr::summarise(
        max_weight = max(weight),
        max_reps = max(reps),
        max_volume = max(serie_load),
        workout_volume = sum(serie_load),
        workout_reps = sum(reps),
        .groups = "drop_last"
      ) |>
      select(!!sym(input$group_per), exercise,
             !!sym(name_cat_to_plot) := sym(cat_to_plot)) |>
      mutate(!!name_cat_to_plot := rollmean(!!sym(name_cat_to_plot),
        k = input$moving_average,
        fill = NA, align = "right")) |>
      ungroup() |>
      filter(!is.na(!!sym(name_cat_to_plot))) |>
      ggplot(aes(!!sym(input$group_per),!!sym(name_cat_to_plot), color = exercise)) +
      geom_line(alpha = 0.7, size = 1.3) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "Date",
        y = name_cat_to_plot
      )

    if (input$y_axis_0){
      g <- g +
        expand_limits(y = 0)
    }

    if (input$trend_line) {
      g <- g +
        geom_smooth(
          formula = y ~ x,
          method = "lm",
          se = FALSE,
          lty = 2,
          size = 1.4,
          alpha = 0.6
        )
    }

    if (length(input$exercise) == 1){
      g <- g +
        labs(color = "Exercise")
    }else{
      g <- g +
        labs(color = "Exercises")
    }

    if (input$facet){
      g <- g +
        facet_wrap(~exercise, ncol = 1, scales = "free_y") +
        theme(legend.position = "none",
              strip.text = element_text(face = "bold", size = 18))

      g <- ggplotly(g, height = 300 * length(input$exercise))
    }
    else {
      ggplotly(g, height = 450)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
