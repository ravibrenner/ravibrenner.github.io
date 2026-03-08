library(shiny)
library(bslib)
library(dplyr)
# 
# if (!file.exists("travel_time_grid.rds")) {
#   download.file("travel_time_grid.rds", "travel_time_grid.rds")
# }
travel_time_grid <- readRDS("travel_time_grid.rds")

ui <- page_sidebar(
  sidebar = sidebar(open = "open",
    uiOutput("orig_select"),
    uiOutput("train_select"),
    uiOutput("dest_select")
  ),
  card(plotOutput("plot"))
)

server <- function(input, output, session) {
  output$orig_select <- renderUI({
    origin_station_list <- travel_time_grid |>
      pull(stop_name_orig) |> 
      unique() |>
      sort()
    
    selectInput("orig_select_input","Select Origin",
                choices = origin_station_list)
  })
  
  output$train_select <- renderUI({
    req(input$orig_select_input)
    
    train_options <- travel_time_grid |>
      filter(stop_name_orig == input$orig_select_input) |>
      pull(route_list) |>
      unique() |>
      sort() 
    
    selectInput("train_select_input","Select trains",
                choices = train_options)
  })
  
  output$dest_select <- renderUI({
    req(input$orig_select_input, input$train_select_input)
    
    dest_station_list <- travel_time_grid |>
      filter(stop_name_orig == input$orig_select_input,
             route_list == input$train_select_input) |>
      pull(stop_name_dest) |>
      unique() |>
      sort()
    
    selectInput("dest_select_input","Select Destination",
                choices = dest_station_list)
  })
  
  output$plot <- renderPlot({
    req(input$orig_select_input, input$dest_select_input)
    
    df <- travel_time_grid |>
      ungroup() |>
      filter(stop_name_orig == input$orig_select_input,
             route_list == input$train_select_input,
             stop_name_dest == input$dest_select_input,) |>
      select(route_id, direction_letter, lag_time, route_color) |>
      arrange(lag_time) |>
      mutate(wait_time = last(lag_time) - lag_time) |>
      mutate(wait_time_minutes = round(as.numeric(wait_time)/60))  |>
      select(-lag_time,-wait_time)
    
    df |>
      ggplot(aes(x = 1, y = reorder(route_id, -wait_time_minutes))) +
      geom_point(aes(color = I(route_color)), size = 30) +
      geom_text(aes(label = route_id),
                color = "white", 
                fontface = "bold", size = 15) +
      geom_text(aes(x = 2, label = paste(wait_time_minutes, "min")), 
                hjust = 0, size = 12, fontface = "bold", family = "sans") +
      scale_x_continuous(limits = c(1, 2)) +
      theme_void() 
  })
}

shinyApp(ui = ui, server = server)
