library(shiny)
library(tidyverse)
library(lubridate)
library(gt)
library(ggrepel)
library(pins)

# Read in Data ---------------------------------
#board <- board_rsconnect(server = "https://connect.rstudioservices.com")
board <- board_rsconnect(server = "connect.rstudioservices.com")
timeline_data <- pin_read(board, "ryan/acct_timeline_data")

# Month/Year Data
date_range <- seq(lubridate::today() - months(12),
                        lubridate::today() + months(2), by='month')

month_year_data <- tibble(floor_date(date_range, unit = "month")) %>% 
  mutate(month_label = month(date_range, label = TRUE, abbr = TRUE)) %>% 
  mutate(year_label = year(date_range))


ui <- fluidPage(
  fluidRow(column(12, align="center",
                  selectInput("acct_name", "", choices = unique(timeline_data$acct_name), selected = "Merck"),
                  plotOutput("plot", click = "plot_click", width = "800px", height = "800px"))),
  gt_output("data")
)
server <- function(input, output, session) {
  
  # filter data
  timeline_data_filt <- reactive({
    timeline_data %>% 
      # Filter for account name
      filter(acct_name == input$acct_name) %>% 
      # Filter for date range
      filter(between(event_date, min(date_range), max(date_range)))
  })
  
  output$plot <- renderPlot({
    
    # Time plot
    ggplot(data = timeline_data_filt(), aes(y = event_date, 
                               x = x_axis, 
                               label = event_label)) +
      theme_minimal() +
      geom_vline(xintercept=0, 
                 color = "black", 
                 size=0.3) +
      coord_cartesian(xlim = c(-1, 1)) +
      theme(axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(), 
            legend.position = "none"
      ) +
      geom_point(aes(fill = event_type), pch = 21, size = 3, color = "black") +
      geom_text(data = month_year_data, 
                aes(y = date_range, 
                    x = 0.05, 
                    label = month_label), 
                size = 3) +
      geom_text(data = month_year_data,
                aes(y = date_range,
                    x = -0.05, 
                    label = year_label),
                size = 3, 
                color = "grey") +
      geom_label_repel(data = filter(timeline_data_filt(), event_type == "Sales"),
                       aes(fill = event_subtype), nudge_x = -0.6, size = 2.5) +
      geom_label_repel(data = filter(timeline_data_filt(), event_type == "CS"), 
                       aes(fill = event_subtype),nudge_x = 0.6, size = 2.5)
  }, res = 96)
  
  # Table under plot
  output$data <- render_gt({
    nearPoints(timeline_data_filt(), input$plot_click) %>% 
      gt() %>%
      cols_hide(
        columns = c(
          event_type, x_axis, event_acct_id, acct_name
        )
      ) %>% 
      fmt_markdown(columns = event_notes) %>% 
      tab_options(#table.width = px(600), 
                  table.font.size = "90%")
  })
}

shinyApp(ui = ui, server = server)