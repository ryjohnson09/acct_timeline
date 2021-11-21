library(shiny)
library(tidyverse)
library(lubridate)
library(gt)
library(ggrepel)
library(pins)
library(scales)

# Read in Data ---------------------------------
board <- board_rsconnect(server = "https://connect.rstudioservices.com") # Running on RSC
#board <- board_rsconnect(server = "connect.rstudioservices.com") # Running locally
timeline_data <- pin_read(board, "ryan/acct_timeline_data")

# Month/Year Data
date_range <- seq(lubridate::today() - months(12),
                        lubridate::today() + months(2), by='month')

month_year_data <- tibble(floor_date(date_range, unit = "month")) %>% 
  mutate(month_label = month(date_range, label = TRUE, abbr = TRUE)) %>% 
  mutate(year_label = year(date_range))


ui <- fillPage(column(12, align="center",
                  # Account Name
                  selectInput("acct_name", "", 
                              choices = sort(unique(timeline_data$acct_name)), 
                              selected = "Amgen"),
                  # Engagement Type
                  checkboxGroupInput("engagement_type", "",
                                     choices = c("Sales", "CS", "Support"), 
                                     selected = c("Sales", "CS"), 
                                     inline = TRUE)),
                  # Plot
                  plotOutput("plot", click = "plot_click", 
                             width = "100%",
                             height = "75%"
                             )
)

server <- function(input, output, session) {
  
  # filter data
  timeline_data_filt <- reactive({
    timeline_data %>% 
      # Filter for account name
      filter(acct_name == input$acct_name) %>% 
      # Filter for date range
      filter(between(event_date, min(date_range), max(date_range))) %>% 
      # Filter for point type
      filter(event_type %in% input$engagement_type)
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
      # Sales points
      geom_point(data = filter(timeline_data_filt(), event_type == "Sales"),
                       fill = "#f9c74f", pch = 21, size = 3, color = "black") +
      # CS points
      geom_point(data = filter(timeline_data_filt(), event_type == "CS"),
                 fill = "#4d908e", pch = 21, size = 3, color = "black") +
      # Support points
      geom_point(data = filter(timeline_data_filt(), event_type == "Support"),
                 fill = "#f94144", pch = 21, size = 3, color = "black") +
      # Date labels
      geom_text(data = month_year_data, 
                aes(y = date_range, 
                    x = 0.01, 
                    label = month_label), 
                size = 3, hjust = "left") +
      geom_text(data = month_year_data,
                aes(y = date_range,
                    x = -0.01, 
                    label = year_label),
                size = 3, hjust = "right",
                color = "grey") +
      # Point labels
      geom_label_repel(data = filter(timeline_data_filt(), event_type == "Sales"),
                       aes(fill = event_label), nudge_x = -0.7, size = 2) +
      geom_label_repel(data = filter(timeline_data_filt(), event_type == "CS"),
                       aes(fill = event_subtype),nudge_x = 0.6, size = 2) +
      geom_label_repel(data = filter(timeline_data_filt(), event_type == "Support"),
                       aes(fill = event_subtype),nudge_x = -0.3, size = 2)
  }, res = 96)
  
  # Shiny alert showing plot
  observeEvent(input$plot_click, {
    showModal(modalDialog(
        gt_output("data"),
        easyClose = TRUE
      ))
  })
  
  
  # Table under plot
  output$data <- render_gt({
    nearPoints(timeline_data_filt(), input$plot_click) %>% 
      gt() %>%
      cols_hide(
        columns = c(
          event_type, x_axis, event_acct_id, 
          acct_name, event_date, event_label,
          event_subtype
        )
      ) %>% 
      fmt_markdown(columns = event_notes) %>% 
      cols_align(
        align = c("left"),
        columns = everything()
      ) %>% 
      tab_options(#table.width = px(600), 
                  table.font.size = "90%",
                  column_labels.hidden = TRUE)
  })
}

shinyApp(ui = ui, server = server)