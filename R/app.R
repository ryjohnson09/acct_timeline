library(shiny)
library(tidyverse)
library(lubridate)
library(gt)
library(ggrepel)

#source("dummy_data.R")

ui <- fluidPage(
  fluidRow(column(12, align="center",
  plotOutput("plot", click = "plot_click", width = "800px", height = "800px"))),
  gt_output("data")
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    # Time plot
    ggplot(data = final_data, aes(y = event_date, 
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
            legend.position = "bottom"
      ) +
      geom_point(aes(fill = event_type), pch = 21, size = 3, color = "black") +
      geom_text(data = month_data, 
                aes(y = month_date_range, 
                    x = 0.05, 
                    label = month_label), 
                size = 3) +
      geom_text(data = month_data,
                aes(y = month_date_range,
                    x = -0.05, 
                    label = year_label),
                size = 3, 
                color = "grey") +
      geom_label_repel(data = filter(final_data, event_type == "Sales"), nudge_x = -0.4, size = 2) +
      geom_label_repel(data = filter(final_data, event_type == "CS"), nudge_x = 0.4, size = 2)
  }, res = 96)
  
  # Table under plot
  output$data <- render_gt({
    nearPoints(final_data, input$plot_click) %>% 
      gt() %>%
      cols_hide(
        columns = c(
          event_type, x_axis, event_acct_id, acct_name
        )
      ) %>% 
      fmt_markdown(columns = event_notes) %>% 
      tab_options(table.width = px(600))
  })
}

shinyApp(ui = ui, server = server)