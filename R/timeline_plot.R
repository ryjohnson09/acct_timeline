library(tidyverse)
library(lubridate)
library(ggrepel)

#source("R/dummy_data.R")

time_plot <- ggplot(data = final_data, aes(y = event_date, 
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
  geom_point(aes(color = event_type), size = 3) +
  geom_text(data = month_data, 
            aes(y = month_date_range, 
                x = 0.05, 
                label = month_label), 
            size = 3) +
  geom_label_repel(data = filter(final_data, event_type == "Sales"), nudge_x = -0.4) +
  geom_label_repel(data = filter(final_data, event_type == "CS"), nudge_x = 0.4)

time_plot








gt(data = ex_data) %>%
  cols_hide(
    columns = c(
      event_type, x_axis
    )
  ) %>% 
  fmt_markdown(columns = event_notes) %>% 
  tab_options(table.width = px(800))
