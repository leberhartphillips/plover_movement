# define the plotting theme to be used in subsequent ggplots
luke_theme <- 
  theme_bw() +
  theme(
    text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.x  = element_text(size = 8), 
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 0.5, colour = "grey40"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey")
  )

# set the ggplotting theme
theme_set(luke_theme)

# line plot of battery decay
real_battery_decay <-
  rbind(as.data.frame(NFTag55719),
        as.data.frame(NFTag21200)) %>% 
  filter(!is.na(battery)) %>% 
  group_by(ring) %>% 
  mutate(voltage_rate = battery/lag(battery, default = first(battery))) %>%
  ggplot() +
  geom_point(aes(x = as.Date(timestamp, format = "%Y-%m-%d", shape = ), y = battery), color = "grey") +
  geom_line(aes(x = as.Date(timestamp, format = "%Y-%m-%d"), y = battery, color = ring)) +
  theme_bw() +
  luke_theme +
  ylab("Battery voltage") +
  xlab("Date") +
  scale_x_date(date_labels = "%d/%b/%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.78, 0.5),
        legend.title = element_text(size = 9)) +
  labs(color = "Bird ring") +
  scale_color_brewer(palette = "Dark2", direction = -1)

ggsave(real_battery_decay, 
       filename = "products/figs/55719_21200_battery_decay.jpeg", width = 8, height = 4)

proportional_battery_change <-
  rbind(as.data.frame(NFTag55719),
        as.data.frame(NFTag21200)) %>% 
  group_by(tag_ID) %>% 
  mutate(voltage_rate = battery/lag(battery, default = first(battery)),
         voltage_change = battery - lag(battery, default = first(battery))) %>%
  ggplot() +
  geom_line(aes(x = as.Date(timestamp, format = "%Y-%m-%d"), y = voltage_rate, color = tag_ID)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
  luke_theme +
  ylab("Proportional battery voltage change between fixes") +
  xlab("Date") +
  scale_x_date(date_labels = "%d/%b/%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(color = "Nanofix tag") +
  facet_grid(tag_ID ~ .)

absolute_battery_change <-
  rbind(as.data.frame(NFTag55719),
        as.data.frame(NFTag21200)) %>% 
  group_by(tag_ID) %>% 
  mutate(voltage_rate = battery/lag(battery, default = first(battery)),
         voltage_change = battery - lag(battery, default = first(battery))) %>%
  ggplot() +
  geom_line(aes(x = as.Date(timestamp, format = "%Y-%m-%d"), y = voltage_change, color = tag_ID)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  luke_theme +
  ylab("Absolute battery voltage change between fixes") +
  xlab("Date") +
  scale_x_date(date_labels = "%d/%b/%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(color = "Nanofix tag") +
  facet_grid(tag_ID ~ .)

# schedule_change <-
rbind(as.data.frame(NFTag55719),
      as.data.frame(NFTag21200)) %>% 
  group_by(tag_ID) %>% 
  mutate(schedule_change = timestamp - lag(timestamp, default = first(timestamp))) %>% 
  mutate(schedule_change = as.numeric(schedule_change)/60/60) %>% 
  ggplot() +
  geom_point(aes(y = battery, x = schedule_change, color = as.numeric(timestamp))) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  luke_theme +
  ylab("Battery voltage") +
  xlab("Hours between consecutive fixes") +
  # xlab("Date") +
  # scale_x_date(date_labels = "%d/%b/%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "Date") +
  facet_grid(tag_ID ~ .)
