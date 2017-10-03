library(ggplot2)

ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point()

ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point(position = "jitter")

ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point(position = "jitter") +
  facet_wrap(~ active)

ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_point(position = "jitter") +
  facet_grid(medium ~ active)

ggplot(final_dat, aes(x = pathotype, y = value)) +
  geom_boxplot() +
  facet_grid(medium ~ active)

ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
  geom_boxplot() +
  facet_wrap(~ medium)

library(ggbeeswarm)
ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
  geom_quasirandom() +
  facet_wrap(~ medium)

ggplot(filter(final_dat, active != "W3"), aes(x = pathotype, y = value, color = active)) +
  geom_quasirandom() +
  facet_wrap(~ medium)

thr_dat <- mutate(final_dat, thr = value > 0.07)

ggplot(thr_dat, aes(x = thr)) +
  geom_bar()

ggplot(thr_dat, aes(x = thr, fill = medium)) +
  geom_bar()

ggplot(thr_dat, aes(x = thr, fill = medium)) +
  geom_bar(position = "fill")

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill")

mean_dat <- group_by(final_dat, active, medium, pathotype) %>% 
  summarise(mean_value = mean(value),
            sd_value = sd(value))

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_bar(position = "dodge", stat = "identity") + +
  facet_wrap(~ active, ncol = 1)

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  facet_wrap(~ active, ncol = 1)

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, ncol = 1)

library(plotly)
ggplotly(ggplot(thr_dat, aes(x = medium, fill = thr)) +
           geom_bar(position = "fill"))

ggplotly(ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
           geom_boxplot() +
           facet_wrap(~ medium))

# 1. Using a bar chart compare maximum values for each medium and pathotype