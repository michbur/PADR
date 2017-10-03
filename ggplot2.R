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

# 1. Add points to boxplots. What happened to outliers?
# 2. Create a boxplot only for active == "W1 and pathotype == "UPEC"

library(ggbeeswarm)
ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
  geom_quasirandom() +
  facet_wrap(~ medium)

ggplot(filter(final_dat, active != "W3"), aes(x = pathotype, y = value, color = active)) +
  geom_quasirandom() +
  facet_wrap(~ medium)


ggplot(final_dat, aes(x = value)) +
  geom_density()

ggplot(final_dat, aes(x = value)) +
  geom_density() +
  facet_wrap(~ medium)

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density() +
  facet_wrap(~ medium)

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ medium)

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_grid(pathotype ~ medium)

# 1. Create a density plot for each pathotype and medium.

thr_dat <- mutate(final_dat, thr = value > 0.07)

ggplot(thr_dat, aes(x = thr)) +
  geom_bar()

ggplot(thr_dat, aes(x = thr, fill = medium)) +
  geom_bar()

ggplot(thr_dat, aes(x = thr, fill = medium)) +
  geom_bar(position = "fill")

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill")

# 1. Using facets and bar charts show threshold data separately
# for each active substance.
# 2. Show on a barchart number of strains from each pathotype.

mean_dat <- group_by(final_dat, active, medium, pathotype) %>% 
  summarise(mean_value = mean(value),
            sd_value = sd(value))

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value)) +
  geom_tile(color = "black") +
  facet_wrap(~ medium)

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

# 1. Using a bar chart compare median values for each medium and pathotype. 
# Use median absolute deviation (mad()) as a dispersion measure.
# 2. Using a heat map compare median values for each medium and pathotype. 

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, ncol = 1) + 
  coord_flip()

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, nrow = 1) + 
  coord_flip()

p <- ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, ncol = 1)

p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

p + theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

my_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  legend.position = "bottom")

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  my_theme

# 1. Create your own theme. See ?theme
# 2. See possible themes using theme_bw

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill")

unique(thr_dat[["medium"]])

thr_dat2 <- mutate(thr_dat,
                   medium = factor(medium, levels = c("LB", "BHI", "M63", "TSB")))

ggplot(thr_dat2, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill")

# Reverse the sequence of active (W3, W2, W1) and create a bar chart, 
# with the fraction of strains above threshold for each possible value
# of active.

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  scale_fill_discrete("Threshold")

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  scale_fill_manual("Threshold", values = c("orange", "lightblue3"))

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  geom_point() +
  facet_wrap(~ medium) +
  scale_color_continuous(low = "white", high = "black")

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  geom_point() +
  facet_wrap(~ medium) +
  scale_color_continuous(low = "white", high = "black") +
  scale_fill_continuous(low = "blue", high = "red")

ggplot(mean_dat, aes(x = pathotype, y = active, color = mean_value, size = sd_value)) +
  geom_point() +
  facet_wrap(~ medium) 

ggplot(mean_dat, aes(x = pathotype, y = active, color = mean_value, size = sd_value)) +
  geom_point() +
  facet_wrap(~ medium) +
  scale_size_continuous(range = c(5, 10))

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  facet_wrap(~ medium) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = (max(mean_dat[["mean_value"]]) - min(mean_dat[["mean_value"]]))/2)

# Create a heatmap with gradient scale, where midpoint is the median of mean_value

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_wrap( ~ medium)

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_wrap( ~ medium) +
  coord_cartesian(xlim = c(0, 0.1))


library(plotly)
ggplotly(ggplot(thr_dat, aes(x = medium, fill = thr)) +
           geom_bar(position = "fill"))

ggplotly(ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
           geom_boxplot() +
           facet_wrap(~ medium))

