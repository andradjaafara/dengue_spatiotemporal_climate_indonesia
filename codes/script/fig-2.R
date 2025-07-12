#### Figure 2: Wavelet analysis and phase lags
#### Province-level phase lag analysis

#### Libraries for plotting
library(ggplot2)
library(dplyr)
library(sf)
library(patchwork)
library(tidyverse)
library(shadowtext)
source("codes/functions.R")

#### Load processed data
source("codes/data_processing.R")

#### Create Figure 2A: Phase lags plot with confidence intervals
fig_2A <- province_phase_stats %>% 
  select(idadmin1, admin1 = province, median_phase_lag, lower_ci, upper_ci) %>% 
  left_join(admin1_EN) %>% 
  mutate(admin1_name = factor(shapeName,
                              levels = as.character(admin1_EN$shapeName)[match(levels(province_phase_stats$province),
                                                                               admin1_indonesia$admin1)])) %>%
  ggplot(aes(x = admin1_name, y = median_phase_lag)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Phase lag from other provinces\n(in months)",
    tag = "A"
  ) +
  coord_cartesian(ylim = c(-5, 5)) +
  scale_y_continuous(breaks = seq(-5, 5, 1)) +
  geom_hline(yintercept = 0, colour = "red", linetype = 2) +
  theme_Publication(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "black")
  )

#### Create Figure 2B: Choropleth map for average phase lags
fig_2B <- ggplot() +
  geom_sf(data = other_shp, fill = "gray80") +  
  geom_sf(data = admin1_shp_with_lags, aes(fill = median_phase_lag), color = "black", size = 0.2) +
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = 0,
    limits = c(-3, 3),
    name = "Average phase lag\n(in months)"
  ) +
  theme_Publication(base_size = 8) +
  labs(tag = "B", x = NULL, y = NULL) +
  geom_shadowtext(
    data = admin1_phase_lags_centroids,
    aes(x = X, y = Y, label = shapeName),
    size = 1.75,  
    color = "white",
    bg.color = 'grey10',
    fontface = "bold",  
    check_overlap = FALSE
  ) +
  coord_sf(xlim = c(95.01098, 141.0194),
           ylim = c(-11.00759, 5.906897))

#### Combine Panels A and B into a single figure
fig_2 <- (fig_2A / fig_2B) + plot_layout(heights = c(1, 1))

#### Save the combined figure
# ggsave("output/fig_2.jpg", fig_2, height = 17, width = 17, unit = "cm", dpi = 600)