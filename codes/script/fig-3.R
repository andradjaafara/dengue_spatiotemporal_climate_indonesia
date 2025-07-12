#### Figure 3: Climate variables bivariate maps and distributions
#### Precipitation, temperature, and relative humidity analysis

#### Libraries for plotting
library(ggplot2)
library(dplyr)
library(sf)
library(patchwork)
library(biscale)
library(cowplot)
library(shadowtext)
source("codes/functions.R")

#### Load processed data
source("codes/data_processing.R")

#### Create bivariate legend
legend_bivariate_plot <- bi_legend(pal = "DkViolet",
                                   dim = 3,
                                   xlab = "Higher Median",
                                   ylab = "Higher CV",
                                   size = 7.5) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

#### Create Figure 3A: Precipitation bivariate map
map_precipitation <- map_precipitation_bi %>% 
  ggplot() +
  geom_sf(data = other_shp, fill = "gray80") +  
  geom_sf(mapping = aes(fill = bi_class), color = "black", linewidth = 0.2, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  geom_shadowtext(
    data = admin1_precipitation_centroids,
    aes(x = X, y = Y, label = shapeName),
    size = 1.75,  
    color = "white",
    bg.color = 'grey10',
    fontface = "bold",  
    check_overlap = FALSE
  ) +
  theme_Publication(base_size = 8) +
  coord_sf(xlim = c(95.01098, 141.0194),
           ylim = c(-11.00759, 5.906897)) +
  labs(x = NULL, y = NULL, tag = "A")

fig_3A <- ggdraw() +
  draw_plot(map_precipitation, 0, 0, 1, 1) +
  draw_plot(legend_bivariate_plot, 0, 0.075, 0.26, 0.4)

#### Create Figure 3B: Temperature bivariate map
map_temperature <- map_temperature_bi %>% 
  ggplot() +
  geom_sf(data = other_shp, fill = "gray80") +  
  geom_sf(mapping = aes(fill = bi_class), color = "black", linewidth = 0.2, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  geom_shadowtext(
    data = admin1_temperature_centroids,
    aes(x = X, y = Y, label = shapeName),
    size = 1.75,  
    color = "white",
    bg.color = 'grey10',
    fontface = "bold",  
    check_overlap = FALSE
  ) +
  theme_Publication(base_size = 8) +
  coord_sf(xlim = c(95.01098, 141.0194),
           ylim = c(-11.00759, 5.906897)) +
  labs(x = NULL, y = NULL, tag = "B")

fig_3B <- ggdraw() +
  draw_plot(map_temperature, 0, 0, 1, 1) +
  draw_plot(legend_bivariate_plot, 0, 0.075, 0.26, 0.4)

#### Create Figure 3C: Relative humidity bivariate map
map_rel_humidity <- map_rel_humidity_bi %>% 
  ggplot() +
  geom_sf(data = other_shp, fill = "gray80") +  
  geom_sf(mapping = aes(fill = bi_class), color = "black", linewidth = 0.2, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  geom_shadowtext(
    data = admin1_rel_humidity_centroids,
    aes(x = X, y = Y, label = shapeName),
    size = 1.75,  
    color = "white",
    bg.color = 'grey10',
    fontface = "bold",  
    check_overlap = FALSE
  ) +
  theme_Publication(base_size = 8) +
  coord_sf(xlim = c(95.01098, 141.0194),
           ylim = c(-11.00759, 5.906897)) +
  labs(x = NULL, y = NULL, tag = "C")

fig_3C <- ggdraw() +
  draw_plot(map_rel_humidity, 0, 0, 1, 1) +
  draw_plot(legend_bivariate_plot, 0, 0.075, 0.26, 0.4)

#### Combine figures
fig_3 <- fig_3A / fig_3B / fig_3C
ggsave("output/fig_3.jpg", fig_3, height = 22, width = 17, unit = "cm", dpi = 600)

#### Create monthly distribution boxplots by region
region2_indonesia_vec <- unique(climate_era_2015_2024_month_summary$region2)
region2_filename <- c("sumatera", "java_bali", "nt_maluku_papua", "kalimantan", "sulawesi", "national")
region_height <- c(15, 10, 10, 10, 10, 5)

fig_monthly_precipitation_barplot <- list()
fig_monthly_temperature_barplot <- list()
fig_monthly_rel_humidity_barplot <- list()

for (i in seq_len(length(region2_indonesia_vec))) {
  
  fig_monthly_precipitation_barplot[[i]] <- climate_era_2015_2024_month_summary %>% 
    filter(region2 == region2_indonesia_vec[i]) %>% 
    ggplot(aes(x = month, y = precipitation_era)) +
    geom_boxplot(fill = "#386cb0") +
    facet_wrap(. ~ shapeName, ncol = 4) +
    theme_Publication(base_size = 8) +
    geom_hline(yintercept = median(climate_era_2015_2024_month_summary$precipitation_era),
               linetype = 2, linewidth = 1.25, col = "red") +
    ylim(0, 710) +
    labs(y = "Precipitation (mm)", x = "Month") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))
  
  fig_monthly_temperature_barplot[[i]] <- climate_era_2015_2024_month_summary %>% 
    filter(region2 == region2_indonesia_vec[i]) %>% 
    ggplot(aes(x = month, y = temperature_era)) +
    geom_boxplot(fill = "#fdb462") +
    facet_wrap(. ~ shapeName, ncol = 4) +
    theme_Publication(base_size = 8) +
    geom_hline(yintercept = median(climate_era_2015_2024_month_summary$temperature_era),
               linetype = 2, linewidth = 1.25, col = "red") +
    ylim(15, 30) +
    labs(y = "Temperature (degree C)", x = "Month") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))
  
  fig_monthly_rel_humidity_barplot[[i]] <- climate_era_2015_2024_month_summary %>% 
    filter(region2 == region2_indonesia_vec[i]) %>% 
    ggplot(aes(x = month, y = rel_humidity_era)) +
    geom_boxplot(fill = "#7fc97f") +
    facet_wrap(. ~ shapeName, ncol = 4) +
    theme_Publication(base_size = 8) +
    geom_hline(yintercept = median(climate_era_2015_2024_month_summary$rel_humidity_era),
               linetype = 2, linewidth = 1.25, col = "red") +
    ylim(50, 100) +
    labs(y = "Relative humidity (%)", x = "Month") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))
  
  ggsave(paste0("output/precipitation_boxplot/precipitation_boxplot_", region2_filename[i], ".jpg"),
         fig_monthly_precipitation_barplot[[i]], height = region_height[i], width = 17, unit = "cm", dpi = 600)
  ggsave(paste0("output/temperature_boxplot/temperature_boxplot_", region2_filename[i], ".jpg"),
         fig_monthly_temperature_barplot[[i]], height = region_height[i], width = 17, unit = "cm", dpi = 600)
  ggsave(paste0("output/rel_humidity_boxplot/rel_humidity_boxplot_", region2_filename[i], ".jpg"),
         fig_monthly_rel_humidity_barplot[[i]], height = region_height[i], width = 17, unit = "cm", dpi = 600)
}