#### Figure 5: Urbanization, surveillance, and dengue incidence relationships
#### Analysis of urbanization patterns and surveillance capability

#### Libraries for plotting
library(ggplot2)
library(dplyr)
library(sf)
library(patchwork)
library(shadowtext)
source("codes/functions.R")

#### Load processed data
source("codes/data_processing.R")

#### Create Figure 5A: Urbanization map
fig_5A <- admin1_urbanisation_shp %>% 
  ggplot() +
  geom_sf(data = other_shp, fill = "gray80") +  
  geom_sf(aes(fill = urban_pop_pct_binary), color = 'black', linewidth = 0.2) +
  geom_shadowtext(
    data = admin1_urbanisation_centroids,
    aes(x = X, y = Y, label = shapeName),
    size = 1.75,  
    color = "white",
    bg.color = 'grey10',
    fontface = "bold",  
    check_overlap = FALSE
  ) +
  theme_Publication(base_size = 8) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "gray80",
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 100)
  ) +
  guides(fill = guide_colorbar(
    barheight = 5
  )) +
  coord_sf(xlim = c(95.01098, 141.0194),
           ylim = c(-11.00759, 5.906897)) +
  labs(fill = "Proportion of population\nliving in urban areas (%)", x = NULL, y = NULL, tag = "A") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.5, "cm"))

#### Create Figure 5B: Density of urban areas map
fig_5B <- admin1_urbanisation_shp %>% 
  ggplot() +
  geom_sf(data = other_shp, fill = "gray80") +  
  geom_sf(aes(fill = density_of_urban), color = 'black', linewidth = 0.2) +
  geom_shadowtext(
    data = admin1_urbanisation_centroids,
    aes(x = X, y = Y, label = shapeName),
    size = 1.75,  
    color = "white",
    bg.color = 'grey10',
    fontface = "bold",  
    check_overlap = FALSE
  ) +
  theme_Publication(base_size = 8) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "gray80",
    breaks = c(0, 50, 100, 150, 200),
    limits = c(0, 205)
  ) +
  guides(fill = guide_colorbar(
    barheight = 5
  )) +
  coord_sf(xlim = c(95.01098, 141.0194),
           ylim = c(-11.00759, 5.906897)) +
  labs(fill = "Density of urban areas", x = NULL, y = NULL, tag = "B") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.5, "cm"))

#### Create Figure 5C: Urban surveillance capability map
fig_5C <- admin1_urban_surveillance_shp %>% 
  ggplot() +
  geom_sf(data = other_shp, fill = "gray80") +  
  geom_sf(aes(fill = surveillance_pop_pct), color = 'black', linewidth = 0.2) +
  geom_shadowtext(
    data = admin1_urban_surveillance_centroids,
    aes(x = X, y = Y, label = shapeName),
    size = 1.75,  
    color = "white",
    bg.color = 'grey10',
    fontface = "bold",  
    check_overlap = FALSE
  ) +
  theme_Publication(base_size = 8) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "gray80",
    breaks = c(0, 25, 50, 75, 100),
    limits = c(0, 100)
  ) +
  guides(fill = guide_colorbar(
    barheight = 5
  )) +
  coord_sf(xlim = c(95.01098, 141.0194),
           ylim = c(-11.00759, 5.906897)) +
  labs(fill = "Urban surveillance\ncapability (%)", x = NULL, y = NULL, tag = "C") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.5, "cm"))

#### Create Figure 5D: Urbanization vs incidence scatter plot
fig_5D <- province_data_overall_adjusted_2016_2024 %>% 
  ggplot() +
  geom_point(aes(x = urban_pop_pct, y = incidence_rate_adj_urban_pop, col = "Adjusted", fill = "Adjusted")) +
  geom_point(aes(x = urban_pop_pct, y = incidence_rate, col = "Not-adjusted", fill = "Not-adjusted")) +
  geom_smooth(method = lm, aes(x = urban_pop_pct, y = incidence_rate_adj_urban_pop, fill = "Adjusted", col = "Adjusted")) +
  geom_smooth(method = lm, aes(x = urban_pop_pct, y = incidence_rate, fill = "Not-adjusted", col = "Not-adjusted")) +
  theme_Publication(base_size = 8) +
  scale_colour_Publication(name = "Incidence rate") +
  scale_fill_Publication(name = "Incidence rate") +
  coord_cartesian(ylim = c(0, 1000)) +
  xlim(0, 100) +
  labs(tag = "D", x = "Proportion of population\nliving in urban (%)", y = "Incidence rate per 100,000")

#### Create Figure 5E: Density of urban vs incidence scatter plot
fig_5E <- province_data_overall_adjusted_2016_2024 %>% 
  ggplot() +
  geom_point(aes(x = density_of_urban, y = incidence_rate_adj_urban_pop, col = "Adjusted", fill = "Adjusted")) +
  geom_point(aes(x = density_of_urban, y = incidence_rate, col = "Not-adjusted", fill = "Not-adjusted")) +
  geom_smooth(method = lm, aes(x = density_of_urban, y = incidence_rate_adj_urban_pop, fill = "Adjusted", col = "Adjusted")) +
  geom_smooth(method = lm, aes(x = density_of_urban, y = incidence_rate, fill = "Not-adjusted", col = "Not-adjusted")) +
  theme_Publication(base_size = 8) +
  scale_colour_Publication(name = "Incidence rate") +
  scale_fill_Publication(name = "Incidence rate") +
  ylim(0, 1000) +
  xlim(0, 205) +
  labs(tag = "E", x = "Density of urban", y = "Incidence rate per 100,000")

#### Combine scatter plots
fig_5DE <- fig_5D | fig_5E

#### Combine all panels
fig_5 <- (free(fig_5A) / free(fig_5B) / free(fig_5C) / fig_5DE) + 
  plot_layout(heights = c(1, 1, 1, 1), widths = c(1, 1, 1, 1))

#### Save figure
ggsave("output/fig_5.jpg", fig_5, height = 24, width = 17, unit = "cm", dpi = 600)

#### Calculate correlation statistics
cat("Correlation between urban population % and incidence rate (not adjusted):\n")
urban_pop_corr_not_adj <- cor.test(province_data_overall_adjusted_2016_2024$urban_pop_pct,
                                   province_data_overall_adjusted_2016_2024$incidence_rate)
print(urban_pop_corr_not_adj)

cat("\nCorrelation between urban population % and incidence rate (adjusted):\n")
urban_pop_corr_adj <- cor.test(province_data_overall_adjusted_2016_2024$urban_pop_pct,
                               province_data_overall_adjusted_2016_2024$incidence_rate_adj_urban_pop)
print(urban_pop_corr_adj)

cat("\nCorrelation between urban density and incidence rate (not adjusted):\n")
urban_density_corr_not_adj <- cor.test(province_data_overall_adjusted_2016_2024$density_of_urban,
                                       province_data_overall_adjusted_2016_2024$incidence_rate)
print(urban_density_corr_not_adj)

cat("\nCorrelation between urban density and incidence rate (adjusted):\n")
urban_density_corr_adj <- cor.test(province_data_overall_adjusted_2016_2024$density_of_urban,
                                   province_data_overall_adjusted_2016_2024$incidence_rate_adj_urban_pop)
print(urban_density_corr_adj)