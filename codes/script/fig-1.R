#### Figure 1: Temporal analysis of dengue periodicity
#### Province-level analysis

#### Libraries for plotting
library(tidyverse)
library(lubridate)
library(patchwork)
library(sf)
library(cowplot)
library(biscale)
library(shadowtext)
source("codes/functions.R")

#### Load processed data
source("codes/data_processing.R")

#### Create Figure 1A: Map of average province-level IR 2016-2024
fig_1A_plot <- admin1_ir_shp %>% 
  ggplot() +
  geom_sf(data = other_shp, fill = "gray80") +  
  geom_sf(aes(fill = incidence_rate), color = 'black', linewidth = 0.2,
          show.legend = c(fill = TRUE, color = FALSE)) +
  geom_shadowtext(
    data = admin1_ir_centroids,
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
    limits = c(0, 200)
  ) +
  guides(fill = guide_colorbar(
    barheight = 0.5,
    label = "none"
  )) +
  coord_sf(xlim = c(95.01098, 141.0194),
           ylim = c(-11.00759, 5.906897)) +
  labs(fill = "Average dengue IR\n(2016-2024)", x = NULL, y = NULL, tag = "A")

fig_1A_legend <- ggpubr::get_legend(fig_1A_plot)
fig_1A_plot2 <- fig_1A_plot + theme(legend.position = "none")

fig_1A <- ggdraw() +
  draw_plot(fig_1A_plot2, 0, 0, 1, 1) +
  draw_plot(fig_1A_legend, 0, 0.075, 0.35, 0.25)

ggsave("output/fig_1A.jpg", fig_1A, height = 8, width = 14, unit = "cm")

#### Create Figure 1B: Bivariate map
# Create bivariate legend
legend_bivariate_plot <- bi_legend(pal = "DkViolet",
                                   dim = 3,
                                   xlab = "Higher Median",
                                   ylab = "Higher CV",
                                   size = 7.5) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# Create bivariate classification
map_incidence_rate_bi <- bi_class(admin1_incidence_rate_shp, 
                                  x = median_incidence_rate, 
                                  y = cv_incidence_rate, 
                                  style = "quantile", 
                                  dim = 3)

# Create bivariate map
map_incidence_rate <- map_incidence_rate_bi %>% 
  ggplot() +
  geom_sf(data = other_shp, fill = "gray80") +  
  geom_sf(mapping = aes(fill = bi_class), color = "black", linewidth = 0.2, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  geom_shadowtext(
    data = admin1_incidence_rate_centroids,
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

fig_1B <- ggdraw() +
  draw_plot(map_incidence_rate, 0, 0, 1, 1) +
  draw_plot(legend_bivariate_plot, 0, 0.075, 0.26, 0.4)

ggsave("output/fig_1B.jpg", fig_1B, height = 8, width = 14, unit = "cm")

#### Create Figure 1C: National longitudinal pattern
fig_1C <- dengue_data_admin0 %>% 
  ggplot() +
  geom_vline(
    xintercept = as.numeric(as.Date(paste0(2016:2024, "-01-01"))), 
    linetype = "dashed", 
    color = "gray70",
    size = 0.3
  ) +
  geom_line(aes(x = date, y = incidence_rate), 
            color = "navy", 
            size = 1) +
  scale_y_continuous(
    trans = "log10",
    labels = function(x) sprintf("%g", x),
    name = "Log of monthly dengue incidence rate"
  ) +
  scale_x_date(
    breaks = as.Date(paste0(2016:2024, "-07-01")),
    labels = 2016:2024,
    expand = c(0.01, 0.01),
    name = NULL
  ) +
  theme_Publication(base_size = 8) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 8, margin = margin(r = 0.1)),
    axis.ticks.x = element_blank()
  ) +
  labs(tag = "C")

ggsave("output/fig_1C.jpg", fig_1C, height = 6, width = 14, unit = "cm")

#### Create Figure 1D: Provincial longitudinal heatmap
fig_1D <- dengue_data_admin1_2 %>% 
  left_join(admin1_national_EN) %>% 
  mutate(
    date_start = floor_date(date, "month"),
    date_end = ceiling_date(date, "month") - days(1)
  ) %>%
  ggplot() +
  geom_rect(
    aes(
      xmin = date_start, 
      xmax = date_end + days(1),
      ymin = as.numeric(factor(shapeName)) - 0.5,
      ymax = as.numeric(factor(shapeName)) + 0.5,
      fill = incidence_rate_scaled
    )
  ) +
  geom_vline(
    xintercept = as.numeric(as.Date(paste0(2016:2024, "-01-01"))),
    color = "white",
    linewidth = 0.2
  ) +
  scale_y_discrete(
    limits = rev(unique(admin1_national_EN$shapeName))
  ) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 8) +
  labs(x = NULL, y = "Province", tag = "D") +
  scale_x_date(
    breaks = as.Date(paste0(2016:2024, "-07-01")),
    labels = 2016:2024,
    expand = c(0, 0),
    name = NULL
  ) +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 8),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    panel.spacing = unit(0, "lines")
  ) +
  geom_hline(yintercept = as.numeric(admin1_national_EN$shapeName[c(11, 18, 20, 25, 31, 33, 39)]) + 0.5)

ggsave("output/fig_1D.jpg", fig_1D, height = 12, width = 17, unit = "cm")

#### Create monthly average heatmap
fig_heatmap_monthly_average <- dengue_data_admin1_monthly_avg %>%
  left_join(admin1_national_EN) %>% 
  ggplot(aes(x = month_name, y = shapeName, fill = incidence_rate_scaled)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 8) +
  labs(x = "Month of the year", y = "Province") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  geom_hline(yintercept = as.numeric(admin1_national_EN$shapeName[c(11, 18, 20, 25, 31, 33, 39)]) + 0.5)

ggsave("output/fig_heatmap_monthly_average.jpg", fig_heatmap_monthly_average, height = 12, width = 14, unit = "cm")

#### Create monthly boxplots by region
region2_indonesia_vec <- unique(dengue_data_admin1_boxplot$region2)
region2_filename <- c("sumatera", "java_bali", "nt_maluku_papua", "kalimantan", "sulawesi", "national")
region_height <- c(15, 10, 10, 10, 10, 5)
fig_monthly_cases_barplot <- list()

for (i in seq_len(length(region2_indonesia_vec))) {
  fig_monthly_cases_barplot[[i]] <- dengue_data_admin1_boxplot %>% 
    filter(region2 == region2_indonesia_vec[i]) %>% 
    ggplot(aes(x = month, y = incidence_rate)) +
    geom_boxplot(fill = "#662506") +
    facet_wrap(. ~ shapeName, ncol = 4) +
    theme_Publication(base_size = 8) +
    geom_hline(yintercept = median(dengue_data_admin1_boxplot$incidence_rate),
               linetype = 2, linewidth = 1.25, col = "red") +
    ylim(0, 90) +
    labs(y = "Incidence rate", x = "Month") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1))
  
  ggsave(paste0("output/cases_boxplot/cases_boxplot_", region2_filename[i], ".jpg"),
         fig_monthly_cases_barplot[[i]], height = region_height[i], width = 17, unit = "cm", dpi = 600)
}

#### Combine Figure 1
fig_1 <- (free(fig_1A) / free(fig_1B) / fig_1C / fig_1D) + 
  plot_layout(heights = c(1, 1, 0.7, 1))

# Save combined figure
# ggsave("output/fig_1.jpg", fig_1, height = 28, width = 17, unit = "cm", dpi = 600)
# ggsave("output/fig_1.svg", fig_1, height = 28, width = 17, unit = "cm", dpi = 600)