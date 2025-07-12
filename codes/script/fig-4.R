#### Figure 4: Climate-dengue correlations and wavelet coherence analysis
#### Comprehensive analysis of climate-dengue relationships

#### Libraries for plotting
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(biwavelet)
library(patchwork)
library(cowplot)
library(snakecase)
source("codes/functions.R")

#### Load processed data
source("codes/data_processing.R")

#### Create Figure 4A: Climate heatmap correlations
fig_corr_climate_incidence <- 
  ggplot(province_corr_long, aes(x = variable, y = shapeName, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 2.25) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1), name = "Correlation") +
  labs(title = "Correlations between climate variables and\ndengue incidence rate by province",
       x = "Climate Variables", y = "Province") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.numeric(province_corr_long$variable[8]) + 0.5) +
  geom_vline(xintercept = as.numeric(province_corr_long$variable[12]) + 0.5) +
  geom_vline(xintercept = as.numeric(province_corr_long$variable[16]) + 0.5)

ggsave("output/fig_corr_climate_incidence.jpg", fig_corr_climate_incidence, height = 17, width = 17, unit = "cm", dpi = 600)

#### Create monthly climate heatmaps
fig_climate_monthly_A <- climate_era_2015_2024_nolag %>% 
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
      fill = precipitation_era_scaled
    )
  ) +
  geom_vline(
    xintercept = as.numeric(as.Date(paste0(2016:2024, "-01-01"))),
    color = "white",
    linewidth = 0.2
  ) +
  scale_y_discrete(
    limits = levels(admin1_national_EN$shapeName)
  ) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 8) +
  labs(x = NULL, y = "Province", tag = "A") +
  scale_x_date(
    breaks = as.Date(paste0(2015:2024, "-07-01")),
    labels = 2015:2024,
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

fig_climate_monthly_B <- climate_era_2015_2024_nolag %>% 
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
      fill = temperature_era_scaled
    )
  ) +
  geom_vline(
    xintercept = as.numeric(as.Date(paste0(2016:2024, "-01-01"))),
    color = "white",
    linewidth = 0.2
  ) +
  scale_y_discrete(
    limits = levels(admin1_national_EN$shapeName)
  ) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 8) +
  labs(x = NULL, y = "Province", tag = "B") +
  scale_x_date(
    breaks = as.Date(paste0(2015:2024, "-07-01")),
    labels = 2015:2024,
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

fig_climate_monthly_C <- climate_era_2015_2024_nolag %>% 
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
      fill = rel_humidity_era_scaled
    )
  ) +
  geom_vline(
    xintercept = as.numeric(as.Date(paste0(2016:2024, "-01-01"))),
    color = "white",
    linewidth = 0.2
  ) +
  scale_y_discrete(
    limits = levels(admin1_national_EN$shapeName)
  ) +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 8) +
  labs(x = NULL, y = "Province", tag = "C") +
  scale_x_date(
    breaks = as.Date(paste0(2015:2024, "-07-01")),
    labels = 2015:2024,
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

#### Create overall monthly patterns
fig_climate_overall_A <- climate_era_2015_2024_monthly_avg_nolag %>%
  left_join(admin1_national_EN) %>% 
  ggplot(aes(x = month_name, y = shapeName, fill = precipitation_era_scaled)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 8) +
  labs(x = NULL, y = "Province", title = "Precipitation", tag = "A") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  geom_hline(yintercept = as.numeric(admin1_national_EN$shapeName[c(11, 18, 20, 25, 31, 33, 39)]) + 0.5)

fig_climate_overall_B <- climate_era_2015_2024_monthly_avg_nolag %>%
  left_join(admin1_national_EN) %>% 
  ggplot(aes(x = month_name, y = shapeName, fill = temperature_era_scaled)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 8) +
  labs(x = NULL, y = NULL, title = "Temperature") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  geom_hline(yintercept = as.numeric(admin1_national_EN$shapeName[c(11, 18, 20, 25, 31, 33, 39)]) + 0.5)

fig_climate_overall_C <- climate_era_2015_2024_monthly_avg_nolag %>%
  left_join(admin1_national_EN) %>% 
  ggplot(aes(x = month_name, y = shapeName, fill = rel_humidity_era_scaled)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_Publication(base_size = 8) +
  labs(x = NULL, y = NULL, title = "Rel. humidity") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  geom_hline(yintercept = as.numeric(admin1_national_EN$shapeName[c(11, 18, 20, 25, 31, 33, 39)]) + 0.5)

#### Create Figure 4B: ONI and dengue time series
fig_4B <- dengue_oni_national %>% 
  select(date, oni, incidence_rate_scaled) %>% 
  pivot_longer(-date, names_to = "var", values_to = "val") %>% 
  mutate(var = factor(var, levels = c("incidence_rate_scaled", "oni"),
                      labels = c("Incidence rate", "ONI"))) %>% 
  ggplot(aes(x = date, y = val, colour = var)) +
  theme_Publication(base_size = 8) +
  labs(x = NULL, y = "Incidence rate/ONI", colour = NULL, tag = "B") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_colour_Publication() +
  geom_hline(yintercept = -0.5, colour = "blue", linewidth = 0.25, linetype = 2) +
  geom_hline(yintercept = -1, colour = "blue", linewidth = 0.25) +
  geom_hline(yintercept = -1.5, colour = "blue", linewidth = 1) +
  geom_hline(yintercept = -2, colour = "blue", linewidth = 1.5) +
  geom_hline(yintercept = 0.5, colour = "red", linewidth = 0.25, linetype = 2) +
  geom_hline(yintercept = 1, colour = "red", linewidth = 0.25) +
  geom_hline(yintercept = 1.5, colour = "red", linewidth = 1) +
  geom_hline(yintercept = 2, colour = "red", linewidth = 1.5) +
  geom_line(linewidth = 1.75)

#### Create Figure 4C: Phase angle correlations
fig_corr_phase_climate_incidence <- 
  ggplot(corr_phase_climate_incidence, aes(x = variable, y = shapeName, fill = corr)) +
  geom_tile() +
  geom_text(aes(label = round(corr, 2)), color = "black", size = 2.25) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1), name = "Correlation") +
  labs(title = "Phase angles correlations",
       x = NULL, y = "Province", tag = "C") +
  theme_Publication(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  geom_vline(xintercept = as.numeric(corr_phase_climate_incidence$variable[4]) + 0.5) +
  geom_vline(xintercept = as.numeric(corr_phase_climate_incidence$variable[8]) + 0.5) +
  geom_hline(yintercept = as.numeric(admin1_national_EN$shapeName[c(11, 18, 20, 25, 31, 33, 39)]) + 0.5)

#### Create ONI correlation plots
fig_corr_oni_incidence <- ggplot(national_province_oni_corr, 
                                 aes(x = variable, y = shapeName, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 2.25) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1), name = "Correlation") +
  labs(x = NULL, y = NULL,
       title = "Incidence correlations") +
  theme_Publication(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = as.numeric(admin1_national_EN$shapeName[c(11, 18, 20, 25, 31, 33, 39)]) + 0.5)

fig_corr_pvalue_phase_climate_incidence <- 
  ggplot(corr_pvalue_phase_climate_incidence, aes(x = variable, y = shapeName, fill = p_categories)) +
  geom_tile() +
  geom_text(aes(label = p_categories), color = "black", size = 2.25) +
  scale_fill_manual(values = c("**" = "darkred", "*" = "red", 
                               "ns" = "lightgray"),
                    name = "Significance") +
  labs(title = "Phase angles correlation p-values",
       x = NULL, y = "Province") +
  theme_Publication(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  geom_vline(xintercept = as.numeric(corr_phase_climate_incidence$variable[4]) + 0.5) +
  geom_vline(xintercept = as.numeric(corr_phase_climate_incidence$variable[8]) + 0.5) +
  geom_hline(yintercept = as.numeric(admin1_national_EN$shapeName[c(11, 18, 20, 25, 31, 33, 39)]) + 0.5)

fig_corr_pvalue_oni_incidence <- ggplot(national_province_oni_corr, 
                                        aes(x = variable, y = shapeName, fill = p_categories)) +
  geom_tile() +
  geom_text(aes(label = p_categories), color = "black", size = 2.25) +
  scale_fill_manual(values = c("**" = "darkred", "*" = "red", 
                               "ns" = "lightgray"),
                    name = "Significance") +
  labs(x = NULL, y = NULL,
       title = "Incidence correlations\np-value") +
  theme_Publication(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = as.numeric(admin1_national_EN$shapeName[c(11, 18, 20, 25, 31, 33, 39)]) + 0.5)

#### Create ONI visualization
fig_oni <- oni_2015_2024 %>% 
  ggplot(aes(x = date, y = oni)) +
  theme_Publication(base_size = 8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-3, 3, 0.5), limits = c(-2.75, 2.75)) +
  labs(x = NULL, y = "ONI") +
  geom_hline(yintercept = -0.5, colour = "blue", linewidth = 0.25, linetype = 2) +
  geom_hline(yintercept = -1, colour = "blue", linewidth = 0.25) +
  geom_hline(yintercept = -1.5, colour = "blue", linewidth = 1) +
  geom_hline(yintercept = -2, colour = "blue", linewidth = 1.5) +
  geom_hline(yintercept = 0.5, colour = "red", linewidth = 0.25, linetype = 2) +
  geom_hline(yintercept = 1, colour = "red", linewidth = 0.25) +
  geom_hline(yintercept = 1.5, colour = "red", linewidth = 1) +
  geom_hline(yintercept = 2, colour = "red", linewidth = 1.5) +
  geom_line(linewidth = 1.25)

#### Combine figures
fig_climate_monthly <- fig_climate_monthly_A / fig_climate_monthly_B / fig_climate_monthly_C
fig_4A <- fig_climate_overall_A | fig_climate_overall_B | fig_climate_overall_C
fig_4C <- (fig_corr_phase_climate_incidence | fig_corr_oni_incidence) + plot_layout(widths = c(1, 0.35))
fig_pvalue <- (fig_corr_pvalue_phase_climate_incidence | fig_corr_pvalue_oni_incidence) + plot_layout(widths = c(1, 0.35))

fig_4 <- (fig_4A / free(fig_4B) / fig_4C) + plot_layout(heights = c(1, 0.8, 1.2))

#### Save figures
ggsave("output/fig_climate_monthly.jpg", fig_climate_monthly, height = 24, width = 17, unit = "cm", dpi = 600)
ggsave("output/fig_4A.jpg", fig_4A, height = 12, width = 17, unit = "cm", dpi = 600)
ggsave("output/fig_4.jpg", fig_4, height = 28, width = 17, unit = "cm", dpi = 600)
ggsave("output/fig_pvalue.jpg", fig_pvalue, height = 14, width = 17, unit = "cm", dpi = 600)
ggsave("output/fig_4C.jpg", fig_4C, height = 14, width = 17, unit = "cm", dpi = 600)
ggsave("output/fig_oni.jpg", fig_oni, height = 12, width = 17, unit = "cm", dpi = 600)
ggsave("output/fig_corr_phase_climate_incidence.jpg", fig_corr_phase_climate_incidence, height = 17, width = 17, unit = "cm", dpi = 600)
ggsave("output/fig_corr_oni_incidence.jpg", fig_corr_oni_incidence, height = 17, width = 10, unit = "cm", dpi = 600)

#### Generate wavelet coherence plots for each province and climate variable
admin1_EN2 <- admin1_national_EN %>% left_join(admin1_indonesia_national)

#### Precipitation coherence plots
for (i in seq_len(nrow(admin1_national_EN))) {
  prep_wavelet <- prepare_wavelet_data(merged_data = climate_incidence_data,
                                       province_name = admin1_EN2$admin1[i],
                                       climate_var = "precipitation_era_scaled",
                                       incidence_var = "incidence_rate_scaled")
  
  dat1 <- cbind(prep_wavelet$data[,1], prep_wavelet$data[,3])
  dat2 <- cbind(prep_wavelet$data[,1], prep_wavelet$data[,2])
  
  wtc.t1t2 <- wtc(dat1, dat2, nrands = 300, dj = 1/12)
  
  provnum <- if (i < 10) paste0("0", i) else as.character(i)
  provname <- as.character(admin1_EN2$shapeName[i])
  outfilename <- paste0("output/precipitation_coherence/precipitation_coherence_", provnum, "_",
                        to_snake_case(provname), ".jpg")
  
  jpeg(outfilename, width = 8, height = 5, unit = "cm", res = 600) 
  par(oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 7.5) + 0.1,
      cex = 0.67, cex.main = 0.67, cex.axis = 0.67, cex.lab = 0.67, cex.sub = 0.67)
  plot(wtc.t1t2, plot.cb = TRUE, plot.phase = TRUE, type = "power.corr.norm", 
       main = admin1_EN2$shapeName[i], lwd.sig = 1.5)
  abline(h = 6/12, lty = 2, col = "blue", lwd = 1.5)
  abline(h = -6/12, lty = 2, col = "blue", lwd = 1.5)
  dev.off()
}

#### Temperature coherence plots
for (i in seq_len(nrow(admin1_national_EN))) {
  prep_wavelet <- prepare_wavelet_data(merged_data = climate_incidence_data,
                                       province_name = admin1_EN2$admin1[i],
                                       climate_var = "temperature_era_scaled",
                                       incidence_var = "incidence_rate_scaled")
  
  dat1 <- cbind(prep_wavelet$data[,1], prep_wavelet$data[,3])
  dat2 <- cbind(prep_wavelet$data[,1], prep_wavelet$data[,2])
  
  wtc.t1t2 <- wtc(dat1, dat2, nrands = 300, dj = 1/12)
  
  provnum <- if (i < 10) paste0("0", i) else as.character(i)
  provname <- as.character(admin1_EN2$shapeName[i])
  outfilename <- paste0("output/temperature_coherence/temperature_coherence_", provnum, "_",
                        to_snake_case(provname), ".jpg")
  
  jpeg(outfilename, width = 8, height = 5, unit = "cm", res = 600) 
  par(oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 7.5) + 0.1,
      cex = 0.67, cex.main = 0.67, cex.axis = 0.67, cex.lab = 0.67, cex.sub = 0.67)
  plot(wtc.t1t2, plot.cb = TRUE, plot.phase = TRUE, type = "power.corr.norm", 
       main = admin1_EN2$shapeName[i], lwd.sig = 1.5)
  abline(h = 6/12, lty = 2, col = "blue", lwd = 1.5)
  abline(h = -6/12, lty = 2, col = "blue", lwd = 1.5)
  dev.off()
}

#### Relative humidity coherence plots
for (i in seq_len(nrow(admin1_national_EN))) {
  prep_wavelet <- prepare_wavelet_data(merged_data = climate_incidence_data,
                                       province_name = admin1_EN2$admin1[i],
                                       climate_var = "rel_humidity_era_scaled",
                                       incidence_var = "incidence_rate_scaled")
  
  dat1 <- cbind(prep_wavelet$data[,1], prep_wavelet$data[,3])
  dat2 <- cbind(prep_wavelet$data[,1], prep_wavelet$data[,2])
  
  wtc.t1t2 <- wtc(dat1, dat2, nrands = 300, dj = 1/12)
  
  provnum <- if (i < 10) paste0("0", i) else as.character(i)
  provname <- as.character(admin1_EN2$shapeName[i])
  outfilename <- paste0("output/rel_humidity_coherence/rel_humidity_coherence_", provnum, "_",
                        to_snake_case(provname), ".jpg")
  
  jpeg(outfilename, width = 8, height = 5, unit = "cm", res = 600) 
  par(oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 7.5) + 0.1,
      cex = 0.67, cex.main = 0.67, cex.axis = 0.67, cex.lab = 0.67, cex.sub = 0.67)
  plot(wtc.t1t2, plot.cb = TRUE, plot.phase = TRUE, type = "power.corr.norm", 
       main = admin1_EN2$shapeName[i], lwd.sig = 1.5)
  abline(h = 6/12, lty = 2, col = "blue", lwd = 1.5)
  abline(h = -6/12, lty = 2, col = "blue", lwd = 1.5)
  dev.off()
}

#### ONI coherence plots
for (i in seq_len(nrow(admin1_national_EN))) {
  prep_wavelet <- prepare_wavelet_data(merged_data = climate_incidence_data,
                                       province_name = admin1_EN2$admin1[i],
                                       climate_var = "oni",
                                       incidence_var = "incidence_rate_scaled")
  
  dat1 <- cbind(prep_wavelet$data[,1], prep_wavelet$data[,3])
  dat2 <- cbind(prep_wavelet$data[,1], prep_wavelet$data[,2])
  
  wtc.t1t2 <- wtc(dat1, dat2, nrands = 300, dj = 1/12)
  
  provnum <- if (i < 10) paste0("0", i) else as.character(i)
  provname <- as.character(admin1_EN2$shapeName[i])
  outfilename <- paste0("output/oni_coherence/oni_coherence_", provnum, "_",
                        to_snake_case(provname), ".jpg")
  
  jpeg(outfilename, width = 8, height = 5, unit = "cm", res = 600) 
  par(oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 7.5) + 0.1,
      cex = 0.67, cex.main = 0.67, cex.axis = 0.67, cex.lab = 0.67, cex.sub = 0.67)
  plot(wtc.t1t2, plot.cb = TRUE, plot.phase = TRUE, type = "power.corr.norm", 
       main = admin1_EN2$shapeName[i], lwd.sig = 1.5)
  abline(h = 6/12, lty = 2, col = "blue", lwd = 1.5)
  abline(h = -6/12, lty = 2, col = "blue", lwd = 1.5)
  dev.off()
}