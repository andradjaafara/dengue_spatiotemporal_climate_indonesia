#### Data Processing Script for Dengue Analysis
#### This script processes all data needed for figures

#### Libraries for data processing
library(tidyverse)
library(lubridate)
library(sf)
library(janitor)
library(biwavelet)
library(reshape2)
library(ncf)
library(readxl)
library(confintr)
library(snakecase)
library(dtwclust)
library(rsoi)
source("codes/functions.R")

#### Read and prepare administrative data
admin2_indonesia <- read_csv("data/admin2_38.csv") %>% 
  clean_names()

admin1_indonesia <- admin2_indonesia %>% 
  select(region, idadmin1, admin1) %>% 
  unique()

#### Read shapefiles
admin1_shp <- read_sf(dsn="data/shapefiles/admin1", layer="admin1_38", stringsAsFactors = FALSE)
other_shp <- read_sf(dsn="data/shapefiles/other", layer="idn_neighbours", stringsAsFactors = FALSE)

#### Prepare administrative name mappings
admin1_EN <- admin1_shp %>% 
  st_drop_geometry() %>% 
  select(shapeName, idadmin1) %>% 
  arrange(idadmin1)
admin1_EN$shapeName <- factor(admin1_EN$shapeName, levels = rev(admin1_EN$shapeName))

admin1_national_EN <- bind_rows(
  admin1_EN %>% mutate(shapeName = as.character(shapeName)),
  tibble(shapeName = "Indonesia", idadmin1 = 99)
) %>% 
  mutate(shapeName = factor(shapeName, levels = c("Indonesia", levels(admin1_EN$shapeName))))

admin1_indonesia_national <- bind_rows(
  admin1_indonesia,
  tibble(region = "NATIONAL", idadmin1 = 99, admin1 = "INDONESIA")
)

#### Process dengue surveillance data at admin1 level
dengue_data_admin1 <- readRDS("data/data_cases_deaths_pop_2016_2024_38.rds") %>% 
  select(-admin1, -admin2, -region) %>% 
  left_join(admin2_indonesia) %>% 
  group_by(region, idadmin1, admin1, year, month) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    pop = sum(pop)
  ) %>% 
  ungroup() %>% 
  mutate(
    incidence_rate = (cases / pop) * 100000,
    cases_add = ifelse(cases == 0, 0.01, cases),
    incidence_rate_add = (cases_add / pop) * 100000,
    month_name = factor(month, levels = 1:12, 
                        labels = c("January", "February", "March", "April", "May", "June", 
                                   "July", "August", "September", "October", "November", "December")),
    date = ymd(paste0(year, "-", month, "-01"))
  ) %>% 
  group_by(admin1) %>% 
  mutate(
    incidence_rate_log = log(incidence_rate_add),
    incidence_rate_scaled = as.numeric(scale(incidence_rate_log))
  ) %>% 
  ungroup() %>% 
  arrange(idadmin1, date)

#### Process dengue surveillance data at national level
dengue_data_admin0 <- readRDS("data/data_cases_deaths_pop_2016_2024_38.rds") %>% 
  group_by(year, month) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    pop = sum(pop)
  ) %>% 
  ungroup() %>% 
  mutate(
    region = "NATIONAL",
    idadmin1 = 99,
    admin1 = "INDONESIA",
    cases_add = ifelse(cases == 0, 1, cases),
    incidence_rate = (cases / pop) * 100000,
    incidence_rate_add = (cases_add / pop) * 100000,
    month_name = factor(month, levels = 1:12, 
                        labels = c("January", "February", "March", "April", "May", "June", 
                                   "July", "August", "September", "October", "November", "December")),
    date = ymd(paste0(year, "-", month, "-01"))
  ) %>% 
  group_by(admin1) %>% 
  mutate(
    incidence_rate_log = log(incidence_rate_add),
    incidence_rate_scaled = as.numeric(scale(incidence_rate_log))
  ) %>% 
  ungroup()

#### Combine provincial and national data
dengue_data_admin1_2 <- bind_rows(dengue_data_admin1, dengue_data_admin0)

#### Calculate monthly averages across years
dengue_data_admin1_monthly_avg <- dengue_data_admin1_2 %>% 
  group_by(region, idadmin1, admin1, month, month_name) %>% 
  summarise(incidence_rate = median(incidence_rate)) %>% 
  ungroup() %>% 
  arrange(idadmin1, month) %>% 
  group_by(admin1) %>% 
  mutate(
    incidence_rate_log = log(incidence_rate),
    incidence_rate_scaled = as.numeric(scale(incidence_rate))
  ) %>% 
  ungroup()

#### Calculate yearly summaries
dengue_data_admin1_yearly <- dengue_data_admin1_2 %>% 
  group_by(region, idadmin1, admin1, year) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    pop = mean(pop)
  ) %>% 
  ungroup() %>% 
  mutate(
    incidence_rate = (cases / pop) * 100000,
    cfr = deaths / cases * 100
  )

#### Calculate overall 2016-2024 average incidence rates
dengue_data_admin1_overall_2016_2024 <- dengue_data_admin1_yearly %>% 
  group_by(region, idadmin1, admin1) %>% 
  summarise(incidence_rate = mean(incidence_rate)) %>% 
  ungroup() %>% 
  arrange(idadmin1)

#### Calculate summary statistics including coefficient of variation
dengue_data_admin1_overall_summary <- dengue_data_admin1 %>% 
  mutate(month = month(date)) %>% 
  group_by(region, idadmin1, admin1) %>% 
  summarise(
    median_incidence_rate = median(incidence_rate),
    mean_incidence_rate = mean(incidence_rate),
    sd_incidence_rate = sd(incidence_rate),
    cv_incidence_rate = sd_incidence_rate / mean_incidence_rate
  ) %>% 
  ungroup()

#### Prepare data for boxplot analysis by regions
dengue_data_admin1_boxplot <- dengue_data_admin1 %>% 
  mutate(month = factor(format(date, "%B"), levels = month.name)) %>% 
  left_join(admin1_national_EN) %>%
  mutate(shapeName = factor(as.character(shapeName), levels = rev(levels(shapeName)))) %>% 
  mutate(region2 = case_when(
    region %in% c("NUSA TENGGARA", "MALUKU", "PAPUA") ~ "NUSA TENGGARA, MALUKU, & PAPUA",
    region == "SUMATERA" ~ "SUMATERA",
    region == "JAVA & BALI" ~ "JAVA & BALI",
    region == "KALIMANTAN" ~ "KALIMANTAN",
    region == "SULAWESI" ~ "SULAWESI",
    TRUE ~ region
  )) %>% 
  arrange(idadmin1, year)

#### Prepare spatial data for mapping
admin1_ir_shp <- admin1_shp %>% 
  left_join(dengue_data_admin1_overall_2016_2024)

#### Calculate centroids for map labels
admin1_ir_centroids <- admin1_ir_shp %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = admin1_ir_shp$shapeName) %>%
  st_drop_geometry()

#### Prepare bivariate mapping data
admin1_incidence_rate_shp <- admin1_shp %>% 
  left_join(dengue_data_admin1_overall_summary)

admin1_incidence_rate_centroids <- admin1_incidence_rate_shp %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = admin1_incidence_rate_shp$shapeName) %>%
  st_drop_geometry()

#### Wavelet analysis preparation and execution
# Extract unique admin1 names
admin1_names <- admin2_indonesia %>% pull(admin1) %>% unique()

# Prepare province-level dataset for wavelet analysis
province_data <- dengue_data_admin1

# Prepare data for wavelet analysis at the province level
province_ts <- prepare_for_wavelet(province_data, "admin1", "incidence_rate_scaled")

#### Run wavelet analysis at the province level
province_wavelet <- conduct_wavelet_analysis(
  province_ts$ts_data,
  province_ts$valid_groups
)

#### Calculate phase differences at the province level
province_phase_months <- create_phase_matrix(
  province_wavelet$phase_angles,
  province_ts$valid_groups
)

#### Calculate average phase lags
province_phase_lags <- calculate_phase_lags(province_phase_months)

#### Create data frames with phase lags for visualization
province_lags_df <- data.frame(
  province = names(province_phase_lags),
  phase_lag = province_phase_lags,
  region = province_data$region[match(names(province_phase_lags), province_data$admin1)]
)

#### Calculate phase statistics from the phase difference matrix
province_phase_stats <- calculate_phase_stats(province_phase_months) 
province_phase_stats <- province_phase_stats %>% 
  left_join(admin1_indonesia %>% 
              select(region, idadmin1, province = admin1) %>% 
              mutate(province = factor(province, levels = province_phase_stats$province)))

#### Join phase lag data with province shapefile for mapping
admin1_shp_with_lags <- admin1_shp %>%
  left_join(province_phase_stats)

#### Calculate centroids for phase lag map labels
admin1_phase_lags_centroids <- admin1_shp_with_lags %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = admin1_shp_with_lags$shapeName) %>%
  st_drop_geometry()

#### Read and process climate data
#### Downloaded from Copernicus Climate Data Store: https://cds.climate.copernicus.eu/datasets
#### Using KrigR package then aggregated to monthly data based on admin levels

#### Precipitation ERA5 Land data
prec_admin0_era_2015_2024 <- readRDS("data/prec_monthly_admin0.rds") %>% 
  select(admin1, date, precipitation_mm_pop_weighted) %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  select(region, shapeName, idadmin1, admin1, date, precipitation = precipitation_mm_pop_weighted) %>% 
  as_tibble() %>% 
  arrange(idadmin1, date)

prec_admin1_era_2015_2024 <- readRDS("data/prec_monthly_admin1.rds") %>% 
  select(idadmin1, date, precipitation_mm_pop_weighted) %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  select(region, shapeName, idadmin1, admin1, date, precipitation = precipitation_mm_pop_weighted) %>% 
  as_tibble() %>% 
  arrange(idadmin1, date)

prec_admin1_era_2015_2024 <- bind_rows(prec_admin1_era_2015_2024, prec_admin0_era_2015_2024)

#### Temperature ERA5 Land data
temp_admin0_era_2015_2024 <- readRDS("data/temp_monthly_admin0.rds") %>% 
  select(admin1, date, temperature_celsius_pop_weighted) %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  select(region, shapeName, idadmin1, admin1, date, temperature = temperature_celsius_pop_weighted) %>% 
  as_tibble() %>% 
  arrange(idadmin1, date)

temp_admin1_era_2015_2024 <- readRDS("data/temp_monthly_admin1.rds") %>% 
  select(shapeName, idadmin1, date, temperature_celsius_pop_weighted) %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  select(region, shapeName, idadmin1, admin1, date, temperature = temperature_celsius_pop_weighted) %>% 
  as_tibble() %>% 
  arrange(idadmin1, date)

temp_admin1_era_2015_2024 <- bind_rows(temp_admin1_era_2015_2024, temp_admin0_era_2015_2024)

#### Relative humidity ERA5 Land data
humi_admin0_era_2015_2024 <- readRDS("data/humid_monthly_admin0.rds") %>% 
  select(admin1, date, rhumidity_pop_weighted) %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  select(region, shapeName, idadmin1, admin1, date, rel_humidity = rhumidity_pop_weighted) %>% 
  as_tibble() %>% 
  arrange(idadmin1, date)

humi_admin1_era_2015_2024 <- readRDS("data/humid_monthly_admin1.rds") %>% 
  select(shapeName, idadmin1, date, rhumidity_pop_weighted) %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  select(region, shapeName, idadmin1, admin1, date, rel_humidity = rhumidity_pop_weighted) %>% 
  as_tibble() %>% 
  arrange(idadmin1, date)

humi_admin1_era_2015_2024 <- bind_rows(humi_admin1_era_2015_2024, humi_admin0_era_2015_2024)

#### Oceanic Nino Index
#### The Oceanic Nino Index is average sea surface temperature 
#### in the Nino 3.4 region (120W to 170W) averaged over three months
oni_2015_2024 <- download_oni() %>% 
  clean_names() %>% 
  filter(year(date) >= 2015) %>% 
  arrange(date)

#### Create lagged climate data up to 3 months
#### Precipitation lagged data
prec_admin1_era_2015_2024_lag1 <- prec_admin1_era_2015_2024 %>% 
  mutate(date = date %m+% months(1)) %>% 
  select(region, idadmin1, admin1, shapeName, date, precipitation_era_lag1 = precipitation)

prec_admin1_era_2015_2024_lag2 <- prec_admin1_era_2015_2024 %>% 
  mutate(date = date %m+% months(2)) %>% 
  select(region, idadmin1, admin1, shapeName, date, precipitation_era_lag2 = precipitation)

prec_admin1_era_2015_2024_lag3 <- prec_admin1_era_2015_2024 %>% 
  mutate(date = date %m+% months(3)) %>% 
  select(region, idadmin1, admin1, shapeName, date, precipitation_era_lag3 = precipitation)

#### Temperature lagged data
temp_admin1_era_2015_2024_lag1 <- temp_admin1_era_2015_2024 %>% 
  mutate(date = date %m+% months(1)) %>% 
  select(region, idadmin1, admin1, shapeName, date, temperature_era_lag1 = temperature)

temp_admin1_era_2015_2024_lag2 <- temp_admin1_era_2015_2024 %>% 
  mutate(date = date %m+% months(2)) %>% 
  select(region, idadmin1, admin1, shapeName, date, temperature_era_lag2 = temperature)

temp_admin1_era_2015_2024_lag3 <- temp_admin1_era_2015_2024 %>% 
  mutate(date = date %m+% months(3)) %>% 
  select(region, idadmin1, admin1, shapeName, date, temperature_era_lag3 = temperature)

#### Relative humidity lagged data
humi_admin1_era_2015_2024_lag1 <- humi_admin1_era_2015_2024 %>% 
  mutate(date = date %m+% months(1)) %>% 
  select(region, idadmin1, admin1, shapeName, date, rel_humidity_era_lag1 = rel_humidity)

humi_admin1_era_2015_2024_lag2 <- humi_admin1_era_2015_2024 %>% 
  mutate(date = date %m+% months(2)) %>% 
  select(region, idadmin1, admin1, shapeName, date, rel_humidity_era_lag2 = rel_humidity)

humi_admin1_era_2015_2024_lag3 <- humi_admin1_era_2015_2024 %>% 
  mutate(date = date %m+% months(3)) %>% 
  select(region, idadmin1, admin1, shapeName, date, rel_humidity_era_lag3 = rel_humidity)

#### ONI lagged data
oni_2015_2024_lag1 <- oni_2015_2024 %>% 
  mutate(date = date %m+% months(1)) %>% 
  select(date, oni_lag1 = oni)

oni_2015_2024_lag2 <- oni_2015_2024 %>% 
  mutate(date = date %m+% months(2)) %>% 
  select(date, oni_lag2 = oni)

oni_2015_2024_lag3 <- oni_2015_2024 %>% 
  mutate(date = date %m+% months(3)) %>% 
  select(date, oni_lag3 = oni)

#### Combine all climate data with lags
prec_admin1_era_2016_2024 <- prec_admin1_era_2015_2024 %>% 
  select(region, idadmin1, admin1, shapeName, date, precipitation_era = precipitation) %>% 
  left_join(prec_admin1_era_2015_2024_lag1) %>% 
  left_join(prec_admin1_era_2015_2024_lag2) %>% 
  left_join(prec_admin1_era_2015_2024_lag3) %>% 
  filter(year(date) %in% 2016:2024)

temp_admin1_era_2016_2024 <- temp_admin1_era_2015_2024 %>% 
  select(region, idadmin1, admin1, shapeName, date, temperature_era = temperature) %>% 
  left_join(temp_admin1_era_2015_2024_lag1) %>%
  left_join(temp_admin1_era_2015_2024_lag2) %>%
  left_join(temp_admin1_era_2015_2024_lag3) %>%
  filter(year(date) %in% 2016:2024)

humi_admin1_era_2016_2024 <- humi_admin1_era_2015_2024 %>% 
  select(region, idadmin1, admin1, shapeName, date, rel_humidity_era = rel_humidity) %>% 
  left_join(humi_admin1_era_2015_2024_lag1) %>%
  left_join(humi_admin1_era_2015_2024_lag2) %>%
  left_join(humi_admin1_era_2015_2024_lag3) %>%
  filter(year(date) %in% 2016:2024)

oni_2016_2024 <- oni_2015_2024 %>% 
  select(date, oni) %>% 
  left_join(oni_2015_2024_lag1) %>%
  left_join(oni_2015_2024_lag2) %>%
  left_join(oni_2015_2024_lag3) %>%
  filter(year(date) %in% 2016:2024)

#### Create comprehensive climate dataset
climate_2016_2024 <- prec_admin1_era_2016_2024 %>% 
  left_join(temp_admin1_era_2016_2024) %>% 
  left_join(humi_admin1_era_2016_2024) %>% 
  left_join(oni_2016_2024)

#### Merge climate and dengue incidence data
climate_incidence_data <- climate_2016_2024 %>%
  left_join(dengue_data_admin1_2 %>% 
              select(idadmin1, admin1, date, incidence_rate, incidence_rate_scaled), 
            by = c("idadmin1", "admin1", "date")) %>% 
  group_by(region, idadmin1, admin1) %>% 
  mutate(
    precipitation_era_scaled = as.numeric(scale(log(precipitation_era))),
    precipitation_era_lag1_scaled = as.numeric(scale(log(precipitation_era_lag1))),
    precipitation_era_lag2_scaled = as.numeric(scale(log(precipitation_era_lag2))),
    precipitation_era_lag3_scaled = as.numeric(scale(log(precipitation_era_lag3))),
    temperature_era_scaled = as.numeric(scale(temperature_era)),
    temperature_era_lag1_scaled = as.numeric(scale(temperature_era_lag1)),
    temperature_era_lag2_scaled = as.numeric(scale(temperature_era_lag2)),
    temperature_era_lag3_scaled = as.numeric(scale(temperature_era_lag3)),
    rel_humidity_era_scaled = as.numeric(scale(rel_humidity_era)),
    rel_humidity_era_lag1_scaled = as.numeric(scale(rel_humidity_era_lag1)),
    rel_humidity_era_lag2_scaled = as.numeric(scale(rel_humidity_era_lag2)),
    rel_humidity_era_lag3_scaled = as.numeric(scale(rel_humidity_era_lag3))
  ) %>% 
  ungroup()

#### Define climate variables for correlation analysis
climate_variables <- colnames(climate_incidence_data)[c(18:21, 24:35)]

#### Calculate province-level correlations with climate variables
provinces <- unique(climate_incidence_data$admin1)

#### Create empty dataframe to store correlation results
province_corr <- data.frame(
  admin1 = rep(provinces, each = 16),
  variable = rep(climate_variables, times = length(provinces)),
  correlation = NA_real_,
  p_value = NA_real_,
  lower_ci = NA_real_,
  upper_ci = NA_real_,
  stringsAsFactors = FALSE
)

#### Calculate correlations for each province and climate variable
for (province in provinces) {
  province_data <- climate_incidence_data %>% filter(admin1 == province)
  
  for (var in unique(province_corr$variable)) {
    corr_results <- calculate_correlation(province_data[[var]], province_data$incidence_rate_scaled, boot_sample = 500)
    
    province_corr[province_corr$admin1 == province & province_corr$variable == var, 
                  c("correlation", "p_value", "lower_ci", "upper_ci")] <- corr_results
  }
  print(paste0(province, ": done"))
}

#### Create wide format correlation matrix for heatmap visualization
province_corr_wide <- province_corr %>%
  select(admin1, variable, correlation) %>%
  pivot_wider(names_from = variable, values_from = correlation) %>% 
  left_join(admin1_indonesia) %>% 
  left_join(admin1_EN)

#### Create climate data without lags for visualization
climate_era_2015_2024_nolag <- prec_admin1_era_2015_2024 %>% 
  left_join(temp_admin1_era_2015_2024) %>% 
  left_join(humi_admin1_era_2015_2024) %>% 
  left_join(oni_2015_2024) %>% 
  select(region, idadmin1, admin1, date,
         precipitation_era = precipitation, temperature_era = temperature, 
         rel_humidity_era = rel_humidity, oni) %>% 
  group_by(admin1) %>% 
  mutate(
    precipitation_era_scaled = as.numeric(scale(log(precipitation_era))),
    temperature_era_scaled = as.numeric(scale(temperature_era)),
    rel_humidity_era_scaled = as.numeric(scale(rel_humidity_era))
  )

#### Calculate climate summary statistics for bivariate mapping
climate_era_2015_2024_overall_summary <- climate_era_2015_2024_nolag %>% 
  mutate(month = month(date)) %>% 
  group_by(region, idadmin1, admin1) %>% 
  summarise(
    mean_precipitation = mean(precipitation_era),
    median_precipitation = median(precipitation_era),
    sd_precipitation = sd(precipitation_era),
    cv_precipitation = sd(precipitation_era) / mean_precipitation,
    mean_temperature = mean(temperature_era),
    median_temperature = median(temperature_era),
    sd_temperature = sd(temperature_era),
    cv_temperature = sd(temperature_era) / mean_temperature,
    mean_rel_humidity = mean(rel_humidity_era),
    median_rel_humidity = median(rel_humidity_era),
    sd_rel_humidity = sd(rel_humidity_era),
    cv_rel_humidity = sd(rel_humidity_era) / mean_rel_humidity
  ) %>% 
  ungroup()

#### Prepare spatial data for climate bivariate mapping
admin1_climate_shp <- admin1_shp %>% 
  left_join(climate_era_2015_2024_overall_summary)

#### Create bivariate classifications for climate variables
map_precipitation_bi <- bi_class(admin1_climate_shp, 
                                 x = median_precipitation, 
                                 y = cv_precipitation, 
                                 style = "quantile", 
                                 dim = 3)

map_temperature_bi <- bi_class(admin1_climate_shp, 
                               x = median_temperature, 
                               y = cv_temperature, 
                               style = "quantile", 
                               dim = 3)

map_rel_humidity_bi <- bi_class(admin1_climate_shp, 
                                x = median_rel_humidity, 
                                y = cv_rel_humidity, 
                                style = "quantile", 
                                dim = 3)

#### Calculate centroids for climate map labels
admin1_precipitation_centroids <- map_precipitation_bi %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = map_precipitation_bi$shapeName) %>%
  st_drop_geometry()

admin1_temperature_centroids <- map_temperature_bi %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = map_temperature_bi$shapeName) %>%
  st_drop_geometry()

admin1_rel_humidity_centroids <- map_rel_humidity_bi %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = map_rel_humidity_bi$shapeName) %>%
  st_drop_geometry()

#### Prepare monthly climate data for boxplot analysis
climate_era_2015_2024_month_summary <- climate_era_2015_2024_nolag %>% 
  mutate(month = factor(format(date, "%B"), levels = month.name)) %>% 
  left_join(admin1_national_EN) %>%
  mutate(shapeName = factor(as.character(shapeName), levels = rev(levels(shapeName)))) %>% 
  mutate(region2 = case_when(
    region %in% c("NUSA TENGGARA", "MALUKU", "PAPUA") ~ "NUSA TENGGARA, MALUKU, & PAPUA",
    region == "SUMATERA" ~ "SUMATERA",
    region == "JAVA & BALI" ~ "JAVA & BALI",
    region == "KALIMANTAN" ~ "KALIMANTAN",
    region == "SULAWESI" ~ "SULAWESI",
    TRUE ~ region
  ))

#### Prepare correlation data for heatmap visualization
province_corr_long <- province_corr %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  mutate(variable = factor(variable, levels = c("precipitation_era_scaled", "precipitation_era_lag1_scaled",
                                                "precipitation_era_lag2_scaled", "precipitation_era_lag3_scaled",
                                                "temperature_era_scaled", "temperature_era_lag1_scaled",  
                                                "temperature_era_lag2_scaled", "temperature_era_lag3_scaled",
                                                "rel_humidity_era_scaled", "rel_humidity_era_lag1_scaled",  
                                                "rel_humidity_era_lag2_scaled", "rel_humidity_era_lag3_scaled",
                                                "oni", "oni_lag1", "oni_lag2", "oni_lag3"),
                           labels = c("Precipitation", "Precipitation (lag 1)", "Precipitation (lag 2)", "Precipitation (lag 3)",
                                      "Temperature", "Temperature (lag 1)", "Temperature (lag 2)", "Temperature (lag 3)",
                                      "Rel. humidity", "Rel. humidity (lag 1)", "Rel. humidity (lag 2)", "Rel. humidity (lag 3)",
                                      "ONI", "ONI (lag 1)", "ONI (lag 2)", "ONI (lag 3)")))

#### Calculate monthly averages for climate data visualization
climate_era_2015_2024_monthly_avg_nolag <- climate_era_2015_2024_nolag %>% 
  mutate(month = month(date)) %>% 
  group_by(region, idadmin1, admin1, month) %>% 
  summarise(
    precipitation_era = mean(precipitation_era),
    temperature_era = mean(temperature_era),
    rel_humidity_era = mean(rel_humidity_era)
  ) %>% 
  ungroup() %>% 
  group_by(admin1) %>% 
  mutate(
    precipitation_era_scaled = as.numeric(scale(log(precipitation_era))),
    temperature_era_scaled = as.numeric(scale(temperature_era)),
    rel_humidity_era_scaled = as.numeric(scale(rel_humidity_era)),
    month_name = factor(month, levels = 1:12, 
                        labels = c("January", "February", "March", "April", "May", "June", 
                                   "July", "August", "September", "October", "November", "December"))
  )

#### Advanced wavelet analysis for climate-dengue relationships
#### Prepare time series data for wavelet analysis
province_precipitation_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "precipitation_era")
province_precipitation_lag1_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "precipitation_era_lag1")
province_precipitation_lag2_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "precipitation_era_lag2")
province_precipitation_lag3_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "precipitation_era_lag3")

province_temperature_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "temperature_era")
province_temperature_lag1_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "temperature_era_lag1")
province_temperature_lag2_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "temperature_era_lag2")
province_temperature_lag3_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "temperature_era_lag3")

province_rel_humidity_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "rel_humidity_era")
province_rel_humidity_lag1_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "rel_humidity_era_lag1")
province_rel_humidity_lag2_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "rel_humidity_era_lag2")
province_rel_humidity_lag3_ts <- prepare_for_wavelet(climate_2016_2024, "admin1", "rel_humidity_era_lag3")

province_incidence_ts <- prepare_for_wavelet(climate_incidence_data, "admin1", "incidence_rate_scaled")

#### Run wavelet analysis for all climate variables and lags
province_precipitation_wavelet <- conduct_wavelet_analysis(province_precipitation_ts$ts_data, province_precipitation_ts$valid_groups)
province_precipitation_lag1_wavelet <- conduct_wavelet_analysis(province_precipitation_lag1_ts$ts_data, province_precipitation_lag1_ts$valid_groups)
province_precipitation_lag2_wavelet <- conduct_wavelet_analysis(province_precipitation_lag2_ts$ts_data, province_precipitation_lag2_ts$valid_groups)
province_precipitation_lag3_wavelet <- conduct_wavelet_analysis(province_precipitation_lag3_ts$ts_data, province_precipitation_lag3_ts$valid_groups)

province_temperature_wavelet <- conduct_wavelet_analysis(province_temperature_ts$ts_data, province_temperature_ts$valid_groups)
province_temperature_lag1_wavelet <- conduct_wavelet_analysis(province_temperature_lag1_ts$ts_data, province_temperature_lag1_ts$valid_groups)
province_temperature_lag2_wavelet <- conduct_wavelet_analysis(province_temperature_lag2_ts$ts_data, province_temperature_lag2_ts$valid_groups)
province_temperature_lag3_wavelet <- conduct_wavelet_analysis(province_temperature_lag3_ts$ts_data, province_temperature_lag3_ts$valid_groups)

province_rel_humidity_wavelet <- conduct_wavelet_analysis(province_rel_humidity_ts$ts_data, province_rel_humidity_ts$valid_groups)
province_rel_humidity_lag1_wavelet <- conduct_wavelet_analysis(province_rel_humidity_lag1_ts$ts_data, province_rel_humidity_lag1_ts$valid_groups)
province_rel_humidity_lag2_wavelet <- conduct_wavelet_analysis(province_rel_humidity_lag2_ts$ts_data, province_rel_humidity_lag2_ts$valid_groups)
province_rel_humidity_lag3_wavelet <- conduct_wavelet_analysis(province_rel_humidity_lag3_ts$ts_data, province_rel_humidity_lag3_ts$valid_groups)

province_incidence_wavelet <- conduct_wavelet_analysis(province_incidence_ts$ts_data, province_incidence_ts$valid_groups)

#### Create comprehensive phase angle data
phase_climate_incidence_list <- list()
provinces_loop <- province_rel_humidity_lag3_ts$valid_groups

for (i in seq_len(length(provinces_loop))) {
  phase_climate_incidence_list[[i]] <-
    tibble(
      admin1 = provinces_loop[i],
      date = province_precipitation_wavelet$dates,
      precipitation = province_precipitation_wavelet$phase_angles[[provinces_loop[i]]],
      precipitation_lag1 = province_precipitation_lag1_wavelet$phase_angles[[provinces_loop[i]]],
      precipitation_lag2 = province_precipitation_lag2_wavelet$phase_angles[[provinces_loop[i]]],
      precipitation_lag3 = province_precipitation_lag3_wavelet$phase_angles[[provinces_loop[i]]],
      temperature = province_temperature_wavelet$phase_angles[[provinces_loop[i]]],
      temperature_lag1 = province_temperature_lag1_wavelet$phase_angles[[provinces_loop[i]]],
      temperature_lag2 = province_temperature_lag2_wavelet$phase_angles[[provinces_loop[i]]],
      temperature_lag3 = province_temperature_lag3_wavelet$phase_angles[[provinces_loop[i]]],
      rel_humidity = province_rel_humidity_wavelet$phase_angles[[provinces_loop[i]]],
      rel_humidity_lag1 = province_rel_humidity_lag1_wavelet$phase_angles[[provinces_loop[i]]],
      rel_humidity_lag2 = province_rel_humidity_lag2_wavelet$phase_angles[[provinces_loop[i]]],
      rel_humidity_lag3 = province_rel_humidity_lag3_wavelet$phase_angles[[provinces_loop[i]]],
      incidence = province_incidence_wavelet$phase_angles[[provinces_loop[i]]]
    )
}
phase_climate_incidence <- bind_rows(phase_climate_incidence_list)

#### Calculate phase angle correlations between climate and incidence
corr_phase_climate_incidence_list <- list()
corr_pvalue_phase_climate_incidence_list <- list()

for (i in seq_len(length(provinces_loop))) {
  corr_phase_climate_incidence_list[[i]] <-
    tibble(
      admin1 = provinces_loop[i],
      precipitation = cor(phase_climate_incidence_list[[i]]$precipitation, phase_climate_incidence_list[[i]]$incidence),
      precipitation_lag1 = cor(phase_climate_incidence_list[[i]]$precipitation_lag1, phase_climate_incidence_list[[i]]$incidence),
      precipitation_lag2 = cor(phase_climate_incidence_list[[i]]$precipitation_lag2, phase_climate_incidence_list[[i]]$incidence),
      precipitation_lag3 = cor(phase_climate_incidence_list[[i]]$precipitation_lag3, phase_climate_incidence_list[[i]]$incidence),
      temperature = cor(phase_climate_incidence_list[[i]]$temperature, phase_climate_incidence_list[[i]]$incidence),
      temperature_lag1 = cor(phase_climate_incidence_list[[i]]$temperature_lag1, phase_climate_incidence_list[[i]]$incidence),
      temperature_lag2 = cor(phase_climate_incidence_list[[i]]$temperature_lag2, phase_climate_incidence_list[[i]]$incidence),
      temperature_lag3 = cor(phase_climate_incidence_list[[i]]$temperature_lag3, phase_climate_incidence_list[[i]]$incidence),
      rel_humidity = cor(phase_climate_incidence_list[[i]]$rel_humidity, phase_climate_incidence_list[[i]]$incidence),
      rel_humidity_lag1 = cor(phase_climate_incidence_list[[i]]$rel_humidity_lag1, phase_climate_incidence_list[[i]]$incidence),
      rel_humidity_lag2 = cor(phase_climate_incidence_list[[i]]$rel_humidity_lag2, phase_climate_incidence_list[[i]]$incidence),
      rel_humidity_lag3 = cor(phase_climate_incidence_list[[i]]$rel_humidity_lag3, phase_climate_incidence_list[[i]]$incidence)
    )
  
  corr_pvalue_phase_climate_incidence_list[[i]] <-
    tibble(
      admin1 = provinces_loop[i],
      precipitation = round(cor.test(phase_climate_incidence_list[[i]]$precipitation, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      precipitation_lag1 = round(cor.test(phase_climate_incidence_list[[i]]$precipitation_lag1, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      precipitation_lag2 = round(cor.test(phase_climate_incidence_list[[i]]$precipitation_lag2, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      precipitation_lag3 = round(cor.test(phase_climate_incidence_list[[i]]$precipitation_lag3, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      temperature = round(cor.test(phase_climate_incidence_list[[i]]$temperature, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      temperature_lag1 = round(cor.test(phase_climate_incidence_list[[i]]$temperature_lag1, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      temperature_lag2 = round(cor.test(phase_climate_incidence_list[[i]]$temperature_lag2, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      temperature_lag3 = round(cor.test(phase_climate_incidence_list[[i]]$temperature_lag3, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      rel_humidity = round(cor.test(phase_climate_incidence_list[[i]]$rel_humidity, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      rel_humidity_lag1 = round(cor.test(phase_climate_incidence_list[[i]]$rel_humidity_lag1, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      rel_humidity_lag2 = round(cor.test(phase_climate_incidence_list[[i]]$rel_humidity_lag2, phase_climate_incidence_list[[i]]$incidence)$p.value, 2),
      rel_humidity_lag3 = round(cor.test(phase_climate_incidence_list[[i]]$rel_humidity_lag3, phase_climate_incidence_list[[i]]$incidence)$p.value, 2)
    )
}

#### Process correlation results for visualization
corr_phase_climate_incidence <- bind_rows(corr_phase_climate_incidence_list) %>% 
  pivot_longer(-admin1, names_to = "variable", values_to = "corr") %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  mutate(variable = factor(variable, levels = c("precipitation", "precipitation_lag1",
                                                "precipitation_lag2", "precipitation_lag3",
                                                "temperature", "temperature_lag1",  
                                                "temperature_lag2", "temperature_lag3",
                                                "rel_humidity", "rel_humidity_lag1",  
                                                "rel_humidity_lag2", "rel_humidity_lag3"),
                           labels = c("Precipitation", "Precipitation (lag 1)", "Precipitation (lag 2)", "Precipitation (lag 3)",
                                      "Temperature", "Temperature (lag 1)", "Temperature (lag 2)", "Temperature (lag 3)",
                                      "Rel. humidity", "Rel. humidity (lag 1)", "Rel. humidity (lag 2)", "Rel. humidity (lag 3)")))

corr_pvalue_phase_climate_incidence <- bind_rows(corr_pvalue_phase_climate_incidence_list) %>% 
  pivot_longer(-admin1, names_to = "variable", values_to = "pvalue") %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  mutate(variable = factor(variable, levels = c("precipitation", "precipitation_lag1",
                                                "precipitation_lag2", "precipitation_lag3",
                                                "temperature", "temperature_lag1",  
                                                "temperature_lag2", "temperature_lag3",
                                                "rel_humidity", "rel_humidity_lag1",  
                                                "rel_humidity_lag2", "rel_humidity_lag3"),
                           labels = c("Precipitation", "Precipitation (lag 1)", "Precipitation (lag 2)", "Precipitation (lag 3)",
                                      "Temperature", "Temperature (lag 1)", "Temperature (lag 2)", "Temperature (lag 3)",
                                      "Rel. humidity", "Rel. humidity (lag 1)", "Rel. humidity (lag 2)", "Rel. humidity (lag 3)"))) %>% 
  mutate(p_categories = cut(pvalue, 
                            breaks = c(0, 0.01, 0.05, 1),
                            labels = c("**", "*", "ns"),
                            include.lowest = TRUE))

#### Process national-level dengue data for ONI analysis
dengue_data_national <- readRDS("data/data_cases_deaths_pop_2016_2024_38.rds") %>% 
  select(-admin1, -admin2, -region) %>% 
  left_join(admin2_indonesia) %>% 
  group_by(year, month) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    pop = sum(pop)
  ) %>% 
  ungroup() %>% 
  mutate(
    incidence_rate = (cases / pop) * 100000,
    month_name = factor(month, levels = 1:12, 
                        labels = c("January", "February", "March", "April", "May", "June", 
                                   "July", "August", "September", "October", "November", "December")),
    incidence_rate_scaled = scale(log(incidence_rate)),
    date = ymd(paste0(year, "-", month, "-01"))
  )

#### Merge ONI and dengue data
dengue_oni_national <- oni_2015_2024 %>% 
  left_join(dengue_data_national %>% select(date, incidence_rate, incidence_rate_scaled, month_name))

dengue_oni_national_2016_2024 <- oni_2016_2024 %>% 
  left_join(dengue_data_national %>% select(date, incidence_rate, incidence_rate_scaled, month_name))

#### Calculate national-level ONI correlations
national_oni_corr <- data.frame(
  admin1 = rep("INDONESIA", each = 4),
  variable = rep(c("oni", "oni_lag1", "oni_lag2", "oni_lag3"), times = 1),
  correlation = NA_real_,
  p_value = NA_real_,
  lower_ci = NA_real_,
  upper_ci = NA_real_,
  stringsAsFactors = FALSE
)

for (var in c("oni", "oni_lag1", "oni_lag2", "oni_lag3")) {
  corr_results <- calculate_correlation(dengue_oni_national_2016_2024[[var]], 
                                        dengue_oni_national_2016_2024$incidence_rate_scaled, 
                                        boot_sample = 500)
  
  national_oni_corr[national_oni_corr$admin1 == "INDONESIA" & national_oni_corr$variable == var, 
                    c("correlation", "p_value", "lower_ci", "upper_ci")] <- corr_results
}

#### Combine national and provincial ONI correlations
national_province_oni_corr <- bind_rows(
  national_oni_corr,
  province_corr %>% filter(variable %in% c("oni", "oni_lag1", "oni_lag2", "oni_lag3"))
) %>% 
  left_join(admin1_indonesia_national) %>% 
  left_join(admin1_national_EN) %>% 
  mutate(variable = factor(variable, levels = c("oni", "oni_lag1", "oni_lag2", "oni_lag3"),
                           labels = c("ONI", "ONI (lag 1)", "ONI (lag 2)", "ONI (lag 3)"))) %>% 
  mutate(p_categories = cut(p_value, 
                            breaks = c(0, 0.01, 0.05, 1),
                            labels = c("**", "*", "ns"),
                            include.lowest = TRUE))

#### Process urbanization and surveillance data for Figure 5
#### Calculate province area from shapefiles
admin1_area <- admin1_shp %>%
  mutate(area = st_area(.) / 1000000) %>% 
  st_drop_geometry() %>% 
  select(idadmin1, area)

#### Calculate yearly province data with population density
province_data_yearly <- dengue_data_admin1 %>% 
  group_by(region, idadmin1, admin1, year) %>% 
  summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    pop = mean(pop)
  ) %>% 
  ungroup() %>% 
  mutate(
    incidence_rate = (cases / pop) * 100000,
    cfr = deaths / cases * 100
  ) %>% 
  left_join(admin1_area) %>% 
  mutate(pop_density = pop / area)

#### Calculate overall 2016-2024 summary with population density
province_data_overall_2016_2024 <- province_data_yearly %>% 
  group_by(region, idadmin1, admin1) %>% 
  summarise(
    incidence_rate = mean(incidence_rate),
    pop_density = mean(pop_density)
  ) %>% 
  ungroup()

#### Read urbanization and surveillance data
urbanisation_admin1 <- readRDS("data/urbanisation_admin1.rds") %>% 
  mutate(density_of_urban = urban_pop_pct_binary / urban_area_pct_binary)

surveillance_admin1 <- readRDS("data/surveillance_admin1.rds")
urban_surveillance_admin1 <- readRDS("data/urban_surveillance_admin1.rds")

#### Calculate surveillance-adjusted incidence rates
province_data_overall_adjusted_2016_2024 <- province_data_overall_2016_2024 %>% 
  left_join(surveillance_admin1 %>% 
              select(idadmin1, surveillance_pop_pct, surveillance_area_pct)) %>% 
  left_join(urban_surveillance_admin1 %>% 
              select(idadmin1, urban_surveillance_pop_pct = surveillance_pop_pct,
                     urban_surveillance_area_pct = surveillance_area_pct)) %>% 
  mutate(
    incidence_rate_adj_pop = incidence_rate / surveillance_pop_pct * 100,
    incidence_rate_adj_area = incidence_rate / surveillance_area_pct * 100,
    incidence_rate_adj_urban_pop = incidence_rate / urban_surveillance_pop_pct * 100,
    incidence_rate_adj_urban_area = incidence_rate / urban_surveillance_area_pct * 100
  ) %>% 
  left_join(urbanisation_admin1 %>% 
              select(idadmin1, urban_pop_pct = urban_pop_pct_binary, urban_area_pct = urban_area_pct_binary)) %>% 
  mutate(
    density_of_urban = urban_pop_pct / urban_area_pct,
    incidence_rate_adj_urban_pop_pct = incidence_rate / urban_pop_pct * 100
  )

#### Prepare spatial data for urbanization and surveillance mapping
admin1_urbanisation_shp <- admin1_shp %>% 
  left_join(urbanisation_admin1)

admin1_surveillance_shp <- admin1_shp %>% 
  left_join(surveillance_admin1)

admin1_urban_surveillance_shp <- admin1_shp %>% 
  left_join(urban_surveillance_admin1)

admin1_incidence_shp <- admin1_shp %>% 
  left_join(province_data_overall_adjusted_2016_2024)

#### Calculate centroids for map labels
admin1_urbanisation_centroids <- admin1_urbanisation_shp %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = admin1_urbanisation_shp$shapeName) %>%
  st_drop_geometry()

admin1_surveillance_centroids <- admin1_surveillance_shp %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = admin1_surveillance_shp$shapeName) %>%
  st_drop_geometry()

admin1_urban_surveillance_centroids <- admin1_urban_surveillance_shp %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = admin1_urban_surveillance_shp$shapeName) %>%
  st_drop_geometry()

admin1_incidence_centroids <- admin1_incidence_shp %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(shapeName = admin1_incidence_shp$shapeName) %>%
  st_drop_geometry()