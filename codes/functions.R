#### 1. theme_Publication: ggplot publication theme that I like
#### source: https://rpubs.com/koundy/71792

theme_Publication <- function(base_size=12, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold", hjust = 0.5),
            # size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.5, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(1.5,1.0,1.0,1.0),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

#### 2. impute_incidence_rates: function to impute missing incidence rates with median of same month in other years
impute_incidence_rates <- function(data) {
  # calculate medians by district and month
  median_rates <- data %>%
    filter(!is.na(incidence_rate)) %>%
    group_by(idadmin1, idadmin2, month_factor) %>%
    summarise(
      median_incidence = median(incidence_rate, na.rm = TRUE),
      .groups = "drop"
    )
  
  # for cases where we don't have any data for a specific district-month combination
  # calculate medians by province and month as a fallback
  province_median_rates <- data %>%
    filter(!is.na(incidence_rate)) %>%
    group_by(idadmin1, month_factor) %>%
    summarise(
      province_median_incidence = median(incidence_rate, na.rm = TRUE),
      .groups = "drop"
    )
  
  # for extreme cases, calculate country-level medians by month
  country_median_rates <- data %>%
    filter(!is.na(incidence_rate)) %>%
    group_by(month_factor) %>%
    summarise(
      country_median_incidence = median(incidence_rate, na.rm = TRUE),
      .groups = "drop"
    )
  
  # perform the imputation with a multi-level fallback strategy
  imputed_data <- data %>%
    # join with district-level medians
    left_join(median_rates, by = c("idadmin1", "idadmin2", "month_factor")) %>%
    # join with province-level medians
    left_join(province_median_rates, by = c("idadmin1", "month_factor")) %>%
    # join with country-level medians
    left_join(country_median_rates, by = "month_factor") %>%
    # impute using the most specific available median: i.e, if district available, use district
    mutate(
      imputed_incidence_rate = case_when(
        !is.na(incidence_rate) ~ incidence_rate,
        !is.na(median_incidence) ~ median_incidence,
        !is.na(province_median_incidence) ~ province_median_incidence,
        !is.na(country_median_incidence) ~ country_median_incidence,
        TRUE ~ 0  # last resort if no medians available
      ),
      imputation_source = case_when(
        !is.na(incidence_rate) ~ "original",
        !is.na(median_incidence) ~ "district_median",
        !is.na(province_median_incidence) ~ "province_median",
        !is.na(country_median_incidence) ~ "country_median",
        TRUE ~ "zero_default"
      )
    ) %>%
    # calculate imputed cases
    mutate(
      imputed_cases = case_when(
        !is.na(cases) ~ cases,
        TRUE ~ round(imputed_incidence_rate * pop / 100000)
      )
    ) %>%
    # clean up unused columns
    select(-median_incidence, -province_median_incidence, -country_median_incidence)
  
  return(imputed_data)
}

#### 3. prepare_for_wavelet: function to prepare time series for wavelet analysis
#### default minimum cases is 100, this is across all period of data
prepare_for_wavelet <- function(data, group_var, var, min_cases = 100) {
  if (grepl("incidence",var)){
    # calculate total cases by group to filter out areas with too few cases
    # if it's case data
    total_cases <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") 
    
    # filter groups with sufficient cases
    valid_groups <- total_cases %>%
      filter(total_cases >= min_cases) %>%
      pull(!!sym(group_var))
    
    # filter data to keep only valid groups
    filtered_data <- data %>%
      filter(!!sym(group_var) %in% valid_groups)
    
    # create wide format for time series analysis
    ts_data <- filtered_data %>%
      select(!!sym(group_var), date, !!sym(var)) %>%
      pivot_wider(
        names_from = !!sym(group_var),
        values_from = !!sym(var)
      )
    
    # fill any missing values (0 cases)
    ts_data[is.na(ts_data)] <- 0
    
    return(list(
      ts_data = ts_data,
      valid_groups = valid_groups
    ))
  } else {
    
    # create wide format for time series analysis
    ts_data <- data %>%
      select(!!sym(group_var), date, !!sym(var)) %>%
      pivot_wider(
        names_from = !!sym(group_var),
        values_from = !!sym(var)
      )
    
    # fill any missing values (0 cases)
    ts_data[is.na(ts_data)] <- 0
    
    valid_groups <- data %>%
      pull(!!sym(group_var)) %>% unique()
    
    return(list(
      ts_data = ts_data,
      valid_groups = valid_groups
    ))
    
  }
}

prepare_for_wavelet <- function(data, group_var, var, min_cases = 100) {
  # create wide format for time series analysis
  ts_data <- data %>%
    select(!!sym(group_var), date, !!sym(var)) %>%
    pivot_wider(
      names_from = !!sym(group_var),
      values_from = !!sym(var)
    )
  
  # fill any missing values (0 cases)
  ts_data[is.na(ts_data)] <- 0
  
  valid_groups <- data %>%
    pull(!!sym(group_var)) %>% unique()
  
  return(list(
    ts_data = ts_data,
    valid_groups = valid_groups
  ))
}

#### 4. conduct_wavelet_analysis: function to conduct wavelet analysis on a time series
conduct_wavelet_analysis <- function(ts_data, group_names) {
  # extract date column
  dates <- ts_data$date
  n_dates <- length(dates)
  
  # create empty lists to store results
  wt_results <- list()
  phase_angles <- list()
  
  # loop through each region/province/district
  for (i in 1:length(group_names)) {
    group <- group_names[i]
    
    # extract and prepare time series
    ts <- ts_data[[group]]
    
    # log transform and standardize
    # ts_log <- log(ts + 0.1)  # adding 0.1 to avoid log(0)
    # ts_std <- scale(ts_log)
    
    # perform wavelet transform
    wt <- wt(cbind(1:n_dates, as.numeric(ts)), 
             dt = 1, # 1 month time step
             mother = "morlet")
    
    # store all wavelet analysis results
    wt_results[[group]] <- wt
    
    # find period closest to 12 months (annual cycle)
    annual_idx <- which.min(abs(wt$period - 12))
    
    # extract phase angles for annual component
    phase_angles[[group]] <- wt$phase[annual_idx, ]
  }
  
  return(list(
    wt_results = wt_results,
    phase_angles = phase_angles,
    dates = dates
  ))
}

#### 5. mean_circular_diff: function to calculate mean circular difference between phase angles
#### the idea is to calculate the difference in phase between pair of places
#### convert the difference into a range of -π to π
#### average the difference of phase between this pair of places
#### this is the initial function, but this would make wrong calculation that earlier wave would become
#### later wave
# mean_circular_diff <- function(phase_a, phase_b) {
#   # calculate circular differences
#   diff <- phase_a - phase_b
#   # adjust to be in range -π to π
#   diff <- (diff + pi) %% (2*pi) - pi
#   # return mean
#   return(mean(diff, na.rm = TRUE))
# }

mean_circular_diff <- function(phase_a, phase_b) {
  # calculate circular differences
  diff <- phase_b - phase_a
  # adjust to be in range -π to π
  diff <- (diff + pi) %% (2*pi) - pi
  # return mean
  return(mean(diff, na.rm = TRUE))
}

#### 6. create_phase_matrix: function to create phase difference matrix for a set of areas
#### use phase angles calculated from the conduct_wavelet_analysis function
create_phase_matrix <- function(phase_angles, group_names) {
  n_groups <- length(group_names)
  
  # initialize matrices
  phase_diffs <- matrix(0, nrow = n_groups, ncol = n_groups)
  colnames(phase_diffs) <- rownames(phase_diffs) <- group_names
  
  # calculate all pairwise phase differences
  for (i in 1:n_groups) {
    for (j in 1:n_groups) {
      if (i != j) {
        phase_diff <- mean_circular_diff(
          phase_angles[[group_names[i]]],
          phase_angles[[group_names[j]]]
        )
        phase_diffs[i, j] <- phase_diff
      }
    }
  }
  
  # convert phase differences to months (2π = 12 months)
  phase_months <- phase_diffs * 12 / (2*pi)
  
  return(phase_months)
}

#### 7. calculate_phase_lags: function to calculate average phase lag for each area
#### from phase matrix, calculate average phase lags for each province compared to other provinces
calculate_phase_lags <- function(phase_matrix) {
  # for each area, calculate average phase difference from all other areas
  n_areas <- nrow(phase_matrix)
  phase_lags <- numeric(n_areas)
  names(phase_lags) <- rownames(phase_matrix)
  
  for (i in 1:n_areas) {
    # average phase difference from all other areas
    phase_lags[i] <- mean(phase_matrix[i, -i], na.rm = TRUE)
  }
  
  return(phase_lags)
}

#### 8. calculate_phase_stats: function to calculate 95% confidence intervals
#### same as above, but also calculate the 95% confidence interval
calculate_phase_stats <- function(phase_matrix) {
  n_provinces <- nrow(phase_matrix)
  province_names <- rownames(phase_matrix)
  
  # create data frame to store results
  results <- data.frame(
    province = character(n_provinces),
    median_phase_lag = numeric(n_provinces),
    lower_ci = numeric(n_provinces),
    upper_ci = numeric(n_provinces),
    stringsAsFactors = FALSE
  )
  
  # calculate statistics for each province
  for (i in 1:n_provinces) {
    # get phase differences with all other provinces
    phase_diffs <- phase_matrix[i, -i]  # Exclude self-comparison
    
    # calculate median and 95% range
    results$province[i] <- province_names[i]
    results$median_phase_lag[i] <- median(phase_diffs, na.rm = TRUE)
    results$lower_ci[i] <- quantile(phase_diffs, 0.025, na.rm = TRUE)
    results$upper_ci[i] <- quantile(phase_diffs, 0.975, na.rm = TRUE)
  }
  
  # sort provinces by median phase lag
  results <- results[order(results$median_phase_lag), ]
  
  # convert province to factor with levels in the sorted order for plotting
  results$province <- factor(results$province, levels = results$province)
  
  return(results)
}

#### 9. calculate_distance_km: function to calculate distance matrix between locations' centroids in kilometers
calculate_distance_km <- function(lon1, lat1, lon2, lat2) {
  # convert degrees to radians
  lon1_rad <- lon1 * pi / 180
  lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  lat2_rad <- lat2 * pi / 180
  
  # earth radius in kilometers
  R <- 6371
  
  # haversine formula
  dlon <- lon2_rad - lon1_rad
  dlat <- lat2_rad - lat1_rad
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  
  return(distance)
}

#### 10. fit_spline_covariance: function to fit spline to correlation vs distance data
fit_spline_covariance <- function(data, corr_col) {
  #### sort by distance
  data <- data[order(data$distance), ]
  
  #### create bins for distance
  n_bins <- 30
  bin_size <- max(data$distance, na.rm = TRUE) / n_bins
  data$bin <- cut(data$distance, breaks = seq(0, max(data$distance, na.rm = TRUE) + bin_size, by = bin_size))
  
  #### calculate mean correlation by bin
  bin_summary <- data %>%
    group_by(bin) %>%
    summarize(
      mean_dist = mean(distance, na.rm = TRUE),
      mean_corr = mean(!!sym(corr_col), na.rm = TRUE),
      sd_corr = sd(!!sym(corr_col), na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(mean_dist), !is.na(mean_corr))
  
  #### fit a smooth spline
  spline_fit <- smooth.spline(bin_summary$mean_dist, bin_summary$mean_corr, 
                              spar = 0.5)  # Adjust smoothing parameter as needed
  
  #### generate prediction grid
  pred_grid <- seq(0, max(data$distance, na.rm = TRUE), length.out = 200)
  pred_values <- predict(spline_fit, pred_grid)
  
  #### calculate country-wide correlation
  country_corr <- mean(data[[corr_col]], na.rm = TRUE)
  
  #### find where correlation drops below country-wide level
  if(any(pred_values$y < country_corr)) {
    idx <- which(pred_values$y < country_corr)[1]
    crossover_distance <- pred_values$x[idx]
  } else {
    crossover_distance <- NA
  }
  
  #### bootstrap confidence intervals
  boot_spline <- function(data, i) {
    boot_data <- data[i, ]
    boot_data <- boot_data[order(boot_data$distance), ]
    
    #### bin the data
    boot_data$bin <- cut(boot_data$distance, 
                         breaks = seq(0, max(boot_data$distance, na.rm = TRUE) + bin_size, by = bin_size))
    
    #### calculate mean by bin
    boot_summary <- boot_data %>%
      group_by(bin) %>%
      summarize(
        mean_dist = mean(distance, na.rm = TRUE),
        mean_corr = mean(!!sym(corr_col), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(mean_dist), !is.na(mean_corr))
    
    #### fit spline
    boot_spline <- try(smooth.spline(boot_summary$mean_dist, boot_summary$mean_corr, spar = 0.5),
                       silent = TRUE)
    
    if(inherits(boot_spline, "try-error")) {
      return(rep(NA, length(pred_grid)))
    } else {
      return(predict(boot_spline, pred_grid)$y)
    }
  }
  
  #### run bootstrap
  set.seed(123)
  boot_results <- boot(data, boot_spline, R = 500)
  
  #### calculate confidence intervals
  ci_matrix <- matrix(NA, nrow = length(pred_grid), ncol = 2)
  for(i in 1:length(pred_grid)) {
    ci <- boot.ci(boot_results, type = "perc", index = i)
    if(!inherits(ci, "try-error") && !is.null(ci$percent)) {
      ci_matrix[i, ] <- ci$percent[4:5]
    }
  }
  
  #### return results
  return(list(
    distances = pred_grid,
    mean_correlation = pred_values$y,
    lower_ci = ci_matrix[, 1],
    upper_ci = ci_matrix[, 2],
    country_correlation = country_corr,
    crossover_distance = crossover_distance
  ))
}

#### 11. create_correlation_plot: function to create correlation vs distance plot
create_correlation_plot <- function(spline_data, title, y_lab) {
  # Create dataframe for plotting
  plot_data <- data.frame(
    distance = spline_data$distances,
    correlation = spline_data$mean_correlation,
    lower_ci = spline_data$lower_ci,
    upper_ci = spline_data$upper_ci
  )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = distance, y = correlation)) +
    geom_point(data=dist_corr_df,aes(x = distance, y = coherence), col="darkgray", alpha = 0.25) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "gray80", alpha = 0.5) +
    geom_line(color = "blue", size = 1) +
    geom_hline(yintercept = spline_data$country_correlation, color = "red", linetype = "solid", size = 1) +
    geom_vline(xintercept = spline_data$crossover_distance, linetype = "solid") +
    geom_hline(yintercept = 0, color = "red", linetype = 2) +
    annotate("text", x = spline_data$crossover_distance, y = 0.9, 
             label = round(spline_data$crossover_distance), hjust = -0.3) +
    labs(
      # title = title,
      x = "Distance (km)",
      y = y_lab
    ) +
    ylim(-0.5, 1) +
    scale_y_continuous(breaks = c(-0.5,-0.25,0,0.25,0.5,0.75,1),limits=c(-0.5,1)) +
    theme_minimal() +
    theme(
      panel.border = element_rect(fill = NA, color = "black"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14)
    )
  
  return(p)
}

#### 12. prepare_wavelet_data: function to prepare time series data for wavelet coherence analysis
prepare_wavelet_data <- function(merged_data, province_name, climate_var, 
                                 incidence_var = "incidence_rate") {
  
  # Filter data for specific province
  province_data <- merged_data %>%
    filter(admin1 == province_name) %>%
    arrange(date) %>%
    select(date, all_of(c(incidence_var, climate_var))) %>%
    na.omit()
  
  # Create time vector (decimal years for biwavelet)
  province_data$time <- decimal_date(province_data$date)
  
  # Create matrix format required by biwavelet
  # Column 1: time, Column 2: variable 1, Column 3: variable 2
  wavelet_matrix <- matrix(
    c(province_data$time, 
      province_data[[incidence_var]], 
      province_data[[climate_var]]), 
    ncol = 3
  )
  
  return(list(
    data = wavelet_matrix,
    province = province_name,
    climate_var = climate_var,
    incidence_var = incidence_var
  ))
}

#### 13. calculate_correlation: function to calculate correlation with confidence intervals
calculate_correlation <- function(x, y, method = "pearson", boot_sample = 1000) {
  if (all(is.na(x)) || all(is.na(y)) || length(x) < 5 || length(y) < 5) {
    return(c(cor = NA, p_value = NA, lower_ci = NA, upper_ci = NA))
  }
  
  # Remove NAs
  complete_cases <- complete.cases(x, y)
  x <- x[complete_cases]
  y <- y[complete_cases]
  
  # Calculate correlation
  cor_test <- cor.test(x, y, method = method)
  cor_test_ci <- ci_cor(x, y, method = method, type = "bootstrap", R = boot_sample, seed = 1234)
  
  return(c(
    cor = cor_test$estimate,
    p_value = cor_test$p.value,
    lower_ci = cor_test_ci$interval[1],
    upper_ci = cor_test_ci$interval[2]
  ))
}

# Function to calculate synchrony and coherence matrices
# calculate_correlation_matrices <- function(ts_data, phase_angles, group_names) {
#   n_groups <- length(group_names)
#   
#   # Initialize matrices
#   sync_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
#   coherence_matrix <- matrix(0, nrow = n_groups, ncol = n_groups)
#   colnames(sync_matrix) <- rownames(sync_matrix) <- group_names
#   colnames(coherence_matrix) <- rownames(coherence_matrix) <- group_names
#   
#   # Calculate pairwise correlations
#   for (i in 1:n_groups) {
#     for (j in 1:n_groups) {
#       # Get time series for both groups
#       ts_i <- ts_data[[group_names[i]]]
#       ts_j <- ts_data[[group_names[j]]]
#       
#       # Calculate epidemic synchrony (correlation of case counts)
#       sync_matrix[i, j] <- cor(ts_i, ts_j, use = "pairwise.complete.obs")
#       
#       # Get phase angles for both groups
#       phase_i <- phase_angles[[group_names[i]]]
#       phase_j <- phase_angles[[group_names[j]]]
#       
#       # Calculate phase coherence (correlation of phase angles)
#       coherence_matrix[i, j] <- cor(phase_i, phase_j, use = "pairwise.complete.obs")
#     }
#   }
#   
#   return(list(
#     sync_matrix = sync_matrix,
#     coherence_matrix = coherence_matrix
#   ))
# }

# Function to calculate distance matrix between areas
# calculate_distance_matrix <- function(coords_df, area_column) {
#   n_areas <- nrow(coords_df)
#   dist_matrix <- matrix(0, nrow = n_areas, ncol = n_areas)
#   rownames(dist_matrix) <- colnames(dist_matrix) <- coords_df[[area_column]]
#   
#   for (i in 1:n_areas) {
#     for (j in 1:n_areas) {
#       x1 <- coords_df$lat[i]
#       y1 <- coords_df$lon[i]
#       x2 <- coords_df$lat[j]
#       y2 <- coords_df$lon[j]
#       
#       # Calculate Euclidean distance
#       dist_matrix[i, j] <- sqrt((x2-x1)^2 + (y2-y1)^2)
#     }
#   }
#   
#   return(dist_matrix)
# }
