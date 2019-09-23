library(tidyquant)
library(cranlogs)

# PART 1: Tidy Period Apply

# Various tidyverse packages
pkgs <- c(
  "tidyr", "lubridate", "dplyr", 
  "broom", "tidyquant", "ggplot2", "purrr", 
  "stringr", "knitr"
)

# Get the downloads for the individual packages
tidyverse_downloads <- cran_downloads(
  packages = pkgs, 
  from     = "2017-01-01", 
  to       = "2017-06-30") %>%
  tibble::as_tibble() %>%
  group_by(package)

# Visualize the package downloads
tidyverse_downloads %>%
  ggplot(aes(x = date, y = count, color = package)) +
  geom_point() +
  labs(title = "tidyverse packages: Daily downloads", x = "") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

# "apply" functions available from xts
tq_transmute_fun_options()$xts %>%
  stringr::str_subset("^apply")

# weekly averages
mean_tidyverse_downloads_w <- tidyverse_downloads %>%
  tq_transmute(
    select     = count,
    mutate_fun = apply.weekly, 
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "mean_count"
  )
# dplyr method requires group_by
tidyverse_downloads %>%
  group_by(package, week = week(date), date) %>%
  summarise(mean_count = mean(count, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-week)
mean_tidyverse_downloads_w

# plot the weekly averages to better see trends
mean_tidyverse_downloads_w %>%
  ggplot(aes(x = date, y = mean_count, color = package)) +
  geom_point() +
  geom_smooth(method = "loess") + 
  labs(title = "tidyverse packages: Average daily downloads by week", x = "", 
       y = "Mean Daily Downloads by Week") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) + 
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

# Custom function to return mean, sd, quantiles
custom_stat_fun <- function(x, na.rm = TRUE, ...) {
  # x     = numeric vector
  # na.rm = boolean, whether or not to remove NA's
  # ...   = additional args passed to quantile
  c(mean    = mean(x, na.rm = na.rm),
    stdev   = sd(x, na.rm = na.rm),
    quantile(x, na.rm = na.rm, ...)) 
}

# Testing custom_stat_fun
options(digits = 4)
set.seed(3366)
nums  <- c(10 + 1.5*rnorm(10), NA)
probs <- c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)
custom_stat_fun(nums, na.rm = TRUE, probs = probs)

# Applying the custom function by week
stats_tidyverse_downloads_w <- tidyverse_downloads %>%
  tq_transmute(
    select = count,
    mutate_fun = apply.weekly, 
    FUN = custom_stat_fun,
    na.rm = TRUE,
    probs = probs
  )
stats_tidyverse_downloads_w

# visualize
stats_tidyverse_downloads_w %>%
  ggplot(aes(x = date, y = `50%`, color = package)) +
  # Ribbon
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), 
              color = palette_light()[[1]], fill = palette_light()[[1]], alpha = 0.5) +
  # Points
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) + 
  # Aesthetics
  labs(title = "tidyverse packages: Median daily downloads by week", x = "",
       subtitle = "Range of 1st and 3rd quartile to show volatility",
       y = "Median Daily Downloads By Week") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) + 
  scale_color_tq(theme = "dark") +
  theme_tq() +
  theme(legend.position="none")

# how are sd and mean related (higher volatility ~ more downloads)
stats_tidyverse_downloads_w %>%
  ggplot(aes(x = stdev, y = mean, color = package)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "tidyverse packages: Mean vs standard deviation of daily downloads by week") +
  facet_wrap(~ package, ncol = 3, scale = "free") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")




# PART 2: Tidy Rolling Functions

# Visualize the package downloads
tidyverse_downloads %>%
  ggplot(aes(x = date, y = count, color = package)) +
  # Data
  geom_point(alpha = 0.5) +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "tidyverse packages: Daily downloads", x = "",
       subtitle = "2017-01-01 through 2017-06-30",
       caption = "Downloads data courtesy of cranlogs package") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

# "roll" functions from zoo
tq_mutate_fun_options()$zoo %>%
  stringr::str_subset("^roll")

# "run" functions from TTR
tq_mutate_fun_options()$TTR %>%
  stringr::str_subset("^run")

# Condensed function options... lot's of 'em
tq_mutate_fun_options() %>%
  str()

# Rolling mean
tidyverse_downloads_rollmean <- tidyverse_downloads %>%
  tq_mutate(
    # tq_mutate args
    select     = count,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 28,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "mean_28"
  ) %>%
  tq_mutate(
    # tq_mutate args
    select     = count,
    mutate_fun = rollapply,
    # rollapply args
    width      = 84,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "mean_84"
  )
# dplyr method: rollmean is faster than rollapply(FUN=mean)
tidyverse_downloads %>%
  mutate(mean_28 = rollmean(x = count,
                            k = 28,
                            align = "right",
                            fill = NA,
                            na.rm = TRUE),
         mean_84 = rollmean(x = count,
                            k = 84,
                            align = "right",
                            fill = NA,
                            na.rm = TRUE))

# ggplot
tidyverse_downloads_rollmean %>%
  ggplot(aes(x = date, y = count, color = package)) +
  # Data
  geom_point(alpha = 0.1) +
  geom_line(aes(y = mean_28), color = palette_light()[[1]], size = 1) +
  geom_line(aes(y = mean_84), color = palette_light()[[2]], size = 1) +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "tidyverse packages: Daily Downloads", x = "",
       subtitle = "28 and 84 Day Moving Average") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

# inspect slow trend and fast trends, proximity to each other
tidyverse_downloads_rollmean %>%
  ggplot(aes(x = date, color = package)) +
  # Data
  # geom_point(alpha = 0.5) +  # Drop "count" from plots
  geom_line(aes(y = mean_28), color = palette_light()[[1]], linetype = 1, size = 1) +
  geom_line(aes(y = mean_84), color = palette_light()[[2]], linetype = 1, size = 1) +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "tidyverse packages: Daily downloads", x = "", y = "",
       subtitle = "Zoomed In: 28 and 84 Day Moving Average") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

# Custom function to return mean, sd, 95% conf interval
custom_stat_fun_2 <- function(x, na.rm = TRUE) {
  # x     = numeric vector
  # na.rm = boolean, whether or not to remove NA's
  
  m  <- mean(x, na.rm = na.rm)
  s  <- sd(x, na.rm = na.rm)
  hi <- m + 2*s
  lo <- m - 2*s
  
  ret <- c(mean = m, stdev = s, hi.95 = hi, lo.95 = lo) 
  return(ret)
}

# Roll apply using custom stat function
tidyverse_downloads_rollstats <- tidyverse_downloads %>%
  tq_mutate(
    select     = count,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 28,
    align      = "right",
    by.column  = FALSE,
    FUN        = custom_stat_fun_2,
    # FUN args
    na.rm      = TRUE
  )
tidyverse_downloads_rollstats

# Bollinger bands: rollmean and +- 2sd
tidyverse_downloads_rollstats %>%
  ggplot(aes(x = date, color = package)) +
  # Data
  geom_point(aes(y = count), color = "grey40", alpha = 0.5) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), alpha = 0.4) +
  geom_point(aes(y = mean), size = 1, alpha = 0.5) +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "tidyverse packages: Volatility and Trend", x = "",
       subtitle = "28-Day Moving Average with 95% Confidence Interval Bands (+/-2 Standard Deviations)") +
  scale_color_tq(theme = "light") +
  theme_tq() +
  theme(legend.position="none")




# PART 3: Tidy Rolling Correlations
library(corrr)      # Tidy correlation tables and correlation plotting
library(cowplot)    # Multiple plots with plot_grid()

# Get data for total CRAN downloads
all_downloads <- cran_downloads(from = "2017-01-01", to = "2017-06-30") %>%
  tibble::as_tibble()

# Visualize the downloads
all_downloads %>%
  ggplot(aes(x = date, y = count)) +
  # Data
  geom_point(alpha = 0.5, color = palette_light()[[1]], size = 2) +
  geom_line() + #my addition, reveals a weekly pattern
  # Aesthetics
  labs(title = "Total CRAN Packages: Daily downloads", x = "",
       subtitle = "2017-01-01 through 2017-06-30",
       caption = "Downloads data courtesy of cranlogs package") +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  theme(legend.position="none")

# "run" functions from TTR
tq_mutate_fun_options()$TTR %>%
  stringr::str_subset("^run")

# If first arg is x (and no y) --> us tq_mutate()
args(runSD)

# If first two arguments are x and y --> use tq_mutate_xy()
args(runCor)

# Correlation table
tidyverse_static_correlations <- tidyverse_downloads %>%
  # Data wrangling
  spread(key = package, value = count) %>%
  left_join(all_downloads, by = "date") %>%
  rename(all_cran = count) %>%
  select(-date) %>%
  # Correlation and formating
  correlate() # corrr: returns correlation dataframe instead of matrix

# Pretty printing
tidyverse_static_correlations %>%
  shave(upper = F)

# Network plot
gg_all <- tidyverse_static_correlations %>%
  network_plot(colours = c(palette_light()[[1]], "white", palette_light()[[5]]), legend = TRUE) +
  labs(
    title = "Correlations of tidyverse Package Downloads to Total CRAN Downloads",
    subtitle = "Looking at January through June, tidyquant is a clear outlier"
  ) +
  expand_limits(x = c(-0.75, 0.25), y = c(-0.4, 0.4)) +
  theme_tq() +
  theme(legend.position = "bottom")
gg_all

# Get rolling correlations
tidyverse_rolling_corr <- tidyverse_downloads %>%
  # Data wrangling
  left_join(all_downloads, by = "date") %>%
  select(date, package, count.x, count.y) %>%
  # Mutation
  tq_mutate_xy(
    x          = count.x,
    y          = count.y,
    mutate_fun = runCor, 
    # runCor args
    n          = 30,
    use        = "pairwise.complete.obs",
    # tq_mutate args
    col_rename = "rolling_corr"
  )

# Join static correlations with rolling correlations
tidyverse_static_correlations <- tidyverse_static_correlations %>%
  select(rowname, all_cran) %>%
  rename(package = rowname)

tidyverse_rolling_corr <- tidyverse_rolling_corr %>%
  left_join(tidyverse_static_correlations, by = "package") %>%
  rename(static_corr = all_cran)

# Plot
tidyverse_rolling_corr %>%
  ggplot(aes(x = date, color = package)) +
  # Data
  geom_line(aes(y = static_corr), color = "red") +
  geom_point(aes(y = rolling_corr), alpha = 0.5) +
  facet_wrap(~ package, ncol = 3, scales = "free_y") +
  # Aesthetics
  scale_color_tq() +
  labs(
    title = "tidyverse: 30-Day Rolling Download Correlations, Package vs Total CRAN",
    subtitle = "Relationships are dynamic vs static correlation (red line)",
    x = "", y = "Correlation"
  ) +
  theme_tq() +
  theme(legend.position="none")

# Redrawing Network Plot from April through June
gg_subset <- tidyverse_downloads %>%
  # Filter by date >= April 1, 2017
  filter(date >= ymd("2017-04-01")) %>%
  # Data wrangling
  spread(key = package, value = count) %>%
  left_join(all_downloads, by = "date") %>%
  rename(all_cran = count) %>%
  select(-date) %>%
  # Correlation and formating
  correlate() %>%
  # Network Plot
  network_plot(colours = c(palette_light()[[2]], "white", palette_light()[[4]]), legend = TRUE) +
  labs(
    title = "April through June (Last 3 Months)",
    subtitle = "tidyquant correlation is increasing"
  ) +
  expand_limits(x = c(-0.75, 0.25), y = c(-0.4, 0.4)) +
  theme_tq() +
  theme(legend.position = "bottom")

# Modify the January through June network plot (previous plot)
gg_all <- gg_all +
  labs(
    title = "January through June (Last 6 months)",
    subtitle = "tidyquant is an outlier"
  )

# Format cowplot
cow_net_plots <- plot_grid(gg_all, gg_subset, ncol = 2)
title <- ggdraw() + 
  draw_label(label = 'tidyquant is getting "tidy"-er',
             fontface = 'bold', size = 18)
cow_out <- plot_grid(title, cow_net_plots, ncol=1, rel_heights=c(0.1, 1))
cow_out




# PART 4: Lags and Autocorrelations
library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data

# Visualize the package downloads
tidyverse_downloads %>%
  ggplot(aes(x = date, y = count, color = package)) +
  # Data
  geom_point(alpha = 0.5) +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "tidyverse packages: Daily downloads", x = "",
       subtitle = "2017-01-01 through 2017-06-30",
       caption = "Downloads data courtesy of cranlogs package") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

# tidyquant Integrated functions
tq_mutate_fun_options() %>%
  glimpse()

# first look into lag.xts()
set.seed(1)
my_time_series_tbl <- tibble(
  date   = seq.Date(ymd("2017-01-01"), length.out = 10, by = "day"),
  value  = 1:10 + rnorm(10)
)
my_time_series_tbl

# Bummer, man! lag.xts only works on time-series objects
my_time_series_tbl %>%           
  lag.xts(k = 1:5)

# Success! Got our lags 1 through 5. One problem: no original values
my_time_series_tbl %>%
  tk_xts(silent = TRUE) %>%
  lag.xts(k = 1:5)

# dplyr has a lag() function that works on non-time-series objects, 
# but it can only calculate one lag value at a time and requires some wrangling
my_time_series_tbl %>%           
  mutate(value.2 = lag(value, n = 2))

# "We need our original values so we can analyze the counts against the lags. If we want to get the original values too, we can do something like this."
# Convert to xts
my_time_series_xts <- my_time_series_tbl %>%
  tk_xts(silent = TRUE)
# Get original values and lags in xts
my_lagged_time_series_xts <- 
  merge.xts(my_time_series_xts, lag.xts(my_time_series_xts, k = 1:5))
# Convert back to tbl
my_lagged_time_series_xts %>%
  tk_tbl()

# This is nice, we didn't need to coerce to xts and it merged for us
my_time_series_tbl %>%
  tq_mutate(
    select     = value,
    mutate_fun = lag.xts,
    k          = 1:5
  )

# Use tq_mutate() to get lags 1:28 using lag.xts()
k <- 1:28
col_names <- paste0("lag_", k)

tidyverse_lags <- tidyverse_downloads %>%
  tq_mutate(
    select     = count,
    mutate_fun = lag.xts,
    k          = 1:28,
    col_rename = col_names
  )
tidyverse_lags

# Calculate the autocorrelations and 95% cutoffs
tidyverse_count_autocorrelations <- tidyverse_lags %>%
  gather(key = "lag", value = "lag_value", -c(package, date, count)) %>%
  mutate(lag = str_sub(lag, start = 5) %>% as.numeric) %>%
  group_by(package, lag) %>%
  summarize(
    cor = cor(x = count, y = lag_value, use = "pairwise.complete.obs"),
    cutoff_upper = 2/(n())^0.5,
    cutoff_lower = -2/(n())^0.5
  )
tidyverse_count_autocorrelations

# Visualize the autocorrelations
tidyverse_count_autocorrelations %>%
  ggplot(aes(x = lag, y = cor, color = package, group = package)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  # Add cutoffs
  geom_line(aes(y = cutoff_upper), color = "blue", linetype = 2) +
  geom_line(aes(y = cutoff_lower), color = "blue", linetype = 2) +
  # Add facets
  facet_wrap(~ package, ncol = 3) +
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  scale_color_tq() +
  theme_tq() +
  labs(
    title = paste0("Tidyverse ACF Plot: Lags ", rlang::expr_text(k)),
    subtitle = "Appears to be a weekly pattern",
    x = "Lags"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Get the absolute autocorrelations
tidyverse_absolute_autocorrelations <- tidyverse_count_autocorrelations %>%
  ungroup() %>%
  mutate(
    lag = as_factor(as.character(lag)),
    cor_abs = abs(cor)
  ) %>%
  select(lag, cor_abs) %>%
  group_by(lag) 
tidyverse_absolute_autocorrelations
# (can be simplified using transmute instead of mutate)
tidyverse_count_autocorrelations %>%
  ungroup() %>%
  transmute(
    lag = as_factor(as.character(lag)),
    cor_abs = abs(cor)
  ) %>%
  group_by(lag) 

# Visualize boxplot of absolute autocorrelations
break_point <- 1.5*IQR(tidyverse_absolute_autocorrelations$cor_abs) %>% signif(3)
tidyverse_absolute_autocorrelations %>%    
  ggplot(aes(x = fct_reorder(lag, cor_abs, .desc = TRUE) , y = cor_abs)) +
  # Add boxplot
  geom_boxplot(color = palette_light()[[1]]) +
  # Add horizontal line at outlier break point
  geom_hline(yintercept = break_point, color = "red") +
  annotate("text", label = paste0("Outlier Break Point = ", break_point), 
           x = 24.5, y = break_point + .03, color = "red") +
  # Aesthetics
  expand_limits(y = c(0, 1)) +
  theme_tq() +
  labs(
    title = paste0("Absolute Autocorrelations: Lags ", rlang::expr_text(k)),
    subtitle = "Weekly pattern is consistently above outlier break point",
    x = "Lags"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
