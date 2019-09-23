tidyverse_static_correlations %>%
  network_plot(legend = TRUE) +
  labs(
    title = "Correlations of tidyverse Package Downloads to Total CRAN Downloads",
    subtitle = "Looking at January through June, tidyquant is a clear outlier"
  ) +
  expand_limits(x = c(-0.75, 0.25), y = c(-0.4, 0.4)) +
  scale_x_continuous(breaks = c(-.75, -.25, 0, .25)) +
  scale_color_viridis_c(limits = 0:1) + 
  theme_tq() +
  theme(legend.position = "bottom")

library(tidyquant)

stocks <- tq_get(x = "AAPL", get = "stock.prices")
stocks %>% 
  ggplot() +
  geom_line(aes(x = date, y = close), color = "forestgreen")

library(dygraphs)

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

tidyverse_downloads %>%
  left_join(all_downloads, by = "date") %>%
  select(date, package, count.x, count.y) %>%
  mutate(rolling_cor = runCor(count.x, count.y, n = 30))




tidytest <- tidyverse_downloads
tidytest$count[c(333, 444, 555, 666, 777, 888, 999)] <- NA

# tq_mutate - rollapply mean
system.time({ tidytest %>%
  tq_mutate(select = count,
            mutate_fun = rollapply, 
            width = 28,
            align = "right",
            FUN = mean,
            na.rm = TRUE,
            col_rename = "mean_28") %>%
  tq_mutate(select = count,
            mutate_fun = rollapply,
            width = 84,
            align = "right",
            FUN = mean,
            na.rm = TRUE, 
            col_rename = "mean_84") })

# tq_mutate - rollmean  << this is faster
system.time({ tidytest %>%
    tq_mutate(select = count,
              mutate_fun = rollmean, 
              k = 28,
              align = "right",
              na.rm = TRUE,
              fill = NA,
              col_rename = "mean_28") %>%
    tq_mutate(select = count,
              mutate_fun = rollmean,
              k = 84,
              align = "right",
              na.rm = TRUE, 
              fill = NA,
              col_rename = "mean_84") })

# mutate - rollapply mean  << even faster
system.time({ tidytest %>%
    mutate(mean_28 = rollapply(data = count,
                              width = 28,
                              align = "right",
                              FUN = mean,
                              fill = NA,
                              na.rm = TRUE),
           mean_84 = rollapply(data = count,
                              width = 84,
                              align = "right",
                              FUN = mean,
                              fill = NA,
                              na.rm = TRUE)) })

# mutate - rollmean  <<  this is the fastest
system.time({ tidytest %>%
    mutate(mean_28 = rollmean(x = count,
                              k = 28,
                              align = "right",
                              fill = NA,
                              na.rm = TRUE),
           mean_84 = rollmean(x = count,
                              k = 84,
                              align = "right",
                              fill = NA,
                              na.rm = TRUE)) })

