library(shiny)
runExample("01_hello")      # a histogram
runExample("02_text")       # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg")        # global variables
runExample("05_sliders")    # slider bars
runExample("06_tabsets")    # tabbed panels
runExample("07_widgets")    # help text and submit buttons
runExample("08_html")       # Shiny app built from HTML
runExample("09_upload")     # file upload wizard
runExample("10_download")   # file download wizard
runExample("11_timer")      # an automated timer

runApp("update_price_app")


# scales without removing data
tesla %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(color = "snow", size = 2) +
  geom_line(color = "darkred") + 
  coord_cartesian(xlim = as.Date(c("2010-07-01", "2012-12-01"))) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Tesla Stock Prices", x = "Date", y = "Adjusted Close Price")

# removes data when scaling x (date), but doesn't want to adjust y-axis
tesla %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(color = "snow", size = 2) +
  geom_line(color = "darkred") + 
  scale_x_date(limits = as.Date(c("2010-07-01", "2012-12-01"))) +
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Tesla Stock Prices", x = "Date", y = "Adjusted Close Price")

# scales y-axis after filtering x
tesla %>%
  filter(between(date, as.Date("2010-07-01"), as.Date("2012-12-01"))) %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(color = "snow", size = 2) +
  geom_line(color = "darkred") + 
  geom_line(aes(y = rollmean(adjusted, 100, fill = c(0,0,0))), color = "navyblue") +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Tesla Stock Prices", x = "Date", y = "Adjusted Close Price")




