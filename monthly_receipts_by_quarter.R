# Average monthly receipts by quarter, 2023-2026
# Year-to-date receipts ($M) from the published quarterly revenue reports (2026 Q3 from the draft).
# Q1 covers 2 months of obligations (Jan-Feb); Q2 and Q3 cover 3 months each.
library(ggplot2)

ytd <- data.frame(
  tax  = rep(c("Telephone", "Cable Television", "Natural Gas", "Steam"), each = 4),
  year = rep(2023:2026, times = 4),
  q1   = c(2.514, 1.88, 1.87, 1.72,   0.991, 1.92, 1.75, 1.67,   3.729, 1.53, 3.83, 3.71,   0.297, 0.45, 0.41, 0.39),
  h1   = c(4.986, 4.83, 4.63, 4.11,   4.105, 4.71, 3.48, 3.98,   8.116, 6.47, 7.96, 8.11,   0.732, 0.91, 0.91, 0.82),
  q3   = c(7.914, 7.49, 8.71, 6.59,   7.963, 7.31, 6.84, 6.51,  10.040, 8.45, 9.47, 10.30,  1.062, 1.26, 1.27, 1.07)
)

monthly <- rbind(
  data.frame(tax = ytd$tax, year = ytd$year, quarter = "Q1", value = ytd$q1 / 2),
  data.frame(tax = ytd$tax, year = ytd$year, quarter = "Q2", value = (ytd$h1 - ytd$q1) / 3),
  data.frame(tax = ytd$tax, year = ytd$year, quarter = "Q3", value = (ytd$q3 - ytd$h1) / 3)
)
monthly$tax  <- factor(monthly$tax, levels = c("Telephone", "Cable Television", "Natural Gas", "Steam"))
monthly$year <- factor(monthly$year)

p <- ggplot(monthly, aes(quarter, value, colour = year, group = year)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ tax, scales = "free_y") +
  scale_y_continuous(labels = function(x) paste0("$", x, "M")) +
  labs(title = "Average Monthly Receipts by Quarter", x = NULL, y = NULL, colour = NULL)

print(p)
ggsave("monthly_receipts_by_quarter.png", p, width = 8, height = 5.5, dpi = 150)
