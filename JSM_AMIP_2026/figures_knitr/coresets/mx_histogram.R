mx_data <- coreset_env$mx_data

# linear below x0, log above x0, 1st order smooth at x0, for x0 > 0
lin_log <- function(x, x0) {
  ifelse(
    x > x0,
    x0 * (1 - log(x0)) + x0 * log(x),
    x)
}

threshold <- 500
signed_lin_log <- function(x, x0=threshold) {
  return(sign(x) * lin_log(abs(x), x0=x0))
}

profit_break_vals <- c(0, 500, 3000, 10000)
trans_break_vals <- signed_lin_log(profit_break_vals)

mx_data %>%
  filter(profit != 0) %>%
  mutate(treatment_label=case_when(treatment == 1 ~ "Treatment", TRUE ~ "Control")) %>%
  ggplot() +
    geom_histogram(aes(x=profit_trans, fill=factor(treatment_label)), bins=500) +
    facet_grid(treatment_label ~ .) +
    geom_vline(aes(xintercept=mean_profit)) +
  annotate("rect", 
           xmin = threshold, xmax = Inf, 
           ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = 0.4) +
  annotate("rect", 
           xmin = -Inf, xmax = -threshold, 
           ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = 0.4) +
  ylab(NULL) +
  xlab("Profit") +
  scale_x_continuous(
    breaks = c(-trans_break_vals, trans_break_vals),
    labels = c(-profit_break_vals, profit_break_vals) 
  ) +
  annotate("text", 
           x = c(threshold + 100, -threshold - 2000),
           y = c(50, 50), 
           label = "Shaded regions are\nlog-transformed ", 
           hjust = -0.1, vjust = 1, 
           size = 2, color = "black") +
  theme(legend.position = "none")  # Hides all legends
