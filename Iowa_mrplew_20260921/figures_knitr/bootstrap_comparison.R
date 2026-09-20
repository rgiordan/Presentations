
clean_sd_method <- function(method) {
  case_when(
    method == "ij_sd" ~ "MrPlew estimate",
    method == "sd" ~ "MrP parametric bootstrap",
    method == "raking_sd" ~ "Raking estimate",
    TRUE ~ "UNKNOWN"
  )
}


clean_dataset_name <- function(analysis) {
  case_when(
    analysis == "alexander" ~ "Name change dataset",
    analysis == "laxphilips" ~ "Gay marriage dataset",
    TRUE ~ "UNKNOWN"
  )
}


# Both ij_sd and sd are of the MrP estimator
method_colors <- c(
  "MrPlew estimate" = "#F8766D",
  "Raking estimate" = "#00BA38",
  "MrP parametric bootstrap" = "#F876B0"
)


boot_df <-
  boot_env$mrp_df %>%
  #filter(analysis %in% c("alexander", "laxphilips")) %>%
  filter(analysis %in% c("alexander")) %>%
  left_join(data.frame(
    analysis=c("alexander", "laxphilips"),
    raking_sd=c(
      sqrt(attr(alex$basic_metrics$raking, "var")),
      sqrt(attr(lax$basic_metrics$raking, "var"))
    )), by="analysis") %>%
  pivot_longer(cols=-analysis) %>%
  filter(name %in% c("sd", "ij_sd", "raking_sd")) %>%
  mutate(method_label=clean_sd_method(name)) %>%
  mutate(analysis_label=clean_dataset_name(analysis))


plt <- boot_df %>%
  ggplot(aes(x=analysis_label, y=value, fill=method_label)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=method_colors) +
  xlab(NULL) +
  labs(fill="") +
  ylab("Frequentist standard deviation")

print(plt)