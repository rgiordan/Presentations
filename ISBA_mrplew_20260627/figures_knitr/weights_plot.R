
method_names <- c(
  "mrplew"="MrPlew",
  "raking"="Raking")

GetWeightDf <- function(analysis_env) {
  weight_df <-
    data.frame(analysis_env$weight_df) %>%
    mutate(analysis=analysis_env$name,
           analysis_simple=analysis_env$simple_name) %>%
    mutate(w_scale=w * analysis_env$save_list$n_obs_survey) %>%
    mutate(method_name=method_names[method])
  return(weight_df)
}

weight_df <-
  bind_rows(
    GetWeightDf(alexander),
    GetWeightDf(laxphilips),
    GetWeightDf(stories))

w_thresh <- 30
num_over_thresh <-
  weight_df %>%
  filter(analysis_simple=="alexander") %>%
  filter(w_scale > w_thresh) %>%
  nrow()

weight_filtered_df <-
  weight_df %>%
  filter((analysis_simple != "alexander") | (w_scale < w_thresh))

annotations <- data.frame(
  analysis = alexander$name,
  method_name = "Raking",
  x = 8,
  y = 1000,
  label = sprintf("(%d raking weights > %0.f\nomitted from plot)",
                  num_over_thresh, w_thresh)
)

# weight_plt <-
#   ggplot(weight_filtered_df) +
#   geom_histogram(aes(x=w_scale), bins=90) +
#   facet_grid(method_name ~ analysis, scales="free") +
#   geom_text(data = annotations, aes(x = x, y = y, label = label),
#             color = "red", size = 8) +
#   xlab("Weights") + 
#   geom_vline(aes(xintercept=0))



# weight_plt_alexander <-
#   weight_filtered_df %>%
#   filter(analysis_simple == "alexander") %>%
#   ggplot() +
#   geom_histogram(aes(x=w_scale), bins=90) +
#   facet_grid(method_name ~ ., scales="free") +
#   geom_text(data = annotations, aes(x = x, y = y, label = label),
#             color = "red", size = 2) +
#   xlab("Weights") + ylab(NULL) +
#   geom_vline(aes(xintercept=0))


weight_plt_lax <-
  weight_filtered_df %>%
  filter(analysis_simple == "laxphilips") %>%
  ggplot() +
  geom_histogram(aes(x=w_scale), bins=90) +
  facet_grid(method_name ~ ., scales="free") +
  geom_text(data = annotations, aes(x = x, y = y, label = label),
            color = "red", size = 4) +
  xlab("Weights") + ylab(NULL) +
  geom_vline(aes(xintercept=0))



weight_lax <-
  weight_filtered_df %>%
  filter(analysis_simple == "laxphilips")

weight_biv_df <- 
  bind_cols(
    weight_lax %>% filter(method == "mrplew") %>% rename(mrplew=w_scale),
    weight_lax %>% filter(method == "raking") %>% rename(raking=w_scale)
  )

annotations_biv <- annotations
annotations_biv$x <- 5
annotations_biv$y <- -2
weight_plt_biv_lax <-
  weight_biv_df %>%
    ggplot() +
    geom_point(aes(x=raking, y=mrplew), color="gray", alpha=0.5) +
    geom_density_2d(aes(x=raking, y=mrplew)) +
    geom_vline(aes(xintercept=0)) +
    geom_hline(aes(yintercept=0)) +
    geom_text(data = annotations_biv, 
              aes(x = x, y = y, label = label),
              color = "red", size = 4) +
    xlab("Raking") + ylab("MrPlew")

