timestamp <- "1678395669" # This is better, has more MC draws
df <- read.csv(
  file.path(data_path,
    sprintf("poisson_refit_%s.csv", timestamp)), as.is=TRUE) %>%
  mutate(is_cond=as.logical(is_cond),
         drop_group=as.logical(drop_group))

timestamp <- "1678395669"
dist_df <- read.csv(
  file.path(data_path,
    sprintf("poisson_marginals_%s.csv", timestamp)), as.is=TRUE)

GetGridSpacing <- function(grid) {
    min(diff(grid))
}

dist_df <- dist_df %>%
    group_by(num_obs) %>%
    mutate(dtheta=GetGridSpacing(theta_grid),
           dens=p_grid / dtheta) %>%
    mutate(e_theta=sum(theta_grid * p_grid),
           theta_grid_center=theta_grid - e_theta)
