# Run simulations using a model from Shao and Tu chapter 3.4
# based on Hinkley and Wei (1984).

library(tidyverse)
library(gridExtra)
library(latex2exp)

ComputeStatistics <- function(y, z, delta) {
    delta_hat <- mean(y) / mean(z)
    var_hat <- sum((y - z * delta_hat)^2) / (sum(z)^2)
    sd_hat <- sqrt(var_hat)
    return(list(delta_hat=delta_hat,
                var_hat=var_hat,
                sd_hat=sd_hat,
                t_hat=(delta_hat - delta) / sd_hat))
}


# Making z (the second variable) closer to zero will give more pathological results
DrawData <- function(num_obs, mean1=1.0, mean2=0.5, sd1=0.3, sd2=0.6) {
    y <- rnorm(num_obs, mean=mean1, sd=sd1)
    z <- rnorm(num_obs, mean=mean2, sd=sd2)
    delta <- mean1 / mean2
    return(c(
        list(y=y, z=z, num_obs=num_obs, delta=delta),
        ComputeStatistics(y, z, delta)))
}


DrawBootstrap <- function(data) {
    y <- data$y[sample.int(data$num_obs, data$num_obs, replace=TRUE)]
    z <- data$z[sample.int(data$num_obs, data$num_obs, replace=TRUE)]
    delta <- data$delta_hat
    return(c(
        list(y=y, z=z, num_obs=data$num_obs, delta=delta),
        ComputeStatistics(y, z, delta)))
}


QQPlot <- function(x, y) {
    qq_stats <- qqplot(x, y);
    ggplot(data.frame(x=qq_stats$x, y=qq_stats$y, plot=FALSE)) +
        geom_point(aes(x=x, y=y)) +
        geom_abline(aes(slope=1, intercept=0))
}


ComputeCDF <- function(x) {
    n <- length(x)
    return(list(x=sort(x), cdf=(1:n)/n))
}

num_outer_sims <- 5
num_boots <- 500
num_sims <- 1000

num_obs_min <- 10
num_obs_max <- 100
num_obs_length <- 10
#num_obs_vec <- floor(seq(sqrt(num_obs_min), sqrt(num_obs_max), length.out=num_obs_length)^2)
num_obs_vec <- floor(1 / seq(1 / sqrt(num_obs_min), 1 / sqrt(num_obs_max), length.out=num_obs_length)^2)
rhoinf_df <- data.frame()
1 / sqrt(num_obs_vec) %>% diff()

# TODO: Rao Blackwellize the dataset to reduce noise?
sim_time <- Sys.time()
pb <- txtProgressBar(style=3, min=1, max=num_obs_length * num_outer_sims)
pb_i <- 1
for (num_obs in num_obs_vec) {
    data_sim_list <- lapply(1:num_sims, function(...) { DrawData(num_obs) })
    for (sim in 1:num_outer_sims) {
        setTxtProgressBar(pb, pb_i)
        pb_i <- pb_i + 1
        data <- DrawData(num_obs)
        data_boot_list <- lapply(1:num_boots, function(...) { DrawBootstrap(data) })
        
        t_hat_sims <- map_dbl(data_sim_list, pluck("t_hat"))
        t_hat_boot <- map_dbl(data_boot_list, pluck("t_hat"))
        t_hat_norm <- rnorm(num_sims)
        
        delta_hat_sims <- map_dbl(data_sim_list, pluck("delta_hat"))
        delta_hat_boot <- map_dbl(data_boot_list, pluck("delta_hat"))
        delta_hat_norm <- rnorm(num_sims, mean=data$delta_hat, sd=data$sd_hat)
        
        GetKSDf <- function(x, y, metric, method) {
            data.frame(rhoinf=as.numeric(ks.test(x, y)$statistic),
                       metric=metric, method=method,
                       num_obs=num_obs, sim=sim)
        }
        
        rhoinf_df <- bind_rows(
            rhoinf_df,
            GetKSDf(t_hat_sims, t_hat_boot, "t_hat", "boot"),
            GetKSDf(t_hat_sims, t_hat_norm, "t_hat", "norm"),
            GetKSDf(t_hat_sims, delta_hat_boot, "delta_hat", "boot"),
            GetKSDf(t_hat_sims, delta_hat_norm, "delta_hat", "norm")
        )
    }
}
close(pb)
sim_time <- Sys.time() - sim_time
print(sim_time)


rhoinf_df %>%
    group_by(metric, method, num_obs) %>%
    summarize(rhoinf=median(rhoinf)) %>%
    ggplot() +
    geom_line(aes(x=1/sqrt(num_obs), y=rhoinf, color=method)) +
    scale_x_log10() + scale_y_log10() +
    facet_grid(metric ~ ., scales="free")


ggplot(rhoinf_df %>% filter(num_obs == num_obs_vec[1])) +
    geom_density(aes(x=rhoinf, color=method, fill=method, group=method), alpha=0.5) +
    facet_grid(num_obs ~ metric, scales="free") +
    scale_x_log10()


# Look at the QQplots using the last outer sim
grid.arrange(
    QQPlot(t_hat_sims, t_hat_boot) + ggtitle("t_hat boot"),
    QQPlot(t_hat_sims, t_hat_norm) + ggtitle("t_hat norm"),
    
    QQPlot(delta_hat_sims, delta_hat_boot) + ggtitle("delta_hat boot"),
    QQPlot(delta_hat_sims, delta_hat_norm) + ggtitle("delta_hat norm"),
    
    ncol=2    
)





# Do a single examples to make plots for the presentation

num_obs <- 100
num_boots <- 1000
num_sims <- 10000

data <- DrawData(num_obs)
data_boot_list <- lapply(1:num_boots, function(...) { DrawBootstrap(data) })
data_sim_list <- lapply(1:num_sims, function(...) { DrawData(num_obs) })
data_sim_truth_list <- lapply(1:num_sims, function(...) { DrawData(5000) })

t_hat_truth <- map_dbl(data_sim_truth_list, pluck("t_hat"))
t_hat_sims <- map_dbl(data_sim_list, pluck("t_hat"))
t_hat_boot <- map_dbl(data_boot_list, pluck("t_hat"))
t_hat_norm <- rnorm(num_sims)


GetCDF <- function(x, x_grid=NULL) {
    if (is.null(x_grid)) {
        num_points <- length(x) *4
        x_grid <- seq(min(x), max(x), length.out=num_points)
    }
    x_sort <- sort(x)
    fhat <- sapply(x_grid, function(x) { mean(x_sort <= x)})
    return(data.frame(x=x_grid, fhat=fhat))
}

x_grid <- seq(min(t_hat_truth), max(t_hat_truth), length.out=1000)

cdf_df <- bind_rows(
    GetCDF(t_hat_truth, x_grid) %>% mutate(stat="truth"),
    GetCDF(t_hat_boot, x_grid) %>% mutate(stat="boot"),
    GetCDF(t_hat_norm, x_grid) %>% mutate(stat="norm"),
    GetCDF(t_hat_sims, x_grid) %>% mutate(stat="sim")) %>%
    pivot_wider(id_cols=x, names_from=stat, values_from=fhat)
head(cdf_df)


png("~/Downloads/cdf_w_norm.png", width=8, height=4, units="in", res=300)
grid.arrange(
    ggplot(cdf_df) +
        geom_line(aes(x=x, y=boot, color="H_boot")) +
        geom_line(aes(x=x, y=norm, color="H_norm")) +
        geom_line(aes(x=x, y=sim, color="H_N")) +
        ylab("P(T <= x)") +
        ggtitle("Distribution functions")
    ,
    ggplot(cdf_df) +
        geom_line(aes(x=x, y=(boot - sim), color="H_boot - H_N")) +
        geom_line(aes(x=x, y=(sim - norm), color="H_N - H_norm"))  +
        ggtitle("Distribution functions differences") +
        ylab(NULL)
    , ncol=2
)
dev.off()

png("~/Downloads/cdf_no_norm.png", width=8, height=4, units="in", res=300)
grid.arrange(
    ggplot(cdf_df) +
        geom_line(aes(x=x, y=boot, color="H_boot")) +
        geom_line(aes(x=x, y=sim, color="H_N")) +
        ylab("P(T <= x)") +
        ggtitle("Distribution functions")
    ,
    ggplot(cdf_df) +
        geom_line(aes(x=x, y=(boot - sim), color="H_boot - H_N")) +
        ggtitle("Distribution functions differences") +
        ylab(NULL)
    , ncol=2
)
dev.off()

