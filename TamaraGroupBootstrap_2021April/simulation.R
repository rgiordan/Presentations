library(tidyverse)
library(gridExtra)


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
DrawData <- function(num_obs, mean1=1.0, mean2=1.0, sd1=0.3, sd2=0.6) {
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


ComputeCDF <- function(x) {
    n <- length(x)
    return(list(x=sort(x), cdf=(1:n)/n))
}

num_outer_sims <- 100
num_boots <- 1000
num_sims <- 1000

num_obs_min <- 20
num_obs_max <- 1000
num_obs_length <- 20
num_obs_vec <- floor(seq(sqrt(num_obs_min), sqrt(num_obs_max), length.out=num_obs_length)^2)
rhoinf_df <- data.frame()

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


ggplot(rhoinf_df) +
    geom_density(aes(x=rhoinf, color=method, fill=method, group=method), alpha=0.5) +
    facet_grid(num_obs ~ metric) +
    scale_x_log10()


# Look at the QQplots using the last outer sim
QQPlot <- function(x, y) {
    qq_stats <- qqplot(x, y);
    ggplot(data.frame(x=qq_stats$x, y=qq_stats$y)) +
        geom_point(aes(x=x, y=y)) +
        geom_abline(aes(slope=1, intercept=0))
}


grid.arrange(
    QQPlot(t_hat_sims, t_hat_boot) + ggtitle("t_hat boot"),
    QQPlot(t_hat_sims, t_hat_norm) + ggtitle("t_hat norm"),
    
    QQPlot(delta_hat_sims, delta_hat_boot) + ggtitle("delta_hat boot"),
    QQPlot(delta_hat_sims, delta_hat_norm) + ggtitle("delta_hat norm"),
    
    ncol=2    
)


