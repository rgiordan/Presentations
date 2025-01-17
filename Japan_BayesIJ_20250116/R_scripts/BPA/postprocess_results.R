# Postprocess the results of run_leisler_bat_bootstrap.R for use in the paper.

library(tidyverse)
library(bayesijlib)
library(latex2exp)
library(gridExtra)

git_repo_dir <- "/home/rgiordan/Documents/git_repos/InfinitesimalJackknifeWorkbench/"
bpa_dir <- file.path(git_repo_dir, "src/bayes/example-models/BPA")
model_dir <- file.path(bpa_dir, "/Ch.07") 
base_filename <- file.path(model_dir, "leisleri_base_result.Rdata")
boot_filename <- file.path(model_dir, "leisleri_boot_result.Rdata")
data_filename <- file.path(model_dir, "leisleri_processed_data.Rdata")
opt_filename <- file.path(model_dir, "leisleri_opt_result.Rdata")

output_dir <- file.path(git_repo_dir, "writing/bayes/data/BPA")

load(base_filename)
load(boot_filename)
load(data_filename)
load(opt_filename)



#########################
# Examine the covariances.

num_exch_obs <- ncol(lp_draws)
boot_draws <- do.call(rbind, lapply(boot_results, function(x) { x$boot_par_means }))
boot_cov <- cov(boot_draws, boot_draws)    
boot_se <- GetCovarianceMatrixSE(boot_draws, boot_draws, FALSE)
rownames(boot_se) <- colnames(boot_se) <- rownames(boot_cov)

cov_long_df <-
    bind_rows(
        CovarianceMatrixToDataframe(ij_cov, remove_repeats=TRUE) %>%
            mutate(method="ij", metric="mean"),
        CovarianceMatrixToDataframe(num_exch_obs * bayes_cov, remove_repeats=TRUE) %>%
            mutate(method="bayes", metric="mean"),
        CovarianceMatrixToDataframe(num_exch_obs * boot_cov, remove_repeats=TRUE) %>%
            mutate(method="boot", metric="mean"),
        CovarianceMatrixToDataframe(ij_mcmc_se, remove_repeats=TRUE) %>%
            mutate(method="ij", metric="se"),
        CovarianceMatrixToDataframe(num_exch_obs * bayes_se, remove_repeats=TRUE) %>%
            mutate(method="bayes", metric="se"),
        CovarianceMatrixToDataframe(num_exch_obs * boot_se, remove_repeats=TRUE) %>%
            mutate(method="boot", metric="se")
    )


# parameter labels
unique(cov_long_df$row_variable)
GetVariableLabel <- function(variable, variable_label) {
    data.frame(variable, variable_label, stringsAsFactors=FALSE)
}

var_label_df <- bind_rows(
    GetVariableLabel("mean_p", "p_{\\mathrm{mean}}"),
    GetVariableLabel("log_sigma", "\\log \\sigma_y"),
    GetVariableLabel("mean_phi", "\\phi_{\\mathrm{mean}}")
)

# method labels
unique(cov_long_df$method)
GetMethodLabel <- function(method, method_label) {
    data.frame(method, method_label, stringsAsFactors=FALSE)
}

method_label_df <- bind_rows(
    GetMethodLabel("ij", "IJ"),
    GetMethodLabel("bayes", "Bayes"),
    GetMethodLabel("boot", "Boot")
)

# method_label_df <- bind_rows(
#     GetMethodLabel("ij", "\\hat{V}^{\\mathrm{IJ}}"),
#     GetMethodLabel("bayes", "\\hat{V}^{\\mathrm{bayes}}"),
#     GetMethodLabel("boot", "\\hat{V}^{\\mathrm{boot}}")
# )


cov_wide_df <-
    cov_long_df %>%
    pivot_wider(id_cols=c(row_variable, column_variable, method),
                names_from=metric, values_from=value) %>%
    filter(row_variable == column_variable) %>%
    rename(variable=row_variable) %>%
    inner_join(var_label_df, by="variable") %>%
    mutate(variable_label=factor(variable_label)) %>%
    inner_join(method_label_df, by="method") %>%
    mutate(method_label=factor(method_label))
    
levels(cov_wide_df$variable_label) <-
    TeX(sprintf("$%s$", levels(cov_wide_df$variable_label)))
# levels(cov_wide_df$method_label) <-
#     TeX(sprintf("$%s$", levels(cov_wide_df$method_label)))


cross_cov_wide_df <-
    cov_long_df %>%
    pivot_wider(id_cols=c(row_variable, column_variable, method),
                names_from=metric, values_from=value) %>%
    filter(row_variable != column_variable) %>%
    mutate(pars=paste(row_variable, column_variable)) %>%
    filter(row_variable != "sigma", column_variable != "sigma") %>%
    inner_join(var_label_df %>% rename(row_variable=variable),
               by="row_variable") %>%
    rename(row_variable_label=variable_label) %>%
    inner_join(var_label_df %>% rename(column_variable=variable),
               by="column_variable") %>%
    rename(column_variable_label=variable_label) %>%
    mutate(variable=paste(row_variable, column_variable, sep=",  \\quad")) %>%
    mutate(variable_label=paste(row_variable_label, column_variable_label, sep=", ")) %>%
    mutate(variable_label=factor(variable_label)) %>%
    inner_join(method_label_df, by="method") %>%
    mutate(method_label=factor(method_label))

levels(cross_cov_wide_df$variable_label) <-
    TeX(sprintf("$%s$", levels(cross_cov_wide_df$variable_label)))

    

################################
# Look at SE estimates

ij_mcmc_se %>% diag()
boot_ij_cov_se %>% diag()

bayes_se %>% diag()
boot_bayes_cov_se %>% diag()


ij_mcmc_se %>% diag() /
    boot_ij_cov_se %>% diag()

bayes_se %>% diag() /
    boot_bayes_cov_se %>% diag()


################################
# Define some useful variables.

num_mcmc_draws <- nrow(par_draws)

mcmc_means <- colMeans(par_draws)
opt_means

boot_time_sec_vec <- lapply(
    boot_results, function(x) { as.numeric(x$ij_mcmc_time, units="secs") }) %>% unlist()
boot_time_sec <- sum(boot_time_sec_vec)

ij_mcmc_time

boot_time_sec / as.numeric(ij_mcmc_time, units="secs")
boot_results[[1]]$ij_mcmc_time
num_boots <- length(boot_results)

###################
# Save

save(num_exch_obs,
     num_mcmc_draws,
     num_boots,
     cov_wide_df,
     cross_cov_wide_df,
     boot_time_sec,
     ij_mcmc_time,
     marr_df,
     num_se_blocks,
     num_se_draws,
     mcmc_means,
     opt_means,
     file=file.path(output_dir, "leisleri_results.Rdata"))




################################################
################################################
################################################

stop()

# Test some plotting 

MakePlot <- function(v, df, y_label, use_legend=FALSE) {
    plt <- ggplot(df %>% filter(variable == v)) +
        geom_bar(aes(y=mean, fill=method_label, x=(method_label)),
                 position="dodge", stat="Identity") +
        geom_errorbar(aes(ymin=mean - 2 * se, ymax=mean + 2 * se, x=method_label)) +
        facet_grid(. ~ variable_label, scales="free", labeller=label_parsed) +
        ylab(y_label) +
        theme(axis.title.x=element_blank())
    if (!use_legend) {
        plt <- plt + theme(legend.position="None")
    }
    return(plt)
}



plots <- list()
for (v in unique(cov_wide_df$variable)) {
    plots[[v]] <- MakePlot(v, cov_wide_df, "Variance")
}

#plots <- list()
for (v in unique(cross_cov_wide_df$variable)) {
    plots[[v]] <- MakePlot(v, cross_cov_wide_df, "Covariance")
}
grid.arrange(
    plots[[1]],
    plots[[2]],
    plots[[3]],
    plots[[4]],
    plots[[5]],
    plots[[6]],
    ncol=3
)



####################
# Cross covariances

ggplot(cross_cov_wide_df) +
    geom_bar(aes(y=mean, fill=method, x=method),
             position="dodge", stat="Identity") +
    geom_errorbar(aes(ymin=mean - 2 * se, ymax=mean + 2 * se, x=method)) +
    facet_grid(. ~ pars, scales="free") +
    ylab("Variance")



# Data graph

ggplot(marr_df) +
    geom_raster(aes(x=recapture_time, y=capture_time, fill=value)) +
    geom_raster(aes(x=recapture_time, y=capture_time, fill=value),
                data=marr_df %>% filter(value==0), fill="white") +
    scale_x_continuous(limits = c(1, 19), expand = c(0, 0)) +
    scale_y_continuous(limits = c(1, 18), expand = c(0, 0)) +
    xlab("Recapture time") + ylab("Capture time") +
    labs(fill="Count") + theme_bw() +
    theme_replace(panel.grid = element_blank(),
                  panel.border = element_line(colour = 'black', size = 2)) +
    ggtitle("Leisleri's bats catpure / recapture data") +
    scale_fill_gradient(low="light blue", high="black")

