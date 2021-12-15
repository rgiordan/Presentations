library(tidyverse)
library(infer)
library(latex2exp)

setwd("/home/rgiordan/Documents/git_repos/Presentations/DavisInterview_20211213/cis")
set.seed(123)


source("load_data.R")


##############


ggplot(estimates) +
    geom_histogram(aes(x=mu_hat), fill="dodgerblue3", color="lightgrey", bins=30) +
    geom_vline(aes(xintercept=mu)) +
    ylab("Count") + 
    xlab("Price per night (Canadian dollars)") +
    PlotAxisArrow(mu_hat, height=500)


ggplot(estimates) +
    geom_histogram(aes(x=mu_hat), fill="dodgerblue3", color="lightgrey", bins=30, alpha=0.1) +
    geom_vline(aes(xintercept=mu), alpha=0.1) +
    ylab("Count") + 
    xlab(TeX("Estimated Price per night (Canadian dollars): $\\hat{\\mu}$")) +
    PlotAxisArrow(mu_hat, height=500)


ggplot(estimates) +
    geom_histogram(aes(x=mu_hat), fill="dodgerblue3", color="lightgrey", bins=30, alpha=0.1) +
    geom_vline(aes(xintercept=mu), alpha=0.1) +
    ylab("Count") + 
    xlab("Price per night (Canadian dollars)") +
    PlotAxisArrow(mu_hat, height=500)




ggplot(estimates) +
    geom_histogram(aes(x=mu_hat), fill="dodgerblue3", color="lightgrey", bins=30) +
    geom_vline(aes(xintercept=mu)) +
    ylab("Count") + 
    xlab("Price per night (Canadian dollars)") +
    PlotAxisArrow(mu_hat_lower, height=500) +
    PlotAxisArrow(mu_hat_upper, height=500) +
    geom_segment(
        aes(x=mu_lower, xend = mu_upper, y=height, yend=height, group=replicate),
        arrow=arrow(ends = "both", angle = 90, length = unit(.2,"cm")),
        size=3,
        data=data.frame(
            mu_lower=mu_hat_lower,
            mu_upper=mu_hat_upper,
            replicate=0,
            height=500))



ggplot(estimates) +
    geom_histogram(aes(x=mu_hat), fill="dodgerblue3", color="lightgrey", bins=30, alpha=0.2) +
    geom_vline(aes(xintercept=mu)) +
    ylab("Count") + 
    xlab("Price per night (Canadian dollars)") +
    geom_segment(
        aes(x=mu_lower, xend = mu_upper, y=height, yend=height, group=replicate, color=covers),
             arrow=arrow(ends = "both", angle = 90, length = unit(.2,"cm")),
             data=segments) +
    scale_color_manual(name="Contains the truth:", values=c("black", "red"), breaks=c(TRUE, FALSE))




