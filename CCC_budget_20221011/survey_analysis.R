library(tidyverse)

working_dir <- "~/Downloads"
df <- read.csv(file.path(working_dir, "Copy of Anonymous financial survey - Form Responses 1.csv"))

# Replace the dollar-sign-and-comma formatting with actual numeric values
raw_income <- df$X2021.estimated.annual.income %>%
    as.character() %>%
    str_replace("^\\$", "") %>%
    str_replace(",", "") %>%
    as.numeric()

# Sanity check that the conversion to numbers worked correctly
cbind(income, df$X2021.estimated.annual.income %>% as.character())

# I'll be looking at the income distribution, so we may as well sort it
income <- sort(income)
num_families <- length(income)


# Doesn't look like a single power law
plot(log10(1:length(income)), log10(income))


# A function to calculate how much tuition we charge a familiy.
# Arguemnts:
#   income: The family's income (can be a vector)
#   mean_tuition: The budgeted tuition divided by number of families
#   rate: The proportion of a family's income that goes to tuition if not capped (5% by default)
#   min_prop: The minimum proportion of mean_tuition a family can pay (10% by default)
#   min_prop: The maximum proportion of mean_tuition a family can pay (200% by default)
EvaluateTuition <- function(income, mean_tuition, rate=0.05, min_prop=0.1, max_prop=2.0) {
    min_tuition <- min_prop * mean_tuition
    max_tuition <- max_prop * mean_tuition
    tuition <- rate * income
    tuition <- ifelse(tuition < min_tuition, min_tuition, tuition)
    tuition <- ifelse(tuition > max_tuition, max_tuition, tuition)
    return(tuition)
}

# The mean tuition is set by the school budget.
mean_tuition <- 7300
tuition <- EvaluateTuition(income, mean_tuition=mean_tuition)

# The "mean tuition" is not really a mean tuition with the unadjusted incomes 
# Maybe debt &c would knock down the original number
mean(tuition) / mean_tuition

# Let's simulate the variability in the incomes by sampling from the observed

# incomes with replacement a bunch of times and computing the amount we get
tuition_sims <- c()
num_sims <- 1000
for (sim in 1:num_sims) {
    income_boot <- sample(income, replace=TRUE) %>% sort()
    tuition_boot <- EvaluateTuition(income_boot, mean_tuition=mean_tuition)
    tuition_sims <- c(tuition_sims, sum(tuition_boot))        
}

# Let's look at the simulations
sim_df <- data.frame(sim=1:num_sims, tuition=tuition_sims, mean_tuition=mean_tuition)

sd(sim_df$tuition / num_families)
sd(sim_df$tuition / num_families) / mean(sim_df$tuition / num_families)

ggplot(sim_df) +
    geom_histogram(aes(x=tuition / num_families), bins=50) +
    geom_vline(aes(xintercept=mean_tuition), color="red") +
    expand_limits(x=0)
