library(tidyverse)
library(infer)


setwd("/home/rgiordan/Documents/git_repos/Presentations/DavisInterview_20211213/cis")
set.seed(123)

airbnb_raw <- read_csv("data/listings.csv")

airbnb_raw %>%
    group_by(room_type) %>%
    summarize(n=n())

# It's a bit different from the textbook
airbnb_raw %>%
    summarize(
        n =  sum(room_type == "Entire home/apt"),
        proportion = sum(room_type == "Entire home/apt") / n()) %>%
    pull(proportion)


# Clean and filter
airbnb <- airbnb_raw %>%
    filter(room_type == "Entire home/apt") %>%
    rename(price_str=price) %>%
    mutate(price_clean=price_str %>%
               str_replace("\\$", "") %>%
               str_replace(",", ""),
           price=as.numeric(price_clean),
           id=1:n()) %>%
    select(id, price)



mu  <- airbnb %>%
    summarize(mean_price = mean(price)) %>%
    pull(mean_price)

airbnb_samples <- airbnb %>%
    rep_sample_n(size=40, reps=1000)


estimates <-
    airbnb_samples %>%
    group_by(replicate) %>%
    summarize(mu_hat=mean(price))
    
ggplot(estimates) +
    geom_histogram(aes(x=mu_hat))