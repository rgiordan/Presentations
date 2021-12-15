#airbnb_raw <- read_csv("data/listings.csv")
# save(airbnb_raw, file="data/listings.Rdata")
load("data/listings.Rdata")


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
    rep_sample_n(size=200, reps=4000)


estimates <-
    airbnb_samples %>%
    group_by(replicate) %>%
    summarize(mu_hat=mean(price))


alpha <- 0.9
quantile(estimates$mu_hat, 0.9)
quantile(estimates$mu_hat, (1 - !!alpha) / 2)

estimates_quantiles <-
    estimates %>%
    summarize(mu_hat_med=median(mu_hat),
              mu_lower=quantile(mu_hat, (1 - !!alpha) / 2),
              mu_upper=quantile(mu_hat, 1 - (1 - !!alpha) / 2)) %>%
    mutate(w_lower=mu_hat_med - mu_lower,
           w_upper=mu_upper - mu_hat_med)
estimates_quantiles
w_lower <- estimates_quantiles$w_lower
w_upper <- estimates_quantiles$w_upper


PlotAxisArrow <- function(loc, height=10) {
    geom_segment(
        aes(x=!!loc, xend=!!loc, y=!!height, yend=0),
        arrow=arrow())
}




segments <- 
    estimates %>%
    filter(replicate <= 100) %>%
    mutate(mu_lower=mu_hat - !!w_lower,
           mu_upper=mu_hat + !!w_upper,
           height=30 * replicate,
           covers=((mu > mu_lower) & (mu < mu_upper)))
sum(segments$covers)



mu_hat <- quantile(estimates$mu_hat, 0.975)
mu_hat_lower <- mu_hat - 1.1 * abs(mu_hat - mu)
mu_hat_upper <- mu_hat + w_upper

