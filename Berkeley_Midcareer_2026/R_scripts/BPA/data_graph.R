marr_df <- bats_env$marr_df
ggplot(marr_df) +
    geom_raster(aes(x=recapture_time, y=capture_time, fill=value)) +
    geom_raster(aes(x=recapture_time, y=capture_time, fill=value),
                data=marr_df %>% filter(value==0), fill="white") +
    scale_x_continuous(limits = c(1, 19), expand = c(0, 0)) +
    scale_y_continuous(limits = c(1, 18), expand = c(0, 0)) +
    xlab("Recapture time") + ylab("Capture time") +
    labs(fill="Count") + theme_bw() +
    theme(panel.grid = element_blank()) +
    ggtitle("Leisleri's bats catpure / recapture data") +
    scale_fill_gradient(low="light blue", high="black")
