prediction_constants_df <- sim_data$prediction_constants_df
predictions_band_df <- sim_data$predictions_band_df
plt <- predictions_band_df %>%
    ggplot(aes(x=delta, color=method, group=name)) +
    geom_ribbon(alpha=0.1, aes(ymin=q10, ymax=q90, fill=name, color=NULL)) +
    geom_line(aes(y=value, x=delta, color=method, group=name), 
            data=prediction_constants_df)

print(plt)