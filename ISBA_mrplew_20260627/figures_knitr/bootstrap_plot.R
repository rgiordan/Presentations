
method_names <- c(
  "mrplew"="MrPlew",
  "raking"="Raking",
  "boot_nonpar"="Bootstrap of MCMC (nonpararametric)",
  "boot_par"="Bootstrap of MCMC (parametric)",
  "post"="Posterior"
)

GetBootRow <- function(analysis_env) {
  analysis_env$freq_sd_df %>%
    mutate(post=analysis_env$save_list$mrp_sd_rootn) %>%
    mutate(analysis=analysis_env$name)
}

boot_df <- bind_rows(
  GetBootRow(alexander),
  GetBootRow(laxphilips),
  GetBootRow(stories)) %>%
  pivot_longer(cols=-analysis) %>%
  mutate(method_name=method_names[name])


boot_plt <- boot_df %>%
  filter(name != "raking", name != "post") %>%
  ggplot(aes(fill=method_name, y=value, x=analysis)) +
    geom_bar(position="dodge", stat="identity") +
    labs(fill="Method") +
    ylab("Frequentist standard deviation estimate") +
    theme(axis.title.x=element_blank()) +
    MethodColorScale(aesthetic="fill",
                     extra_levels=method_names[c(3, 4)])


var_plt <- boot_df %>%
  filter(name != "boot_nonpar", name != "boot_par", name != "post") %>%
  ggplot(aes(fill=method_name, y=value, x=analysis)) +
    geom_bar(position="dodge", stat="identity") +
    labs(fill="Method") +
    ylab("Frequentist standard deviation estimate") +
    theme(axis.title.x=element_blank()) +
    MethodColorScale(aesthetic="fill")





lax_var_plt <- boot_df %>%
  filter(name != "boot_nonpar", name != "boot_par", name != "post") %>%
  filter(analysis == "Same-Sex Marriage") %>%
  ggplot(aes(fill=method_name, y=value, x=analysis)) +
    geom_bar(position="dodge", stat="identity") +
    labs(fill="Method") +
    ylab("Frequentist standard deviation estimate") +
    theme(axis.title.x=element_blank()) +
    MethodColorScale(aesthetic="fill")


lax_var_boot_plt <- boot_df %>%
  filter(name != "boot_nonpar", name != "post") %>%
  filter(analysis == "Same-Sex Marriage") %>%
  ggplot(aes(fill=method_name, y=value, x=analysis)) +
    geom_bar(position="dodge", stat="identity") +
    labs(fill="Method") +
    ylab("Frequentist standard deviation estimate") +
    theme(axis.title.x=element_blank()) +
    MethodColorScale(aesthetic="fill",
    extra_levels=c("Bootstrap of MCMC (parametric)"))

lax_var_boot_bayes_plt <- boot_df %>%
  filter(name != "boot_nonpar") %>%
  filter(analysis == "Same-Sex Marriage") %>%
  ggplot(aes(fill=method_name, y=value, x=analysis)) +
    geom_bar(position="dodge", stat="identity") +
    labs(fill="Method") +
    ylab("Frequentist standard deviation estimate") +
    theme(axis.title.x=element_blank()) +
    MethodColorScale(aesthetic="fill", 
                     extra_levels=c("Bootstrap of MCMC (parametric)", "Posterior"))
