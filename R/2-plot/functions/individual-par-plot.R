plot_individual_par <- function(optimised_par) {
  df_individual <- optimised_par$individual %>% 
    map_dfr(function(x) {
      tibble(subj = x$subj, 
             par = par$optim$which,
             value = x$par)
    }) %>% 
    spread(par, value)
  
  p_individual_main <- df_individual %>% 
    ggplot(aes(x = ltm_weight, y = noise)) + 
    geom_point() + 
    scale_x_continuous("Long-term memory weight") +
    scale_y_continuous("Noise")
  
  p_individual_m1 <- cowplot::axis_canvas(p_individual_main, axis = "x") +
    geom_histogram(data = df_individual, aes(x = ltm_weight), bins = 15, fill = "black")
  
  p_individual_m2 <- cowplot::axis_canvas(p_individual_main, axis = "y", coord_flip = TRUE) +
    geom_histogram(data = df_individual, aes(x = noise), bins = 15, fill = "black") +
    coord_flip()
  
  p <- suppressWarnings({
    p_individual_main %>% 
      cowplot::insert_xaxis_grob(p_individual_m1, position = "top") %>% 
      cowplot::insert_yaxis_grob(p_individual_m2, position = "right")
  })
  
  suppressWarnings(ggsave("output/individual-par.eps", 
                          plot = suppressWarnings(cowplot::ggdraw(p)), 
                          width = 4.5, height = 4.5))
  
  write_csv(df_individual, "output/individual-par.csv")
}
