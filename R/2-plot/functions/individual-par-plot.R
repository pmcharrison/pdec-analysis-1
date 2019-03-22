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
    scale_y_continuous("Noise") + 
    theme(aspect.ratio = 1)
  
  p_individual_m1 <- cowplot::axis_canvas(p_individual_main, axis = "x", coord_flip = TRUE) +
    geom_boxplot(data = df_individual, aes(x = 0, y = ltm_weight), fill = "#7aadff") + 
    coord_flip()
  
  p_individual_m2 <- cowplot::axis_canvas(p_individual_main, axis = "y") +
    geom_boxplot(data = df_individual, aes(0, y = noise), fill = "#7aadff")
  
  p <- suppressWarnings({
    p_individual_main %>% 
      cowplot::insert_xaxis_grob(p_individual_m1, 
                                 position = "top", 
                                 height = grid::unit(0.1, "null")) %>%
      cowplot::insert_yaxis_grob(p_individual_m2, 
                                 position = "right",
                                 width = grid::unit(0.1, "null"))
  })
  
  suppressWarnings(ggsave("output/individual-par.eps", 
                          plot = suppressWarnings(cowplot::ggdraw(p)), 
                          width = 4.5, height = 4.5))
  
  write_csv(df_individual, "output/individual-par.csv")
  
  cowplot::ggdraw(p)
}
