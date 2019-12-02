library(tidyverse)
library(ppm)

plot_mod <- function(par, opt, max_time = 25, plot_boundary_2 = TRUE) {
  mod <- new_ppm_decay(
    alphabet_size = 10000,
    order_bound = par$order_bound, 
    ltm_weight = par$ltm_weight,
    noise = 0,
    ltm_half_life = par$ltm_half_life,
    ltm_asymptote = par$ltm_asymptote,
    stm_weight = par$stm_weight, 
    stm_duration = par$stm_duration,
    buffer_weight = par$buffer_weight,
    buffer_length_time = par$buffer_length_time, 
    buffer_length_items = par$buffer_length_items,
    only_learn_from_buffer = par$only_learn_from_buffer, 
    only_predict_from_buffer = par$only_predict_from_buffer
  )
  time <- seq(from = 0, to = max_time, by = opt$tone_length)
  seq = seq_along(time)
  model_seq(mod,
            seq = seq,
            time = time, 
            train = TRUE,
            predict = FALSE, 
            zero_indexed = TRUE)
  boundary_1 <- par$buffer_length_items
  boundary_2 <- boundary_1 + par$stm_duration / opt$tone_length
  
  p <- tibble(time = time,
         pos = seq_along(time),
         weight = map2_dbl(time, pos, ~ get_weight(mod, 
                                                   n_gram = 1,
                                                   time = .x,
                                                   pos = .y,  
                                                   update_excluded = FALSE,
                                                   zero_indexed = TRUE))) %>% 
    ggplot(aes(pos, weight)) + 
    geom_line() + 
    scale_x_continuous("Tone number",
                       sec.axis = sec_axis(trans = ~ . * opt$tone_length, 
                                           name = "Time (s)")) +
    scale_y_continuous("Weight", limits = c(0, NA)) +
    geom_vline(xintercept = boundary_1, linetype = "dotted") +
    ggpubr::theme_pubr() +
    theme(aspect.ratio = 1)
  if (plot_boundary_2) {
    p <- p + geom_vline(xintercept = boundary_2, linetype = "dotted")
  } 
  p
}
# plot_mod()
# plot_mod(stm_duration = 0,
#          ltm_weight = 0.25,
#          ltm_half_life = 10,
#          ltm_asymptote = 0.05)
