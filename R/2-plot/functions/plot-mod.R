library(tidyverse)
library(ppm)

plot_mod <- function(par, opt) {
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
  time <- seq(from = 0, to = 25, by = opt$tone_length)
  seq = seq_along(time)
  model_seq(mod,
            seq = seq,
            time = time, 
            train = TRUE,
            predict = FALSE, 
            zero_indexed = TRUE)
  tibble(time = time,
         pos = seq_along(time),
         weight = map2_dbl(time, pos, ~ get_weight(mod, 
                                                   n_gram = 1,
                                                   time = .x,
                                                   pos = .y,  
                                                   update_excluded = FALSE,
                                                   zero_indexed = TRUE))) %>% 
    ggplot(aes(time, weight)) + 
    geom_line() + 
    scale_x_continuous("Time") +
    scale_y_continuous("Weight", limits = c(0, NA)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    ggpubr::theme_pubr() +
    theme(aspect.ratio = 1)
}
# 
# plot_mod()
# plot_mod(stm_duration = 0,
#          ltm_weight = 0.25,
#          ltm_half_life = 10,
#          ltm_asymptote = 0.05)
