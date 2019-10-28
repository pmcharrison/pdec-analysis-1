source("R/1-model/1-setup.R")
library(tidyverse)
theme_set(theme_bw())

opt <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05,
  response_time = 1,
  cp = list(
    method = "Mann-Whitney",
    t1_error_rate = 10000,
    startup = 20 
  )
)

ppm_par <- list(
  orig = list(
    buffer_length_time = 0,
    buffer_weight = 1,
    buffer_length_time = 0,
    buffer_length_items = 0,
    only_learn_from_buffer = FALSE,
    only_predict_from_buffer = FALSE,
    
    stm_weight = 1,
    stm_duration = 0,
    
    ltm_weight = 1,
    ltm_half_life = 1e70,
    ltm_asymptote = 1,
    
    noise = 0,
    order_bound = 4
  ),
  
  optim = list(
    buffer_length_time = 1e6,
    buffer_weight = 1,
    buffer_length_items = 15,
    only_learn_from_buffer = TRUE,
    only_predict_from_buffer = TRUE,
    
    stm_weight = 1,
    stm_duration = 15,
      
    ltm_weight = 0.02,
    ltm_half_life = 500,
    ltm_asymptote = 0, 
    
    noise = 1.3,
    order_bound = 4
  )
)

dat <- readRDS(file = "output/data-00-participants.rds")


res <- analyse_experiments(data, opt, ppm_par)
saveRDS(res, "output/model-results.rds")
p <- plot_experiments(res)
ggsave(paste0("output/plots/plot ", format(Sys.time()), " .png"), plot = p)
