source("R/1-model/1-setup.R")

par <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05, # seconds
  ppm = c(
    buffer_length_time = 2,
    buffer_length_items = 15,
    buffer_weight = 1.6,
    stm_half_life = 0.8,
    stm_weight = 0.45,
    ltm_weight = 0.005,
    noise = 0.5,
    order_bound = 10
  ),
  optim = list(
    which = c("noise", "ltm_weight"),
    lower = c(0, 0),
    upper = c(10, 0.45),
    control = list(
      maxeval = 50,
      ftol_abs = 0.01
    ),
    na_penalty = 1
  ),
  cp = list(
    method = "Mann-Whitney",
    t1_error_rate = 10000,
    startup = 20 
  ),
  alphabet = readRDS(file = "output/alphabet.rds")
)

dat <- readRDS(file = "output/data-00-participants.rds")

res_opt <- conduct_optimisations(dat, par)

saveRDS(res_opt, "output/data-01-optimised-par.rds")
saveRDS(par, "output/data-01-all-par.rds")
