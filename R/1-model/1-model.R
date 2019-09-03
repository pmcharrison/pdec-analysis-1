source("R/1-model/1-setup.R")

opt <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05,
  ppm = c(
    ltm_weight = 1,
    ltm_half_life = 10,
    noise = 0,
    stm_weight = 1,
    stm_duration = 0,
    order_bound = 10
  ),
  # optim = list(
  #   which = c("noise", "ltm_weight"),
  #   lower = c(0, 0),
  #   upper = c(10, 0.45),
  #   control = list(
  #     maxeval = 100,
  #     ftol_abs = 0.001
  #   ),
  #   na_penalty = 1
  # ),
  cp = list(
    method = "Mann-Whitney",
    t1_error_rate = 10000,
    startup = 20 
  )
)

model_dataset <- function(data, ppm_par, alphabet, opt) {
  
}

ppm_dataset(

dat <- readRDS(file = "output/data-00-participants.rds")

optim_res <- ppm_optim(dat, opt)
optim_analyses <- get_optimised_analyses(optim_res, dat, opt)

# saveRDS(optimised_par, "output/data-01-optimised-par.rds")
# saveRDS(par, "output/data-01-all-par.rds")
# saveRDS(optimised_analyses, "output/data-01-optimised-analyses.rds")
