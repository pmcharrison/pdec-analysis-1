source("R/1-model/1-setup.R")

par <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05, # seconds
  ppm = list(
    buffer_length_time = 2,
    buffer_length_items = 13,
    buffer_weight = 0.77,
    stm_half_life = 2,
    stm_weight = 0.53,
    ltm_weight = 0.005,
    noise = 0.9,
    order_bound = 10
  ),
  cp = list(
    method = "Mann-Whitney",
    t1_error_rate = 10000,
    startup = 20 
  ),
  alphabet = readRDS(file = "output/alphabet.rds")
)

dat <- readRDS(file = "output/data-01-participants.rds")
# dat <- dat %>% filter(subj == 1) # for testing only

# dat <- dat %>% filter(subj < 4 & trialN < 4 & block == 2) # for testing only
# dat <- dat %>% filter(subj == 1 & trialN < 4 & block == 1) # for testing only
 
dat <- add_idyom_ic(dat, par)
dat <- add_change_points(dat, par = par)

saveRDS(dat, "output/data-02-models.rds")
saveRDS(par, "output/data-02-model-par.rds")
