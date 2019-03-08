source("R/1-model/1-setup.R")

par <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05, # seconds
  ppm = quote(PPMdecay::ppm_options(PPMdecay::decay_buffer(
    buffer_time = 2,
    buffer_items = 13,
    buffer_rate = 0.77,
    stm_half_life = 2,
    stm_rate = 0.53,
    ltm_rate = 0.1,
    noise = 0.5
  ), order_bound = 10)),
  cp = list(
    method = "Mann-Whitney",
    t1_error_rate = 10000,
    startup = 20 
  )
)

dat <- readRDS(file = "output/data-01-participants.rds")
# dat <- dat %>% filter(subj < 4 & trialN < 4 & block == 2) # for testing only
# dat <- dat %>% filter(subj < 4) # for testing only
dat <- dat %>% filter(subj == 1 & trialN < 4 & block == 1) # for testing only

alphabet <- readRDS(file = "output/alphabet.rds")
dat <- add_idyom_ic(dat, alphabet, par)
# dat <- add_change_points(dat, ic_col = "idyom_ic", label = "idyom", par = par)

saveRDS(dat, "output/data-02-models.rds")
yaml::write_yaml(par, "output/data-02-models.yaml")
