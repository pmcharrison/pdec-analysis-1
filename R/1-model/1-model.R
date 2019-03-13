source("R/1-model/1-setup.R")

par <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05, # seconds
  ppm = c(
    buffer_length_time = 2,
    buffer_length_items = 13,
    buffer_weight = 0.77,
    stm_half_life = 2,
    stm_weight = 0.53,
    ltm_weight = 0.005,
    noise = 0.9,
    order_bound = 10
  ),
  optim = list(
    which = c("noise", "ltm_weight"),
    lower = c(0, 0),
    upper = c(10, 0.53),
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

dat <- readRDS(file = "output/data-01-participants.rds")
dat <- dat %>% filter(subj == 1) # for testing only

ppm_optim <- function(dat, par) {
  nloptr::sbplx(
    x0 = par$ppm[par$optim$which],
    fn = ppm_cost,
    lower = par$optim$lower,
    upper = par$optim$upper,
    nl.info = FALSE,
    control = par$optim$control,
    dat, 
    par
  )
}

ppm_cost <- function(coef, dat, par) {
  ppm_par <- par$ppm
  ppm_par[par$optim$which] <- coef
  dat %>% 
    add_idyom_ic(ppm_par, par$alphabet, par$tone_length) %>% 
    add_change_points(par) %>% 
    mutate(model_reaction_time = mod_lag_tones * par$tone_length) %>% 
    filter(condition %in% 1:2) %>% 
    filter(correct) %>% 
    summarise(cost = mean(abs(model_reaction_time - RTadj), na.rm = TRUE) + 
                par$optim$na_penalty * mean(is.na(model_reaction_time))) %>% 
    as.numeric() %>% 
    print()
}

x1 <- ppm_optim(dat, par)



# dat <- dat %>% filter(subj < 4 & trialN < 4 & block == 2) # for testing only
# dat <- dat %>% filter(subj == 1 & trialN < 4 & block == 1) # for testing only

dat <- add_idyom_ic(dat, par)
dat <- add_change_points(dat, par = par)

saveRDS(dat, "output/data-02-models.rds")
saveRDS(par, "output/data-02-model-par.rds")
