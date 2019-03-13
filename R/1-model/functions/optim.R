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
    add_change_points(par$cp, par$alphabet) %>% 
    mutate(model_reaction_time = mod_lag_tones * par$tone_length) %>% 
    filter(condition %in% 1:2) %>% 
    filter(correct) %>% 
    summarise(cost = mean(abs(model_reaction_time - RTadj), na.rm = TRUE) + 
                par$optim$na_penalty * mean(is.na(model_reaction_time))) %>% 
    as.numeric() %>% 
    print()
}

conduct_optimisations <- function(dat, par) {
  res <- list()
  message("Analysing individuals...")
  res$individual <- map(unique(dat$subj), function(s) c(
    subj = s,
    ppm_optim(dat %>% filter(subj == s), par)
  ))
  
  message("Analysing aggregated data...")
  res$combined <- ppm_optim(dat, par)
  res
}
