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

ppm_dataset <- function(dat, coef, par) {
  ppm_par <- par$ppm
  ppm_par[par$optim$which] <- coef
  dat %>% 
    add_idyom_ic(ppm_par, par$alphabet, par$tone_length) %>% 
    add_change_points(par$cp, par$alphabet) %>% 
    mutate(model_reaction_time = mod_lag_tones * par$tone_length)
}

# ppm_cost <- function(coef, dat, par) {
#   dat %>% 
#     ppm_dataset(coef, par) %>% 
#     filter(condition %in% 1:2) %>% 
#     filter(correct) %>% 
#     summarise(cost = mean(abs(model_reaction_time - RTadj), na.rm = TRUE) + 
#                 par$optim$na_penalty * mean(is.na(model_reaction_time))) %>% 
#     as.numeric() %>% 
#     print()
# }

ppm_cost <- function(coef, dat, par) {
  dat %>% 
    ppm_dataset(coef, par) %>% 
    select(subj, cond, block, RTadj, model_reaction_time) %>% 
    filter(!is.na(cond)) %>% 
    group_by(subj, cond, block) %>% 
    summarise_all(funs(mean), na.rm = TRUE) %>% 
    ungroup() %>% 
    select(-subj) %>%
    group_by(cond, block) %>% 
    summarise_all(funs(mean)) %>% 
    mutate(err = (model_reaction_time - RTadj) ^ 2) %>% 
    pull(err) %>% 
    mean() %>% 
    print()
}

conduct_optimisations <- function(dat, par) {
  res <- list()
  
  res$individual <- if (par$analyse_individuals) {
    message("Analysing individuals...")
    map(unique(dat$subj), function(s) c(
      subj = s,
      ppm_optim(dat %>% filter(subj == s), par)
    ))
  }
    
  res$combined <- if (par$analyse_combined) {
    message("Analysing aggregated data...")
    ppm_optim(dat, par)
  }

  res
}

get_optimised_analyses <- function(dat, par, optimised_par) {
  list(
    individual = if (par$analyse_individuals)
      get_individual_optimised_analyses(dat, par, optimised_par),
    
    combined = if (par$analyse_combined)
      get_combined_optimised_analyses(dat, par, optimised_par)
  )
}

get_individual_optimised_analyses <- function(dat, par, optimised_par) {
  ind_par <- optimised_par$individual
  purrr::map_dfr(ind_par, function(x) {
    dat %>% 
      filter(subj == x$subj) %>% 
      ppm_dataset(x$par, par)
  })
}

get_combined_optimised_analyses <- function(dat, par, optimised_par) {
  ppm_dataset(dat, coef = optimised_par$combined$par, par)
}
