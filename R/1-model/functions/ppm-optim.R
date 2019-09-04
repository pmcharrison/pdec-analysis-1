ppm_optim <- function(data, opt) {
  nloptr::sbplx(
    x0 = opt$ppm[opt$optim$which],
    fn = ppm_cost,
    lower = opt$optim$lower,
    upper = opt$optim$upper,
    nl.info = FALSE,
    control = opt$optim$control,
    data = data, 
    alphabet = alphabet,
    opt = opt
  )
}

ppm_cost <- function(coef, data, alphabet, opt) {
  stop("Out of date")
  data %>% 
    ppm_dataset(alphabet, coef, opt) %>% 
    select(subj, cond, block, RTadj, model_reaction_time) %>% 
    filter(!is.na(cond)) %>% 
    mutate(model_reaction_time = if_else(model_reaction_time < 0,
                                         as.numeric(NA),
                                         model_reaction_time)) %>% 
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

get_optimised_analyses <- function(optim_res, data, alphabet, opt) {
  ppm_dataset(data, alphabet = alphabet, coef = optim_res$par, opt = opt)
}
