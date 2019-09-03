get_optimised_analyses <- function(optim_res, dat, opt) {
  ppm_dataset(dat, coef = optim_res$par, opt)
}

ppm_optim <- function(dat, opt) {
  nloptr::sbplx(
    x0 = opt$ppm[opt$optim$which],
    fn = ppm_cost,
    lower = opt$optim$lower,
    upper = opt$optim$upper,
    nl.info = FALSE,
    control = opt$optim$control,
    dat, 
    opt
  )
}

ppm_dataset <- function(dat, coef, opt) {
  ppm_par <- opt$ppm
  ppm_par[opt$optim$which] <- coef
  dat %>% 
    add_ppm_ic(ppm_par, opt$alphabet, opt$tone_length) %>% 
    add_change_points(opt$cp, opt$alphabet) %>% 
    mutate(model_reaction_time = mod_lag_tones * opt$tone_length)
}

ppm_cost <- function(coef, dat, opt) {
  dat %>% 
    ppm_dataset(coef, opt) %>% 
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

add_ppm_ic <- function(dat, ppm_par, alphabet, tone_length) {
  stopifnot(order(dat$subj, dat$block, dat$trialN) == seq_len(nrow(dat)))
  
  dat %>% 
    group_by(subj) %>% 
    mutate(
      time = estimate_trial_time(block, trialN),
      mod = ic_subj(seq, 
                    time,
                    alphabet = !!alphabet,
                    ppm_par = !!ppm_par,
                    sub = unique(subj),
                    tone_length = !!tone_length)
    ) %>% 
    ungroup()
}

estimate_trial_time <- function(block, trialN) {
  estimate_block_time(block) + (trialN - 1) * 8
}

estimate_block_time <- function(block) {
  # Each sequence is 7 seconds long, add a second for response -> 8 seconds per trial
  # 60 trials per block -> 480 seconds per block, + 20 seconds for break -> 500 seconds per block
  
  # Blocks 1-5 are on day 1 
  # Block 6 is on day 2
  # Block 7 is on day 3
  recode(block,
         `1` = 8.1 * 60 * 0,
         `2` = 8.1 * 60 * 1,
         `3` = 8.1 * 60 * 2,
         `4` = 8.1 * 60 * 3,
         `5` = 8.1 * 60 * 4,
         `6` = 24 * 60 * 60,
         `7` = 7 * 7 * 24 * 60 * 60)
}

ic_subj <- function(seqs, start_times, alphabet, ppm_par, subj, tone_length) {
  stopifnot(is.list(seqs), length(subj) == 1)
  
  print("Instantiating a PPM model with the following parameters: ")
  print(ppm_par)
  
  mod <- ppm::new_ppm_decay(alphabet_size = length(alphabet),
                            order_bound = ppm_par[["order_bound"]],
                            buffer_length_time = ppm_par[["buffer_length_time"]],
                            buffer_length_items = ppm_par[["buffer_length_items"]],
                            buffer_weight = ppm_par[["buffer_weight"]],
                            stm_half_life = ppm_par[["stm_half_life"]],
                            stm_weight = ppm_par[["stm_weight"]],
                            ltm_weight = ppm_par[["ltm_weight"]],
                            noise = ppm_par[["noise"]])
  
  message("Performing information_theoretic analyses on subject ", subj, "...")
  
  N <- length(seqs)
  pb <- utils::txtProgressBar(max = N, style = 3)
  res <- vector(mode = "list", length = N)
  for (i in seq_len(N)) {
    seq <- factor(seqs[[i]], levels = alphabet)
    res[[i]] <- ppm::model_seq(
      model = mod, 
      seq = seq,
      time = seq(from = start_times[i], 
                 by = tone_length, 
                 length.out = length(seqs[[i]])),
      return_distribution = FALSE
    )
    
    utils::setTxtProgressBar(pb, value = i)
  }
  close(pb)
  
  res
}

ic_subj <- memoise::memoise(ic_subj, cache = memoise::cache_filesystem(path = "cache/ic_subj"))
