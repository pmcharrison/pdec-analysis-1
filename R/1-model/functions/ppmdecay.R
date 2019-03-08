add_idyom_ic <- function(dat, par) {
  stopifnot(order(dat$subj, dat$block, dat$trialN) == seq_len(nrow(dat)))
  alphabet <- par$alphabet
  
  dat %>% 
    group_by(subj) %>% 
    mutate(
      time = estimate_trial_time(block, trialN),
      mod = ic_subj(seq, 
                    time,
                    alphabet = !!alphabet,
                    par = !!par,
                    sub = unique(subj))
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

ic_subj <- function(seqs, start_times, alphabet, par, subj) {
  stopifnot(is.list(seqs), length(subj) == 1)
  
  ppm_par <- eval(par$ppm)
  mod <- PPMdecay::new_model(alphabet = alphabet, max_order_bound = ppm_par$order_bound)
  
  message("Performing information_theoretic analyses on subject ", subj, "...")

  N <- length(seqs)
  pb <- utils::txtProgressBar(max = N, style = 3)
  res <- vector(mode = "list", length = N)
  for (i in seq_len(N)) {
    res[[i]] <- PPMdecay::predict_seq(
      mod, 
      seq = seqs[[i]],
      time = seq(from = start_times[i], 
                 by = par$tone_length, 
                 length.out = length(seqs[[i]])),
      options = ppm_par
    )
    
    utils::setTxtProgressBar(pb, value = i)
  }
  close(pb)
  
  res
}

ic_subj <- memoise::memoise(ic_subj, cache = memoise::cache_filesystem(path = "cache/ic_subj"))
