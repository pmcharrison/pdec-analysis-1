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
  
  mod <- ppm::new_ppm_decay(alphabet_size = length(alphabet),
                            order_bound = par$ppm$order_bound,
                            buffer_length_time = par$ppm$buffer_length_time,
                            buffer_length_items = par$ppm$buffer_length_items,
                            buffer_weight = par$ppm$buffer_weight,
                            stm_half_life = par$ppm$stm_half_life,
                            stm_weight = par$ppm$stm_weight,
                            ltm_weight = par$ppm$ltm_weight,
                            noise = par$ppm$noise)
  
  message("Performing information_theoretic analyses on subject ", subj, "...")

  N <- length(seqs)
  pb <- utils::txtProgressBar(max = N, style = 3)
  res <- vector(mode = "list", length = N)
  for (i in seq_len(N)) {
    seq <- as.integer(factor(seqs[[i]], levels = alphabet)) - 1L
    res[[i]] <- ppm::model_seq(
      model = mod, 
      seq = seq,
      time = seq(from = start_times[i], 
                 by = par$tone_length, 
                 length.out = length(seqs[[i]]))
    )
    
    utils::setTxtProgressBar(pb, value = i)
  }
  close(pb)
  
  res
}

ic_subj <- memoise::memoise(ic_subj, cache = memoise::cache_filesystem(path = "cache/ic_subj"))
