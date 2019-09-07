ppm_dataset <- function(data, alphabet, ppm_par, opt) {
  data %>% 
    add_ppm_ic(ppm_par, alphabet, opt$tone_length) %>% 
    add_change_points(opt$cp, alphabet) %>% 
    mutate(model_reaction_time = mod_lag_tones * opt$tone_length)
}

add_ppm_ic <- function(data, ppm_par, alphabet, tone_length) {
  stopifnot(order(data$subj, data$block, data$trialN) == seq_len(nrow(data)))
  
  data %>% 
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
  
  stopifnot(all(block %in% 1:5))
  
  recode(block,
         `1` = 8.1 * 60 * 0,
         `2` = 8.1 * 60 * 1,
         `3` = 8.1 * 60 * 2,
         `4` = 8.1 * 60 * 3,
         `5` = 8.1 * 60 * 4)
  # `6` = 24 * 60 * 60,
  # `7` = 7 * 7 * 24 * 60 * 60)
}

ic_subj <- function(seqs, start_times, alphabet, ppm_par, subj, tone_length) {
  stopifnot(is.list(seqs), length(subj) == 1)
  
  print("Instantiating a PPM model with the following parameters: ")
  print(ppm_par)
  
  mod <- ppm::new_ppm_decay(alphabet_size = length(alphabet),
                            ltm_weight = ppm_par[["ltm_weight"]],
                            ltm_half_life = ppm_par[["ltm_half_life"]],
                            noise = ppm_par[["noise"]],
                            stm_weight = ppm_par[["stm_weight"]],
                            stm_duration = ppm_par[["stm_duration"]],
                            order_bound = ppm_par[["order_bound"]])
  
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

R.utils::mkdirs("/Downloads/peter/cache/ic_subj")
ic_subj <- memoise::memoise(ic_subj, 
                            cache = memoise::cache_filesystem(path = "/Downloads/peter/cache/ic_subj"))

cp_trial <- function(row, cp_par, alphabet) {
  x <- row$mod[[1]]$information_content
  stopifnot(is.numeric(x))
  
  cp <- cpm::detectChangePoint(x, 
                               cpmType = cp_par$method, 
                               ARL0 = cp_par$t1_error_rate,
                               startup = cp_par$startup)
  cp_stat <- rep(as.numeric(NA), times = length(x))
  cp_stat[seq_along(cp$Ds)] <- cp$Ds
  cp_stat[seq_len(cp_par$startup - 1L)] <- as.numeric(NA)
  res <- list(
    statistic = cp_stat,
    change_detected = cp$changeDetected,
    pos_when_change_detected = if (cp$changeDetected) cp$detectionTime else as.integer(NA)
  )
  # res$lag_tones <- res$pos_when_change_detected - (row$transition + length(alphabet))
  res$lag_tones <- res$pos_when_change_detected - row$transition
  res
}


add_change_points <- function(data, cp_par, alphabet) {
  message("Computing change points...")
  
  N <- nrow(data)
  
  data$mod_change_detected <- rep(as.logical(NA), times = N)
  data$mod_pos_when_change_detected <- rep(as.integer(NA), times = N)
  data$mod_lag_tones <- rep(as.integer(NA), times = N)
  
  pb <- utils::txtProgressBar(max = N, style = 3)
  
  for (i in seq_len(N)) {
    cp <- cp_trial(data[i, ], cp_par, alphabet)
    data$mod[[i]]$cp_statistic <- cp$statistic
    data$mod_change_detected[i] <- cp$change_detected
    data$mod_pos_when_change_detected[i] <- cp$pos_when_change_detected
    data$mod_lag_tones[i] <- cp$lag_tones
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  
  data
}

R.utils::mkdirs("/Downloads/peter/cache/add_change_points")
add_change_points <- memoise::memoise(
  add_change_points, 
  cache = memoise::cache_filesystem("/Downloads/peter/cache/add_change_points")
)
