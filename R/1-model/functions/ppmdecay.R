add_idyom_ic <- function(dat, alphabet, par) {
  stopifnot(order(dat$subj, dat$block, dat$trialN) == seq_len(nrow(dat)))
  
  dat %>% 
    group_by(subj) %>% 
    mutate(mod = ic_subj(seq, 
                         alphabet = !!alphabet,
                         par = !!par,
                         sub = unique(subj))) %>% 
    ungroup()
}


ic_subj <- function(seqs, alphabet, par, subj) {
  stopifnot(is.list(seqs), length(subj) == 1)
  
  ppm_par <- eval(par$ppm)
  mod <- PPMdecay::new_model(alphabet = alphabet, max_order_bound = ppm_par$order_bound)
  
  message("Performing information_theoretic analyses on subject ", subj, "...")

  N <- length(seqs)
  pb <- utils::txtProgressBar(max = N, style = 3)
  res <- vector(mode = "list", length = N)
  for (i in seq_len(N)) {
    res[[i]] <- PPMdecay::predict_seq(mod, seq = seqs[[i]], options = ppm_par)
    utils::setTxtProgressBar(pb, value = i)
  }
  close(pb)
  
  res
}

ic_subj <- memoise::memoise(ic_subj, cache = memoise::cache_filesystem(path = "cache/ic_subj"))
