add_idyom_ic <- function(dat, alphabet, par) {
  # Check that the data are organised by increasing time
  stopifnot(order(dat$subj, dat$block, dat$trialN) == seq_len(nrow(dat)))
  
  dat %>% 
    group_by(subj) %>% 
    mutate(idyom_ic = ic_subj(seq,
                              alphabet = !!alphabet,
                              par = !!par[c("ppm_order_bound", 
                                            "ppm_shortest_deterministic",
                                            "ppm_exclusion",
                                            "ppm_update_exclusion", 
                                            "ppm_escape",
                                            "ppm_half_life",
                                            "ppm_start",
                                            "ppm_end")],
                              sub = unique(subj))) %>% 
    ungroup()
}


ic_subj <- function(seqs, alphabet, par, subj) {
  stopifnot(is.list(seqs))
  mod <- new_model(alphabet = alphabet, max_order_bound = par$ppm_order_bound)
  
  message("Performing information_theoretic analyses on subject ", subj, "...")
  plyr::llply(seqs, function(seq) {
    predict_seq(mod, seq = seq, options = ppm_options(
      order_bound = par$ppm_order_bound,
      shortest_deterministic = par$ppm_shortest_deterministic,
      exclusion = par$ppm_exclusion,
      update_exclusion = par$ppm_update_exclusion,
      escape = par$ppm_escape, 
      decay = decay_exp(half_life = par$ppm_half_life, 
                        start = par$ppm_start,
                        end = par$ppm_end)
    ))
  }, .progress = "text")
}

ic_subj <- memoise::memoise(ic_subj, cache = memoise::cache_filesystem(path = "cache/ic_subj"))
