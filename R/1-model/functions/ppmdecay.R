ppm_par <- function(order_bound = 10L,
                    shortest_deterministic = FALSE,
                    exclusion = FALSE,
                    update_exclusion = FALSE,
                    escape = "A",
                    half_life = 50,
                    start = 1,
                    end = 1) {
  x <- as.list(environment())
  x[order(names(x))]
}

add_idyom_ic <- function(dat, alphabet, par) {
  # Check that the data are organised by increasing time
  stopifnot(order(dat$subj, dat$block, dat$trialN) == seq_len(nrow(dat)))
  
  dat <- group_by(dat, subj)
  
  for (i in seq_along(par$mod)) {
    label <- names(par$mod)[i]
    dat <- mutate(dat, 
                  !!label := ic_subj(seq,
                                     alphabet = !!alphabet,
                                     par = !!par$mod[[i]][c("order_bound", 
                                                            "shortest_deterministic",
                                                            "exclusion",
                                                            "update_exclusion", 
                                                            "escape",
                                                            "half_life",
                                                            "start",
                                                            "end")],
                                     sub = unique(subj)))
  }
  dat %>% ungroup()
}


ic_subj <- function(seqs, alphabet, par, subj) {
  stopifnot(is.list(seqs))
  mod <- new_model(alphabet = alphabet, max_order_bound = par$order_bound)
  
  message("Performing information_theoretic analyses on subject ", subj, "...")
  plyr::llply(seqs, function(seq) {
    predict_seq(mod, seq = seq, options = ppm_options(
      order_bound = par$order_bound,
      shortest_deterministic = par$shortest_deterministic,
      exclusion = par$exclusion,
      update_exclusion = par$update_exclusion,
      escape = par$escape, 
      decay = decay_exp(half_life = par$half_life, 
                        start = par$start,
                        end = par$end)
    ))
  }, .progress = "text")
}

ic_subj <- memoise::memoise(ic_subj, cache = memoise::cache_filesystem(path = "cache/ic_subj"))
