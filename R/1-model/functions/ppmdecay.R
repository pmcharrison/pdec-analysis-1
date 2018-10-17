ic_subj <- function(seqs, alphabet, par, subj) {
  stopifnot(is.list(seqs))
  mod <- new_model(alphabet = alphabet, max_order_bound = par$ppm_order_bound)
  
  message("Performing information_theoretic analyses on subject ", subj, "...")
  plyr::llply(seqs, function(seq) {
    predict_seq(mod, seq = seq, options = ppm_options(
      order_bound = par$ppm_order_bound,
      shortest_deterministic = par$ppm_shortest_deterministic,
      update_exclusion = par$ppm_update_exclusion,
      escape = par$ppm_escape
    ))
  }, .progress = "text")
}

ic_subj <- memoise::memoise(ic_subj, cache = memoise::cache_filesystem(path = "cache/ic_subj"))
