ic_subj <- function(seqs, alphabet, par, subj) {
  stopifnot(is.list(seqs))
  mod <- new_model(alphabet = alphabet, max_order_bound = par$order_bound)
  
  message("Performing information_theoretic analyses on subject ", subj, "...")
  plyr::llply(seqs, function(seq) {
    predict_seq(mod, seq = seq, options = ppm_options(order_bound = par$order_bound))
  }, .progress = "text")
}

ic_subj <- memoise::memoise(ic_subj, cache = memoise::cache_filesystem(path = "cache/ic_subj"))
