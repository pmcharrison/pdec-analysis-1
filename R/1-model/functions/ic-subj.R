ic_subj <- function(seqs, alphabet, par, msg = NULL) {
  stopifnot(is.list(seqs))
  mod <- new_model(alphabet = alphabet, max_order_bound = par$order_bound)
  x <- map(seqs, function(seq) {
    predict_seq(mod, seq = seq, options = ppm_options(order_bound = par$order_bound))
  })
  if (!is.null(msg)) message(msg)
  x
}
