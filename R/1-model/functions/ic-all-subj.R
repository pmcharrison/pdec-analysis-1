# ic_all_subj <- function(subj, seq, alphabet, par) {
#   n_seq <- length(seq)
#   stopifnot(length(subj) == n_seq)
#   unique_subj <- subj %>% unique %>% order
#   x <- vector(mode = "list", length = n_seq)
#   pb <- utils::txtProgressBar(max = n_seq, style = 3)
#   for (i in seq_along(unique_subj)) {
#     s <- unique_subj[i]
#     x[dat$subj == s] <- ic_subj(seqs = dat$seq[dat$subj == s],
#                                 alphabet = alphabet,
#                                 par = par)
#     utils::setTxtProgressBar(pb, i)
#   }
#   close(pb)
#   x
# }
