ppm_optim <- function(data, opt) {
  nloptr::sbplx(
    x0 = opt$ppm[opt$optim$which],
    fn = ppm_cost,
    lower = opt$optim$lower,
    upper = opt$optim$upper,
    nl.info = FALSE,
    control = opt$optim$control,
    data = data, 
    alphabet = alphabet,
    opt = opt
  )
}

get_optimised_analyses <- function(optim_res, data, alphabet, opt) {
  ppm_dataset(data, coef = optim_res$par, alphabet = alphabet, opt = opt)
}
