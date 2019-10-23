analyse_experiments <- function(data, opt, ppm_par) {
  map(dat, function(exp) {
    map(ppm_par, function(par) {
      set.seed(1)
      ppm_dataset(
        data = exp$data, 
        alphabet = exp$alphabet,
        ppm_par = par,
        opt = opt
      )
    })
  })
}
