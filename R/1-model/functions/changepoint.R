cp_trial <- function(row, par) {
  x <- row$mod[[1]]$information_content
  stopifnot(is.numeric(x))
  
  cp <- cpm::detectChangePoint(x, 
                               cpmType = par$cp$method, 
                               ARL0 = par$cp$t1_error_rate,
                               startup = par$cp$startup)
  cp_stat <- rep(as.numeric(NA), times = length(x))
  cp_stat[seq_along(cp$Ds)] <- cp$Ds
  cp_stat[seq_len(par$cp$startup - 1L)] <- as.numeric(NA)
  res <- list(
    statistic = cp_stat,
    change_detected = cp$changeDetected,
    pos_when_change_detected = if (cp$changeDetected) cp$detectionTime else as.integer(NA)
  )
  res$lag_tones <- res$pos_when_change_detected - (row$transition + length(par$alphabet))
  res
}


add_change_points <- function(dat, par) {
  message("Computing change points for all participants...")
  
  N <- nrow(dat)
  
  dat$mod_change_detected <- rep(as.logical(NA), times = N)
  dat$mod_pos_when_change_detected <- rep(as.integer(NA), times = N)
  dat$mod_lag_tones <- rep(as.integer(NA), times = N)

  pb <- utils::txtProgressBar(max = N, style = 3)
  
  for (i in seq_len(N)) {
    cp <- cp_trial(dat[i, ], par)
    dat$mod[[i]]$cp_statistic <- cp$statistic
    dat$mod_change_detected[i] <- cp$change_detected
    dat$mod_pos_when_change_detected[i] <- cp$pos_when_change_detected
    dat$mod_lag_tones[i] <- cp$lag_tones
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  dat
}

add_change_points <- memoise::memoise(add_change_points, 
                                      cache = memoise::cache_filesystem("cache/add_change_points"))
