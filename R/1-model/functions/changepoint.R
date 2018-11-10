est_change <- function(information_content, par) {
  map_dfr(seq_along(information_content), function(i) {
    if (i <= par$cp_burn_in) {
      location <- as.integer(NA)
      conf <- as.numeric(NA)
    } else {
      c(location, conf) %<-% changepoint::cpt.mean(information_content[1:i], 
                                                   method = par$cp_method, 
                                                   penalty = par$cp_penalty,
                                                   class = FALSE) %>% as.list
      # Could also do Bayesian changepoint analysis (bcp package)
      # though that would be pretty slow
    }
    tibble(cp_location = location, cp_conf = conf)
  })
}

est_change <- memoise::memoise(est_change, cache = memoise::cache_filesystem("cache/est_change"))

add_change_points <- function(dat, ic_col, label, par) {
  message("Computing change points for all participants...")
  dat[[ic_col]] <- plyr::llply(dat[[ic_col]], function(ic) {
    bind_cols(ic,
              est_change(ic$information_content, par[c("cp_burn_in", "cp_method",
                                                       "cp_penalty")]))
  }, .progress = "text")
  location <- map_int(dat[[ic_col]], function(ic) {
    which(ic$cp_conf >= par$cp_threshold)[1]
  })
  detected <- !is.na(location)
  num_tones <- location - dat$transition
  reaction_time <- num_tones * par$tone_length
  dat[[paste0(label, "_cp_detected")]] <- detected
  dat[[paste0(label, "_cp_location")]] <- location
  dat[[paste0(label, "_cp_num_tones")]] <- num_tones
  dat[[paste0(label, "_cp_reaction_time")]] <- reaction_time
  dat
}

# c(full, summary) %<-% map(c(TRUE, FALSE), function(class) {
#   changepoint::cpt.mean(information_content, 
#                         method = par$cp_method, 
#                         penalty = par$cp_penalty,
#                         class = class)
# })
# confidence <- summary["conf.value"] %>% as.numeric
# detected <- confidence > par$cp_threshold
# estimate <- as.integer(if (detected) summary["cpt"] else NA)
# list(full = full, detected = detected, estimate = estimate, confidence = confidence)

# add_change_points <- function(dat, ic_col, label, par) {
#   d <- map(dat[[ic_col]], pull, "information_content") %>% 
#     map(est_change, par) %>% 
#     map(function(x) tibble(full = list(x$full), 
#                            detected = x$detected,
#                            estimate = x$estimate, 
#                            confidence = x$confidence) %>% 
#           set_names(~ paste(label, ., sep = "_cp_"))) %>% 
#     bind_rows %>% 
#     bind_cols(dat, .)
#   d[[paste0(label, "_cp_lag")]] <- d[[paste0(label, "_cp_estimate")]] - d$transition
#   d
# }
