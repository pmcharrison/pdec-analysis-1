est_change <- function(information_content, par) {
  final_est <- as.integer(NA)
  for (i in seq(from = par$cp_burn_in, to = length(information_content))) {
    c(est, conf) %<-% changepoint::cpt.mean(information_content[1:i], 
                                            method = par$cp_method, 
                                            penalty = par$cp_penalty,
                                            class = FALSE) %>% as.list
    if (conf >= par$cp_threshold && est >= par$cp_burn_in) {
      final_est <- as.integer(est)
      break
    }
  }
  final_est
}

add_change_points <- function(dat, ic_col, label, par) {
  est <- map(dat[[ic_col]], pull, "information_content") %>% 
    map_int(est_change, par)
  detected <- !is.na(est)
  lag <- est - (dat$transition + 1) # because transition is 0-indexed??
  warning("Check definition of lag")
  
  dat[[paste0(label, "_cp_detected")]] <- detected
  dat[[paste0(label, "_cp_estimate")]] <- est
  dat[[paste0(label, "_cp_lag")]] <- lag
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
