infer_cond <- function(df) {
  mapping <- df %>% 
    select(cond, condition) %>% 
    table() %>%
    as_tibble() %>% 
    group_by(condition) %>% 
    group_split() %T>% 
    map(~ if (sum(.$n > 0) > 1) stop("invalid mapping between cond and condition")) %>% 
    Filter(function(x) any(x$n > 0), .) %>% 
    map(filter, n > 0) %>% 
    map(select, - n) %>% 
    bind_rows()
  
  stopifnot(!anyDuplicated(mapping$cond),
            !anyDuplicated(mapping$condition))
  
  new <- df %>% 
    mutate(cond = plyr::mapvalues(condition,
                                  from = mapping$condition,
                                  to = mapping$cond,
                                  warn_missing = FALSE))
  
  stopifnot(!any(!is.na(df$cond) & df$cond != new$cond))
  
  new
}
