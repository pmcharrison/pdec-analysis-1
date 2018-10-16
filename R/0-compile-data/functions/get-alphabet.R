get_alphabet <- function(df) {
  df$seq %>% map(unique) %>% do.call(c, .) %>% unique %>% sort
}
