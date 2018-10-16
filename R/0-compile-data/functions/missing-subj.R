missing_subj <- integer()
add_missing_subj <- function(subj) {
  missing_subj <<- c(missing_subj, subj) %>% unique
}
is_missing_subj <- function(subj) subj %in% missing_subj
