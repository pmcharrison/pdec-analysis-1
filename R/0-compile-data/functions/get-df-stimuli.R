get_df_stimuli <- function(files) {
  map2(files$stimuli$subj, files$stimuli$path, function(subj, path) {
    if (!file.exists(path)) {
      stopifnot(is_missing_subj(subj))
      NULL
    } else {
      x <- readLines(path) %>% trimws() %>% strsplit(split = " ") %>% map(as.integer)

      tibble(subj = subj, 
             # trialN = seq_along(x),
             seq = x)
    }
  }) %>% Filter(Negate(is.null), .) %>% bind_rows
}
