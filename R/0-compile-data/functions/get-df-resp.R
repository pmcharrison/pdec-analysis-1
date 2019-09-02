get_df_resp <- function(path_response) {
  x <- read.table(path_response,
                  header = TRUE) %>% as_tibble
  x <- x[order(x$subj, x$block, x$trialN), ]
  x <- add_column(x, correct = TRUE, .after = "trialN")
  x
}
