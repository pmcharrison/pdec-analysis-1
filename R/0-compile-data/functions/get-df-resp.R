get_df_resp <- function() {
  x <- read.table("input/exp1_retention/response/exp1_retention.txt",
                  header = TRUE) %>% as.tibble
  x <- x[order(x$subj, x$block, x$trialN), ]
  x <- add_column(x, correct = TRUE, .after = "trialN")
  x
}
