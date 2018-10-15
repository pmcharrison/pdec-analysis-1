library(tidyverse)

missing_subj <- integer()
add_missing_subj <- function(subj) {
  missing_subj <<- c(missing_subj, subj) %>% unique
}

df_resp <- read.table("input/exp1_retention/response/exp1_retention.txt",
                      header = TRUE)
subj <- df_resp$subj %>% unique
files <- list(trials = tibble(subj = subj,
                              name = sprintf("stimDescription_subj_%i_exp1_retention.txt",
                                             subj),
                              path = file.path("input/exp1_retention/stimuli/",
                                               name)),
              stimuli = tibble(subj = subj,
                               name = sprintf("stimuli_subj_%i_exp1_retention.txt",
                                              subj),
                               path = file.path("input/exp1_retention/stimuli/",
                                                name)))
df_trials <- map2(files$trials$subj, files$trials$path, function(subj, path) {
  if (!file.exists(path)) {
    add_missing_subj(subj)
    NULL
  } else {
    read.table(path, header = TRUE) %>%
      as.tibble %>% 
      add_column(subj, .before = 1)
  }
}) %>% Filter(Negate(is.null), .)

df_stimuli <- map2(files$stimuli$subj, files$stimuli$path, function(subj, path) {
  if (!file.exists(path)) {
    add_missing_subj(subj)
    NULL
  } else {
    x <- readLines(path) %>% trimws() %>% strsplit(split = " ") %>%
      map(as.integer)
    tibble(subj = subj, seq = x)
  }
}) %>% Filter(Negate(is.null), .)
