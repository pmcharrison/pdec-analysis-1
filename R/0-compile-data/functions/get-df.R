get_df <- function(subj, df_trials, df_stimuli, df_resp, max_block) {
  browser()
  map_dfr(subj, function(s) {
    d_trials <- df_trials %>% filter(subj == s)
    d_stimuli <- df_stimuli %>% filter(subj == s) %>% slice(seq_len(nrow(d_trials)))
    d <- add_column(d_trials, seq = d_stimuli$seq) %>% 
      filter(block <= max_block)
    d
  }) %>% left_join(df_resp, by = c("subj", "block", "trialN")) %>% 
    mutate(correct = case_when(
      !is.na(correct) ~ TRUE,
      condition %in% c(1, 2) ~ FALSE,
      TRUE ~ NA
    ))
}
