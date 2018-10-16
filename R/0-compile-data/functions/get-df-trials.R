library(tidyverse)

get_df_trials <- function(files, par) {
  map2(files$trials$subj, files$trials$path, function(subj, path) {
    if (!file.exists(path)) {
      add_missing_subj(subj)
      NULL
    } else {
      t <- read.table(path, header = TRUE) %>%
        as.tibble %>% 
        add_column(subj, .before = 1)
      
      # toneNum should be a repeating vector of length par$seq_len,
      # indexing the tones in the sequence
      all(t$toneNum == seq_len(par$seq_len))
      # We use this knowledge to generate trialN
      
      num_seq_in_block <- t %>% filter(block == 1) %>% nrow %>% divide_by(par$seq_len)
      num_block <- t$block %>% unique %>% length
      
      if (is.null(get_info("num_seq_in_block"))) {
        set_info("num_seq_in_block", num_seq_in_block, "Number of sequences in each block")
      } else {
        stopifnot(num_seq_in_block == get_info("num_seq_in_block"))
      }
      
      t <- add_column(
        .data = t,
        trialN = seq_len(num_seq_in_block) %>% 
          rep(each = par$seq_len) %>% 
          rep(times = num_block),
        .before = "toneNum")
      
      # Check that we wouldn't lose any information by collapsing over toneNum
      stopifnot(t %>% group_by(block, trialN) %>% 
                  do(data.frame(n_unique = nrow(unique(.data %>% select(- toneNum))))) %>% 
                  pull(n_unique) %>% (function(x) all(x == 1)))
      t <- t %>% select(- toneNum)
      t <- unique(t)
      t
    }
  }) %>% Filter(Negate(is.null), .) %>% bind_rows
}
