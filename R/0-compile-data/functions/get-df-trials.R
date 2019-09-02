library(tidyverse)

get_df_trials <- function(files) {
  map2(files$trials$subj, files$trials$path, function(subj, path) {
    if (!file.exists(path)) {
      add_missing_subj(subj)
      NULL
    } else {
      t <- read.table(path, header = TRUE) %>%
        as_tibble %>% 
        add_column(subj, .before = 1) %>% 
        add_column(trialN = NA, .before = "toneNum") %>% 
        mutate(new_seq = c(TRUE, diff(toneNum) < 0)) %>% 
        group_by(block) %>% 
        mutate(trialN = cumsum(new_seq)) %>% 
        ungroup()

      num_block <- t$block %>% unique %>% length
      num_seq_by_block <- t %>% group_by(block) %>% summarise(num_seq = max(trialN))
      num_seq_in_block <- unique(num_seq_by_block$num_seq)
      
      if (length(num_seq_in_block) > 1)
        stop("different blocks contained different numbers of sequences")
      
      if (is.null(get_info("num_seq_in_block"))) {
        set_info("num_seq_in_block", num_seq_in_block, "Number of sequences in each block")
      } else {
        stopifnot(num_seq_in_block == get_info("num_seq_in_block"))
      }
      
      t$new_seq <- NULL
      
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
