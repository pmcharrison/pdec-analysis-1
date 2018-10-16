check_df <- function(df) {
  # Check that condition == 1 corresponds to cond == TARGET or NA
  df %>% filter(condition == 1 & !is.na(cond)) %>% 
    transmute(test = cond == "TARGET") %>% 
    pull(test) %>% 
    any %>% 
    stopifnot()
  
  # Check that condition == 2 corresponds to cond == RANDREG or NA
  df %>% filter(condition == 2 & !is.na(cond)) %>% 
    transmute(test = cond == "RANDREG") %>% 
    pull(test) %>% 
    any %>% 
    stopifnot()
  
  # All participants took 420 stimuli
  (table(df$subj) == 420) %>% all %>% stopifnot
  
  # All participants took 7 blocks
  df %>% group_by(subj) %>% summarise(n_block = length(unique(block))) %>% 
    transmute(test = n_block == 7) %>% 
    pull(test) %>% all %>% stopifnot
  
  # Each block comprises trialN from 1 to 60
  df %>% group_by(subj, block) %>% summarise(test = all(trialN == 1:60)) %>% 
    pull(test) %>% all %>% stopifnot
}
