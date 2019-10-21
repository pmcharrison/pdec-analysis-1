library(testthat)

# Check the output of the first experiment compilation
check_data_exp_1 <- function(df) {
  df %>% select(condition, cond) %>% table

  # Check that condition == 1 corresponds to cond == TARGET or NA
  df %>% filter(condition == 1 & !is.na(cond)) %>% 
    transmute(test = cond == "TARGET") %>% 
    pull(test) %>% 
    all %>% 
    stopifnot()
  
  # Check that condition == 2 corresponds to cond == RANDREG or NA
  df %>% filter(condition == 2 & !is.na(cond)) %>% 
    transmute(test = cond == "RANDREG") %>% 
    pull(test) %>% 
    all %>% 
    stopifnot()
  
  # All participants took 420 stimuli
  (table(df$subj) == 300) %>% all %>% stopifnot
  
  # All participants took 5 blocks
  df %>% group_by(subj) %>% summarise(n_block = length(unique(block))) %>% 
    transmute(test = n_block == 5) %>% 
    pull(test) %>% all %>% stopifnot
  
  # Each block comprises trialN from 1 to 60
  df %>% group_by(subj, block) %>% summarise(test = all(trialN == 1:60)) %>% 
    pull(test) %>% all %>% stopifnot
}

# Check the output of experiment 4a compilation
check_data_exp_4a <- function(df) {
  df %>% select(condition, cond) %>% unique %>% arrange(condition) %>%
    na.omit() %>% 
    mutate(cond = as.character(cond)) %>% 
    expect_equal(tribble(~ condition, ~ cond,
                         1L, "TARGET", 
                         2L, "RANDREG"))

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
  
  # All participants took 300 stimuli
  (table(df$subj) == 27 + 108 + 135 + 15 + 15) %>% all %>% stopifnot
  
  # All participants took 5 blocks
  df %>% group_by(subj) %>% summarise(n_block = length(unique(block))) %>% 
    transmute(test = n_block == 5) %>% 
    pull(test) %>% all %>% stopifnot
}

# Check the output of experiment 7 compilation
check_data_exp_7 <- function(df) {
  df %>%
    select(cond, condition) %>%
    na.omit() %>%
    unique() %>%
    arrange(condition) %>% 
    mutate(cond = as.character(cond)) %>% 
    expect_equal(tribble(
      ~ cond,    ~ condition,
      "RANREGr",  1L,
      "RANREG",   2L,
      "REPinRANr", 6L,
      "REPinRAN",  7L,
    ))

  # All participants took 410 stimuli
  (table(df$subj) == 410) %>% all %>% stopifnot
  
  # All participants took 5 blocks
  df %>% group_by(subj) %>% summarise(n_block = length(unique(block))) %>% 
    transmute(test = n_block == 5) %>% 
    pull(test) %>% all %>% stopifnot
}
