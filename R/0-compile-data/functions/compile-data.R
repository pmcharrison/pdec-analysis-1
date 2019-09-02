library(magrittr)

compile_data <- function(path_response, 
                         path_stim,
                         format_stim,
                         format_stim_desc,
                         max_block = Inf,
                         subj_exclude = numeric()) {
  
  # Assumptions:
  # - each row of df_resp is a unique combination of subj, block, and trialN
  # - trialN maps to rows of df_stimuli and df_trials
  
  # Note: df_resp only contains correct trials
  df_resp <- get_df_resp(path_response)
  
  subj <- df_resp$subj %>% unique() %>% setdiff(subj_exclude)
  set_info("subj", subj, "Unique subject IDs")
  
  files <- get_files(subj,
                     path_stim,
                     format_stim,
                     format_stim_desc)
  
  df_trials <- get_df_trials(files = files)
  df_stimuli <- get_df_stimuli(files = files)
  
  df <- get_df(subj = subj,
               df_trials = df_trials,
               df_stimuli = df_stimuli,
               df_resp = df_resp,
               max_block = max_block)

  alphabet <- get_alphabet(df)
  
  list(
    data = df,
    alphabet = alphabet
  )
}
