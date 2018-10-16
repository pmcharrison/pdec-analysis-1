source("R/0-compile-data/setup.R")

par <- list(seq_len = 140L,
            max_block = 7L) # Filter out data for max_block > 7

# Assumptions:
# - each row of df_resp is a unique combination of subj, block, and trialN
# - trialN maps to rows of df_stimuli and df_trials

set_info("seq_len", par$seq_len, "Number of tones in each sequence")

# Note: df_resp only contains correct trials
df_resp <- get_df_resp()

subj <- df_resp$subj %>% unique
set_info("subj", subj, "Unique subject IDs")

files <- get_files(subj)
  
df_trials <- get_df_trials(files = files, par = par)
df_stimuli <- get_df_stimuli(files = files)
df <- get_df(subj = subj,
             df_trials = df_trials,
             df_stimuli = df_stimuli,
             df_resp = df_resp)

check_df(df)
alphabet <- get_alphabet(df)

if (!dir.exists("output")) dir.create("output")
saveRDS(df, "output/participant-data.rds")
saveRDS(alphabet, "output/alphabet.rds")
