library(tidyverse)

for (f in list.files("R/0-compile-data/functions/", full.names = TRUE, pattern = "\\.R$"))
  source(f)

R.utils::mkdirs("output")

data <- list()

message("Compiling data for experiment 1...")
data$exp_1 <- compile_data(
  path_response = "input/exp1_retention/response/exp1_retention.txt",
  path_stim = "input/exp1_retention/stimuli",
  format_stim = "stimuli_subj_%i_exp1_retention.txt",
  format_stim_desc = "stimDescription_subj_%i_exp1_retention.txt",
  max_block = 5L
)
check_data_exp_1(data$exp_1$data)

message("Compiling data for experiment 4a...")
data$exp_4a <- compile_data(
  path_response = "input/exp4a_interruption/response/exp4a_interruption.txt",
  path_stim = "input/exp4a_interruption/stimuli",
  format_stim = "stimuli_subj_%i_exp4a_interruption.txt",
  format_stim_desc = "stimDescription_subj_%i_exp4a_interruption.txt",
  max_block = 5L
)
check_data_exp_4a(data$exp_4a$data)

message("Compiling data for experiment 7...")
data$exp_7 <- compile_data(
  path_response = "input/exp7_immediaterep/response/exp7_immediaterep.txt",
  path_stim = "input/exp7_immediaterep/stimuli",
  format_stim = "stimuli_subj_%i_exp7_immediaterep.txt",
  format_stim_desc = "stimDescription_subj_%i_exp7_immediaterep.txt",
  max_block = 5L
  # subj_exclude = 20 
)
check_data_exp_7(data$exp_7$data)

saveRDS(data, "output/data-00-participants.rds")
