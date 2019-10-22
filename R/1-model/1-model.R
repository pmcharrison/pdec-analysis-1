source("R/1-model/1-setup.R")
library(tidyverse)
theme_set(theme_bw())

opt <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05,
  response_time = 1,
  cp = list(
    method = "Mann-Whitney",
    t1_error_rate = 10000,
    startup = 20 
  )
)

opt_7 <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05,
  cp = list(
    method = "Mann-Whitney",
    t1_error_rate = 370,
    startup = 20 
  )
)

ppm_pars <- list(
  orig = c(
    stm_weight = 1,
    stm_duration = 0,
    ltm_weight = 1,
    ltm_half_life = 1e70,
    noise = 0,
    order_bound = 10
  ),
  # exp_decay = c(
  #   stm_weight = 1,
  #   stm_duration = 0,
  #   ltm_weight = 0.4,
  #   ltm_half_life = 150,
  #   noise = 1,
  #   order_bound = 10
  # ),
  stm_ltm_decay = c(
    stm_weight = 0.8,
    stm_duration = 5,
    ltm_weight = 0.025,
    ltm_half_life = 200,
    noise = 1,
    order_bound = 10
  )
)

dat <- readRDS(file = "output/data-00-participants.rds")
source("R/2-plot/functions/plot-ic-profile.R")

set.seed(1)
x1 <- ppm_dataset(
  data = dat$exp_1$data,
  alphabet = dat$exp_1$alphabet,
  ppm_par = ppm_pars$orig,
  opt = opt
)
plot_blocks(x1)

set.seed(1)
x2 <- ppm_dataset(
  data = dat$exp_1$data, # %>% filter(subj %in% 1:5),
  alphabet = dat$exp_1$alphabet,
  ppm_par = c(
    stm_weight = 1,
    stm_duration = 5,
    ltm_weight = 0.01,
    ltm_half_life = 400,
    noise = 0.4,
    order_bound = 10
  ),
  opt = opt
)
plot_ic_profile(x2 %>% filter(block == 5 | rep == 1), opt, loess = FALSE, xlim = c(NA, 50))
plot_ic_profile_2(x2 %>% filter(block == 5 | rep == 1), opt)
plot_blocks(x2 %>% filter(model_reaction_time > 0), error_bar = TRUE, line = TRUE, ribbon = FALSE)

set.seed(1)
y1 <- ppm_dataset(
  data = dat$exp_4a$data %>% filter(subj %in% 1:5),
  alphabet = dat$exp_4a$alphabet,
  ppm_par = ppm_pars$orig,
  opt = opt
)
plot_blocks(y1, error_bar = FALSE, line = FALSE, ribbon = FALSE)

set.seed(1)
y2 <- ppm_dataset(
  data = dat$exp_4a$data, # %>% filter(subj %in% 1:5),
  alphabet = dat$exp_4a$alphabet,
  ppm_par = c(
    stm_weight = 1,
    stm_duration = 5,
    ltm_weight = 0.01,
    ltm_half_life = 1000,
    noise = 0.45,
    order_bound = 10
  ),
  opt = opt
)

y2 %>% 
  filter(model_reaction_time > 0) %>% 
  plot_blocks(error_bar = TRUE, line = FALSE, ribbon = FALSE)

set.seed(1)
z1 <- ppm_dataset(
  data = dat$exp_7$data %>% filter(subj %in% 1:16),
  alphabet = dat$exp_7$alphabet,
  ppm_par = ppm_pars$orig,
  opt = opt
)
plot_blocks(
  z1 %>% filter(block %in% 1:4), 
  cond_list = c(
    "#1471B9" = "RANREG",
    "#EEC00D" = "RANREGr",
    "#CD534B" = "REPinRAN",
    "#00FF00" = "REPinRANr"
  ), 
  subtract_1_sec_from = c("RANREG", "RANREGr"),
  error_bar = TRUE, ribbon = FALSE
)

# Note: in the first few experiments (1 and 4), 
# our reaction times were plotted relative to the beginning of the regular phase.
# So, an untrained ideal observer model should take about 1.3 seconds to detect
# the phase change - 1 second to observe a complete cycle, then 0.3 seconds to 
# see that this cycle is repeating.
# When the cycle is familiar from a previous trial, the ideal observer doesn't need
# the 1-second component, and just takes c. 0.3 seconds to detect the phase change.

# In experiment 8, we change the plotting convention. We subtract 1 second 
# from the RANREG and RANREGr conditions; this means that 0 corresponds to the 
# onset of the second cycle. This is for comparability with the REPinRAN and REPinRANr 
# conditions, where 0 also corresponds to the onset of the second cycle.
# We have to subtract 1 because the underlying reaction time data still
# defines the zero as the onset of the first cycle, not the onset of the second cycle.

# v1 (ltm_weight = 0.01, ltm_half_life = 1000): REPinRAN is too difficult
# v2 (ltm_weight = 0.1, ltm_half_life = 500)
# v3 (ltm_weight = 0.1, ltm_half_life = 50): I think we need a buffer
# v4 ()

set.seed(1)
z2 <- ppm_dataset(
  data = dat$exp_7$data %>% filter(subj %in% 1:16),
  alphabet = dat$exp_7$alphabet,
  ppm_par = list( # These parameters should make REPinRAN(r) fail but ???? weird pattern of results
    buffer_length_time = 2,
    buffer_length_time = 2,
    buffer_length_items = 1e6,
    only_learn_from_buffer = TRUE,
    only_predict_from_buffer = TRUE,
    
    buffer_weight = 1,
    
    # buffer_length_time = 0,
    # buffer_weight = 1,
    # buffer_length_time = 0,
    # buffer_length_items = 0,
    # only_learn_from_buffer = FALSE,
    # only_predict_from_buffer = FALSE,
    
    stm_weight = 1,
    stm_duration = 10,
    ltm_weight = 0.01,
    ltm_half_life = 1000,
    ltm_asymptote = 0, 
    noise = 0.85,
    order_bound = 4
  ),
  opt = opt
)
plot_blocks(z2 %>% filter(block %in% 1:4), 
            cond_list = c(
              "#1471B9" = "RANREG",
              "#EEC00D" = "RANREGr",
              "#CD534B" = "REPinRAN",
              "#00FF00" = "REPinRANr"
            ),
            subtract_1_sec_from = c("RANREG", "RANREGr"),
            error_bar = FALSE, ribbon = TRUE)

plot_block(z2, 
           block = 5,
           cond_list = c(
             "#1471B9" = "RANREG",
             "#EEC00D" = "RANREGr",
             "#00FF00" = "REPinRANr"
           ),
           subtract_1_sec_from = c("RANREG", "RANREGr", "REPinRANr"))

plot_blocks(z2 %>% filter(block %in% 5), 
            cond_list = c(
              "#1471B9" = "RANREG",
              "#EEC00D" = "RANREGr",
              "#00FF00" = "REPinRANr"
            ),
            subtract_1_sec_from = c("RANREG", "RANREGr", "REPinRANr"),
            error_bar = TRUE, ribbon = FALSE)

x2 %>%
  mutate(condition = recode(condition, 
                            `1` = 'TARGET',
                            `2` = 'RANDREG',
                            `3` = "RAND",
                            `4` = 'STEP',
                            `5` = 'CONT')) %>% 
  group_by(condition) %>%
  summarise(model_hit_rate = mean(!is.na(model_reaction_time)),
            human_hit_rate = mean(!is.na(correct) & correct),
            n = n())

# This is about right, except the baselining is off
# Note: behavioural RTs in Exp 7 are benchmarked to the effective 
# transition, not the transition
z2 %>% 
  mutate(condition = recode(condition, 
                            `1` = 'RREGr',
                            `2` = 'RREG',
                            `3` = 'RAND',
                            `4` = 'STEP',
                            `5` = 'CONT', 
                            `6` = 'REPinRANr', 
                            `7` ='REPinRAN')) %>% 
  filter(block %in% 1:4 & 
           condition %in% c('RREG', 'RREGr', 'REPinRAN', 'REPinRANr')) %>% 
  group_by(block, condition) %>% 
  summarise(model_hit_rate = mean(!is.na(model_reaction_time)),
            human_hit_rate = mean(!is.na(correct) & correct),
            human_rt = mean(RTadj, na.rm = TRUE),
            rt_mean = na.omit(model_reaction_time) %>% mean(),
            rt_n = na.omit(model_reaction_time) %>% length(),
            rt_sd = na.omit(model_reaction_time) %>% sd(),
            rt_se = rt_sd / sqrt(rt_n),
            rt_95_lower = rt_mean - 1.96 * rt_se,
            rt_95_upper = rt_mean + 1.96 * rt_se) %>% {print(.); .} %>% 
  ggplot(aes(block, rt_mean, colour = condition)) + 
  geom_line()


z1 %>% 
  # dat$exp_7$data %>% 
  mutate(condition = recode(condition, 
                            `1` = 'RREGr',
                            `2` = 'RREG',
                            `3` = 'RAND',
                            `4` = 'STEP',
                            `5` = 'CONT', 
                            `6` = 'REPinRANr', 
                            `7` ='REPinRAN')) %>% 
  filter(block == 5) %>% 
  pull(condition) %>% table

filter(condition == "REPinRANr" & block == 5) %>% 
  slice(4) %>% pull(mod) %>% `[[`(1) %>% select(pos, information_content) %>% mutate(pos = pos - min(pos)) %>% plot



filter(block == 5) %>% 
  filter(condition %in% c("RREGr", "RREG", "REPinRANr")) %>% 
  group_by(condition) %>% 
  summarise(model_hit_rate = mean(!is.na(model_reaction_time)),
            human_hit_rate = mean(!is.na(correct) & correct),
            human_rt = mean(RTadj, na.rm = TRUE),
            rt_mean = na.omit(model_reaction_time) %>% mean(),
            rt_n = na.omit(model_reaction_time) %>% length(),
            rt_sd = na.omit(model_reaction_time) %>% sd(),
            rt_se = rt_sd / sqrt(rt_n),
            rt_95_lower = rt_mean - 1.96 * rt_se,
            rt_95_upper = rt_mean + 1.96 * rt_se) %>% {print(.); .} %>%
  # summarise(human_rt = mean(RTadj, na.rm = TRUE)) %>% 
  ggplot(aes(x = condition, y = rt_mean, 
             ymin = rt_95_lower,
             ymax = rt_95_upper,
             fill = condition)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(width = 0.2)

# z1 %>% 
#   filter()
# 
# 
#   table()
#   cond %>% table


# z %>% filter(cond %in% c("RANDREG", "TARGET")) %>% # &
#                # !is.na(model_reaction_time)) %>%
#   group_by(block, cond) %>% 
#   summarise(human_rt = mean(RTadj, na.rm = TRUE)) %>% 
#   ggplot(aes(x = block, y = human_rt, colour = cond)) + 
#   geom_point()




plot_blocks(x)



optim_res <- ppm_optim(dat, opt)
optim_analyses <- get_optimised_analyses(optim_res, dat, opt)

# saveRDS(optimised_par, "output/data-01-optimised-par.rds")
# saveRDS(par, "output/data-01-all-par.rds")
# saveRDS(optimised_analyses, "output/data-01-optimised-analyses.rds")
