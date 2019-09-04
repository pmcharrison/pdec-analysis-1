source("R/1-model/1-setup.R")

library(tidyverse)
theme_set(theme_bw())

opt <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05,
  # optim = list(
  #   which = c("noise", "ltm_weight"),
  #   lower = c(0, 0),
  #   upper = c(10, 0.45),
  #   control = list(
  #     maxeval = 100,
  #     ftol_abs = 0.001
  #   ),
  #   na_penalty = 1
  # ),
  cp = list(
    method = "Mann-Whitney",
    t1_error_rate = 10000,
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

x <- ppm_dataset(
  data = dat$exp_1$data %>% filter(subj == 1),
  alphabet = dat$exp_1$alphabet,
  ppm_par = ppm_pars$orig,
  opt = opt
)

set.seed(1)
y <- ppm_dataset(
  data = dat$exp_1$data, # %>% filter(subj %in% 1:5),
  alphabet = dat$exp_1$alphabet,
  ppm_par = ppm_pars$stm_ltm_decay,
  opt = opt
)
plot_blocks(y)

z <- ppm_dataset(
  data = dat$exp_4a$data %>% filter(subj %in% 1:5),
  alphabet = dat$exp_4a$alphabet,
  ppm_par = ppm_pars$stm_ltm_decay,
  opt = opt
)
plot_blocks(z)

z %>% filter(cond %in% c("RANDREG", "TARGET")) %>% # &
               # !is.na(model_reaction_time)) %>%
  group_by(block, cond) %>% 
  summarise(human_rt = mean(RTadj, na.rm = TRUE)) %>% 
  ggplot(aes(x = block, y = human_rt, colour = cond)) + 
  geom_point()
  

plot_blocks <- function(x, colours = c("#1471B9", "#EEC00D")) {
  x %>% 
    filter(cond %in% c("RANDREG", "TARGET") &
             !is.na(model_reaction_time)) %>%
    group_by(block, cond) %>% 
    summarise(human_rt = mean(RTadj),
              rt_mean = mean(model_reaction_time),
              rt_n = n(),
              rt_sd = sd(model_reaction_time),
              rt_se = rt_sd / sqrt(rt_n),
              rt_95_lower = rt_mean - 1.96 * rt_se,
              rt_95_upper = rt_mean + 1.96 * rt_se) %>% {print(.); .} %>% 
    ggplot(aes(x = block, y = rt_mean, 
               ymin = rt_95_lower, ymax = rt_95_upper,
               colour = cond,
               fill = cond)) +
    geom_hline(yintercept = 1.45, linetype = "dotted") +
    geom_hline(yintercept = 1.75, linetype = "dotted") +
    geom_line() + 
    geom_point() +
    # geom_errorbar(width = 0.1) +
    scale_color_manual(values = colours) +
    scale_fill_manual(values = colours) +
    geom_ribbon(alpha = 0.1, colour = "white")
    # scale_color_viridis_d() +
    # scale_fill_viridis_d()
}

plot_blocks(x)



optim_res <- ppm_optim(dat, opt)
optim_analyses <- get_optimised_analyses(optim_res, dat, opt)

# saveRDS(optimised_par, "output/data-01-optimised-par.rds")
# saveRDS(par, "output/data-01-all-par.rds")
# saveRDS(optimised_analyses, "output/data-01-optimised-analyses.rds")
