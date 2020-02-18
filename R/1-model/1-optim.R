#### Setup ####

source("R/1-model/1-setup.R")
source("R/2-plot/functions/plot-blocks.R")
library(tidyverse)
theme_set(theme_bw())

cache_dir <- "~/Downloads/analyse-experiments"
R.utils::mkdirs(cache_dir)
analyse_experiments <- memoise::memoise(
  analyse_experiments,
  cache = memoise::cache_filesystem(cache_dir)
)

opt <- list(
  seq_length = 20L, # number of tones
  tone_length = 0.05,
  response_time = 1,
  cp = list(
    method = "Mann-Whitney",
    # method = "Lepage",
    t1_error_rate = 10000,
    startup = 20 
  )
)

ppm_par_start <- list(
    buffer_length_time = 1e6,
    buffer_weight = 1,
    buffer_length_items = 15,
    only_learn_from_buffer = TRUE,
    only_predict_from_buffer = TRUE,
    
    stm_weight = 1,
    
    stm_duration = 15, # optim
    ltm_weight = 0.02, # optim
    ltm_half_life = 500, # optim
    ltm_asymptote = 0,
    
    noise = 1.3, # optim
    order_bound = 4
  )
)

ppm_par_optimise <- list(
  stm_duration = c(0, 1e6),
  ltm_weight = c(0, 1)
  ltm_half_life = c(1e-3, 1e6),
  noise = c(0, 10)
)

#### Functions ####

eval_par <- function(par, data, opt) {
  stopifnot(length(par) == 4)
  all_par <- ppm_par_start
  all_par[c(
    "stm_duration",
    "ltm_weight",
    "ltm_half_life",
    "noise"
  )] <- par
  res <- analyse_experiments(dat[c("exp_1", "exp_4a")],
                             opt, 
                             all_par)
  cost <- compute_cost(res)
  stop("Here we want to save the current results, maybe in a summary figure".)
}

compute_cost <- function(res) {
  bind_rows(
    exp_1 = inner_join(get_exp_1_behavioural_means(),
                       get_exp_1_model_means(res),
                       by = c("cond", "block")) %>% mutate(experiment = "1"),
    exp_4a = inner_join(get_exp_4a_behavioural_means(),
                        get_exp_4a_model_means(res),
                        by = c("cond", "block")) %>% mutate(experiment = "4a")
  ) %>% 
    mutate(abs_error = abs(model_rt - behavioural_rt)) %>% 
    pull(abs_error) %>% 
    mean()
}

get_exp_1_behavioural_means <- function() {
  read_delim("input/behavioural-plot-data/exp1_retention_avg.txt", delim = "\t") %>% 
    pivot_longer(cols = starts_with(c("RANDREG", "TARGET"))) %>% 
    mutate(cond = gsub("_.*", "", name),
           block = gsub("[A-Z]*", "", name),
           block = gsub("^_", "", block),
           block = gsub("_.*", "", block) %>% as.integer()) %>% 
    select(- name) %>% 
    group_by(cond, block) %>% 
    summarise(behavioural_rt = mean(value)) %>% 
    ungroup()
}

get_exp_4a_behavioural_means <- function() {
  read_delim("input/behavioural-plot-data/exp4a_interruption_avg.txt", delim = "\t") %>% 
    pivot_longer(cols = starts_with(c("RANDREG", "TARGET"))) %>% 
    mutate(cond = gsub("_.*", "", name),
           block = gsub("[A-Z]*", "", name),
           block = gsub("^_", "", block) %>% as.integer()) %>% 
    select(- name) %>% 
    group_by(cond, block) %>% 
    summarise(behavioural_rt = mean(value)) %>% 
    ungroup()
}

get_exp_1_model_means <- function(res) {
  res$exp_1 %>% 
    summarise_blocks(c("RANDREG", "TARGET"), subtract_1_sec_from = character()) %>% 
    select(cond, block, model_rt = rt_mean) %>% 
    mutate(cond = as.character(cond))
}

get_exp_4a_model_means <- function(res) {
  res$exp_4a %>% 
    summarise_blocks(c("RANDREG", "TARGET"), subtract_1_sec_from = character()) %>% 
    select(cond, block, model_rt = rt_mean) %>% 
    mutate(cond = as.character(cond))
}

#### Body ####

if (FALSE) {
  res <- readRDS("output/model-results.rds") %>% map("optim")
}

dat <- readRDS(file = "output/data-00-participants.rds")


res <- analyse_experiments(data, opt, ppm_par)
saveRDS(res, "output/model-results.rds")
saveRDS(ppm_par, "output/ppm-par.rds")
saveRDS(opt, "output/opt.rds")

ppm_par$optim %>% {
  tibble(Parameter = names(.),
         Value = unlist(.))
} %>% 
  write_csv("output/optim-ppm-par.csv")

p <- plot_experiments(res)
