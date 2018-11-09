source("R/1-model/1-setup.R")

par <- list(ppm_escape = "A",
            ppm_order_bound = 10L,
            ppm_shortest_deterministic = FALSE,
            ppm_update_exclusion = FALSE,
            seq_length = 20L, # number of tones
            tone_length = 0.05, # seconds
            cp_method = "AMOC",
            cp_penalty = "SIC",
            cp_threshold = 0.95,
            cp_burn_in = 30L)

dat <- readRDS(file = "output/data-01-participants.rds")
# dat <- dat %>% filter(subj < 4 & trialN < 4 & block == 2) # for testing only
# dat <- dat %>% filter(subj < 4) # for testing only
# dat <- dat %>% filter(subj == 1) # for testing only

alphabet <- readRDS(file = "output/alphabet.rds")

# Check that the data are organised by increasing time
stopifnot(order(dat$subj, dat$block, dat$trialN) == seq_len(nrow(dat)))

dat <- dat %>% 
  group_by(subj) %>% 
  mutate(idyom_ic = ic_subj(seq,
                            alphabet = !!alphabet,
                            par = !!par,
                            sub = unique(subj))) %>% 
  ungroup()
dat <- add_change_points(dat, ic_col = "idyom_ic", label = "idyom", par = par)

saveRDS(dat, "output/data-02-models.rds")
yaml::write_yaml(par, "output/data-02-models.yaml")
