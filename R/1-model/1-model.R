source("R/1-model/setup.R")

par <- list(order_bound = 10L,
            cp_method = "AMOC",
            cp_penalty = "SIC",
            cp_threshold = 0.2,
            cp_burn_in = 30L)

dat <- readRDS(file = "output/participant-data.rds")
dat <- dat %>% filter(subj < 4 & trialN < 4 & block == 2) # for testing only

alphabet <- readRDS(file = "output/alphabet.rds")

# Check that the data are organised by increasing time
stopifnot(order(dat$subj, dat$block, dat$trialN) == seq_len(nrow(dat)))

dat <- dat %>% 
  group_by(subj) %>% 
  mutate(idyom_ic = ic_subj(seq,
                            alphabet = !!alphabet,
                            par = !!par,
                            msg = sprintf("finished analysing subject %i...", 
                                          unique(subj))))
dat <- add_change_points(dat, ic_col = "idyom_ic", label = "idyom", par = par)

