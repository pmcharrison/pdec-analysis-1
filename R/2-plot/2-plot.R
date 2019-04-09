source("R/2-plot/2-setup.R")

par <- readRDS("output/data-01-all-par.rds")
optimised_par <- readRDS("output/data-01-optimised-par.rds")
optimised_analyses <- readRDS("output/data-01-optimised-analyses.rds")

plot_individual_par(optimised_par) %>% 
  ggsave(plot = ., 
         filename = "output/individual-par.pdf",
         width = 4.5, 
         height = 4.5)
plot_individual_analyses(optimised_analyses) %>% 
  ggsave(plot = ., 
         filename = "output/individual-analyses.pdf",
         width = 16, 
         height = 3.5)
plot_combined_analyses(optimised_analyses) %>% 
  ggsave(plot = ., 
         filename = "output/overall-analyses.pdf",
         width = 7, 
         height = 3.75)
plot_block_1_v_5(optimised_analyses) %>% 
  ggsave(plot = ., 
         filename = "output/block-1-v-5.pdf",
         width = 6, 
         height = 3.25)
plot_ic_profile(optimised_analyses) %>% 
  ggsave(plot = ., 
         file = "output/ic-profile.pdf", 
         width = 7, 
         height = 6)
plot_ic_profile_2(optimised_analyses) %>% 
  ggsave(plot = ., 
         file = "output/ic-profile-2.pdf", 
         width = 7, 
         height = 6)
get_ic_profile_data(optimized_analyses) %>% 
  write_csv("output/ic-profiles.csv")

