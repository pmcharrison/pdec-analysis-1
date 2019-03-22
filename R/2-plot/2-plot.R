source("R/2-plot/2-setup.R")

par <- readRDS("output/data-01-all-par.rds")
optimised_par <- readRDS("output/data-01-optimised-par.rds")
optimised_analyses <- readRDS("output/data-01-optimised-analyses.rds")

warning("Rule for filtering out negative model reaction times")

optimised_par$combined

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
