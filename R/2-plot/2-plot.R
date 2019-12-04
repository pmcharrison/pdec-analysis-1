source("R/2-plot/2-setup.R")
source("R/1-model/functions/plot-experiments.R")

opt <- readRDS("output/opt.rds")
ppm_par <- readRDS("output/ppm-par.rds")
model_results <- readRDS("output/model-results.rds")

invisible(suppressMessages(capture.output(p <- plot_experiments(model_results))))
ggsave("output/plots/all-experiments.pdf", plot = p, width = 6, height = 12, dpi = 300)

plot_ic_profile(model_results$exp_1$optim, opt, loess = FALSE, xlim = c(-10, 40), plot_points = FALSE)
ggsave("output/plots/ic-profile-block-1-v-5.pdf", width = 5, height = 5, dpi = 300)

plot_mod(ppm_par$optim, opt, max_time = 25)
ggsave("output/plots/optim-memory-profile.pdf", width = 4, height = 4, dpi = 300)

plot_mod(ppm_par$optim, opt, max_time = 5, plot_boundary_2 = FALSE)
ggsave("output/plots/optim-memory-profile-short.pdf", width = 4, height = 4, dpi = 300)

if (FALSE) {
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
           width = 5, 
           height = 4)
  plot_ic_profile_2(optimised_analyses) %>% 
    ggsave(plot = ., 
           file = "output/ic-profile-2.pdf", 
           width = 7, 
           height = 6)
  get_ic_profile_data(optimized_analyses) %>% 
    write_csv("output/ic-profiles.csv")
}

