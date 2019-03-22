plot_block_1_v_5 <- function(optimised_analyses) {
  optimised_analyses$individual %>% 
    filter(!is.na(cond)) %>% 
    filter(block %in% c(1, 5)) %>% 
    group_by(subj, block, cond) %>% 
    summarise(rt = mean(RTadj, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(cond, rt) %>% 
    mutate(block = paste("Block", block)) %>% 
    ggplot(aes(RANDREG, TARGET)) + 
    geom_point() + 
    scale_x_continuous("Novel RT (s)") + 
    scale_y_continuous("Target RT (s)") + 
    facet_wrap(~ block) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
    theme(aspect.ratio = 1)
}
