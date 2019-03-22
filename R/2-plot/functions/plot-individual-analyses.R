plot_individual_analyses <- function(optimised_analyses) {
  optimised_analyses$individual %>%
    filter(correct) %>% 
    select(subj, cond, block, RTadj, model_reaction_time) %>%
    mutate(model_reaction_time = if_else(model_reaction_time < 0,
                                         as.numeric(NA),
                                         model_reaction_time)) %>% 
    filter(!is.na(cond) & !is.na(model_reaction_time)) %>% 
    gather(data_source, reaction_time, RTadj, model_reaction_time) %>% 
    group_by(subj, cond, block, data_source) %>%
    summarise_all(funs(mean, 
                       sd,
                       n = length,
                       se = sd / sqrt(n), 
                       ymin = mean - se, ymax = mean + se)) %>%
    ungroup() %>%
    mutate(data_source = recode(data_source,
                                model_reaction_time = "Simulated", 
                                RTadj = "Observed"),
           cond = recode(cond,
                         RANDREG = "Novel",
                         TARGET = "Repeated")) %>% 
    ggplot(aes()) + 
    geom_line(aes(x = block, y = mean, colour = cond, linetype = cond)) +
    geom_ribbon(aes(x = block, ymin = ymin, ymax = ymax, fill = cond, group = paste(cond, data_source)),
                alpha = 0.3) + 
    scale_x_continuous("Block") + 
    scale_y_continuous("Reaction time (s)") +
    scale_fill_manual("Condition", values = c("black", "blue")) + 
    scale_color_manual("Condition", values = c("black", "blue")) +
    scale_linetype_manual("Condition", values = c("dashed", "solid")) +
    facet_grid(data_source ~ subj) +
    theme(aspect.ratio = 1,
          legend.position = "bottom")
}
