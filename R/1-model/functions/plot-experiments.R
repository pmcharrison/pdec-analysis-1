plot_experiments <- function(res) {
  panel_1 <- egg::ggarrange(
    plot_blocks(res$exp_1$orig, error_bar = TRUE, line = TRUE, ribbon = FALSE) + theme(legend.position = "none"),
    plot_blocks(res$exp_1$optim, error_bar = TRUE, line = TRUE, ribbon = TRUE) + scale_y_continuous(NULL),
    nrow = 1
  )
  
  exp_2_blocks <- function(x) {
    plyr::mapvalues(x, 1:5, c(1, 1.5, 2, 2.5, 3))
  }

  panel_2 <- egg::ggarrange(
    plot_blocks(res$exp_4a$orig %>% mutate(block = exp_2_blocks(block)), 
                error_bar = TRUE, 
                line = FALSE,
                ribbon = FALSE) + 
      scale_x_continuous("Block", breaks = 1:3) +
      theme(legend.position = "none"),
    plot_blocks(res$exp_4a$optim %>% mutate(block = exp_2_blocks(block)),
                error_bar = TRUE, 
                line = FALSE,
                ribbon = FALSE) +
      scale_x_continuous("Block", breaks = 1:3) +
      scale_y_continuous(NULL),
    nrow = 1
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
  
  exp_7_plots <- map(res$exp_7, function(z) {
    p1 <- plot_blocks(
      z %>% filter(block %in% 1:4), 
      cond_list = c(
        "#1471B9" = "RANREG",
        "#EEC00D" = "RANREGr",
        "#CD534B" = "REPinRAN",
        "#00FF00" = "REPinRANr"
      ), 
      subtract_1_sec_from = c("RANREG", "RANREGr"),
      error_bar = TRUE, ribbon = FALSE
    )
    
    p2 <- plot_block(z, 
               block = 5,
               cond_list = c(
                 "#1471B9" = "RANREG",
                 "#EEC00D" = "RANREGr",
                 "#00FF00" = "REPinRANr"
               ),
               subtract_1_sec_from = c("RANREG", "RANREGr", "REPinRANr"))
    list(blocks_1_to_4 = p1, 
         block_5 = p2)
  })
  
  panel_3 <- egg::ggarrange(
    exp_7_plots$orig$blocks_1_to_4 + theme(legend.position = "none"),
    exp_7_plots$optim$blocks_1_to_4 + scale_y_continuous(NULL),
    nrow = 1
  )
  
  panel_4 <- egg::ggarrange(
    exp_7_plots$orig$block_5 + theme(legend.position = "none"),
    exp_7_plots$optim$block_5 + scale_y_continuous(NULL),
    nrow = 1
  )
  
  panel_5 <- egg::ggarrange(
    plot_block_5_by_presentation(res$exp_7$orig) + theme(legend.position = "none"),
    plot_block_5_by_presentation(res$exp_7$optim) + scale_y_continuous(NULL), #+ theme(legend.position = "bottom", 
                                                                              #       legend.direction = "vertical"),
    nrow = 1
  )

  cowplot::plot_grid(panel_1, panel_2, panel_3, panel_4, panel_5,
                     ncol = 1,
                     labels = "AUTO", 
                     rel_heights = c(1, 1, 1, 0.85, 1))
                     # labels = c("Exp. 1", "Exp. 4a", "Exp. 7 (1-4)", "Exp. 7 (5)"))
}

plot_block_5_by_presentation <- function(
  exp_7, 
  cond_list = c(
    "#1471B9" = "RANREG",
    "#EEC00D" = "RANREGr",
    "#00FF00" = "REPinRANr"
  )) {
  exp_7 %>% 
    filter(block == 5 &
             !is.na(rep) &
             !is.na(model_reaction_time) &
             model_reaction_time > 0 & 
             cond %in% cond_list) %>% 
    group_by(subj, rep, cond) %>% 
    summarise(rt_mean = mean(model_reaction_time, na.rm = TRUE)) %>% 
    
    # summarise_model_rt() %>% 
    # select(rep, cond, rt_mean) %>% 
    pivot_wider(id_cols = c("subj", "rep"), names_from = cond, values_from = rt_mean) %>% 
    ungroup() %>% 
    mutate("RANREG - RANREGr*" = RANREG - REPinRANr,
           "RANREG - RANREGr" = RANREG - RANREGr) %>% 
    select(subj, rep, `RANREG - RANREGr*`, `RANREG - RANREGr`) %>% 
    pivot_longer(cols = c("RANREG - RANREGr*",
                          "RANREG - RANREGr"),
                 values_to = "model_reaction_time") %>% 
    group_by(rep, name) %>% 
    summarise_model_rt(na.rm = TRUE) %>% 
    ggplot(aes(rep, rt_mean, ymin = rt_95_lower, ymax = rt_95_upper, fill = name)) + 
    geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
    geom_errorbar(width = 0.3, position = position_dodge(width = 1)) +
    scale_x_continuous(NULL) +
    scale_y_continuous("RT advantage (s)") + 
    scale_fill_manual(NULL, values = c("RANREG - RANREGr*" = "#00FF00",
                                       "RANREG - RANREGr" = "#EEC00D")) +
    guides(fill = guide_legend(reverse = TRUE)) + 
    theme(aspect.ratio = 1)
}
