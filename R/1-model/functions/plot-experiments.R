plot_experiments <- function(res) {
  plot_blocks(res$exp_1$orig %>% filter(model_reaction_time > 0), error_bar = TRUE, line = TRUE, ribbon = FALSE)
  plot_blocks(res$exp_1$optim %>% filter(model_reaction_time > 0), error_bar = TRUE, line = TRUE, ribbon = FALSE)
  
  plot_blocks(res$exp_4a$orig %>% filter(model_reaction_time > 0), error_bar = TRUE, line = TRUE, ribbon = FALSE)
  plot_blocks(res$exp_4a$optim %>% filter(model_reaction_time > 0), error_bar = TRUE, line = TRUE, ribbon = FALSE)
  
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
  
  map(res$exp_7, function(z) {
    plot_blocks(
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
    
    plot_block(z2, 
               block = 5,
               cond_list = c(
                 "#1471B9" = "RANREG",
                 "#EEC00D" = "RANREGr",
                 "#00FF00" = "REPinRANr"
               ),
               subtract_1_sec_from = c("RANREG", "RANREGr", "REPinRANr"))
  })
}
