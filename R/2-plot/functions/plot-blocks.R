library("ggsci")
library(magrittr)

plot_block <- function(x, 
                       block,
                       cond_list = c("#1471B9" = "RANDREG",
                                     "#EEC00D" = "TARGET"),
                       subtract_1_sec_from = character()) {
  stopifnot(length(block) == 1)
  x %>% 
    filter(block == !!block &
             !is.na(model_reaction_time) &
             model_reaction_time > 0) %>% 
    summarise_blocks(cond_list, subtract_1_sec_from) %>%
    ggplot(aes(cond, rt_mean, 
               ymin = rt_mean - rt_se,
               ymax = rt_mean + rt_se,
               # ymin = rt_95_lower, 
               # ymax = rt_95_upper,
               colour = cond,
               fill = cond)) + 
    geom_point(alpha = 0.8, size = 2, shape = 21) +
    geom_errorbar(width = 0) +
    scale_x_discrete(NULL) +
    scale_y_continuous("Reaction time (s)") +
    scale_color_manual("Condition", values = names(cond_list)) +
    scale_fill_manual("Condition", values = names(cond_list)) +
    theme(aspect.ratio = 1,
          axis.text.x = element_blank())
}

summarise_blocks <- function(x, cond_list, subtract_1_sec_from) {
  stopifnot(all(subtract_1_sec_from %in% cond_list))
  x %>% 
    filter(cond %in% cond_list &
             !is.na(model_reaction_time) &
             model_reaction_time > 0) %>% 
    mutate(cond = factor(cond, levels = cond_list),
           RTadj = if_else(cond %in% subtract_1_sec_from,
                           RTadj - 1,
                           RTadj),
           model_reaction_time = if_else(cond %in% subtract_1_sec_from,
                                         model_reaction_time - 1,
                                         model_reaction_time)) %>% 
    group_by(block, cond, subj) %>% 
    summarise(model_reaction_time = mean(model_reaction_time, na.rm = TRUE)) %>% 
    group_by(block, cond) %>% 
    summarise_model_rt() %>% 
    ungroup()
}

summarise_model_rt <- function(x, na.rm = FALSE) {
  x %>% 
    summarise(# human_rt = mean(RTadj, na.rm = TRUE),
      rt_mean = mean(model_reaction_time, na.rm = na.rm),
      rt_n = sum(!is.na(model_reaction_time)),
      rt_sd = sd(model_reaction_time, na.rm = na.rm),
      rt_se = rt_sd / sqrt(rt_n),
      rt_95_lower = rt_mean - 1.96 * rt_se,
      rt_95_upper = rt_mean + 1.96 * rt_se)
}

plot_blocks <- function(x, 
                        cond_list = c("RANDREG",
                                      "TARGET"),
                        subtract_1_sec_from = character(),
                        hline_1 = NULL,
                        hline_2 = NULL,
                        error_bar = FALSE,
                        ribbon = TRUE,
                        line = TRUE) {
  p <- summarise_blocks(x, cond_list, subtract_1_sec_from) %>% 
    rename(Condition = cond) %>% 
    print() %T>% 
    write_csv("temp.csv") %>% 
    ggplot(aes(x = block, y = rt_mean, 
               ymin = rt_mean - rt_se,
               ymax = rt_mean + rt_se,
               # ymin = rt_95_lower, 
               # ymax = rt_95_upper,
               colour = Condition,
               fill = Condition)) +
    scale_x_continuous("Block") +
    scale_y_continuous("Reaction time (s)") +
    geom_point(alpha = 0.8, size = 2, shape = 21) +
    theme(aspect.ratio = 1)
  
  if (is.null(names(cond_list))) {
    p <- p +
      ggsci::scale_color_jco() +
      ggsci::scale_fill_jco()
  } else {
    p <- p +
      scale_color_manual(values = names(cond_list)) +
      scale_fill_manual(values = names(cond_list))
  }
  
  if (!is.null(hline_1)) 
    p <- p + geom_hline(yintercept = hline_1, linetype = "dotted")
  
  if (!is.null(hline_2)) 
    p <- p + geom_hline(yintercept = hline_2, linetype = "dotted")
  
  if (line) p <- p + geom_line(alpha = 0.8)
  if (error_bar) p <- p + geom_errorbar(alpha = 0.6, width = 0)
  if (ribbon) p <- p + geom_ribbon(alpha = 0.1, colour = NA)
  
  p
}
