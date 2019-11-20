get_ic_profile_data <- function(x) {
  x %>% 
    filter(!is.na(cond)) %>% 
    select(subj, block, trialN, cond, transition, mod_pos_when_change_detected, mod) %>% 
    pmap(function(subj, block, trialN, cond, transition, mod_pos_when_change_detected, mod) {
      tibble(subj, block, trialN, cond, transition, mod_pos_when_change_detected,
             pos = mod$pos - mod$pos[1] + 1L,
             information_content = mod$information_content)
    }) %>% 
    bind_rows()
}

plot_ic_profile <- function(x, opt, loess = TRUE, span = 0.1, xlim = c(NA, NA), 
                            ribbon = TRUE) {
  p <- x %>% 
    filter(!is.na(cond)) %>% 
    filter(cond == "TARGET" & block %in% c(1, 5)) %>% 
    select(block, transition, mod_pos_when_change_detected, mod) %>% 
    mutate(block = paste("Block", block)) %>% 
    select(block, transition, mod) %>% 
    pmap(function(block, transition, mod) {
      tibble(
        block = block,
        pos = mod$pos - mod$pos[1] + 1L,
        rel_pos = pos - transition,
        information_content = mod$information_content
      )
    }) %>% 
    bind_rows() %>% 
    group_by(block, rel_pos) %>% 
    summarise(ic_mean = mean(information_content),
              ic_sd = sd(information_content),
              n = n(),
              ic_se = ic_sd / sqrt(n),
              ic_upper_95 = ic_mean + 1.96 * ic_se,
              ic_lower_95 = ic_mean - 1.96 * ic_se) %>% 
    {if (is.na(xlim[1])) . else filter(., rel_pos >= xlim[1])} %>% 
    {if (is.na(xlim[2])) . else filter(., rel_pos <= xlim[2])} %>% 
    ggplot(aes(rel_pos, ic_mean, ymin = ic_lower_95, ymax = ic_upper_95, 
               colour = block, fill = block)) +
    {if (loess) 
      geom_smooth(span = span, method = "loess", se = FALSE) else
      geom_line()} +
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    scale_x_continuous("Tone number", 
                       sec.axis = sec_axis(~ (.) * opt$tone_length,
                                           name = "Time (s)"),
                       limits = xlim) +
    scale_y_continuous("Information content") + 
    scale_colour_viridis_d(NULL) +
    scale_fill_viridis_d(NULL) +
    theme(aspect.ratio = 1,
          legend.position = c(0.8, 0.8))
  
  if (ribbon) p <- p + geom_ribbon(alpha = 0.2, linetype = "blank")
  
  p
  # geom_vline(aes(xintercept = mod_pos_when_change_detected), df2, linetype = "dashed") +
  # facet_wrap(~ block, ncol = 1)
}

plot_ic_profile_2 <- function(x, opt) {
  df1 <- x %>% 
    filter(!is.na(cond)) %>% 
    filter(block == 5) %>% 
    select(block, cond, transition, mod_pos_when_change_detected, mod) %>% 
    mutate(cond = recode(cond, 
                         RANDREG = "Novel",
                         TARGET = "Repeated"))
  
  df2 <- df1 %>% 
    select(- mod) %>% 
    group_by(cond) %>% 
    summarise_all(mean, na.rm = TRUE)
  
  df3 <- df1 %>% 
    select(cond, mod) %>% 
    pmap(function(cond, mod) {
      tibble(
        cond = cond,
        pos = mod$pos - mod$pos[1] + 1L,
        information_content = mod$information_content
      )
    }) %>% 
    bind_rows() %>% 
    group_by(cond, pos) %>% 
    summarise(ic_mean = mean(information_content),
              ic_sd = sd(information_content))
  
  df3 %>% 
    ggplot(aes(pos, ic_mean, ymin = ic_mean - ic_sd, ymax = ic_mean + ic_sd)) +
    geom_line() + 
    geom_ribbon(alpha = 0.2, fill = "blue") + 
    geom_vline(aes(xintercept = transition), df2, linetype = "dashed") +
    geom_vline(aes(xintercept = mod_pos_when_change_detected), df2, linetype = "dashed") +
    scale_x_continuous("Tone number", 
                       sec.axis = sec_axis(~ (. - 1) * opt$tone_length,
                                           name = "Time (s)")) +
    scale_y_continuous("Information content") +
    facet_wrap(~ cond, ncol = 1) + 
    theme(strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")))
}
