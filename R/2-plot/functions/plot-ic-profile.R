get_ic_profile_data <- function(optimized_analyses) {
  optimised_analyses$combined %>% 
    filter(!is.na(cond)) %>% 
    select(subj, block, trialN, cond, transition, mod_pos_when_change_detected, mod) %>% 
    pmap(function(subj, block, trialN, cond, transition, mod_pos_when_change_detected, mod) {
      tibble(subj, block, trialN, cond, transition, mod_pos_when_change_detected,
             pos = mod$pos - mod$pos[1] + 1L,
             information_content = mod$information_content)
    }) %>% 
    bind_rows()
}

plot_ic_profile <- function(optimised_analyses) {
  df1 <- optimised_analyses$combined %>% 
    filter(!is.na(cond)) %>% 
    filter(cond == "TARGET" & block %in% c(1, 5)) %>% 
    select(block, transition, mod_pos_when_change_detected, mod) %>% 
    mutate(block = paste("Block", block))
  
  df2 <- df1 %>% 
    select(- mod) %>% 
    group_by(block) %>% 
    summarise_all(mean, na.rm = TRUE)
  
  df3 <- df1 %>% 
    select(block, mod) %>% 
    pmap(function(block, mod) {
      tibble(
        block = block,
        pos = mod$pos - mod$pos[1] + 1L,
        information_content = mod$information_content
      )
    }) %>% 
    bind_rows() %>% 
    group_by(block, pos) %>% 
    summarise(ic_mean = mean(information_content),
              ic_sd = sd(information_content))
  
  df3 %>% 
    filter(pos >= 50) %>% 
    ggplot(aes(pos, ic_mean, ymin = ic_mean - ic_sd, ymax = ic_mean + ic_sd, 
               colour = block, fill = block)) +
    geom_line() + 
    geom_vline(aes(xintercept = transition), df2, linetype = "dashed") +
    
    scale_x_continuous("Tone number", 
                       sec.axis = sec_axis(~ (. - 1) * par$tone_length,
                                           name = "Time (s)")) +
    scale_y_continuous("Information content") + 
    scale_colour_viridis_d(NULL) +
    theme(aspect.ratio = 1)
  # geom_ribbon(alpha = 0.2, colour = "black") + 
  # geom_vline(aes(xintercept = mod_pos_when_change_detected), df2, linetype = "dashed") +
  # facet_wrap(~ block, ncol = 1)
}

plot_ic_profile_2 <- function(optimised_analyses) {
  df1 <- optimised_analyses$combined %>% 
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
                       sec.axis = sec_axis(~ (. - 1) * par$tone_length,
                                           name = "Time (s)")) +
    scale_y_continuous("Information content") +
    facet_wrap(~ cond, ncol = 1) + 
    theme(strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")))
}
