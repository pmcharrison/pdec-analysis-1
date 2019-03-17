library(tidyverse)

theme_set(theme_classic())
  
tibble(
  one_gram = c("C", "C", "B", "A", rep(LETTERS[1:3], times = 3)),
  two_gram = c(NA, map2_chr(one_gram[- length(one_gram)], one_gram[2:length(one_gram)], paste0)),
  three_gram = c(NA, NA, map2_chr(one_gram[seq_len(length(one_gram) - 2L)], two_gram[-c(1:2)], paste0)),
  pos = seq_along(one_gram),
) %>% 
  gather("group", "value", one_gram, two_gram, three_gram) %>% 
  na.omit() %>% 
  group_by(group) %>% 
  mutate(count = map_int(seq_along(value), function(i) sum(value[1:i] == value[i]))) %>% View
  mutate(
    familiarity = if_else(group == "seq", 0,
                          if_else(group == "two_gram", 
                                  pmax(0, pos - 8),
                                  2 * pmax(0, pos - 9)))
  ) %>%
  mutate(group = recode_factor(
    group, 
    seq = "1",
    two_gram = "2",
    three_gram = "3"
  )) %>% 
  ggplot(aes(x = pos, y = 1, label = value, fill = familiarity)) + 
  geom_label() + 
  scale_x_continuous("Position", breaks = 1:13) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  facet_wrap(~ group, ncol = 1, strip.position = "left", ) + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())


  geom
