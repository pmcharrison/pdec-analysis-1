library(tidyverse)

theme_set(theme_classic() + theme(
  axis.text = element_text(colour = "black"),
  axis.ticks = element_line(colour = "black")
))

panel <- function(prefix, familiarity, detect, label_x = TRUE, legend = FALSE,
                  label_facets = FALSE) {
  p <- tibble(
    one_gram = c(prefix, rep(LETTERS[1:3], times = 3)),
    two_gram = c(NA, map2_chr(one_gram[- length(one_gram)], one_gram[2:length(one_gram)], paste0)),
    three_gram = c(NA, NA, map2_chr(one_gram[seq_len(length(one_gram) - 2L)], two_gram[-c(1:2)], paste0)),
    pos = seq_along(one_gram),
  ) %>% 
    gather("group", "value", one_gram, two_gram, three_gram) %>% 
    na.omit() %>% 
    group_by(group) %>% 
    mutate(count = map_int(seq_along(value), function(i) sum(value[1:i] == value[i]))) %>%
    ungroup() %>% 
    familiarity() %>%
    mutate(group = recode_factor(
      group, 
      one_gram = "1",
      two_gram = "2",
      three_gram = "3"
    )) %>% 
    ggplot(aes(x = pos, y = 1, label = value, fill = familiarity)) + 
    geom_vline(xintercept = 4.5) +
    geom_vline(xintercept = detect) +
               # linetype = "dotted") +
               # colour = rgb(126, 81, 255, maxColorValue = 255)) +
    geom_label() + 
    scale_x_continuous(NULL, breaks = 1:16) +
    scale_fill_gradient("Relative familiarity",
                        low = "white", high = "#2377ff", limits = c(0, 16),
                        guide = if (legend) "colourbar" else 
                          guide = FALSE) +
    facet_wrap(~ group, ncol = 1, strip.position = "left", ) + 
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          panel.spacing = unit(0, "lines"))
          # legend.position = "bottom")
  if (!label_x) p <- p + theme(axis.title.x = element_text(colour = "white"),
                               axis.text.x = element_text(colour = "white"))
  if (!legend) p <- p + theme(
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.key = element_rect(fill = "white")
  )
  if (!label_facets) p <- p + theme(
    strip.background = element_blank(),
    strip.text = element_blank()
  )
  p
}

p1 <- panel(c("C", "C", "B", "A"),
            function(df) {
              mutate(
                df,
                familiarity = if_else(group == "one_gram",
                                      0,
                                      # case_when(
                                      #   pos == 1 ~ 0,
                                      #   pos == 2 ~ 1,
                                      #   pos == 3 ~ 0,
                                      #   pos == 4 ~ 0,
                                      #   TRUE ~ 1 
                                      # ),
                                      if_else(group == "two_gram", 
                                              pmax(0, pos - 8),
                                              2 * pmax(0, pos - 9)))
              )
            },
            detect = 11.5,
            label_x = FALSE,
            legend = FALSE)

p2 <- panel(
  c("B", "A", "A", "B"),
  function(df) {
    mutate(
      df,
      familiarity = if_else(group == "one_gram",
                            0,
                            if_else(group == "two_gram", 
                                    pmin(12, if_else(pos < 6, 0,
                                                     2 + pos - 6)),
                                    pmin(16, 2 * if_else(pos < 7, 0,
                                                         2 + pos - 7))))
    )
  },
  detect = 9.5,
  label_x = FALSE)

p3 <- panel(c("C", "B", "B", "A"),
            function(df) {
              mutate(
                df,
                familiarity = if_else(group == "one_gram", 0,
                                      if_else(group == "two_gram", 
                                              if_else(pos >= 6, 12, 0),
                                              if_else(pos >= 7, 16, 0)))
              )
            },
            detect = 7.5,
            label_x = TRUE,
            label_facets = FALSE)

legend <- panel("A", function(df) mutate(df, familiarity = 2),
                detect = 5,
                legend = TRUE) %>% cowplot::get_legend()

cowplot::plot_grid(
  cowplot::plot_grid(p1, p2, p3, ncol = 1),
  legend,
  nrow = 1, 
  rel_widths = c(5, 1)
)

ggsave("output/schematic-figure.pdf", width = 9, height = 4)
