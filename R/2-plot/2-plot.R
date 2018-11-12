source("R/2-plot/2-setup.R")

dat <- readRDS("output/data-02-models.rds")
# dat <- readRDS("output/archive/escape-a/data-02-models.rds")

p1 <- dat %>% 
  select(subj, cond, block, RTadj, idyom_cp_reaction_time) %>% 
  filter(!is.na(cond)) %>% 
  group_by(subj, cond, block) %>% 
  summarise_all(funs(mean), na.rm = TRUE) %>% 
  group_by(cond, block) %>% 
  gather(data_source, reaction_time, RTadj, idyom_cp_reaction_time) %>% 
  select(- subj) %>% 
  group_by(cond, block, data_source) %>%
  summarise_all(funs(mean, sd, n = length, se = sd / sqrt(n), 
                     ymin = mean - se, ymax = mean + se)) %>% 
  ungroup() %>%
  mutate(data_source = recode(data_source,
                              idyom_cp_reaction_time = "Simulated", 
                              RTadj = "Observed"),
         cond = recode(cond,
                       RANDREG = "Novel",
                       TARGET = "Repeated")) %>% 
  ggplot(aes()) + 
  geom_line(aes(x = block, y = mean, colour = cond, linetype = data_source)) +
  geom_ribbon(aes(x = block, ymin = ymin, ymax = ymax, fill = cond, group = paste(cond, data_source)),
              alpha = 0.3) + 
  scale_x_continuous("Block") + 
  scale_y_continuous("Reaction time (s)") +
  scale_fill_manual("Condition", values = c("black", "blue")) + 
  scale_color_manual("Condition", values = c("black", "blue")) + 
  scale_linetype_discrete("Type") + 
  theme(aspect.ratio = 1)
plot(p1)
ggsave("fig-1--ideal-model--rt-by-block-and-cond.pdf", path = "output",
       width = 4.5, height = 4)

p2 <- dat %>% 
  select(block, cond, idyom_cp_reaction_time) %>% 
  filter(!is.na(cond)) %>% 
  filter(!is.na(idyom_cp_reaction_time)) %>% 
  mutate(cond = recode(cond,
                       RANDREG = "Novel",
                       TARGET = "Repeated")) %>% 
  ggplot(aes(x = cond, y = idyom_cp_reaction_time)) + 
  geom_violin(bw = 0.03, fill = "black") + 
  theme(aspect.ratio = 1) + 
  coord_flip() + 
  facet_wrap(~ block) +
  scale_y_continuous("Model reaction time (s)") +
  scale_x_discrete("Condition")
plot(p2)
ggsave("fig-2--ideal-model--model-rt-by-cond.pdf", path = "output",
       width = 4, height = 4)

dat %>%
  filter(subj == 1 & cond == "TARGET" & block == 6) %>% 
  select(transition, idyom_ic) %>% 
  mutate(trial_id = seq_along(transition)) %>% 
  by_row(function(x) {
    x$idyom_ic[[1]] %>% 
      select(information_content) %>% 
      mutate(trial_id = x$trial_id,
             when = seq_along(information_content) - x$transition) %>% 
      filter(- 10 <= when & when <= 30)
  }, .collate = "rows", .labels = FALSE) %>% 
  select(information_content, when) %>% 
  group_by(when) %>% 
  summarise_all(funs(mean = mean, sd = sd, n = length, se = sd / sqrt(n),
                     ymin = mean - sd, ymax = mean + sd)) %>% 
  ggplot(aes(x = when, y = mean, ymin = ymin, ymax = ymax)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(alpha = 0.25, fill = "blue") +
  scale_x_continuous("Tone number (relative to transition)") + 
  scale_y_continuous("Information content (bits/tone)") + 
  theme(aspect.ratio = 1)
ggsave("fig-3--ideal-model--p1-ic-by-tone.pdf", path = "output",
       width = 4, height = 4)

dat %>%
  filter(cond == "TARGET" & rep == 1 & block %in% c(1, 2)) %>% 
  by_row(function(x) {
    mods <- c("mod_null", "mod_manual")
    map_dfr(mods, function(mod) {
      x[[mod]][[1]] %>% 
        select(information_content) %>% 
        mutate(mod = mod,
               block = x$block,
               when = seq_along(information_content) - x$transition) %>% 
        filter(- 10 <= when & when <= 30)
    })
  }, .collate = "rows", .labels = FALSE) %>% 
  select(- .row) %>% 
  group_by(when, block, mod) %>% 
  summarise_all(funs(mean = mean, sd = sd, n = length, se = sd / sqrt(n),
                     ymin = mean - sd, ymax = mean + sd)) %>% 
  ungroup() %>% 
  mutate(block = paste("Block", block),
         mod = recode(mod,
                      mod_manual = "PPM decay",
                      mod_null = "PPM (original)")) %>%
  ggplot(aes(x = when, y = mean, ymin = ymin, ymax = ymax)) + 
  facet_grid(mod ~ block) + 
  geom_line() + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 20, linetype = "dotted") + 
  geom_ribbon(alpha = 0.25, fill = "blue") +
  scale_x_continuous("Tone number (relative to transition)") + 
  scale_y_continuous("Information content (bits/tone)") + 
  theme(aspect.ratio = 1)
