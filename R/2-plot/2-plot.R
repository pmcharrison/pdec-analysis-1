source("R/2-plot/2-setup.R")

dat <- readRDS("output/data-02-models.rds")

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

         
