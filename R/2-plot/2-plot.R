source("R/2-plot/2-setup.R")

dat <- readRDS("output/data-02-models.rds")

dat %>% 
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
  geom_ribbon(aes(x = block, ymin = ymin, ymax = ymax, group = paste(cond, data_source)),
              alpha = 0.3) + 
  scale_x_continuous("Block") + 
  scale_y_continuous("Reaction time (s)") +
  scale_color_manual("Condition", values = c("black", "blue")) + 
  scale_linetype_discrete("Type") + 
  theme(aspect.ratio = 1)

 
