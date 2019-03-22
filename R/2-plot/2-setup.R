library(tidyverse)

theme_set(cowplot::theme_cowplot() + 
            theme(strip.background = element_blank()))

for (f in list.files("R/2-plot/functions/", full.names = TRUE))
  source(f)
