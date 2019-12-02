library(tidyverse)

# theme_set(cowplot::theme_cowplot() + 
#             theme(strip.background = element_blank()))

s <- 12
theme_set(
  theme_bw() + theme(
    axis.text=element_text(size = s),
    axis.title=element_text(size = s),
    legend.title = element_text(size = s),
    legend.text = element_text(size = s)
  )
)

for (f in list.files("R/2-plot/functions/", full.names = TRUE))
  source(f)
