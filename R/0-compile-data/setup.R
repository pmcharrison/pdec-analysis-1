library(tidyverse)

for (f in list.files("R/0-compile-data/functions/", full.names = TRUE, pattern = "\\.R$"))
     source(f)
