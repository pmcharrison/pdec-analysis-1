library(tidyverse)
library(zeallot)

loadNamespace("memoise")
loadNamespace("plyr")
loadNamespace("cpm")
loadNamespace("ppm")
loadNamespace("nloptr")

if (!dir.exists("cache")) dir.create("cache")

for (f in list.files("R/1-model/functions/", full.names = TRUE, pattern = "\\.R$"))
  source(f)
