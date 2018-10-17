library(tidyverse)
library(PPMdecay)
library(changepoint)
library(zeallot)

loadNamespace("memoise")

if (!dir.exists("cache")) dir.create("cache")

for (f in list.files("R/1-model/functions/", full.names = TRUE, pattern = "\\.R$"))
  source(f)