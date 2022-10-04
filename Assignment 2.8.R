library(readr)
library(dplyr)
library(tidyverse)
library(foreign)

#Load the data
choll <- read.spss("data/voorbeeld7_1.sav", to.data.frame = TRUE,
                   locale = locale(encoding="ISO-8859-1"))
head(choll)

library(ggplot2)
