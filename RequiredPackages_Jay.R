

## Required packages
remove.packages("tidyverse")
install.packages("tidyverse")
remove.packages("cli")
##install.packages("cli") ## Should install tar.gz 3.3.0 version manually. 
install.packages("maps")

remove.packages("rlang")
install.packages("rlang")
library(tidyverse)

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
install.packages("scales")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("stringr")                                    # Install stringr R package
library("stringr")   


## Set working directory
setwd("E:/Dropbox/Study/GitHub/ui_rep_rate")

Sys.setenv(LANG = "en")