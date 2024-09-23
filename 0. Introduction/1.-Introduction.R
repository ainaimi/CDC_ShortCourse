## ----setup, include=FALSE-----------------------------------------------------

pacman::p_load(data.table,tidyverse,ggplot2,ggExtra,formatR,
               gridExtra,skimr,here,Hmisc,RColorBrewer,knitr,
               fontawesome)

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----warning = F, message=F, eval = F-----------------------------------------
#  
#  install.packages("pacman", repos = "http://cran.us.r-project.org")
#  
#  pacman::p_load(tidyverse, here, sandwich, lmtest,
#                 survey, boot, ggplot2, broom)
#  

## ----warning = F, message=F, eval=F-------------------------------------------
#  
#  install.packages("tidyverse")
#  library(tidyverse)
#  
#  install.packages("here")
#  library(here)
#  
#  install.packages("sandwich")
#  library(sandwich)
#  
#  ...
#  
#  install.packages("broom")
#  library(broom)
#  

## ----warning = F, message=F, eval=F-------------------------------------------
#  
#  install.packages("remotes")
#  library(remotes)
#  

## ----warning = F, message = F, eval = F---------------------------------------
#  
#  remotes::install_github("tlverse/tlverse")
#  library(tmle3)
#  library(sl3)
#  

## ----warning = F, message=F, eval=F-------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  
#  devtools::install_github("tlverse/tlverse")
#  library(tmle3)
#  library(sl3)

## ----eval = F-----------------------------------------------------------------
#  
#  install.packages("https://cran.r-project.org/src/contrib/
#                   Archive/imputeMissings/imputeMissings_0.0.3.tar.gz")
#  

## ----figurename, out.width="10cm", fig.align='center', echo=F-----------------
knitr::include_graphics(here("_images", "git_creds_illustration.png"))

