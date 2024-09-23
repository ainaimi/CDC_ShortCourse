## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=40),tidy=TRUE)

packages <- c( "data.table","tidyverse","ggplot2","ggExtra","formatR","broom",
               "gridExtra","skimr","here","Hmisc","RColorBrewer")#,"gmm")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN',dependencies=T)
  }
}

for (package in packages) {
  library(package, character.only=T)
}

remotes::install_github("rstudio/fontawesome")

library(fontawesome)

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----F7, out.width = "10cm", fig.align="center",echo=F------------------------
knitr::include_graphics(here("figures", "FolderStructure.png"))

## ----out.width = "10cm", fig.align="center",echo=F----------------------------
knitr::include_graphics(here("figures", "NewProject1.png"))

## ----out.width = "10cm", fig.align="center",echo=F----------------------------
knitr::include_graphics(here("figures", "NewProject2.png"))

## ----out.width = "10cm", fig.align="center",echo=F----------------------------
knitr::include_graphics(here("figures", "FolderStructure2.png"))

## ----out.width = "18cm", fig.align="center",echo=F----------------------------
knitr::include_graphics(here("figures", "NewProject3.png"))

## -----------------------------------------------------------------------------

pacman::p_load(tidyverse, here, VIM)


## -----------------------------------------------------------------------------

file_loc <- url("https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/1268/20/nhefs.csv")
nhefs_data <- read_csv(file_loc)

# let's look at the data
dim(nhefs_data)

nhefs_data


## -----------------------------------------------------------------------------

nhefs_data <- nhefs_data %>% 
  select(seqn, qsmk, sbp, dbp, sex, age, race, income, wt82_71, death)


## -----------------------------------------------------------------------------

aggr(nhefs_data)


## -----------------------------------------------------------------------------

propMissing <- function(x){
  mean(is.na(x))
}

apply(nhefs_data,2,propMissing)


## -----------------------------------------------------------------------------
nhefs_data <- nhefs_data %>% na.omit()

## -----------------------------------------------------------------------------

nhefs_data <- nhefs_data %>% 
  mutate(map = dbp + (sbp - dbp)/3) %>% 
  select(-sbp, -dbp)


## -----------------------------------------------------------------------------

dim(nhefs_data)

nhefs_data %>% print(n=5)


## -----------------------------------------------------------------------------
getwd()

## ----include=F----------------------------------------------------------------
og <- getwd()

## -----------------------------------------------------------------------------

setwd("/Users/ain/Library/CloudStorage/Dropbox/01 Projects/CDC_ShortCourse/1. useR/3. GoodPrograms")

getwd()

write_csv(nhefs_data, file = "../data/analytic_data.csv")


## ----include=F----------------------------------------------------------------
setwd(og)

## ----tidy = F, warning = F, message = F, eval=F-------------------------------
#  write_csv(nhefs_data, file = "./data/analytic_data.csv")

## -----------------------------------------------------------------------------
here()

## ----tidy = F, warning = F, message = F---------------------------------------

write_csv(nhefs_data, file = here("data","analytic_data.csv"))


## ----out.width = "10cm", fig.align="center",fig.cap="The Data Science Pipeline",echo=F----
knitr::include_graphics(here("Figures","2022_09_20-reproducibility.pdf"))

