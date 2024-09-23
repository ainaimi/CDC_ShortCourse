## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=40),tidy=TRUE)

packages <- c( "data.table","tidyverse","ggplot2","ggExtra","formatR",
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

## ----warning = F, message = F-------------------------------------------------

set.seed(123)
my_dataset <- data.frame(a = runif(10), 
                         b = rnorm(10),
                         c = rbinom(10, 1, .5))

my_dataset

## ----warning = F, message = F-------------------------------------------------

# using base R
my_dataset[1:4, ]

# using base R take 2
index <- 1:4
my_dataset[index, ]

# using tidyverse
my_dataset %>% slice(1:4)

# using head
head(my_dataset, 4)


## ----F2, out.width = "10cm", fig.align="center",echo=F------------------------
knitr::include_graphics(here("figures", "installR.jpg"))

## ----F3, out.width = "15cm", fig.align="center",echo=F------------------------
knitr::include_graphics(here("figures", "installR2.jpg"))

## ----F4, out.width = "15cm", fig.align="center",echo=F------------------------
knitr::include_graphics(here("figures", "rstudio-workbench.png"))

## ----F5, out.width = "10cm", fig.align="center",echo=F------------------------
knitr::include_graphics(here("figures", "installRStudio1.png"))

## ----F7, out.width = "10cm", fig.align="center",echo=F------------------------
knitr::include_graphics(here("figures", "RStudioOptions.png"))

## ----out.width = "10cm", fig.align="center",fig.cap="The Data Science Pipeline",echo=F----
knitr::include_graphics(here("Figures","2022_09_20-reproducibility.pdf"))

