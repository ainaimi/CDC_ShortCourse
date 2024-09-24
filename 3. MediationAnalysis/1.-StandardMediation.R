## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=40),tidy=TRUE)

remotes::install_github("rstudio/fontawesome")

library(fontawesome)

library(here)

library(tidyverse)

knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----simple-mediation, out.width="5cm", fig.align='center', fig.cap="Illustration of overly simplified mediation scenario with a single expsoure (X), a single mediator (M), and a single outcome (Y).", echo=F----
knitr::include_graphics(here("figures", "simple_mediation.png"))

## ----path-analysis, out.width="2.5cm", fig.margin = TRUE, fig.align='center', echo=F----
knitr::include_graphics(here("figures", "sewall_figure.png"))

## ----figurename, out.width="12cm", fig.align='center', fig.cap="Table 2 from Hafeman and Schwartz 2009.", echo=F----
knitr::include_graphics(here("figures", "Hafeman2009Table.png"))

## ----tidy = F, warning = F, message = F---------------------------------------

pacman::p_load(tidyverse,
               here,
               broom,
               boot)

nhefs <- read_csv(here("data","analytic_data.csv"))

nhefs <- nhefs %>% 
  mutate(wt_delta = as.numeric(wt82_71 > median(wt82_71)))

names(nhefs)

summary(lm(qsmk ~ race, data = nhefs))$coefficients
summary(lm(wt_delta ~ race, data = nhefs))$coefficients


## ----tidy = F, warning = F, message = F---------------------------------------

alpha1 <- summary(lm(wt_delta ~ race, data = nhefs))$coefficients["race",]

alpha1

beta1 <- summary(lm(wt_delta ~ race + qsmk, data = nhefs))$coefficients["race",]

beta1


## ----tidy = F, warning = F, message = F---------------------------------------

beta1 <- summary(lm(wt_delta ~ race + qsmk, data = nhefs))$coefficients["race",]

beta1

beta2 <- summary(lm(wt_delta ~ race + qsmk, data = nhefs))$coefficients["qsmk",]

beta2

gamma1  <- summary(lm(qsmk ~ race, data = nhefs))$coefficients["race",]

gamma1


## ----mediation1, out.width="5cm", fig.align='center', fig.cap="When doing mediation analysis, both exposure-outcome and mediator-outcome (and sometimes exposure-mediator) confounding.", echo=F----
knitr::include_graphics(here("figures", "mediation1.png"))

## ----mediation2, out.width="5cm", fig.align='center', fig.cap="Illustration of mediator-outcome confounders affected by the exposure.", echo=F----
knitr::include_graphics(here("figures", "mediation2.png"))

