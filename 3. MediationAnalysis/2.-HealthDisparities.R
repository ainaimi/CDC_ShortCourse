## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=40),tidy=TRUE)

remotes::install_github("rstudio/fontawesome")

library(fontawesome)

library(tidyverse)

library(here)

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----nhefs-dag, out.width="7cm", fig.align='center', fig.cap="Simple diagram illustrating relationships between quitting smoking, map, death, and weight change in the NHEFS data.", echo=F----
knitr::include_graphics(here("figures", "nhefs_dag.pdf"))

## ----tidy = F, warning = F, message = F, echo = T-----------------------------

pacman::p_load(tidyverse,
               here,
               lmtest,
               sandwich,
               boot,
               splines)

file_loc <- url("https://is.gd/VPKKsi")
nhefs <- read_csv(file_loc) %>% 
  mutate(map = dbp + (sbp - dbp)/3,
         wt_delta = as.numeric(wt82_71 > median(wt82_71, na.rm = T))) %>% 
  select(qsmk, map, wt_delta, death, hbp, smokeintensity,
         smokeyrs, hbpmed, age, race, sex, income, 
         exercise, cholesterol, diabetes) %>% 
  na.omit()

nhefs

plot1 <- ggplot(nhefs) +
  geom_histogram(aes(map, 
                     group = factor(qsmk),
                     fill = factor(qsmk)),
                 position = "identity",
                 alpha = 0.5) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ylab("Count") + xlab("Mean Arterial Pressure (mm Hg)") +
  theme(text = element_text(size = 10))

ggsave(here("figures", "weight_change_distribution.pdf"), plot = plot1)


## ----weight-change, out.width="10cm", fig.align='center', fig.cap="Distrtibution of mean arterial pressure stratified by quitting smoking status in the NHEFS data.", echo=F----
knitr::include_graphics(here("figures", "weight_change_distribution.pdf"))

## ----tidy = F, warning = F, message = F---------------------------------------

nhefs <- nhefs %>% 
  mutate(map_binary = as.numeric(map >= 90))

nhefs %>% 
  group_by(map_binary) %>% 
  summarize(mean_wt_change = mean(map),
            max_wt_change = max(map),
            min_wt_change = min(map))


## ----tidy = F, warning = F, message = F---------------------------------------

# PS model for quitting smoking: need to include all confounders of exposure-outcome relation
nhefs$ps_qsmk <- glm(qsmk ~ factor(hbp) + factor(hbpmed) + ns(age, df = 4) + sex + 
                       race + income + ns(cholesterol, df = 4) + factor(diabetes) +
                       ns(smokeintensity, df = 4) + ns(smokeyrs, df = 4), 
                     data = nhefs, 
                     family = binomial("logit"))$fitted.values

nhefs$sw_qsmk <- (mean(nhefs$qsmk)/nhefs$ps_qsmk)*nhefs$qsmk +
  (mean(1 - nhefs$qsmk)/(1 - nhefs$ps_qsmk))*(1 - nhefs$qsmk)

summary(nhefs$sw_qsmk)

# PS model for map_binary: need to include all confounders of mediator-outcome relation
nhefs$ps_map <- glm(map_binary ~ factor(hbp) + factor(hbpmed) + ns(age, df = 4) + sex + 
                      race + income + + ns(cholesterol, df = 4) + factor(diabetes) +
                      ns(smokeintensity, df = 4) + ns(smokeyrs, df = 4) + 
                      factor(exercise) + wt_delta , 
                     data = nhefs, 
                     family = binomial("logit"))$fitted.values

nhefs$sw_map <- (mean(nhefs$map_binary)/nhefs$ps_map)*nhefs$map_binary +
  (mean(1 - nhefs$map_binary)/(1 - nhefs$ps_map))*(1 - nhefs$map_binary)

summary(nhefs$sw_map)

## let's explore distribution of combined weights
summary(nhefs$sw_qsmk*nhefs$sw_map)


## ----tidy = F, warning = F, message = F---------------------------------------

# unadjusted model: quitting smoking and death
mod0 <- lm(death ~ qsmk, data = nhefs)
coeftest(mod0, vcov. = vcovHC)

# adjusted model: quitting smoking and death
mod1 <- lm(death ~ qsmk, data = nhefs, weights = sw_qsmk)
coeftest(mod1, vcov. = vcovHC)

# unadjusted model: quitting smoking and death under map_binary = 0
mod2 <- lm(death ~ qsmk + map_binary + qsmk:map_binary, data = nhefs)
coeftest(mod2, vcov. = vcovHC)

# adjusted model: quitting smoking and death under map_binary = 0
mod3 <- lm(death ~ qsmk + map_binary + qsmk:map_binary, data = nhefs, weights = sw_qsmk*sw_map)
coeftest(mod3, vcov. = vcovHC)


## ----tidy = F, warning = F, message = F, include = F, echo = F----------------

res1 <- c(coeftest(mod1, vcov. = vcovHC)[2,1],
          coeftest(mod1, vcov. = vcovHC)[2,1] - 1.96*coeftest(mod1, vcov. = vcovHC)[2,2],
          coeftest(mod1, vcov. = vcovHC)[2,1] + 1.96*coeftest(mod1, vcov. = vcovHC)[2,2])

res1

res2 <- c(coeftest(mod3, vcov. = vcovHC)[2,1],
          coeftest(mod3, vcov. = vcovHC)[2,1] - 1.96*coeftest(mod3, vcov. = vcovHC)[2,2],
          coeftest(mod3, vcov. = vcovHC)[2,1] + 1.96*coeftest(mod3, vcov. = vcovHC)[2,2])

res2


## ----tidy = F, warning = F, message = F---------------------------------------

file_loc <- url("https://is.gd/VPKKsi")
nhefs <- read_csv(file_loc) %>% 
  mutate(map = dbp + (sbp - dbp)/3) %>% 
  select(qsmk, map, wt82_71, death, hbp, smokeintensity,
         smokeyrs, hbpmed, age, race, sex, income, 
         exercise, cholesterol, diabetes) %>% 
  na.omit()

mod1 <- lm(qsmk ~ race, data = nhefs)

res1 <- coeftest(mod1, vcov. = vcovHC)

res1


## ----race-dag, out.width="10cm", fig.align='center', fig.cap="Diagram depicting relations between race, quitting smoking, weight change, and a host of variables to be adjusted for.", echo=F----
knitr::include_graphics(here("figures", "race_dag.pdf"))

## ----tidy = F, warning = F, message = F---------------------------------------

nhefs <- nhefs %>% 
  mutate(qsmk = 1 - qsmk)


## ----tidy = F, warning = F, message = F---------------------------------------

# PS model for race: fitting intercept only, which is equivalent to not adjusting for anything
nhefs$ps_race <- glm(race ~ 1, 
                     data = nhefs, 
                     family = binomial("logit"))$fitted.values

nhefs$sw_race <- (mean(nhefs$race)/nhefs$ps_race)*nhefs$race +
  (mean(1 - nhefs$race)/(1 - nhefs$ps_race))*(1 - nhefs$race)

summary(nhefs$sw_race)

# PS model for qsmk: need to include all confounders of mediator-outcome relation
# NB: it does not matter that we switched the coding for this variable
nhefs$ps_qsmk <- glm(qsmk ~ factor(hbp) + factor(hbpmed) + ns(age, df = 4) + sex + 
                      race + income + + ns(cholesterol, df = 4) + factor(diabetes) +
                      ns(smokeintensity, df = 4) + ns(smokeyrs, df = 4) + 
                      factor(exercise), 
                     data = nhefs, 
                     family = binomial("logit"))$fitted.values

nhefs$sw_qsmk <- (mean(nhefs$qsmk)/nhefs$ps_qsmk)*nhefs$qsmk +
  (mean(1 - nhefs$qsmk)/(1 - nhefs$ps_qsmk))*(1 - nhefs$qsmk)

summary(nhefs$sw_qsmk)

## let's explore distribution of combined weights
summary(nhefs$sw_race*nhefs$sw_qsmk)


## ----tidy = F, warning = F, message = F---------------------------------------

# adjusted model: quitting smoking and death
mod1 <- lm(wt82_71 ~ race, data = nhefs)
coeftest(mod1, vcov. = vcovHC)

# adjusted model: quitting smoking and death under map_binary = 0
mod2 <- lm(wt82_71 ~ race + qsmk + race:qsmk, data = nhefs, weights = sw_qsmk)
coeftest(mod2, vcov. = vcovHC)


## ----tidy = F, warning = F, message = F, include = F, echo = F----------------

res1 <- c(coeftest(mod1, vcov. = vcovHC)[2,1],
          coeftest(mod1, vcov. = vcovHC)[2,1] - 1.96*coeftest(mod1, vcov. = vcovHC)[2,2],
          coeftest(mod1, vcov. = vcovHC)[2,1] + 1.96*coeftest(mod1, vcov. = vcovHC)[2,2])

res1

res2 <- c(coeftest(mod2, vcov. = vcovHC)[2,1],
          coeftest(mod2, vcov. = vcovHC)[2,1] - 1.96*coeftest(mod2, vcov. = vcovHC)[2,2],
          coeftest(mod2, vcov. = vcovHC)[2,1] + 1.96*coeftest(mod2, vcov. = vcovHC)[2,2])

res2


