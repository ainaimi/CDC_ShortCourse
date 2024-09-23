## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=40),tidy=TRUE)

packages <- c( "data.table","tidyverse","ggplot2","ggExtra","formatR",
               "gridExtra","skimr","here","Hmisc","RColorBrewer")

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

## ----figure1, out.width="10cm", fig.align='center', fig.margin=FALSE, echo=F, fig.cap="Directed acyclic graph depicting confounder adjustment using an outcome modelling approach. In this approach, information from the confounder to the outcome is 'blocked' (blue arrow). With this adjustment, the exposure $X$ os $d$-separated from the outcome $Y$, rendering the estimate of the exposure-outcome association unconfounded."----
knitr::include_graphics(here("figures","2022_03_21-Section3_Figure1.pdf"))

## ----echo = F, results = 'asis'-----------------------------------------------
library(knitr)
conf <- data.frame(C1 = c(0,1,0,1), 
                   C2 = c(0,0,1,1))

conf$`$P(X = 1 \\mid C)$` <- c('$\\expit(\\alpha_0)$', '$\\expit(\\alpha_0 + \\alpha_1)$', '$\\expit(\\alpha_0 + \\alpha_2)$', '$\\expit(\\alpha_0 + \\alpha_1 + \\alpha_2 + \\alpha_3)$')

kable(conf, "pipe")

## ----figure2, out.width="10cm", fig.align='center', fig.margin=FALSE, echo=F, fig.cap="Directed acyclic graph depicting confounder adjustment using an outcome modelling approach. In this approach, information from the confounder to the outcome is 'blocked' (blue arrow). With this adjustment, the exposure $X$ os $d$-separated from the outcome $Y$, rendering the estimate of the exposure-outcome association unconfounded."----
knitr::include_graphics(here("figures","2022_03_21-Section3_Figure2.pdf"))

## ----figure3, out.width="10cm", fig.align='center', fig.margin=FALSE, echo=F, fig.cap="Directed acyclic graph depicting the causal relations between variables in a 'pseudo-population' obtained via inverse probability weighting. Again, with this adjustment, the exposure $X$ os $d$-separated from the outcome $Y$, rendering the estimate of the exposure-outcome association unconfounded."----
knitr::include_graphics(here("figures","2022_03_21-Section3_Figure3.pdf"))

## -----------------------------------------------------------------------------

c <- c(0,0,1,1)
x <- c(1,0,1,0)
n <- c(2,8,9,1)

tibble(x,c,n)


## ----warning = F, message = F-------------------------------------------------

file_loc <- url("https://bit.ly/47ECRcs")

#' This begins the process of cleaning and formatting the data
nhefs <- read_csv(file_loc) %>%
  select(qsmk,wt82_71,sex,age,exercise,
         race,income, marital,school,
         asthma,bronch,
         starts_with("alcohol"),-alcoholpy,
         starts_with("price"),
         starts_with("tax"),
         starts_with("smoke"),
         smkintensity82_71) %>%
  mutate(wt_delta = as.numeric(wt82_71>median(wt82_71, na.rm = T)),
         income=as.numeric(income>15),
         marital=as.numeric(marital>2),
         alcoholfreq=as.numeric(alcoholfreq>1)) %>%
  na.omit(.)

nhefs$id <- 1:nrow(nhefs)

# create the propensity score in the dataset
nhefs$propensity_score <- glm(qsmk ~ exercise + sex + age + race + income + marital + school + asthma + bronch + alcoholfreq, 
                              data = nhefs, 
                              family = binomial("logit"))$fitted.values

# stabilized inverse probability weights
nhefs$sw <- (mean(nhefs$qsmk)/nhefs$propensity_score)*nhefs$qsmk + 
  ((1-mean(nhefs$qsmk))/(1-nhefs$propensity_score))*(1-nhefs$qsmk)

summary(nhefs$sw)

nhefs %>% select(id, wt_delta, qsmk, sex, age, exercise, propensity_score, sw) %>% print(n = 5)


## -----------------------------------------------------------------------------

model_RD_weighted <- glm(wt_delta ~ qsmk, data = nhefs, weights=sw, family = quasibinomial("identity"))

summary(model_RD_weighted)$coefficients


## -----------------------------------------------------------------------------

nhefs$sw_norm <- nhefs$sw/max(nhefs$sw)

summary(nhefs$sw_norm)

nhefs %>% select(id, wt_delta, qsmk, sex, age, exercise, propensity_score, sw, sw_norm) %>% print(n = 5)


## -----------------------------------------------------------------------------

model_RD_weighted_norm <- glm(wt_delta ~ qsmk, data = nhefs, weights=sw_norm, family = quasibinomial("identity"))

summary(model_RD_weighted_norm)$coefficients


## -----------------------------------------------------------------------------

quantile(nhefs$sw, .99)

nhefs <- nhefs %>% mutate(sw_trim = if_else(sw > quantile(sw, .99),
                                            quantile(sw, .99),
                                            sw))

summary(nhefs$sw)

summary(nhefs$sw_trim)


## -----------------------------------------------------------------------------

ggplot(nhefs) + 
  geom_jitter(aes(sw, sw_trim)) + 
  scale_x_continuous(limits = c(0,3.5)) +
  scale_y_continuous(limits = c(0,3.5))


## -----------------------------------------------------------------------------

library(lmtest)
library(sandwich)

coeftest(model_RD_weighted_norm, vcov. = vcovHC)

## ----tidy = F, warning = F, message = F---------------------------------------

library(VGAM)

ps_mod <- vglm(factor(exercise) ~ sex + age + race + income + school, 
               data = nhefs, 
               family = "multinomial")

summary(ps_mod)


## ----tidy = F, warning = F, message = F---------------------------------------

ps_matrix <- cbind(predict(ps_mod, type = "response"), nhefs$exercise)

ps_matrix <- data.frame(ps_matrix)

names(ps_matrix) <- c("pEx0","pEx1","pEx2","Observed_Exercise")

head(ps_matrix)


## -----------------------------------------------------------------------------

summary(ps_matrix[,1])

summary(ps_matrix[,2])

summary(ps_matrix[,3])


## ----tidy = F, warning = F, message = F, fig.show = "hide"--------------------

plot_dat1 <- data.frame(ps_matrix[,c(1,4)], pEx = "pEx0")
plot_dat2 <- data.frame(ps_matrix[,c(2,4)], pEx = "pEx1")
plot_dat3 <- data.frame(ps_matrix[,c(3,4)], pEx = "pEx2")

names(plot_dat1) <- names(plot_dat2) <- names(plot_dat3) <- c("propensity_score", 
                                                              "exposure_level", 
                                                              "pEx")

plot_dat <- rbind(plot_dat1,
                  plot_dat2,
                  plot_dat3)

dim(plot_dat)
dim(ps_matrix)

head(ps_matrix, 10)

head(plot_dat, 10)

ggplot(plot_dat) +
  geom_density(aes(x = propensity_score,
                   group = factor(exposure_level),
                   fill  = factor(exposure_level)),
               bw = .03, alpha = .25) +
  facet_wrap(~ pEx, ncol = 1) +
  scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
  scale_y_continuous(expand = c(0,0))

ggsave(here("figures", "ps_overlap_3level.pdf"), 
       width = 10, 
       height = 16,
       units = "cm")


## ----psthree, out.width="10cm", fig.align='center', fig.margin=FALSE, echo=F, fig.cap="Propensity score overlap for exercise, a three level exposure in the NHEFS data."----
knitr::include_graphics(here("figures","ps_overlap_3level.pdf"))

## ----tidy = F, message = F, warning = F---------------------------------------

head(ps_matrix)

pscore_obs <- NULL
for(i in 1:nrow(ps_matrix)){
  pscore_obs <- rbind(pscore_obs, 
                  ps_matrix[i, ps_matrix[i,]$Observed_Exercise + 1] # note the "+ 1"
                  )
}

head(pscore_obs)


## ----tidy = F, warning = F, message = F---------------------------------------

ps_model <- vglm(factor(exercise) ~ 1, 
               data = nhefs, 
               family = "multinomial")

ps_num <- cbind(predict(ps_model, type = "response"),  nhefs$exercise)

ps_num <- data.frame(ps_num)

names(ps_num) <- c("p0", "p1", "p2", "exercise_level")

head(ps_num)

pscore_num <- NULL
for(i in 1:nrow(ps_num)){
  pscore_num <- rbind(pscore_num,
                      ps_num[i, ps_num[i,]$exercise_level + 1]
  )
}

nhefs$sw_exercise <- pscore_num/pscore_obs

summary(nhefs$sw_exercise)


## ----tidy = F, warning = F, message = F---------------------------------------

modelRD_ex <- lm(wt_delta ~ relevel(factor(exercise), 
                                    ref = "2"), 
                 weight = sw_exercise, 
                 data = nhefs)

coeftest(modelRD_ex, .vcov = vcovHC)


