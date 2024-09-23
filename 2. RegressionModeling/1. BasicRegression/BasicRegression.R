## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=40),tidy=TRUE)



remotes::install_github("rstudio/fontawesome")

library(fontawesome)

knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----tidy = F, warning = F, message = F---------------------------------------

pacman::p_load(tidyverse,
               here,
               broom,
               boot)


## ----include = F--------------------------------------------------------------

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)


## ----tidy = F, warning = F, message = F---------------------------------------

nhefs <- read_csv(here("data","analytic_data.csv"))

dim(nhefs)

nhefs %>% print(n=5)


## ----tidy = F, warning = F, message = F---------------------------------------
#' Here, we start fitting relevant regression models to the data.

#' This model can be used to quantify a conditionally adjusted 
#' mean difference with correct standard error
model_MD <- glm(wt82_71 ~ qsmk + sex + age + race + income + map,
                data = nhefs,
                family = gaussian("identity"))

#' summary() is a base R function that reports the fit of a
#'  regression model neatly in the console
summary(model_MD)

#' tidy() is a broom function that output the fit of a 
#' regression model as a tidy dataset
tidy(model_MD)

## ----tidy = F, warning = F, message = F---------------------------------------

#' interaction: method 1 -- colon includes only interaction
model_MD <- glm(wt82_71 ~ qsmk + sex + age + sex:age + race + income + map,
                data = nhefs,
                family = gaussian("identity"))

summary(model_MD)$coefficients

#' interaction: method 2 -- asterisk includes main effect and interaction
model_MD <- glm(wt82_71 ~ qsmk + sex*age + race + income + map,
                data = nhefs,
                family = gaussian("identity"))

summary(model_MD)$coefficients


## ----tidy = F,out.width = "4cm",fig.align='center',fig.cap="An illustration of a engineering/architectural spline use to draw flexible curves for blueprint diagrams. Source: Wikipedia",echo=F,message=F,warning=F----
  knitr::include_graphics(here("figures", "spline.png"))

## ----tidy = F, warning = F, message = F---------------------------------------

pacman::p_load(splines)

?ns


## ----tidy = F,out.width = "15cm",fig.align='center',echo=F,message=F,warning=F----
  knitr::include_graphics(here("figures", "spline_help.png"))

## ----tidy = F, warning = F, message = F---------------------------------------
#' splines using the ns() function
model_MD <- glm(wt82_71 ~ qsmk + sex*age + race + income + ns(map, df = 4),
                data = nhefs,
                family = gaussian("identity"))

summary(model_MD)$coefficients


## ----tidy = F, warning = F, message = F---------------------------------------

mean_difference1 <- tidy(model_MD)[2,]

mean_difference1


## ----tidy = F, warning = F, message = F---------------------------------------

mean_difference1_LCL <- mean_difference1$estimate - 1.96*mean_difference1$std.error
mean_difference1_UCL <- mean_difference1$estimate + 1.96*mean_difference1$std.error

round(c(mean_difference1_LCL, mean_difference1_UCL), 2)


## ----tidy = F, warning = F, message = F---------------------------------------

nhefs <- nhefs %>% 
  mutate(wt_delta = as.numeric(wt82_71 > median(wt82_71)))

#' This model can be used to quantify a conditionally adjusted OR
#' logit model for the OR with correct standard error
#' including an interaction and spline
model_OR <- glm(wt_delta ~ qsmk + sex*age + race + income + ns(map, df=4),
                data = nhefs,
                family = binomial("logit"))

#' summary() again
summary(model_OR)

#' tidy() again
tidy(model_OR)


## ----tidy = F, warning = F, message = F---------------------------------------

qsmk_log_OR <- tidy(model_OR)[2,]

qsmk_OR <- exp(qsmk_log_OR[1,2])

qsmk_OR

qsmk_OR_LCL <- exp(qsmk_log_OR$estimate - 1.96*qsmk_log_OR$std.error)
qsmk_OR_UCL <- exp(qsmk_log_OR$estimate + 1.96*qsmk_log_OR$std.error)

round(c(qsmk_OR$estimate, qsmk_OR_LCL, qsmk_OR_UCL),2)


## ----echo = F-----------------------------------------------------------------
table2_data <- tibble(`Odds Ratio` = c("GLM Family = Binomial", "GLM Link = Logistic", "Standard Errors = Model Based", " ", " ", " ", " ", " "), 
                      `Risk Ratio` = c("GLM Family = Binomial", "GLM Link = Log", "Standard Errors = Model Based", "GLM Family = Poisson", "GLM Link = Log", "Standard Errors = Sandwich", " ", " "),
                      `Risk Difference` = c("GLM Family = Binomial", "GLM Link = Identity", "Standard Errors = Model Based", "GLM Family = Gaussian", "GLM Link = Identity", "Standard Errors = Sandwich", "Least Squares Regression", "Standard Errors = Sandwich"))

## ----echo=F-------------------------------------------------------------------
knitr::kable(table2_data, "simple", caption = "Methods to use for quantifying conditionally adjusted odds ratios, risk ratios, and risk differences.")

## ----tidy = F, warning = F, message = F---------------------------------------

pacman::p_load(broom,here,tidyverse,rlang,lmtest,sandwich,boot,kableExtra)


## ----tidy = F, warning = F, message = F---------------------------------------

mod <- lm(wt_delta ~ qsmk + sex + qsmk*sex + race + income,
          data = nhefs)

summary(mod)

# we'll need robust SEs:
mod_res <- coeftest(mod, vcov = vcovHC(mod, type = "HC3"))

mod_res


## ----tidy = F, warning = F, message = F---------------------------------------

#' extract point estimate for sex = 0
rd_sex0_m1 <- mod_res[2,1]

#' extract point estimates for sex = 1
rd_sex1_m1 <- mod_res[2,1] + mod_res[6,1]


## ----tidy = F, warning = F, message = F---------------------------------------

#' extract point estimate for sex = 0
rd_sex0_m2 <- mod_res["qsmk",1]

#' extract point estimates for sex = 1
rd_sex1_m2 <- mod_res["qsmk",1] + mod_res["qsmk:sex",1]


## -----------------------------------------------------------------------------

contrast1 <- c(0,1,0,0,0,0)

contrast2 <- c(0,1,0,0,0,1)

rd_sex0_m3 <- coef(mod_res)%*%contrast1

rd_sex1_m3 <- coef(mod_res)%*%contrast2


## ----tidy = F, warning = F, message = F, echo=F-------------------------------
knitr::kable(caption = "Comparison of risk differences from all three methods.",
  rbind(c("Method 1", rd_sex0_m1, rd_sex1_m1),
        c("Method 2", rd_sex0_m2, rd_sex1_m2),
        c("Method 3", rd_sex0_m3, rd_sex1_m3)),
  "simple"
)

## ----tidy = F, warning = F, message = F---------------------------------------

var_rd_sex0_m1 <- mod_res[2,2]

var_rd_sex0_m2 <- mod_res["qsmk",2]


## ----tidy = F, warning = F, message = F---------------------------------------

round(vcovHC(mod, type = "HC3"),4)

sqrt(vcovHC(mod, type = "HC3")[2,2]) == mod_res["qsmk",2]


## ----tidy = F, warning = F, message = F---------------------------------------

var_rd_sex0_m3 <- sqrt(contrast1%*%vcovHC(mod, type = "HC3")%*%contrast1)


## ----tidy = F, warning = F, message = F---------------------------------------

round(vcovHC(mod, type = "HC3"),4)


## ----tidy = F, warning = F, message = F---------------------------------------

var_rd_sex1_m2 <- sqrt(vcovHC(mod, type = "HC3")[2,2] + 
                         vcovHC(mod, type = "HC3")[6,6] + 
                         2*vcovHC(mod, type = "HC3")[2,6])


## ----tidy = F, warning = F, message = F---------------------------------------

var_rd_sex1_m3 <- sqrt(contrast2%*%vcovHC(mod, type = "HC3")%*%contrast2)


## ----tidy = F, warning = F, message = F, echo=F-------------------------------
knitr::kable(caption = "Comparison of standard errors from all three methods.",
  rbind(c("Method 1", var_rd_sex0_m1, "-"),
        c("Method 2", var_rd_sex0_m2, var_rd_sex1_m2),
        c("Method 3", var_rd_sex0_m3, var_rd_sex1_m3)),
  "simple"
)

## ----tidy = F, warning = F, message = F, echo=T-------------------------------
knitr::kable(caption = "Risk differences and 95% CIs using standard errors obtained from method 3.",
  rbind(c("Stratum Sex = 0", round(rd_sex0_m3, 3), 
                             round(rd_sex0_m3 - 1.96*var_rd_sex0_m3, 3), 
                             round(rd_sex0_m3 + 1.96*var_rd_sex0_m3, 3)),
        c("Stratum Sex = 1", round(rd_sex1_m3, 3), 
                             round(rd_sex1_m3 - 1.96*var_rd_sex1_m3, 3), 
                             round(rd_sex1_m3 + 1.96*var_rd_sex1_m3, 3))),
  "simple"
)

