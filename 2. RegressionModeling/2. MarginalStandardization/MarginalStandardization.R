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

nhefs <- read_csv(here("data","analytic_data.csv"))

nhefs <- nhefs %>% 
  mutate(wt_delta = as.numeric(wt82_71 > median(wt82_71)))

#' Regress the outcome against the confounders with interaction
model_MD <- glm(wt82_71 ~ qsmk + sex + age + race + income + map,
                data = nhefs,
                family = gaussian("identity"))

##' Generate predictions for everyone in the sample to obtain 
##' unexposed (mu0 predictions) and exposed (mu1 predictions) risks.
mu1 <- predict(model_MD,newdata=transform(nhefs,qsmk=1),type="response")
mu0 <- predict(model_MD,newdata=transform(nhefs,qsmk=0),type="response")

#' Marginally adjusted mean difference
marg_stand_MD <- mean(mu1) - mean(mu0)

## ----tidy = F, warning = F, message = F---------------------------------------
#' Using the bootstrap to obtain confidence intervals for the marginally adjusted 
#' risk ratio and risk difference.
bootfunc <- function(data,index){
  
  boot_dat <- data[index,]
  
  model_MD_ <- glm(wt82_71 ~ qsmk + sex + age + race + income + map,
                   data = boot_dat,
                   family = gaussian("identity"))
  
  mu1_ <- predict(model_MD_,newdata=transform(boot_dat,qsmk=1),type="response")
  mu0_ <- predict(model_MD_,newdata=transform(boot_dat,qsmk=0),type="response")
  
  #' Marginally adjusted mean difference
  res <- mean(mu1_) - mean(mu0_)
  return(res)
}

#' Run the boot function. Set a seed to obtain reproducibility
set.seed(123)
boot_res <- boot(nhefs,bootfunc,R=2000)

boot_MD <- boot.ci(boot_res)

marg_stand_MD

boot_MD

## ----tidy = F, warning = F, message = F---------------------------------------

#' Regress the outcome against the confounders with interaction
model_OR <- glm(wt_delta ~ qsmk + sex + age + race + income + map,
                data = nhefs,
                family = binomial("logit"))

##' Generate predictions for everyone in the sample to obtain 
##' unexposed (mu0 predictions) and exposed (mu1 predictions) risks.
mu1 <- predict(model_OR,newdata=transform(nhefs,qsmk=1),type="response")
mu0 <- predict(model_OR,newdata=transform(nhefs,qsmk=0),type="response")

#' Marginally adjusted odds ratio
marg_stand_OR <- (mean(mu1)/mean(1-mu1))/(mean(mu0)/mean(1-mu0))
#' Marginally adjusted risk ratio
marg_stand_RR <- mean(mu1)/mean(mu0)
#' Marginally adjusted risk difference
marg_stand_RD <- mean(mu1)-mean(mu0)

#' Using the bootstrap to obtain confidence intervals for the marginally adjusted 
#' risk ratio and risk difference.
bootfunc <- function(data,index){

  boot_dat <- data[index,]
  
  model_OR_ <- glm(wt_delta ~ qsmk + sex + age + race + income + map,
                data = boot_dat,
                family = binomial("logit"))
  
  mu1 <- predict(model_OR_,newdata=transform(boot_dat,qsmk=1),type="response")
  mu0 <- predict(model_OR_,newdata=transform(boot_dat,qsmk=0),type="response")
  
  marg_stand_OR_ <- (mean(mu1)/mean(1-mu1))/(mean(mu0)/mean(1-mu0))
  marg_stand_RR_ <- mean(mu1)/mean(mu0)
  marg_stand_RD_ <- mean(mu1)-mean(mu0)
  
  res <- c(marg_stand_RD_,marg_stand_RR_,marg_stand_OR_)
  
  return(res)
}

#' Run the boot function. Set a seed to obtain reproducibility
set.seed(123)
boot_res <- boot(nhefs,bootfunc,R=2000)

boot_RD <- boot.ci(boot_res,index=1)
boot_RR <- boot.ci(boot_res,index=2)
boot_OR <- boot.ci(boot_res,index=3)

marg_stand_OR
marg_stand_RR
marg_stand_RD

boot_RD
boot_RR
boot_OR


## ----tidy = F, warning = F, message = F---------------------------------------

#' Marginal Standardization
##' To avoid assuming no interaction between 
##' quitting smoking and any of the other variables
##' in the model, we subset modeling among 
##' exposed/unexposed. This code removes qsmk from the model,
##' which will allow us to regress the outcome 
##' against the confounders among the exposed and 
##' the unexposed separately. Doing so will allow us 
##' to account for any potential exposure-covariate interactions
##' that may be present. 

#' Regress the outcome against the confounders 
#' among the unexposed (model0) and then among the exposed (model1)
model0 <- glm(wt_delta ~ sex + age + race + income + map,
              data=subset(nhefs,qsmk==0),
              family=binomial("logit"))
model1 <- glm(wt_delta ~ sex + age + race + income + map,
              data=subset(nhefs,qsmk==1),
              family=binomial("logit"))

##' Generate predictions for everyone in the sample using the model fit to only the 
##' unexposed (mu0 predictions) and only the exposed (mu1 predictions).
mu1 <- predict(model1,newdata=nhefs,type="response")
mu0 <- predict(model0,newdata=nhefs,type="response")

#' Marginally adjusted odds ratio
marg_stand_OR <- (mean(mu1)/mean(1-mu1))/(mean(mu0)/mean(1-mu0))
#' Marginally adjusted risk ratio
marg_stand_RR <- mean(mu1)/mean(mu0)
#' Marginally adjusted risk difference
marg_stand_RD <- mean(mu1)-mean(mu0)

#' Using the bootstrap to obtain confidence intervals for the marginally adjusted 
#' risk ratio and risk difference.
bootfunc <- function(data,index){
  boot_dat <- data[index,]
  model0 <- glm(wt_delta ~ sex + age + race + income + map,
                data=subset(boot_dat,qsmk==0),
                family=binomial("logit"))
  model1 <- glm(wt_delta ~ sex + age + race + income + map,
                data=subset(boot_dat,qsmk==1),
                family=binomial("logit"))
  
  mu1 <- predict(model1,newdata=boot_dat,type="response")
  mu0 <- predict(model0,newdata=boot_dat,type="response")
  
  marg_stand_OR_ <- (mean(mu1)/mean(1-mu1))/(mean(mu0)/mean(1-mu0))
  marg_stand_RR_ <- mean(mu1)/mean(mu0)
  marg_stand_RD_ <- mean(mu1)-mean(mu0)
  res <- c(marg_stand_RD_,marg_stand_RR_,marg_stand_OR_)
  return(res)
}

#' Run the boot function. Set a seed to obtain reproducibility
set.seed(123)
boot_res <- boot(nhefs,bootfunc,R=2000)

boot_RD <- boot.ci(boot_res,index=1)
boot_RR <- boot.ci(boot_res,index=2)
boot_OR <- boot.ci(boot_res,index=3)

marg_stand_OR
marg_stand_RR
marg_stand_RD

boot_RD
boot_RR
boot_OR


