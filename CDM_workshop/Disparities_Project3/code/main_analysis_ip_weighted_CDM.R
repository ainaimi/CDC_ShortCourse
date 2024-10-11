pacman::p_load(
  rio,          
  here,         
  skimr,        
  tidyverse,     
  lmtest,
  sandwich,
  broom,
  VGAM,
  survey
)


options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="average")

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

#### The goal of this analysis is to estimate a quantity that corresponds to 
#### the racial disparity in breastfeeding that would be observed if all women
#### accessed early prenatal care.
#### We will carry out this analysis in a few steps:
#### 1) exploratory and associational:
####    1a.  what do the data look like?
####    1b.  is race/ethnicity associated with prenatal care?
####    1c.  is race/ethnicity associated with breastfeeding? 
####    1d.  is prenatal care associated with breastfeeding?

nsfg0 <- read_csv(here("data", "nsfg_analytic.csv"))
nsfg <- read_csv(here("data", "nsfg_analytic_nm.csv"))

table(nsfg$HISPRACE)

#### let's first construct the design object using NSFG sampling frame:

nsfg_design <- 
  svydesign(
    id = ~ SECU,
    strata = ~ SEST,
    data = nsfg,
    weights = ~ WGTQ1Q16,
    nest = TRUE 
  )

####    1a.  what do the data look like?

names(nsfg)

head(nsfg)

skimr::skim(nsfg)

#### Let's take a close look at race/ethnicity

table(nsfg$HISPRACE)

## basic associations: race:

table(nsfg$PRIORSMK)
table(nsfg$POSTSMKS)
table(nsfg$PAGE)
table(nsfg$PNCAREWK)
table(nsfg$BIRTHORD)

m_priorsmk <- svyglm(PRIORSMK ~ HISPRACE, 
                     design = nsfg_design, 
                     family = poisson("log"))
summary(m_priorsmk)

m_postsmks <- svyglm(POSTSMKS ~ HISPRACE, 
                     design = nsfg_design, 
                     family = binomial("logit"))
summary(m_postsmks)

m_page <- svyglm(PAGE ~ HISPRACE, 
                 design = nsfg_design, 
                 family = gaussian("identity"))
summary(m_page)

m_pnc <- svyglm(PNCAREWK ~ HISPRACE, 
                design = nsfg_design,
                family = gaussian("identity"))
summary(m_pnc)

m_birthorder <- svyglm(BIRTHORD ~ HISPRACE, 
                design = nsfg_design,
                family = poisson("log"))
summary(m_birthorder)

## basic associations: prenatal care:
## note the `95` value is "no prenatal care"
## let's convert this to a binary variable 
## with a cutpoint of 10 weeks

table(nsfg$PNCAREWK)

nsfg <- nsfg %>% 
  mutate(prenatal_care = as.numeric(PNCAREWK<=10))

table(nsfg$PNCAREWK, nsfg$prenatal_care)

nsfg_design <- update(nsfg_design, 
                      prenatal_care = as.numeric(PNCAREWK<=10))

svytable( ~ prenatal_care, 
          design = nsfg_design)

m_prenatal <- svyglm(prenatal_care ~ NH_WHITE + HISPANIC,
                     design = nsfg_design,
                     family = binomial("identity"))
summary(m_prenatal)

## basic associations: breastfeeding:
## we can convert to any versus none 

table(nsfg$BFEEDWKS)

nsfg <- nsfg %>% 
  mutate(breastfeeding = as.numeric(BFEEDWKS > 0))

nsfg_design <- update(nsfg_design, 
                      breastfeeding = as.numeric(BFEEDWKS > 0))

m_bfeed_race <- svyglm(breastfeeding ~ NH_WHITE + HISPANIC,
                       design = nsfg_design,
                       family = binomial("identity"))
summary(m_bfeed_race)

m_bfeed_prenatal <- svyglm(breastfeeding ~ prenatal_care,
                           design = nsfg_design,
                           family = binomial("identity"))
summary(m_bfeed_prenatal)

#######################################################################
#######################################################################
################### PHASE 2: ESTIMATING CDM ###########################
#######################################################################
#######################################################################

#### We'll use inverse probability weighting to estimate the CDM
#### We need a model for the mediator, in this case prenatal care

## mediator model
## we include all confounders of the mediator-outcome relation
## it does not matter here if affected by exposure
mediator_model <- svyglm(prenatal_care ~ NH_WHITE + HISPANIC + PRIORSMK + POSTSMKS +
                           PAGE + PREGORDR + BIRTHORD + FMARCON5 + EDUCMOM + PAYDELIV +
                           MATERNLV + WANTRESP + AGER,
                         design = nsfg_design,
                         family = binomial("logit"))
summary(mediator_model)

nsfg_design <- update(nsfg_design, 
                      propensity_score = mediator_model$fitted.values)
nsfg <- nsfg %>% 
  mutate(propensity_score = mediator_model$fitted.values)

# ps overlap plot
nsfg %>%
  group_by(prenatal_care) %>%
  mutate(WGTQ1Q16_std = WGTQ1Q16/sum(WGTQ1Q16)) %>%
  ggplot(mapping = aes(x = propensity_score, 
                       group = factor(prenatal_care),
                       color = factor(prenatal_care),
                       weight = WGTQ1Q16_std)) + 
  geom_density(bw = 0.06) +
  labs(x = "Propensity Score", y = "Density")

numerator_model <- svyglm(prenatal_care ~ 1,
                           design = nsfg_design,
                           family = binomial("logit"))

nsfg <- nsfg %>% 
  mutate(numerator = numerator_model$fitted.values,
         sw = (numerator/propensity_score)*prenatal_care +
           ((1 - numerator)/(1 - propensity_score))*(1 - prenatal_care))

nsfg_design <- update(nsfg_design, 
                      numerator = numerator_model$fitted.values,
                      sw = (numerator/propensity_score)*prenatal_care +
                        ((1 - numerator)/(1 - propensity_score))*(1 - prenatal_care))

svymean(~ sw, nsfg_design)
svyquantile(~ sw, design = nsfg_design, c(.9,.95,1))

#### fit a weighted outcome model to compute the CDM

nsfg_design_updated <- 
  svydesign(
    id = ~ SECU,
    strata = ~ SEST,
    data = nsfg,
    weights = ~ WGTQ1Q16*sw,
    nest = TRUE 
  )

cdm_model <- svyglm(breastfeeding ~ prenatal_care + NH_WHITE + HISPANIC + prenatal_care:NH_WHITE + prenatal_care:HISPANIC,
                         design = nsfg_design_updated,
                         family = binomial("identity"))
cdm_estimate <- summary(cdm_model)$coefficients["NH_WHITE",1]

overall_model <- svyglm(breastfeeding ~ NH_WHITE + HISPANIC,
                        design = nsfg_design,
                        family = binomial("identity"))
overall_estimate <- summary(overall_model)$coefficients["NH_WHITE",1]

#######################################################################
#######################################################################
################### PHASE 3: DEPLOYING BOOTSTRAP ######################
#######################################################################
#######################################################################

set.seed(123)

boot_func <- function(x){
  print(x)

  # resample clusters from nsfg
  clusters <- as.numeric(names(table(nsfg$SECU)))
  index <- sample(1:length(clusters), length(clusters), replace=TRUE)
  bb <- table(clusters[index])
  boot <- NULL
  for(counter in 1:max(bb)){
    cc <- nsfg[nsfg$SECU %in% names(bb[bb %in% c(counter:max(bb))]),]
    cc$bSECU <- as.numeric(paste0(cc$SECU,counter))
    boot <- rbind(boot, cc)
  }
  
  #boot %>% select(bSECU, SECU)
  
  # construct the survey design object
  nsfg_design_boot <- 
    svydesign(
      id = ~ bSECU,
      strata = ~ SEST,
      data = boot,
      weights = ~ WGTQ1Q16,
      nest = TRUE 
    )
  
  # fit propensity score model
  mediator_model <- svyglm(prenatal_care ~ NH_WHITE + HISPANIC + PRIORSMK + POSTSMKS +
                             PAGE + PREGORDR + BIRTHORD + FMARCON5 + EDUCMOM + PAYDELIV +
                             MATERNLV + WANTRESP + AGER,
                           design = nsfg_design_boot,
                           family = binomial("logit"))
  
  nsfg_design_boot <- update(nsfg_design_boot, 
                             propensity_score = mediator_model$fitted.values)
  boot <- boot %>% 
    mutate(propensity_score = mediator_model$fitted.values)
  
  # fit numerator model
  numerator_model <- svyglm(prenatal_care ~ 1,
                            design = nsfg_design_boot,
                            family = binomial("logit"))
  
  boot <- boot %>% 
    mutate(numerator = numerator_model$fitted.values,
           sw = (numerator/propensity_score)*prenatal_care +
             ((1 - numerator)/(1 - propensity_score))*(1 - prenatal_care))
  
  nsfg_design_boot <- update(nsfg_design_boot, 
                             numerator = numerator_model$fitted.values,
                             sw = (numerator/propensity_score)*prenatal_care +
                               ((1 - numerator)/(1 - propensity_score))*(1 - prenatal_care))
  
  boot <- boot %>% 
    mutate(numerator = numerator_model$fitted.values,
           sw = (numerator/propensity_score)*prenatal_care +
             ((1 - numerator)/(1 - propensity_score))*(1 - prenatal_care))
  
  nsfg_design_boot <- update(nsfg_design_boot, 
                             numerator = numerator_model$fitted.values,
                             sw = (numerator/propensity_score)*prenatal_care +
                               ((1 - numerator)/(1 - propensity_score))*(1 - prenatal_care))
  
  nsfg_design_boot_updated <- 
    svydesign(
      id = ~ bSECU,
      strata = ~ SEST,
      data = boot,
      weights = ~ WGTQ1Q16*sw,
      nest = TRUE 
    )
  
  cdm_model <- svyglm(breastfeeding ~ prenatal_care + NH_WHITE + HISPANIC + prenatal_care:NH_WHITE + prenatal_care:HISPANIC,
                      design = nsfg_design_boot_updated,
                      family = binomial("identity"))
  
  res1 <- summary(cdm_model)$coefficients["NH_WHITE",1]
  
  overall_model <- svyglm(breastfeeding ~ NH_WHITE + HISPANIC,
                          design = nsfg_design_boot_updated,
                          family = binomial("identity"))
  
  res2 <- summary(overall_model)$coefficients["NH_WHITE",1]
  
  return(cbind(res1,res2))
    
}

boot_res <- lapply(1:1000, function(x) boot_func(x))
boot_res <- do.call(rbind, boot_res)

cdm_se     <- sd(boot_res[,1])
overall_se <- sd(boot_res[,2])









