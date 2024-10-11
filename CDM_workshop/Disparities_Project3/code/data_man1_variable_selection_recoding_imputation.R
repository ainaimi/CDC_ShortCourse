pacman::p_load(
  rio,          
  here,         
  skimr,        
  tidyverse,     
  lmtest,
  sandwich,
  broom,
  VIM,
  mice
)

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

### SOME DATA NOTES:
### HISPRACE RECODING TAKEN FROM https://www.cdc.gov/nchs/data/nsfg/App2_RecodeSpecs.pdf
### PREGORDR NOT RECODED. INTEGER VARIABLE INDICATOR ORDER OF CURRENT PREGNANCY
### HPAGELB IS FATHER'S AGE WHEN BABY WAS BORN. RENAMED TO PAGE. https://www.cdc.gov/nchs/data/nsfg/NSFG-2017-2019-FemaleCAPIlite-forPUF-508.pdf
### PRIORSMK IS NUM CIGS SMOKED DURING 6 MO BEFORE FINDING OUT PREGNANT. https://www.cdc.gov/nchs/data/nsfg/NSFG-2017-2019-FemaleCAPIlite-forPUF-508.pdf
### POSTSMKS IS WHETHER SMOKED OR NOT AFTER PREGNANT, RECODE 5 TO 0. https://www.cdc.gov/nchs/data/nsfg/NSFG-2017-2019-FemaleCAPIlite-forPUF-508.pdf
### FMARCON5 IS FORMAL MARITAL STATUS AT CONCEPTION, 5 CATEGORIES: https://www.cdc.gov/nchs/data/nsfg/App2_RecodeSpecs.pdf
### EDUCMOM IS HIGHEST LEVEL OF EDUCATION COMPLETED BY MOTHER: https://www.cdc.gov/nchs/data/nsfg/App2_RecodeSpecs.pdf
### PAYDELIV IS HOW DELIVERY WAS PAID FOR: https://www.cdc.gov/nchs/data/nsfg/App2_RecodeSpecs.pdf
### BFEEDWKS IS DURATION OF BREASTFEEDING IN WEEKS: https://www.cdc.gov/nchs/data/nsfg/App2_RecodeSpecs.pdf
### PNCAREWK THE PREGNANCY WEEK IN WHICH PRENATAL CARE WAS INITIATED. 95 IS NO CARE: https://www.cdc.gov/nchs/data/nsfg/App2_RecodeSpecs.pdf
### MATERNLV IS USE OF MATERNITY LEAVE: https://www.cdc.gov/nchs/data/nsfg/App2_RecodeSpecs.pdf
### WANTRESP IS WANTEDNESS OF PREGNANCY: https://www.cdc.gov/nchs/data/nsfg/App2_RecodeSpecs.pdf
### AGER IS RESPONDENT'S AGE AT INTERVIEW IN YEARS: https://www.cdc.gov/nchs/data/nsfg/App2_RecodeSpecs.pdf


# SELECT KEY VARIABLES FOR CDC WORKSHOP
nsfg <- read.table(here("data", "nsfg_dataimp_01Feb16.txt"),
                sep="\t",
                header = T,
                na.strings=c(".")) %>% 
  select(CASEID, HISPRACE, PREGORDR, HPAGELB, PRIORSMK, 
         POSTSMKS, PNCAREWK, BIRTHORD, FMARCON5, 
         EDUCMOM, PAYDELIV, BFEEDWKS, MATERNLV, WANTRESP, AGER, 
         WGTQ1Q16, SECU, SEST) %>% 
  mutate(
    NH_BLACK = as.numeric(HISPRACE == 3),
    NH_WHITE = as.numeric(HISPRACE == 2),
    HISPANIC = as.numeric(HISPRACE == 1),
    FMARCON5 = factor(FMARCON5, labels = c("Married", 
                                           "Divorced", 
                                           "Widowed", 
                                           "Separated",
                                           "Never Married")),
    EDUCMOM = factor(EDUCMOM, labels = c("Less than High School", 
                                         "High School Graduate", 
                                         "Some College", 
                                         "College or More", 
                                         "No Mother Figure Identified")),
    PAYDELIV = factor(PAYDELIV, labels = c("Insurance", 
                                           "Co-Pay / Out-of-Pocket",
                                           "Medicaid",
                                           "No Payment Required", 
                                           "Some Other Way")),
    MATERNLV = factor(MATERNLV, labels = c("Not Employed During Pregnancy",
                                           "Took Maternity Leave", 
                                           "Not Needed: Job Schedule or Self Employment",
                                           "Did Not Take: Not Permitted",
                                           "Did Not Take: Other")),
    WANTRESP = factor(WANTRESP, labels = c("Later, Overdue",
                                           "Right Time",
                                           "Too soon, mistimed",
                                           "Indifferent",
                                           "Unwanted",
                                           "Don't know, unsure")),
    POSTSMKS = as.numeric(POSTSMKS==1)
    
  ) %>% 
  rename(PAGE = HPAGELB)

names(nsfg)
head(nsfg)

## let's look at missing data:
miss_function <- function(x){
  a <- sum(is.na(x))
  b <- mean(is.na(x))
  return(rbind(a,b))
}

miss_function_output <- data.frame(t(apply(nsfg, 2, miss_function)))

names(miss_function_output) <- c("Missing_Count", "Missing_Proportion")

row_names_missing <- miss_function_output %>% 
  arrange(desc(Missing_Proportion)) %>% 
  filter(Missing_Count>0)

row_names_missing <- row.names(row_names_missing)

missing_output <- lapply(1:length(row_names_missing), function(x)
  table(nsfg[[row_names_missing[x]]], useNA = "always")
)

names(missing_output) <- row_names_missing

missing_output

## what does the distribution of the missing data look like?
row_names_missing
VIM::aggr(nsfg[row_names_missing], cex.axis = .75)

## impute missingness
names(nsfg)
mi_nsfg_ini <- mice(data = nsfg, 
                    m = 1,
                    maxit = 20, 
                    seed = 123)

mi_nsfg_ini$method

mi_nsfg_ini$predictorMatrix

plot(mi_nsfg_ini)

densityplot(mi_nsfg_ini)

nsfg_imputed <- complete(mi_nsfg_ini)

table(nsfg_imputed$HISPRACE)

write_csv(nsfg, here("data", "nsfg_analytic.csv"))
write_csv(nsfg_imputed, here("data", "nsfg_analytic_nm.csv"))
