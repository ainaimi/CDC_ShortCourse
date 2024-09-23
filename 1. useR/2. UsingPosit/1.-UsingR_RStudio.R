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

## -----------------------------------------------------------------------------
2*2*2

## ----eval=T,warning=F,results='hide'------------------------------------------
2 + 2 # add numbers
2 * pi # multiply by a constant
3^4 # powers
runif(5) # random number generation
sqrt(4^2) # functions
log(10) # natural log (i.e., base e)
log(100, base = 10) # log base 10
23 %/% 2 # integer division
23 %% 2 # modulus operator

# scientific notation
5000000000 * 1000
5e9 * 1e3

## -----------------------------------------------------------------------------

a <- 10
a 
a/100
a+10

# R is case sensitive!!!
A <- 15
print(c(a,A))

## -----------------------------------------------------------------------------

`<-`(x, 3)  # Prefix notation 
x <- 3      # Leftwards assignment
3 -> x      # Rightwards assignment
x = 3       # Equal sign


## ----tidy = F, warning = F, message = F, error = T----------------------------

my.test.function <- function(argument = 100){
  if(argument == 100){
    print("Hello!")
  } else{
    print("Goodbye!")
  }
}

system.time(result = my.test.function(100))
system.time(result <- my.test.function(100))

## -----------------------------------------------------------------------------
## Basic functional unit in R is a vector:
# numeric vector
nums <- c(1.1, 3, -5.7)
nums

nums <- rep(nums,2)
nums

# integer vector
ints <- c(1L, 5L, -3L) # force storage as integer not decimal number
# 'L' is for 'long integer' (historical)

# sample nums with replacement
new_nums <- sample(nums,8,replace = TRUE)
new_nums

# logical (i.e., Boolean) vector
bools <- c(TRUE, FALSE, TRUE, FALSE, T, T, F, F)
bools
# character vector
chars <- c("epidemiology is", "the study", 
           "of the", "distribution", 
           "and determinants", "of disease", 
           "in", "a population")
chars

## -----------------------------------------------------------------------------
A <- data.frame(new_nums,bools,chars)
A

## ----tidy = F, warning = F, message = F, error = T----------------------------

new_bools <- bools[-1]

bools
new_bools

B <- data.frame(new_nums,new_bools,chars)
B

## ----tidy = F, warning = F, message = F---------------------------------------
basic_list <- list(rep(1:3,5),
                   "what do you think of R so far?",
                   A)
basic_list[[1]]
basic_list[[2]]
head(basic_list[[3]])

## ----tidy = F, warning = F, message = F---------------------------------------

vals <- seq(2, 12, by = 2)
vals

vals[3]
vals[3:5]
vals[c(1, 3, 6)]
vals[-c(1, 3, 6)]
vals[c(rep(TRUE, 3), rep(FALSE, 2), TRUE)]


## ----tidy = F, warning = F, message = F, error=T------------------------------

A[3,] # first position subsets rows

A[,3] # second position subsets columns

A[2:3,] # subset to a sequence of rows

A[,2:3] # subset to a sequence of rows

## a different way to subset, using the `subset` function
subset(A,
       bools==F,
       select = -bools)

## yet another way to subset columns
A$bools


## -----------------------------------------------------------------------------
# HELP!
?median

help.search('linear regression')

help(package='ggplot2')

## ----tidy = F, warning = F, message = F---------------------------------------
names(iris)

## ----tidy = F, warning = F, message = F---------------------------------------
str(iris)

## -----------------------------------------------------------------------------
class(iris)

## ----tidy = F, warning = F, message = F---------------------------------------

library(ranger)

random_forest_iris <- ranger(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, 
                             data = iris,
                             num.trees = 1000, 
                             mtry = 2, 
                             min.node.size = 20,
                             importance = "permutation")


## ----tidy = F, warning = F, message = F---------------------------------------

names(random_forest_iris)


## ----tidy = F, warning = F, message = F---------------------------------------

random_forest_iris$variable.importance


## ----tidy = F, warning = F, message = F---------------------------------------

## linear model fit using OLS
linear_model_iris <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, 
                        data = iris)


## ----tidy = F, warning = F, message = F---------------------------------------

linear_model_iris$coefficients


## ----tidy = F, warning = F, message = F---------------------------------------

data.frame(
  Model = c("Linear Regression", "Random Forest"),
  rbind(
    linear_model_iris$coefficients[-1],
    random_forest_iris$variable.importance
  )
)


## ----warning=F,message=F------------------------------------------------------
install.packages("tidyverse",repos='http://lib.stat.cmu.edu/R/CRAN')
library(tidyverse)

## ----warning=F,message=F------------------------------------------------------
 install.packages("VIM",repos='http://lib.stat.cmu.edu/R/CRAN')
 library(VIM)

## -----------------------------------------------------------------------------

packages <- c("data.table","tidyverse","here")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}

for (package in packages) {
  library(package, character.only=T)
}


## ----eval = F-----------------------------------------------------------------
#  
#  install.packages("pacman", repos='http://lib.stat.cmu.edu/R/CRAN')
#  library(pacman)
#  
#  p_load(data.table,
#         tidyverse,
#         here)
#  

## -----------------------------------------------------------------------------
library(here)
nhefs <- read_csv(here("data","nhefs.csv"))

## -----------------------------------------------------------------------------
nhefs <- read_csv(url("https://tinyurl.com/2s432xv6"))

## -----------------------------------------------------------------------------
class(nhefs)

## -----------------------------------------------------------------------------
dim(nhefs)

## -----------------------------------------------------------------------------
nhefs <- nhefs %>% select(seqn,qsmk,sex,age,income,sbp,dbp,price71,tax71,race,wt82_71)

## -----------------------------------------------------------------------------
head(nhefs)
# can also use "tail" to see the end of the file
# tail(nhefs)

## -----------------------------------------------------------------------------

propMissing <- function(x){
  mean(is.na(x))
}
propMissing(nhefs[,1])
propMissing(nhefs[,2])


## -----------------------------------------------------------------------------

for (i in 1:ncol(nhefs)){
  output <- propMissing(nhefs[,i])
  print(output)
}


## -----------------------------------------------------------------------------

apply(nhefs,2,propMissing)


## -----------------------------------------------------------------------------

round(apply(nhefs,2,propMissing),3)*100


