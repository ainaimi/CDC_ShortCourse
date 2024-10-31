############
## Example 2
############
# Two-stage element sampling
# Generates artificial data (a 235X3 matrix with 3 columns: state, region, income).
# The variable "state" has 2 categories ('n','s'). 
# The variable "region" has 5 categories ('A', 'B', 'C', 'D', 'E').
# The variable "income" is generated using the U(0,1) distribution. 
data=rbind(matrix(rep('n',165),165,1,byrow=TRUE),matrix(rep('s',70),70,1,byrow=TRUE))
data=cbind.data.frame(data,c(rep('A',115),rep('D',10),rep('E',40),rep('B',30),rep('C',40)),
                      100*runif(235))
names(data)=c("state","region","income")
data=data[order(data$state,data$region),]
table(data$state,data$region)
# Stratified one-stage cluster sampling
# The same data as in Example 2
# the variable 'state' is used as stratification variable 
# 165 units are in the first stratum and 70 in the second one
# the variable 'region' is used as clustering variable
# 1 cluster (region) is drawn in each state using "srswor" 
m=mstage(data, stage=list("stratified","cluster"), varnames=list("state","region"), 
         size=list(c(165,70),c(1,1)),method=list("","srswor")) 
# check the first stage
table(m[[1]]$state)
# check the second stage
table(m[[2]]$region)
# extracts the observed data
xx=getdata(data,m)[[2]]
# check the result
table(xx$state,xx$region)