### IMPORT AND EXAMINE MISSING NSFG DATA FOR SNMM DIRECT EFFECT PAPER

library(mice)
library(VIM)
library(splines)
library(survey)
library(sampling)

m <- read.table(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/nsfg_dataimp_01Feb16.txt",sep="\t",header = T,na.strings=c("."))

head(m);mean(m$breastfeed)

sum(is.na(m$PNCAREWK));sum(is.na(m$BFEEDWKS));m$black<-as.factor(as.numeric(m$HISPRACE==3))
m$bfeed_pnc<-((m$BFEEDWKS-mean(m$BFEEDWKS))/sd(m$BFEEDWKS))*((m$PNCAREWK-mean(m$PNCAREWK, na.rm=TRUE))/sd(m$PNCAREWK, na.rm=TRUE));sum(is.na(m$bfeed_pnc));hist(m$bfeed_pnc)
m$HISPRACE<-as.factor(m$HISPRACE);sum(is.na(m$HISPRACE));m$breastfeed<-as.factor(as.numeric(m$BFEEDWKS>0))
m$multbrthl1<-as.factor(m$multbrthl1);sum(is.na(m$multbrthl1))
m$outcomel1<-as.factor(m$outcomel1);sum(is.na(m$outcomel1))
m$MATERNLV<-ifelse(is.na(m$MATERNLV),NA,as.factor(as.numeric(m$MATERNLV %in% c(2,3,4))));sum(is.na(m$MATERNLV))
m$PAYDELIV<-as.factor(as.numeric(m$PAYDELIV==4));sum(is.na(m$PAYDELIV))
m$POSTSMKS<-as.factor(as.numeric(m$POSTSMKS==1));sum(is.na(m$POSTSMKS))
m$PRIORSMK<-as.factor(as.numeric(m$PRIORSMK>0));sum(is.na(m$PRIORSMK))
m$GETPRENA<-as.factor(m$GETPRENA);sum(is.na(m$GETPRENA))
m$id_centscale<-(m$CASEID-mean(m$CASEID))/sd(m$CASEID);hist(m$id_centscale)
m$PREGORDR_cs<-(m$PREGORDR-mean(m$PREGORDR))/sd(m$PREGORDR);hist(m$PREGORDR_cs)
m$cd12<-as.factor(m$cd12);m$id12<-as.factor(m$id12)
m$recallb<-as.factor(m$recallb);m$nsfg<-as.factor(m$nsfg)
m$highschool<-as.factor(m$highschool);m$university<-as.ordered(m$university);m$EDUCMOM<-as.factor(m$EDUCMOM)
m$WANTRESP<-as.factor(m$WANTRESP);m$FMARCON5<-as.factor(m$FMARCON5)
m$PREGORDR<-as.ordered(m$PREGORDR)

m$clust<-m$SECU*m$SEST
length(unique(m$clust))
table(m$clust)

d1<-svydesign(id=~SECU, weight=~WGTQ1Q16, strata=~SEST,data=m,nest=T)
mod1<-svyglm(breastfeed~black,design=d1,family=quasibinomial(link="log"))
summary(mod1)

b1<-as.svrepdesign(d1,type="bootstrap", replicates=1000)
mod12<-svyglm(breastfeed~black,design=b1,family=quasibinomial(link="log"));as.matrix(coef(mod1))[2,1]
summary(mod12)

mod_boot<-NULL
for(i in 1:1000){
  set.seed(i*100)
  boot_id<-cluster(m,clustername=c("clust"),length(unique(m$clust)),method=c("srswr"))
  m_boot<-getdata(m,boot_id)
  dboot<-svydesign(id=~SECU, weight=~WGTQ1Q16, strata=~SEST,data=m_boot,nest=T)
  options(survey.lonely.psu = "adjust")
  mb<-as.matrix(coef(svyglm(breastfeed~black,design=dboot,family=quasibinomial(link="log"))))[2,1]
  mod_boot<-rbind(mod_boot,mb)
}
summary(mod1);summary(mod12)
sd(mod_boot);mean(mod_boot)











# B SPLINE BASIS FUNCTIONS

clist<-cbind(m$datendl1,m$POVERTY,m$AGER,m$BFEEDWKS,m$BFEEDWKSl1,m$DATECON,m$AGEPREG,m$PRGLNGTH,m$HPAGELB,m$PNCAREWK,m$PNCAREWKl1,m$bfeed_pnc)
name<-as.matrix(c("datendl1s","POVERTYs","AGERs","BFEEDWKSs","BFEEDWKSl1s","DATECONs","AGEPREGs","PRGLNGTHs","HPAGELBs","PNCAREWKs","PNCAREWKl1s","bfeed_pncs"))
for(ii in 1:ncol(clist)){
  X <- bs(clist[,ii],df=3)
  dimnames(X)[[2]]<-paste(name[ii,], 1:ncol(X), sep = "")
m<-cbind(m,X[,-3])
}
head(m)

# CHECK MODELS FOR GETPRENA AND PNCAREWK

hist(m$PNCAREWK)

g<-glm(GETPRENA~id_centscale+outcomel1+datendl1+datendl1s1+datendl1s2
                +university+highschool+FMARCON5+PRGLNGTH+PRGLNGTHs1+PRGLNGTHs2
                +PRIORSMK+PAYDELIV,data=m,family=binomial())
summary(g)

h<-glm(PNCAREWK~id_centscale+outcomel1+parity+university+
         highschool+POVERTY+POVERTYs1+POVERTYs2+AGER+AGERs1+AGERs2+
         WANTRESP+FMARCON5+AGEPREG+AGEPREGs1+AGEPREGs2+PRIORSMK+bfeed_pnc+bfeed_pncs1+bfeed_pncs2+
         PNCAREWKl1+PNCAREWKl1s1+PNCAREWKl1s2,data=m,family=gaussian)
summary(h)

#pdf(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/MissingPlot.pdf",width=12,height=6)
aggr(m,plot=T,sortVars=T,ylabs=c("Proportion Missing", "Missing Patterns"))
#dev.off()


m0<-m[,c("PRGLNGTH",	"PRGLNGTHs2",	"PRGLNGTHs1",	"AGEPREG",	"AGEPREGs2",	"AGEPREGs1",	"DATECON",	"DATECONs2",	"DATECONs1",	"BFEEDWKS",	"BFEEDWKSs2",	"BFEEDWKSs1",	"AGER",	"AGERs2",	"AGERs1",	"POVERTY",	"POVERTYs2",	"POVERTYs1",	"datendl1",	"datendl1s2",	"datendl1s1",	"PREGORDR_cs",	"id_centscale",	"cd12",	"id12",	"csection",	"multbrthl1",	"outcomel1",	"nsfg",	"recallb",	"parity",	"university",	"highschool",	"EDUCMOM",	"HISPRACE",	"WANTRESP",	"FMARCON5",	"ANYUSINT",	"PREGORDR",	"CASEID",	"HPAGELB", "HPAGELBs2",	"HPAGELBs1","BFEEDWKSl1",	"BFEEDWKSl1s2",	"BFEEDWKSl1s1",	"GETPRENA",	"POSTSMKS",	"PRIORSMK","bfeed_pnc",	"bfeed_pncs2",	"bfeed_pncs1","PNCAREWK","PNCAREWKs2",	"PNCAREWKs1",	"PAYDELIV",	"PNCAREWKl1","PNCAREWKl1s2",	"PNCAREWKl1s1",	"MATERNLV")]
head(m0)

########
ini <- mice(m0,seed=123,maxit=0)

print(ini)

# EXTRACT AND EXAMINE DEFAULT PREDICTOR MATRIX
pMatrix<-ini$predictorMatrix
write.table(pMatrix, "~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/pMatrix.txt", sep="\t")

#head(ini$predictorMatrix)

# LOAD NEW MODIFIED PREDICTOR MATRIX

pMatrix2 <- read.table(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/pMatrix2.txt",
                       sep="\t",header = T,row.names=1)

pMat<-as.matrix(pMatrix2)
nrow(pMat);ncol(pMat)

# IMPUTATION

# Subset Data by Racial Categories and "recallb" to Account for Interactions

mr10<-subset(m0,m0$HISPRACE==1&m0$recallb==0);mr10$recallb<-mr10$HISPRACE<-NULL;nrow(mr10);ncol(mr10)
mr20<-subset(m0,m0$HISPRACE==2&m0$recallb==0);mr20$recallb<-mr20$HISPRACE<-NULL;nrow(mr20);ncol(mr20)
mr30<-subset(m0,m0$HISPRACE==3&m0$recallb==0);mr30$recallb<-mr30$HISPRACE<-NULL;nrow(mr30);ncol(mr30)
mr40<-subset(m0,m0$HISPRACE==4&m0$recallb==0);mr40$recallb<-mr40$HISPRACE<-NULL;nrow(mr40);ncol(mr40)
mr11<-subset(m0,m0$HISPRACE==1&m0$recallb==1);mr11$recallb<-mr11$HISPRACE<-NULL;nrow(mr11);ncol(mr11)
mr21<-subset(m0,m0$HISPRACE==2&m0$recallb==1);mr21$recallb<-mr21$HISPRACE<-NULL;nrow(mr21);ncol(mr21)
mr31<-subset(m0,m0$HISPRACE==3&m0$recallb==1);mr31$recallb<-mr31$HISPRACE<-NULL;nrow(mr31);ncol(mr31)
mr41<-subset(m0,m0$HISPRACE==4&m0$recallb==1);mr41$recallb<-mr41$HISPRACE<-NULL;nrow(mr41);ncol(mr41)

iteration<-100;imputation<-50

nsfg_imp10 <- mice(mr10,pred=pMat,seed=123,maxit=iteration,m=imputation)
nsfg_imp20 <- mice(mr20,pred=pMat,seed=234,maxit=iteration,m=imputation)
nsfg_imp30 <- mice(mr30,pred=pMat,seed=345,maxit=iteration,m=imputation)
nsfg_imp40 <- mice(mr40,pred=pMat,seed=456,maxit=iteration,m=imputation)

nsfg_imp11 <- mice(mr11,pred=pMat,seed=567,maxit=iteration,m=imputation)
nsfg_imp21 <- mice(mr21,pred=pMat,seed=678,maxit=iteration,m=imputation)
nsfg_imp31 <- mice(mr31,pred=pMat,seed=789,maxit=iteration,m=imputation)
nsfg_imp41 <- mice(mr41,pred=pMat,seed=890,maxit=iteration,m=imputation)

plot(nsfg_imp10, c("HPAGELB", "BFEEDWKSl1", "GETPRENA"))
plot(nsfg_imp10, c("POSTSMKS", "PRIORSMK", "PAYDELIV"))
plot(nsfg_imp10, c("PNCAREWK", "bfeed_pnc", "PNCAREWKl1"))
plot(nsfg_imp10, c("MATERNLV"))
plot(nsfg_imp20, c("HPAGELB", "BFEEDWKSl1", "GETPRENA"))
plot(nsfg_imp20, c("POSTSMKS", "PRIORSMK", "PAYDELIV"))
plot(nsfg_imp20, c("PNCAREWK", "bfeed_pnc", "PNCAREWKl1"))
plot(nsfg_imp20, c("MATERNLV"))
plot(nsfg_imp3, c("HPAGELB", "BFEEDWKSl1", "GETPRENA"))
plot(nsfg_imp3, c("POSTSMKS", "PRIORSMK", "PAYDELIV"))
plot(nsfg_imp3, c("PNCAREWK", "bfeed_pnc", "PNCAREWKl1"))
plot(nsfg_imp3, c("MATERNLV"))
plot(nsfg_imp4, c("HPAGELB", "BFEEDWKSl1", "GETPRENA"))
plot(nsfg_imp4, c("POSTSMKS", "PRIORSMK", "PAYDELIV"))
plot(nsfg_imp4, c("PNCAREWK", "bfeed_pnc", "PNCAREWKl1"))
plot(nsfg_imp4, c("MATERNLV"))

imputed_all <- complete(nsfg_imp,"long")

write.table(imputed_all, "/Users/Executive/Dropbox/Documents/Research/Papers/SNM_DirectEffect/nsfg_imp.txt", sep=",",row.names=FALSE)

