### IMPORT AND EXAMINE MISSING NSFG DATA FOR SNMM DIRECT EFFECT PAPER

library(mice)
library(VIM)
library(splines)
library(survey)
library(sampling)
library(beepr)

m <- read.table(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/nsfg_dataimp_01Feb16.txt",sep="\t",header = T,na.strings=c("."))

head(m);table(m$breastfeed)

sum(is.na(m$PNCAREWK));sum(is.na(m$BFEEDWKS));m$black<-as.factor(as.numeric(m$HISPRACE==3));m$hispanic<-as.factor(as.numeric(m$HISPRACE==1))
m$bfeed_pnc<-((m$BFEEDWKS-mean(m$BFEEDWKS))/sd(m$BFEEDWKS))*((m$PNCAREWK-mean(m$PNCAREWK, na.rm=TRUE))/sd(m$PNCAREWK, na.rm=TRUE))
m$HISPRACE<-as.factor(m$HISPRACE);sum(is.na(m$HISPRACE));
m$breastfeed<-as.numeric(m$BFEEDWKS>=52)
m$multbrthl1<-as.factor(m$multbrthl1);sum(is.na(m$multbrthl1))
m$outcomel1<-as.factor(m$outcomel1);sum(is.na(m$outcomel1))
m$BIRTHPLC<-as.factor(as.numeric(m$BIRTHPLC==2));
m$MATERNLV<-ifelse(is.na(m$MATERNLV),NA,as.factor(as.numeric(m$MATERNLV %in% c(2,3,4))));sum(is.na(m$MATERNLV))
m$PAYDELIV<-as.numeric(m$PAYDELIV==4);sum(is.na(m$PAYDELIV))
m$POSTSMKS<-as.factor(as.numeric(m$POSTSMKS==1));sum(is.na(m$POSTSMKS))
m$PRIORSMK<-as.factor(as.numeric(m$PRIORSMK>0));sum(is.na(m$PRIORSMK))
m$PREGORDR<-ifelse(m$PREGORDR>=4,4,m$PREGORDR)
m$PREGORDR<-as.ordered(m$PREGORDR)
m$cd12<-as.factor(m$cd12);m$id12<-as.factor(m$id12)
m$recallb<-as.factor(m$recallb);m$nsfg<-as.factor(m$nsfg)
m$highschool<-as.numeric(m$highschool);m$university<-as.ordered(m$university);m$EDUCMOM<-as.factor(m$EDUCMOM)
m$WANTRESP<-as.factor(as.numeric(m$WANTRESP==5));m$FMARCON5<-as.factor(as.numeric(m$FMARCON5==1))
m$breastfeedl1<-as.factor(as.numeric(m$BFEEDWKSl1>52))

#####  DEFINE PRENATAL CARE
m$GETPRENA<- as.numeric(m$PNCAREWK<=10)  #as.numeric(m$GETPRENA==5);sum(is.na(m$GETPRENA))
#####


# DEFINE SAMPLING CLUSTER
m$clust<-m$SECU*m$SEST
length(unique(m$clust))
table(m$clust)

# CENTER AND SCALE
clist0<-cbind(m$datendl1,m$AGER,m$BFEEDWKS,m$BFEEDWKSl1,m$DATECON,m$DATEND,m$AGEPREG,
             m$PRGLNGTH,m$HPAGELB,m$PNCAREWK,m$PNCAREWKl1,m$bfeed_pnc,m$BIRTHWGT_LB1,
             m$clust,m$birthwgtl1,m$time_arrive,m$ipi,m$PREGORDR,
             m$CASEID,m$SECU,m$SEST,m$WGTQ1Q16)
clist0_names<-as.matrix(c("cs_datendl1","csAGER","csBFEEDWKS","csBFEEDWKSl1","csDATECON","csDATEND","csAGEPREG",
                    "csPRGLNGTH","csHPAGELB","csPNCAREWK","csPNCAREWKl1","cs_bfeed_pnc","csBIRTHWGT_LB1",
                    "cs_clust","cs_birthwgtl1","cs_time_arrive","cs_ipi","csPREGORDR",
                    "csID","csSECU","csSEST","csWGT"))
cs_var<-NULL
for(ii in 1:ncol(clist0)){
  cvar <- (clist0[,ii] - mean(clist0[,ii],na.rm=T)) / sd(clist0[,ii],na.rm=T)
  cs_var <- cbind(cs_var,cvar)
}
cs_var<-as.data.frame(cs_var)
names(cs_var)<-clist0_names
head(cs_var)
m<-cbind(m,cs_var)
head(m)

# pnc<-seq(min(subset(m$PNCAREWK,m$PNCAREWK<90)),max(subset(m$PNCAREWK,m$PNCAREWK<90)),length.out=40)
# pred_pnc<-predict(mmm,newdata=data.frame(PNCAREWK=subset(m$PNCAREWK,m$PNCAREWK<90)),type="response")
# pred_pnc<-as.data.frame(pred_pnc)
# pred_pnc<-cbind(pred_pnc,subset(m$PNCAREWK,m$PNCAREWK<90))
# names(pred_pnc)<-c("p","SE","PNC")
# PNC<-row.names(as.matrix(table(pred_pnc$PNC)))
# pp<-merge(PNC,pred_pnc);head(pp)
# pred_pnc<-pred_pnc[unique(round(pred_pnc$PNC),digits=0),]
# head(pred_pnc)
# plot(pred_pnc$PNC,pred_pnc$p)

# B SPLINE BASIS FUNCTIONS FOR FLEXIBLE IMPUTATION
clist<-cbind(m$cs_datendl1,m$csAGER,m$csBFEEDWKS,m$csBFEEDWKSl1,m$csDATECON,m$csDATEND,m$csAGEPREG,
             m$csPRGLNGTH,m$csHPAGELB,m$csPNCAREWK,m$csPNCAREWKl1,m$cs_bfeed_pnc,m$csBIRTHWGT_LB1,
             m$cs_clust,m$cs_birthwgtl1,m$cs_time_arrive,m$cs_ipi,m$csPREGORDR,
             m$csID,m$csSECU,m$csSEST,m$csWGT)
name<-as.matrix(c("cs_datendl1_s","csAGER_s","csBFEEDWKS_s","csBFEEDWKSl1_s","csDATECON_s","csDATEND_s","csAGEPREG_s",
                  "csPRGLNGTH_s","csHPAGELB_s","csPNCAREWK_s","csPNCAREWKl1_s","cs_bfeed_pnc_s","csBIRTHWGT_LB1_s",
                  "cs_clust_s","cs_birthwgtl1_s","cs_time_arrive_s","cs_ipi_s","csPREGORDR_s",
                  "csID_s","csSECU_s","csSEST_s","csWGT_s"))
for(ii in 1:ncol(clist)){
  X <- bs(clist[,ii],df=3)
  dimnames(X)[[2]]<-paste(name[ii,], 1:ncol(X), sep = "")
  m<-cbind(m,X[,-3])
}
head(m)

imp_m<-m[,c("PREGORDR", "MULTBRTH", "BIRTHPLC", "PRIORSMK", "POSTSMKS", "GETPRENA", "OUTCOME",
            "BIRTHORD", "FMARCON5", "PAYDELIV", "MATERNLV", "WANTRESP","HISPRACE","BFEEDWKS","PNCAREWK",
            "EDUCMOM", "nsfg", "highschool", "university", "recallb", "outcomel1", "multbrthl1",
            "usbirth", "preghelp", "id12", "cd12", "black","SECU","SEST","WGTQ1Q16",
            "breastfeed", "breastfeedl1", "cs_datendl1", "csAGER", "csBFEEDWKS", "csBFEEDWKSl1", "csDATECON", "csDATEND",
            "csAGEPREG", "csPRGLNGTH",  "csHPAGELB", "csPNCAREWK", "csPNCAREWKl1", "cs_bfeed_pnc", "csBIRTHWGT_LB1", "cs_clust",
            "cs_birthwgtl1", "cs_time_arrive", "cs_ipi", "csPREGORDR", "csID", "csSECU", "csSEST", "csWGT",
            "cs_datendl1_s1", "cs_datendl1_s2", "csAGER_s1", "csAGER_s2", "csBFEEDWKS_s1", "csBFEEDWKS_s2", "csBFEEDWKSl1_s1",
            "csBFEEDWKSl1_s2", "csDATECON_s1", "csDATECON_s2", "csDATEND_s1", "csDATEND_s2", "csAGEPREG_s1", "csAGEPREG_s2", "csPRGLNGTH_s1",
            "csPRGLNGTH_s2", "csHPAGELB_s1", "csHPAGELB_s2", "csPNCAREWK_s1", "csPNCAREWK_s2", "csPNCAREWKl1_s1", "csPNCAREWKl1_s2",
            "cs_bfeed_pnc_s1", "cs_bfeed_pnc_s2", "csBIRTHWGT_LB1_s1", "csBIRTHWGT_LB1_s2", "cs_clust_s1", "cs_clust_s2",
            "cs_birthwgtl1_s1", "cs_birthwgtl1_s2", "cs_time_arrive_s1", "cs_time_arrive_s2", "cs_ipi_s1", "cs_ipi_s2",
            "csPREGORDR_s1", "csPREGORDR_s2", "csID_s1", "csID_s2", "csSECU_s1", "csSECU_s2", "csSEST_s1", "csSEST_s2",
            "csWGT_s1", "csWGT_s2")]

head(imp_m)

# PREPARE PREDICTION MATRIX FOR IMPUTATION

#pdf(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/MissingPlot.pdf",width=12,height=6)
#aggr(m,plot=T,sortVars=T,ylabs=c("Proportion Missing", "Missing Patterns"))
aggr(imp_m,plot=T,sortVars=T,ylabs=c("Proportion Missing", "Missing Patterns"),only.miss=T)
#dev.off()
m_order<-as.matrix(sapply(imp_m, function(x) sum(is.na(x))))
m_order1 <- names(m_order[order(-m_order[,1]), ])
m0<-imp_m[,m_order1]
resample_size<-table(m0$cs_clust)
m0<-m0[order(m0$cs_clust),]
nrow(m0)

########
ini <- mice(m0,seed=123,maxit=0)

# EXTRACT AND EXAMINE DEFAULT PREDICTOR MATRIX
pMatrix<-ini$predictorMatrix
write.table(pMatrix, "~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/pMatrix.txt", sep="\t")

#head(ini$predictorMatrix)

# LOAD NEW MODIFIED PREDICTOR MATRIX

pMatrix2 <- read.table(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/pMatrix2a.txt",
                       sep="\t",header = T,row.names=1)
pMat<-as.matrix(pMatrix2)
nrow(pMat);ncol(pMat)

strt<-Sys.time()
pY_white<-pY_black<-pY_hispanic<-NULL
pYtildeRD_white<-pYtildeRD_black<-pYtildeRD_hispanic<-NULL
pYtildeRR_white<-pYtildeRR_black<-pYtildeRR_hispanic<-NULL
mYRR<-mYRD<-NULL
mYtildeRR<-mYtildeRD<-NULL
iterboot1_mean<-iterboot1_var<-NULL
iterboot2_mean<-iterboot2_var<-NULL
iterboot3_mean<-iterboot3_var<-NULL

sest_size<-as.matrix(table(imp_m$SEST));row.names(sest_size)<-NULL
c(sest_size)

secu_size<-as.matrix(rep(1,nrow(sest_size)))
c(secu_size)

for(i in 1:5){
  i<-2
  set.seed(i*100)
  ## TAKE A SINGLE STRATIFIED CLUSTER RE-SAMPLE FROM NSFG DATA WITH REPLACEMENT
  boot_id=mstage(imp_m, 
                 stage=list("stratified","cluster"), 
                 varnames=list("SEST","SECU"),
                 size=list(c(sest_size),c(secu_size)),
                 method=list("","srswor"))
  table(boot_id[[1]]$SEST)
  table(boot_id[[2]]$SECU)

  m_boot<-getdata(imp_m,boot_id)[[2]]
  nrow(m_boot);ncol(m_boot)
  head(table(imp_m$SEST,imp_m$SECU))
  head(table(m_boot$SEST,m_boot$SECU))
  
  ## IMPUTE THE SINGLE RESAMPLE
  imputation<-5;iteration<-5
  mr1<-subset(m_boot,m_boot$HISPRACE==1)
  mr1$Prob<-mr1$Replicates<-mr1$ID_unit<-mr1$black<-mr1$Stratum<-NULL;nrow(mr1);ncol(mr1)
  mr2<-subset(m_boot,m_boot$HISPRACE==2)
  mr2$Prob<-mr2$Replicates<-mr2$ID_unit<-mr2$black<-mr2$Stratum<-NULL;nrow(mr2);ncol(mr2)
  mr3<-subset(m_boot,m_boot$HISPRACE==3)
  mr3$Prob<-mr3$Replicates<-mr3$ID_unit<-mr3$black<-mr3$Stratum<-NULL;nrow(mr3);ncol(mr3)

  seed1<-i*123;seed2<-i*234;seed3<-i*345
  nsfg_imp1 <- mice(mr1,pred=pMat,seed=seed1,maxit=iteration,m=imputation,diagnostics=T)
  nsfg_imp2 <- mice(mr2,pred=pMat,seed=seed2,maxit=iteration,m=imputation,diagnostics=T)
  nsfg_imp3 <- mice(mr3,pred=pMat,seed=seed3,maxit=iteration,m=imputation,diagnostics=T)

  nsfg_imp3$method
  
  exctract<-c("GETPRENA","PNCAREWK","MATERNLV")
  dd<-t(as.data.frame(nsfg_imp1$chainMean))[,exctract];dd<-as.data.frame(dd)
  row.names(dd)<-NULL;dd$ii0<-1;dd$iter<-cumsum(dd$ii0);dd$ii0<-NULL;dd$imput<-i
  iterboot1_mean<-rbind(iterboot1_mean,dd)
  dd<-t(as.data.frame(nsfg_imp1$chainVar))[,exctract];dd<-as.data.frame(dd)
  row.names(dd)<-NULL;dd$ii0<-1;dd$iter<-cumsum(dd$ii0);dd$ii0<-NULL;dd$imput<-i
  iterboot1_var<-rbind(iterboot1_var,dd)
  
  dd<-t(as.data.frame(nsfg_imp2$chainMean))[,exctract];dd<-as.data.frame(dd)
  row.names(dd)<-NULL;dd$ii0<-1;dd$iter<-cumsum(dd$ii0);dd$ii0<-NULL;dd$imput<-i
  iterboot2_mean<-rbind(iterboot2_mean,dd)
  dd<-t(as.data.frame(nsfg_imp2$chainVar))[,exctract];dd<-as.data.frame(dd)
  row.names(dd)<-NULL;dd$ii0<-1;dd$iter<-cumsum(dd$ii0);dd$ii0<-NULL;dd$imput<-i
  iterboot2_var<-rbind(iterboot2_var,dd)
  
  dd<-t(as.data.frame(nsfg_imp3$chainMean))[,exctract];dd<-as.data.frame(dd)
  row.names(dd)<-NULL;dd$ii0<-1;dd$iter<-cumsum(dd$ii0);dd$ii0<-NULL;dd$imput<-i
  iterboot3_mean<-rbind(iterboot3_mean,dd)
  dd<-t(as.data.frame(nsfg_imp3$chainVar))[,exctract];dd<-as.data.frame(dd)
  row.names(dd)<-NULL;dd$ii0<-1;dd$iter<-cumsum(dd$ii0);dd$ii0<-NULL;dd$imput<-i
  iterboot3_var<-rbind(iterboot3_var,dd)
  
  imp1 <- complete(nsfg_imp1,"long");imp1$black<-0;imp1$hispanic<-1
  imp2 <- complete(nsfg_imp2,"long");imp2$black<-0;imp2$hispanic<-0
  imp3 <- complete(nsfg_imp3,"long");imp3$black<-1;imp3$hispanic<-0
  
  nsfg_imp<-rbind(imp1,imp2,imp3)
  
  ## TRANSFORM PNCAREWK BACK TO ORIGINAL SCALE AND THEN SUBTRACT 8 WEEKS TO SET AS REFERENT
  #   nsfg_imp$PNCAREWK<-(nsfg_imp$csPNCAREWK*sd_pnc)+mean_pnc
  #   
  #   nsfg_imp$PNCAREWK<-ifelse(nsfg_imp$GETPRENA==0,nsfg_imp$PNCAREWK-8,nsfg_imp$PNCAREWK)
  #   table(nsfg_imp$PNCAREWK);sum(is.na(nsfg_imp$PNCAREWK))
  #   min(nsfg_imp$PNCAREWK)
  dboot<-svydesign(id=~SECU, weight=~WGTQ1Q16, strata=~SEST,data=nsfg_imp,nest=T)
  
  ## TABLE FOR EDUCATION AND INCOME
  summary(svytable(~highschool+PAYDELIV,dboot))
  
  ## COMPUTE THE MEAN OF Y BY "BLACK" STATUS
  p_crude<-svyby(~breastfeed, ~black + hispanic, dboot, svymean)
  ppc1<-as.matrix(p_crude$breastfeed)[1,1]
  ppc2<-as.matrix(p_crude$breastfeed)[2,1]
  ppc3<-as.matrix(p_crude$breastfeed)[3,1]
  pY_white<-rbind(pY_white,ppc1)
  pY_black<-rbind(pY_black,ppc2)
  pY_hispanic<-rbind(pY_hispanic,ppc3)
  
  ## PROPENSITY SCORE MODEL FOR PNC
  table(nsfg_imp$GETPRENA)
  propensity1<-svyglm(GETPRENA~csHPAGELB+csHPAGELB_s1+csHPAGELB_s2+
                        csBFEEDWKSl1+csBFEEDWKSl1_s1+csBFEEDWKSl1_s2+
                        breastfeedl1+breastfeedl1*csBFEEDWKSl1+
                        PRIORSMK+POSTSMKS+PRIORSMK*POSTSMKS+PREGORDR+
                        WANTRESP+FMARCON5+cs_ipi+cs_ipi_s1+cs_ipi_s2+
                        cs_birthwgtl1+cs_birthwgtl1_s1+cs_birthwgtl1_s1+cd12+
                        black+hispanic+highschool+PAYDELIV+
                        black*highschool+black*PAYDELIV+highschool*PAYDELIV,design=dboot,family=quasibinomial(link="logit"))
  summary(propensity1)
  
  nrow(nsfg_imp);nrow(as.matrix(propensity1$fitted.values))

  nsfg_imp$pi_X1<-propensity1$fitted.values
  
  ## REDEFINE SAMPLING FRAME TO INCLUDE PS
  dboot<-svydesign(id=~SECU, weight=~WGTQ1Q16, strata=~SEST,data=nsfg_imp,nest=T)
  options(survey.lonely.psu = "adjust")
  ## ESTIMATE PNC EFFECT USING DOUBLY ROBUST ESTIMATION, KEEP PNC PARAMETER ESTIMATES
  psi_RD<-as.matrix(coef(svyglm(breastfeed~GETPRENA+GETPRENA*black+GETPRENA*hispanic+GETPRENA*highschool+GETPRENA*PAYDELIV
                               +bs(pi_X1,df=4)+bs(pi_X1,df=4)*black+bs(pi_X1,df=4)*hispanic+bs(pi_X1,df=4)*highschool+bs(pi_X1,df=4)*PAYDELIV
                                +black+hispanic+highschool+PAYDELIV
                                +csHPAGELB+csHPAGELB_s1+csHPAGELB_s2
                                +csBFEEDWKSl1+csBFEEDWKSl1_s1+csBFEEDWKSl1_s2
                                #+breastfeedl1+breastfeedl1*csBFEEDWKSl1
                                +PRIORSMK+POSTSMKS+PRIORSMK*POSTSMKS+PREGORDR
                                +WANTRESP+FMARCON5+cs_ipi+cs_ipi_s1+cs_ipi_s2
                                +cs_birthwgtl1+cs_birthwgtl1_s1+cs_birthwgtl1_s1+cd12
                                +black*highschool+black*PAYDELIV+highschool*PAYDELIV
                                +MATERNLV+csBIRTHWGT_LB1+csBIRTHWGT_LB1_s1+csBIRTHWGT_LB1_s2
                                +csPRGLNGTH+csPRGLNGTH_s1+csPRGLNGTH_s2
                                ,design=dboot,family=quasi(link = "identity", variance = "constant"))
                                ))[c("GETPRENA","GETPRENA:black","GETPRENA:hispanic","GETPRENA:highschool","GETPRENA:PAYDELIV"),]  #
  X<-cbind(nsfg_imp$GETPRENA,nsfg_imp$GETPRENA*nsfg_imp$black,nsfg_imp$GETPRENA*nsfg_imp$hispanic,nsfg_imp$GETPRENA*nsfg_imp$highschool,nsfg_imp$GETPRENA*nsfg_imp$PAYDELIV) #
  names(nsfg_imp)
  table(nsfg_imp$GETPRENA)
  mean(X%*%psi_RD)
  
  psi_RR<-as.matrix(coef(svyglm(breastfeed~GETPRENA+GETPRENA*black+GETPRENA*hispanic+GETPRENA*highschool+GETPRENA*PAYDELIV
                                +bs(pi_X1,df=4)+bs(pi_X1,df=4)*black+bs(pi_X1,df=4)*hispanic+bs(pi_X1,df=4)*highschool+bs(pi_X1,df=4)*PAYDELIV
                                +black+hispanic+highschool+PAYDELIV
                                +csHPAGELB+csHPAGELB_s1+csHPAGELB_s2
                                +csBFEEDWKSl1+csBFEEDWKSl1_s1+csBFEEDWKSl1_s2
                                #+breastfeedl1+breastfeedl1*csBFEEDWKSl1
                                +PRIORSMK+POSTSMKS+PRIORSMK*POSTSMKS+PREGORDR
                                +WANTRESP+FMARCON5+cs_ipi+cs_ipi_s1+cs_ipi_s2
                                +cs_birthwgtl1+cs_birthwgtl1_s1+cs_birthwgtl1_s1+cd12
                                +black*highschool+black*PAYDELIV+highschool*PAYDELIV
                                +MATERNLV+csBIRTHWGT_LB1+csBIRTHWGT_LB1_s1+csBIRTHWGT_LB1_s2
                                +csPRGLNGTH+csPRGLNGTH_s1+csPRGLNGTH_s2
                               ,design=dboot,family=quasi(link = "log", variance = "mu")
                                )))[c("GETPRENA","GETPRENA:black","GETPRENA:hispanic","GETPRENA:highschool","GETPRENA:PAYDELIV"),]
  psi_RR
  
  ## CREATE TRANSFORMED OUTCOMES
  psi_RD<-as.matrix(psi_RD);psi_RR<-as.matrix(psi_RR)
  nsfg_imp$tildeY_RD <- nsfg_imp$breastfeed - X%*%psi_RD
  nsfg_imp$tildeY_RR <- nsfg_imp$breastfeed*exp(-X%*%psi_RR)
  
  ## REDEFINE SAMPLING FRAME TO INCLUDE TRANSFORMED OUTCOMES
  dboot<-svydesign(id=~SECU, weight=~WGTQ1Q16, strata=~SEST,data=nsfg_imp,nest=T)
  
  ## COMPUTE THE MEAN OF Y_TILDE BY "BLACK" STATUS UNDER ADDITIVE MODEL
  p_trans<-svyby(~tildeY_RD, ~black+hispanic, dboot, svymean)
  ppt1_RD<-as.matrix(p_trans$tildeY_RD)[1,1]
  ppt2_RD<-as.matrix(p_trans$tildeY_RD)[2,1]
  ppt3_RD<-as.matrix(p_trans$tildeY_RD)[3,1]
  pYtildeRD_white<-rbind(pYtildeRD_white,ppt1_RD)
  pYtildeRD_black<-rbind(pYtildeRD_black,ppt2_RD)
  pYtildeRD_hispanic<-rbind(pYtildeRD_hispanic,ppt3_RD)
  
  ## COMPUTE THE MEAN OF Y_TILDE BY "BLACK" STATUS UNDER MULTIPLICATIVE MODEL
  p_trans<-svyby(~tildeY_RR, ~black+hispanic, dboot, svymean)
  ppt1_RR<-as.matrix(p_trans$tildeY_RR)[1,1]
  ppt2_RR<-as.matrix(p_trans$tildeY_RR)[2,1]
  ppt3_RR<-as.matrix(p_trans$tildeY_RR)[3,1]
  pYtildeRR_white<-rbind(pYtildeRR_white,ppt1_RR)
  pYtildeRR_black<-rbind(pYtildeRR_black,ppt2_RR)
  pYtildeRR_hispanic<-rbind(pYtildeRR_hispanic,ppt3_RR)
  
  mb_RR<-as.matrix(coef(svyglm(breastfeed~black+hispanic,design=dboot,family=quasi(link="log",variance="mu"))))[2:3,1]
  mYRR<-rbind(mYRR,mb_RR)
  mb_RD<-as.matrix(coef(svyglm(breastfeed~black+hispanic,design=dboot,family=quasi(link="identity",variance="constant"))))[2:3,1]
  mYRD<-rbind(mYRD,mb_RD)
  
  mbTilde_RR<-as.matrix(coef(svyglm(tildeY_RR~black+hispanic,design=dboot,family=quasi(link="log",variance="mu"))))[2:3,1]
  mYtildeRR<-rbind(mYtildeRR,mbTilde_RR)
  
  mbTilde_RD<-as.matrix(coef(svyglm(tildeY_RD~black+hispanic,design=dboot,family=quasi(link="identity",variance="constant"))))[2,1]
  mYtildeRD<-rbind(mYtildeRD,mbTilde_RD)
}
beep(8)
#sd(mod_boot);mean(mod_boot);nrow(mod_boot);hist(mod_boot)
print(Sys.time()-strt)

ext<-c("black")
exp(mean(mYRR[,ext]));exp(mean(mYtildeRR[,ext]))
mean(mYRD[,ext]);exp(mean(mYtildeRR[,ext]))



head(iterboot1_mean,25)

d1<-svydesign(id=~SECU, weight=~WGTQ1Q16, strata=~SEST,data=m,nest=T)
b1<-as.svrepdesign(d1,type="bootstrap",replicates=1000)
mod1<-svyglm(breastfeed~black,design=b1,family=quasibinomial(link="log"))
summary(mod1)

gg<-glm(GETPRENA~1,data=mr21)
summary(gg)


PNCAREWK_s1+PNCAREWK_s2+bfeed_pnc+bfeed_pnc_s1+bfeed_pnc_s2
+birthwgtl1+birthwgtl1_s1+birthwgtl1_s2+preghelp
+OUTCOME+BIRTHORD+FMARCON5+WANTRESP+EDUCMOM+nsfg+
  
  prglngthl1+usbirth+count_uid+count_cid+black+breastfeed+datendl1+datendl1_s1+datendl1_s2+
  AGER+AGER_s1+AGER_s2+BFEEDWKS+BFEEDWKS_s1+BFEEDWKS_s2+DATECON+DATECON_s1+DATECON_s2+DATEND+
  DATEND_s1+DATEND_s2+AGEPREG+AGEPREG_s1+AGEPREG_s2+PRGLNGTH+PRGLNGTH_s1+PRGLNGTH_s2+clust+
  clust_s1+clust_s2+time_arrive+time_arrive_s1+time_arrive_s2+ipi+ipi_s1+ipi_s2+csPREGORDR+
  csPREGORDR_s1+csPREGORDR_s2+csID+csID_s1+csID_s2+csSECU+csSECU_s1+csSECU_s2+csSEST+csSEST_s1+
  csSEST_s2+csWGT+csWGT_s1+csWGT_s2

# CHECK MODELS FOR GETPRENA AND PNCAREWK

#g<-glm(GETPRENA~id_centscale+outcomel1+datendl1+datendl1s1+datendl1s2
#                +university+highschool+FMARCON5+PRGLNGTH+PRGLNGTHs1+PRGLNGTHs2
#                +PRIORSMK+PAYDELIV,data=m,family=binomial())
#summary(g)
#h<-glm(PNCAREWK~id_centscale+outcomel1+parity+university+
#         highschool+POVERTY+POVERTYs1+POVERTYs2+AGER+AGERs1+AGERs2+
#         WANTRESP+FMARCON5+AGEPREG+AGEPREGs1+AGEPREGs2+PRIORSMK+bfeed_pnc+bfeed_pncs1+bfeed_pncs2+
#         PNCAREWKl1+PNCAREWKl1s1+PNCAREWKl1s2,data=m,family=gaussian)
#summary(h)

# IMPUTATION

# Subset Data by Racial Categories to Account for Interactions

mr10<-subset(m0,m0$HISPRACE==1);mr10$HISPRACE<-NULL;nrow(mr10);ncol(mr10)
mr20<-subset(m0,m0$HISPRACE==2);mr20$HISPRACE<-NULL;nrow(mr20);ncol(mr20)
mr30<-subset(m0,m0$HISPRACE==3);mr30$HISPRACE<-NULL;nrow(mr30);ncol(mr30)
mr40<-subset(m0,m0$HISPRACE==4);mr40$HISPRACE<-NULL;nrow(mr40);ncol(mr40)

iteration<-100;imputation<-50

nsfg_imp10 <- mice(mr10,pred=pMat,seed=123,maxit=iteration,m=imputation)
nsfg_imp20 <- mice(mr20,pred=pMat,seed=234,maxit=iteration,m=imputation)
nsfg_imp30 <- mice(mr30,pred=pMat,seed=345,maxit=iteration,m=imputation)
nsfg_imp40 <- mice(mr40,pred=pMat,seed=456,maxit=iteration,m=imputation)

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

