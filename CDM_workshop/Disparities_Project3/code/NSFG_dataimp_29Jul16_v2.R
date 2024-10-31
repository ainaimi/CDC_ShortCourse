### IMPORT AND EXAMINE MISSING NSFG DATA FOR SNMM DIRECT EFFECT PAPER

library(mice)
library(VIM)
library(splines)
library(survey)
library(sampling)
library(beepr)
library(epitools)
library(data.table)

m <- read.table(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/nsfg_dataimp_01Feb16.txt",sep="\t",header = T,na.strings=c("."))
table(m$HISPRACE)

sum(is.na(m$PNCAREWK));sum(is.na(m$BFEEDWKS));m$black<-as.factor(as.numeric(m$HISPRACE==3));m$hispanic<-as.factor(as.numeric(m$HISPRACE==1))
m$bfeed_pnc<-((m$BFEEDWKS-mean(m$BFEEDWKS))/sd(m$BFEEDWKS))*((m$PNCAREWK-mean(m$PNCAREWK, na.rm=TRUE))/sd(m$PNCAREWK, na.rm=TRUE))
m$HISPRACE<-as.factor(m$HISPRACE);sum(is.na(m$HISPRACE));m$white<-as.factor(as.numeric(m$HISPRACE==2));
m$breastfeed<-1-as.numeric(m$BFEEDWKS==0)
#table(m$breastfeed);mean(m$breastfeed)
table(m$highschool);mean(m$highschool)
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
m$breastfeedl1<-as.factor(1-as.numeric(m$BFEEDWKSl1==0));m$AGEPREG2<-m$AGEPREG;
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

imp_m<-m[,c("PREGORDR", "MULTBRTH", "BIRTHPLC", "PRIORSMK", "POSTSMKS", "GETPRENA", "OUTCOME","AGER",
            "BIRTHORD", "FMARCON5", "PAYDELIV", "MATERNLV", "WANTRESP","HISPRACE","BFEEDWKS","PNCAREWK",
            "EDUCMOM", "nsfg", "highschool", "university", "recallb", "outcomel1", "multbrthl1",
            "usbirth", "preghelp", "id12", "cd12", "black","SECU","SEST","WGTQ1Q16","white","AGEPREG",
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
aggr(imp_m,plot=T,sortVars=T,ylabs=c("Proportion Missing","Missing Patterns"),only.miss=T)
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
pMatrix2 <- read.table(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/pMatrix2c.txt",
                       sep="\t",header = T,row.names=1)
pMat<-as.matrix(pMatrix2)
head(pMat)
nrow(pMat);ncol(pMat)

pY_white<-pY_black<-pY_hispanic<-NULL
pYtilde_white<-pYtilde_black<-pYtilde_hispanic<-NULL
pY_white.H0<-pY_black.H0<-pY_hispanic.H0<-NULL
pY_white.H1<-pY_black.H1<-pY_hispanic.H1<-NULL
pYtilde_white.H0<-pYtilde_black.H0<-pYtilde_hispanic.H0<-NULL
pYtilde_white.H1<-pYtilde_black.H1<-pYtilde_hispanic.H1<-NULL
epitable.OR<-epitable.RR<-NULL
E.bf<-E.bfR<-E.bfH<-NULL
E.hsR<-E.hs<-NULL
E.gp<-E.gpR<-E.gpH<-NULL
E.ag<-E.agR<-E.agH<-NULL
E.ma<-E.maR<-E.maH<-NULL
E.R1<-E.R2<-E.R3<-NULL
PS_dat<-NULL

iterboot1_mean<-iterboot1_var<-NULL
iterboot2_mean<-iterboot2_var<-NULL
iterboot3_mean<-iterboot3_var<-NULL

strt<-Sys.time()
sest_size<-table(m0$SEST)
secu_size<-c(rep(2,36),rep(4,nrow(sest_size)-36))

rsampl <- function(B){
  lapply(seq_len(B),function(i) {
  #i<-12
  set.seed(i)
  ## TAKE A SINGLE STRATIFIED CLUSTER RE-SAMPLE FROM NSFG DATA WITH REPLACEMENT
  boot_id=mstage(m0, stage=list("stratified","cluster"),
                 varnames=list("SEST","SECU"),
                 size=list(sest_size,secu_size),
                 method=list("","srswr"))
  
  m_boot<-getdata(imp_m,boot_id)[[2]]
  names(m_boot)
  m_orderb<-as.matrix(sapply(m_boot, function(x) sum(is.na(x))))
  m_order1b <- names(m_orderb[order(-m_orderb[,1]), ])
  m_boot<-m_boot[,m_order1b]
  
  ## IMPUTE THE SINGLE RESAMPLE
  imputation<-1;iteration<-5
  mr1<-subset(m_boot,m_boot$HISPRACE==1)
  mr1<-mr1[c(-34,-102,-103,-104,-105)]
  mr2<-subset(m_boot,m_boot$HISPRACE==2)
  mr2<-mr2[c(-34,-102,-103,-104,-105)]
  mr3<-subset(m_boot,m_boot$HISPRACE==3)
  mr3<-mr3[c(-34,-102,-103,-104,-105)]
  
  seed1<-i*123;seed2<-i*234;seed3<-i*345
  nsfg_imp1 <- mice(mr1,pred=pMat,seed=seed1,maxit=iteration,m=imputation,diagnostics=T)
  nsfg_imp2 <- mice(mr2,pred=pMat,seed=seed2,maxit=iteration,m=imputation,diagnostics=T)
  nsfg_imp3 <- mice(mr3,pred=pMat,seed=seed3,maxit=iteration,m=imputation,diagnostics=T)
  
  exctract<-c("GETPRENA","PNCAREWK","MATERNLV")
  iterboot1_mean<-t(as.data.frame(nsfg_imp1$chainMean))[,exctract]
  row.names(iterboot1_mean)<-NULL;iterboot1_mean<-as.data.frame(iterboot1_mean)
  iterboot1_mean$ii0<-1;iterboot1_mean$iter<-cumsum(iterboot1_mean$ii0)
  iterboot1_mean$ii0<-NULL;iterboot1_mean$imput<-i
  data.frame(iterboot1_mean,stringsAsFactors=F)
  })
  do.call(rbind,iterboot1_mean)
}

rsampl(5)


  
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
  names(nsfg_imp)
  
  #####  DEFINE PRENATAL CARE
  nsfg_imp$GP<- ifelse(nsfg_imp$PNCAREWK>10|nsfg_imp$PNCAREWK==95,1,0)
  #as.numeric(nsfg_imp$GETPRENA==5)
  table(nsfg_imp$GP)
    #
  #####
  table(nsfg_imp$GP)
  tab=table(nsfg_imp$GP,nsfg_imp$breastfeed)
  dimnames(tab) <- list(PNC = c("No", "Yes"),
                        Breastfeed = c("No", "Yes"))
  tt1<-epitab(tab, method=c("oddsratio"), rev = "b", verbose = F)
  epitable.OR<-rbind(epitable.OR,tt1)
  tt2<-epitab(tab, method=c("riskratio"), rev = "b", verbose = F)
  epitable.RR<-rbind(epitable.RR,tt2)
#   hist(nsfg_imp$PNCAREWK)
#   abline(v=16,col="red")
#   sum(is.na(nsfg_imp$GP))
#   table(nsfg_imp$GP)
  dboot<-svydesign(id=~SECU, weight=~WGTQ1Q16, strata=~SEST,data=nsfg_imp,nest=T)
  options(survey.lonely.psu = "average")
#   nrow(dboot)
  ## TABLE FOR EDUCATION AND INCOME
  # summary(svytable(~highschool+PAYDELIV,dboot))
  
  svyglm(breastfeed~black+hispanic,design=dboot,family=quasibinomial(link="log"))
  
  ## PROPENSITY SCORE MODEL FOR PNC
  propensity1<-svyglm(GP~csHPAGELB+csHPAGELB_s1+csHPAGELB_s2+
                        csBFEEDWKSl1+csBFEEDWKSl1_s1+csBFEEDWKSl1_s2+
                        breastfeedl1+breastfeedl1*csBFEEDWKSl1+
                        PRIORSMK+POSTSMKS+PRIORSMK*POSTSMKS+PREGORDR+
                        WANTRESP+FMARCON5+cs_ipi+cs_ipi_s1+cs_ipi_s2+
                        cs_birthwgtl1+cs_birthwgtl1_s1+cs_birthwgtl1_s1+
                        black+hispanic+highschool,
                        design=dboot,family=quasibinomial(link="logit"))
  #summary(propensity1)
  print(i)
  print(length(propensity1$fitted.values))
  print(nrow(dboot))
  print(nrow(nsfg_imp))
  
  if(length(propensity1$fitted.values) != nrow(dboot)){
    pY_white<-rbind(pY_white,99)
    pY_black<-rbind(pY_black,99)
    pY_hispanic<-rbind(pY_hispanic,99)
    pYtilde_white<-rbind(pYtilde_white,99)
    pYtilde_black<-rbind(pYtilde_black,99)
    pYtilde_hispanic<-rbind(pYtilde_hispanic,99)
    pY_white.H0 <- rbind(pY_white.H0,99)
    pY_black.H0 <- rbind(pY_black.H0,99)
    pY_hispanic.H0 <- rbind(pY_hispanic.H0,99)
    pYtilde_white.H0 <- rbind(pYtilde_white.H0,99)
    pYtilde_black.H0 <- rbind(pYtilde_black.H0,99)
    pYtilde_hispanic.H0 <- rbind(pYtilde_hispanic.H0,99)
    PS_dat<-rbind(PS_dat,cbind(rep(99,nrow(dboot)),rep(99,nrow(dboot)),rep(99,nrow(dboot))))
    dat1<-as.data.frame(cbind(rep(99,nrow(dboot)),rep(99,nrow(dboot)),rep(99,nrow(dboot))))
    names(dat1)<-c("propensity","GP","B")
    lister<-list(PS_dat,dat1)
    PS_dat<-rbindlist(lister,use.names=T,fill=T)
  } else {
    
  nsfg_imp$pi_X1<-propensity1$fitted.values
  dat1<-as.data.frame(cbind(propensity1$fitted.values,nsfg_imp$GP,rep(i,nrow(nsfg_imp))))
  names(dat1)<-c("propensity","GP","B")
  lister<-list(PS_dat,dat1)
  PS_dat<-rbindlist(lister,use.names=T,fill=T)
  
#   pdf(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/PSOverlap.pdf",width=6,height=6)
#   hist(subset(nsfg_imp$pi_X1,nsfg_imp$GETPRENA==5),xaxs="i", yaxs="i",col=rgb(.1,.1,.1,0.5),xlim=c(0,1),ylim=c(0,20),breaks=seq(0,1,.05),probability=T,
#        main="",xlab="Propensity Score",las=1,tcl=-.1)
#   hist(subset(nsfg_imp$pi_X1,nsfg_imp$GETPRENA==1),col=rgb(.9,.9,.9,0.7),probability=T,add=T)
#   box()
#   dev.off()
  
  ## REDEFINE SAMPLING FRAME TO INCLUDE PS and GP
  dboot<-svydesign(id=~SECU, weight=~WGTQ1Q16, strata=~SEST,data=nsfg_imp,nest=T)
  options(survey.lonely.psu = "average")

  ## ESTIMATE PNC EFFECT USING DOUBLY ROBUST ESTIMATION, KEEP PNC PARAMETER ESTIMATES
  psi_mod<-svyglm(breastfeed~GP+GP*black+GP*hispanic+GP*highschool+GP*black*highschool+GP*hispanic*highschool
                  +pi_X1+pi_X1*black+pi_X1*hispanic+pi_X1*highschool
                  +black+hispanic+highschool+PAYDELIV
                  +csHPAGELB+csHPAGELB_s1+csHPAGELB_s2
                  +csBFEEDWKSl1+csBFEEDWKSl1_s1+csBFEEDWKSl1_s2
                  +PRIORSMK+POSTSMKS+PRIORSMK*POSTSMKS+PREGORDR
                  +WANTRESP+FMARCON5+cs_ipi+cs_ipi_s1+cs_ipi_s2
                  +cs_birthwgtl1+cs_birthwgtl1_s1+cs_birthwgtl1_s1
                  +black*PAYDELIV,
                  design=dboot,family=quasi(link = "identity", variance = "constant"))
  psi_RD<-as.matrix(coef(psi_mod))[c("GP","GP:black","GP:hispanic","GP:highschool","GP:black:highschool","GP:hispanic:highschool"),]
  X<-cbind(nsfg_imp$GP,nsfg_imp$GP*nsfg_imp$black,nsfg_imp$GP*nsfg_imp$hispanic,nsfg_imp$GP*nsfg_imp$highschool,nsfg_imp$GP*nsfg_imp$black*nsfg_imp$highschool,nsfg_imp$GP*nsfg_imp$hispanic*nsfg_imp$highschool) #
  
  ## CREATE TRANSFORMED OUTCOMES
  psi_RD<-as.matrix(psi_RD)
  nsfg_imp$tildeY_RD<-nsfg_imp$breastfeed-X%*%psi_RD
  
  ## REDEFINE SAMPLING FRAME TO INCLUDE TRANSFORMED OUTCOMES
  dboot<-svydesign(id=~SECU, weight=~WGTQ1Q16, strata=~SEST,data=nsfg_imp,nest=T)
  options(survey.lonely.psu = "average")
  
  ## COMPUTE THE MEAN OF Y BY RACE
  p_crude<-svyby(~breastfeed, ~black + hispanic, dboot, svymean)
  ppc1<-as.matrix(p_crude$breastfeed)[1,1]
  ppc2<-as.matrix(p_crude$breastfeed)[2,1]
  ppc3<-as.matrix(p_crude$breastfeed)[3,1]
  pY_white<-rbind(pY_white,ppc1)
  pY_black<-rbind(pY_black,ppc2)
  pY_hispanic<-rbind(pY_hispanic,ppc3)
  
  ## COMPUTE THE MEAN OF Y_TILDE BY RACIAL STATUS
  p_trans<-svyby(~tildeY_RD, ~black+hispanic, dboot, svymean)
  ppt1_RD<-as.matrix(p_trans$tildeY_RD)[1,1]
  ppt2_RD<-as.matrix(p_trans$tildeY_RD)[2,1]
  ppt3_RD<-as.matrix(p_trans$tildeY_RD)[3,1]
  pYtilde_white<-rbind(pYtilde_white,ppt1_RD)
  pYtilde_black<-rbind(pYtilde_black,ppt2_RD)
  pYtilde_hispanic<-rbind(pYtilde_hispanic,ppt3_RD)
  
  ## COMPUTE THE MEAN OF Y BY RACE AND EDUCATION
  p_crudeH<-svyby(~breastfeed, ~black + hispanic + highschool, dboot, svymean)
  ppc1a<-as.matrix(p_crudeH$breastfeed)[1,1]
  ppc2a<-as.matrix(p_crudeH$breastfeed)[2,1]
  ppc3a<-as.matrix(p_crudeH$breastfeed)[3,1]
  ppc1b<-as.matrix(p_crudeH$breastfeed)[4,1]
  ppc2b<-as.matrix(p_crudeH$breastfeed)[5,1]
  ppc3b<-as.matrix(p_crudeH$breastfeed)[6,1]
  pY_white.H0<-rbind(pY_white.H0,ppc1a)
  pY_black.H0<-rbind(pY_black.H0,ppc2a)
  pY_hispanic.H0<-rbind(pY_hispanic.H0,ppc3a)
  pY_white.H1<-rbind(pY_white.H1,ppc1b)
  pY_black.H1<-rbind(pY_black.H1,ppc2b)
  pY_hispanic.H1<-rbind(pY_hispanic.H1,ppc3b)
  
  ## COMPUTE THE MEAN OF Y_TILDE BY RACIAL AND EDUCATIONAL STATUS
  p_trans<-svyby(~tildeY_RD, ~black+hispanic+highschool, dboot, svymean)
  ppt1_RDa<-as.matrix(p_trans$tildeY_RD)[1,1]
  ppt2_RDa<-as.matrix(p_trans$tildeY_RD)[2,1]
  ppt3_RDa<-as.matrix(p_trans$tildeY_RD)[3,1]
  ppt1_RDb<-as.matrix(p_trans$tildeY_RD)[4,1]
  ppt2_RDb<-as.matrix(p_trans$tildeY_RD)[5,1]
  ppt3_RDb<-as.matrix(p_trans$tildeY_RD)[6,1]
  pYtilde_white.H0<-rbind(pYtilde_white.H0,ppt1_RDa)
  pYtilde_black.H0<-rbind(pYtilde_black.H0,ppt2_RDa)
  pYtilde_hispanic.H0<-rbind(pYtilde_hispanic.H0,ppt3_RDa)
  pYtilde_white.H1<-rbind(pYtilde_white.H1,ppt1_RDb)
  pYtilde_black.H1<-rbind(pYtilde_black.H1,ppt2_RDb)
  pYtilde_hispanic.H1<-rbind(pYtilde_hispanic.H1,ppt3_RDb)
  
  #AVERAGE BREASTFEEDING
  E.bf<-rbind(E.bf,svymean(~breastfeed, dboot)[1])
  E.bfR<-rbind(E.bfR,c(p_crude[1,4],p_crude[2,4],p_crude[3,4]))
  temp<-svyby(~breastfeed, ~highschool, dboot, svymean)
  E.bfH<-rbind(E.bfH,c(temp[1,2],temp[2,2]))
  
  #AVERAGE PRENATAL
  E.gp<-rbind(E.gp,svymean(~GP, dboot)[1])
  p_crudeGP<-svyby(~GP, ~black + hispanic, dboot, svymean)
  E.gpR<-rbind(E.gpR,c(p_crudeGP[1,3],p_crudeGP[2,3],p_crudeGP[3,3]))
  temp<-svyby(~GP, ~highschool, dboot, svymean)
  E.gpH<-rbind(E.gpH,c(temp[1,2],temp[2,2]))

  #AVERAGE HIGHSCHOOL
  E.hs<-rbind(E.hs,svymean(~highschool, dboot))
  temp<-svyby(~highschool, ~black+hispanic, dboot, svymean)
  E.hsR<-rbind(E.hsR,c(temp[1,3],temp[2,3],temp[3,3]))
  
  #RACIAL DISTRIBUTION
  E.R1<-rbind(E.R1,svymean(~as.numeric(white==1),dboot))
  E.R2<-rbind(E.R2,svymean(~black,dboot))
  E.R3<-rbind(E.R3,svymean(~hispanic,dboot))
  
  #AGE AT PREGNANCY
  E.ag<-rbind(E.ag,svymean(~AGEPREG, dboot))
  temp<-svyby(~AGEPREG, ~black+hispanic, dboot, svymean)
  E.agR<-rbind(E.agR,c(temp[1,3],temp[2,3],temp[3,3]))
  temp<-svyby(~AGEPREG, ~highschool, dboot, svymean)
  E.agH<-rbind(E.agH,c(temp[1,2],temp[2,2]))
  
  #MARITAL STATUS
  names(m_boot)
  E.ma<-rbind(E.ma,svymean(~as.numeric(FMARCON5==1), dboot))
  temp<-svyby(~FMARCON5, ~black+hispanic, dboot, svymean)
  E.maR<-rbind(E.maR,c(temp[1,3],temp[2,3],temp[3,3]))
  temp<-svyby(~FMARCON5, ~highschool, dboot, svymean)
  E.maH<-rbind(E.maH,c(temp[1,3],temp[2,3]))
  
  #AVERAGE HIGHSCHOOL
#   E.hs<-svymean(~highschool, dboot)
#   temp<-svyby(~highschool, ~black+hispanic, dboot, svymean)
#   E.hsR<-rbind(E.hsR,c(temp[1,3],temp[2,3],temp[3,3]))
  
  }
}
print(Sys.time()-strt)
beep(2)

sum(as.numeric(pY_black[,1]>90))
#

pY_black<-subset(pY_black,pY_black[,1]<90)
pY_white<-subset(pY_white,pY_white[,1]<90)
pY_hispanic<-subset(pY_hispanic,pY_hispanic[,1]<90)
pY_black.H0<-subset(pY_black.H0,pY_black.H0[,1]<90)
pY_white.H0<-subset(pY_white.H0,pY_white.H0[,1]<90)
pY_hispanic.H0<-subset(pY_hispanic.H0,pY_hispanic.H0[,1]<90)
pY_black.H1<-subset(pY_black.H1,pY_black.H1[,1]<90)
pY_white.H1<-subset(pY_white.H1,pY_white.H1[,1]<90)
pY_hispanic.H1<-subset(pY_hispanic.H1,pY_hispanic.H1[,1]<90)

pYtilde_black<-subset(pYtilde_black,pYtilde_black[,1]<90)
pYtilde_white<-subset(pYtilde_white,pYtilde_white[,1]<90)
pYtilde_hispanic<-subset(pYtilde_hispanic,pYtilde_hispanic[,1]<90)
pYtilde_black.H0<-subset(pYtilde_black.H0,pYtilde_black.H0[,1]<90)
pYtilde_white.H0<-subset(pYtilde_white.H0,pYtilde_white.H0[,1]<90)
pYtilde_hispanic.H0<-subset(pYtilde_hispanic.H0,pYtilde_hispanic.H0[,1]<90)
pYtilde_black.H1<-subset(pYtilde_black.H1,pYtilde_black.H1[,1]<90)
pYtilde_white.H1<-subset(pYtilde_white.H1,pYtilde_white.H1[,1]<90)
pYtilde_hispanic.H1<-subset(pYtilde_hispanic.H1,pYtilde_hispanic.H1[,1]<90)

results<-as.data.frame(cbind(pY_black,pY_white,pY_hispanic,pY_black.H0,pY_white.H0,pY_hispanic.H0,
                       pY_black.H1,pY_white.H1,pY_hispanic.H1,
                       pYtilde_black,pYtilde_white,pYtilde_hispanic,pYtilde_black.H0,pYtilde_white.H0,pYtilde_hispanic.H0,
                       pYtilde_black.H1,pYtilde_white.H1,pYtilde_hispanic.H1))
row.names(results)<-NULL
names(results)<-c("pY_black","pY_white","pY_hispanic","pY_black.H0","pY_white.H0","pY_hispanic.H0",
                  "pY_black.H1","pY_white.H1","pY_hispanic.H1",
                  "pYtilde_black","pYtilde_white","pYtilde_hispanic","pYtilde_black.H0","pYtilde_white.H0","pYtilde_hispanic.H0",
                  "pYtilde_black.H1","pYtilde_white.H1","pYtilde_hispanic.H1")
head(results)


results_T1<-as.data.frame(cbind(E.bf,E.bfR,E.bfH,E.gp,E.gpR,E.gpH,E.hs,E.hsR,E.R1,E.R2,E.R3,
                                E.ag,E.agR,E.agH,E.ma,E.maR,E.maH))
names(results_T1)<-c("E.bf","E.bfRw","E.bfRb","E.bfRh","E.bfH0","E.bfH1","E.gp","E.gpRw","E.gpRb","E.gpRh","E.gpH0","E.gpH1","E.hs","E.hsRw","E.hsRb","E.hsRh","E.R1","E.R2","E.R3",
                     "E.ag","E.agRw","E.agRb","E.agRh","E.agH0","E.agH1","E.ma","E.maRw","E.maRb","E.maRh","E.maH0","E.maH1")
head(results_T1)

write.table()

DM_RDbw <- pY_black - pY_white
DM_RDhw <- pY_hispanic - pY_white

DM_RDbw.H0 <- pY_black.H0 - pY_white.H0
DM_RDhw.H0 <- pY_hispanic.H0 - pY_white.H0

DM_RDbw.H1 <- pY_black.H1 - pY_white.H1
DM_RDhw.H1 <- pY_hispanic.H1 - pY_white.H1

DM_RRbw <- pY_black / pY_white
DM_RRhw <- pY_hispanic / pY_white

DM_RRbw.H0 <- pY_black.H0 / pY_white.H0
DM_RRhw.H0 <- pY_hispanic.H0 / pY_white.H0

DM_RRbw.H1 <- pY_black.H1 / pY_white.H1
DM_RRhw.H1 <- pY_hispanic.H1 / pY_white.H1

CDM1_RDw <- pYtilde_white - pY_white
CDM1_RDb <- pYtilde_black - pY_black
CDM1_RDh <- pYtilde_hispanic - pY_hispanic

CDM1_RDw.H0 <- pYtilde_white.H0 - pY_white.H0
CDM1_RDb.H0 <- pYtilde_black.H0 - pY_black.H0
CDM1_RDh.H0 <- pYtilde_hispanic.H0 - pY_hispanic.H0

CDM1_RDw.H1 <- pYtilde_white.H1 - pY_white.H1
CDM1_RDb.H1 <- pYtilde_black.H1 - pY_black.H1
CDM1_RDh.H1 <- pYtilde_hispanic.H1 - pY_hispanic.H1

CDM1_RRw <- pYtilde_white / pY_white
CDM1_RRb <- pYtilde_black / pY_black
CDM1_RRh <- pYtilde_hispanic / pY_hispanic

CDM1_RRw.H0 <- pYtilde_white.H0 / pY_white.H0
CDM1_RRb.H0 <- pYtilde_black.H0 / pY_black.H0
CDM1_RRh.H0 <- pYtilde_hispanic.H0 / pY_hispanic.H0

CDM1_RRw.H1 <- pYtilde_white.H1 / pY_white.H1
CDM1_RRb.H1 <- pYtilde_black.H1 / pY_black.H1
CDM1_RRh.H1 <- pYtilde_hispanic.H1 / pY_hispanic.H1

CDM2_RDbw <- pYtilde_black - pYtilde_white
CDM2_RDhw <- pYtilde_hispanic - pYtilde_white

CDM2_RDbw.H0 <- pYtilde_black.H0 - pYtilde_white.H0
CDM2_RDhw.H0 <- pYtilde_hispanic.H0 - pYtilde_white.H0

CDM2_RDbw.H1 <- pYtilde_black.H1 - pYtilde_white.H1
CDM2_RDhw.H1 <- pYtilde_hispanic.H1 - pYtilde_white.H1

CDM2_RRbw <- pYtilde_black / pYtilde_white
CDM2_RRhw <- pYtilde_hispanic / pYtilde_white

CDM2_RRbw.H0 <- pYtilde_black.H0 / pYtilde_white.H0
CDM2_RRhw.H0 <- pYtilde_hispanic.H0 / pYtilde_white.H0

CDM2_RRbw.H1 <- pYtilde_black.H1 / pYtilde_white.H1
CDM2_RRhw.H1 <- pYtilde_hispanic.H1 / pYtilde_white.H1

mean(DM_RDbw);mean(CDM2_RDbw)
mean(DM_RDbw.H0);mean(CDM2_RDbw.H0)
mean(DM_RDbw.H1);mean(CDM2_RDbw.H1)

mean(DM_RDhw);mean(CDM2_RDhw)
mean(DM_RDhw.H0);mean(CDM2_RDhw.H0)
mean(DM_RDhw.H1);mean(CDM2_RDhw.H1)

mean(CDM1_RDb);mean(CDM1_RDw);mean(CDM1_RDh)
mean(CDM1_RDb.H0);mean(CDM1_RDw.H0);mean(CDM1_RDh.H0)
mean(CDM1_RDb.H1);mean(CDM1_RDw.H1);mean(CDM1_RDh.H1)

# TABLE 1
bf1<-cbind(mean(E.bfR[,1]),quantile(E.bfR[,1],.025),quantile(E.bfR[,1],.975))
bf2<-cbind(mean(E.bfR[,2]),quantile(E.bfR[,2],.025),quantile(E.bfR[,2],.975))
bf3<-cbind(mean(E.bfR[,3]),quantile(E.bfR[,3],.025),quantile(E.bfR[,3],.975))

gp1<-cbind(mean(E.gpR[,1]),quantile(E.gpR[,1],.025),quantile(E.gpR[,1],.975))
gp2<-cbind(mean(E.gpR[,2]),quantile(E.gpR[,2],.025),quantile(E.gpR[,2],.975))
gp3<-cbind(mean(E.gpR[,3]),quantile(E.gpR[,3],.025),quantile(E.gpR[,3],.975))

hs1<-cbind(mean(E.hsR[,1]),quantile(E.hsR[,1],.025),quantile(E.hsR[,1],.975))
hs2<-cbind(mean(E.hsR[,2]),quantile(E.hsR[,2],.025),quantile(E.hsR[,2],.975))
hs3<-cbind(mean(E.hsR[,3]),quantile(E.hsR[,3],.025),quantile(E.hsR[,3],.975))

ag1<-cbind(mean(E.agR[,1]),quantile(E.agR[,1],.025),quantile(E.agR[,1],.975))
ag2<-cbind(mean(E.agR[,2]),quantile(E.agR[,2],.025),quantile(E.agR[,2],.975))
ag3<-cbind(mean(E.agR[,3]),quantile(E.agR[,3],.025),quantile(E.agR[,3],.975))

ma1<-cbind(mean(E.maR[,1]),quantile(E.maR[,1],.025),quantile(E.maR[,1],.975))
ma2<-cbind(mean(E.maR[,2]),quantile(E.maR[,2],.025),quantile(E.maR[,2],.975))
ma3<-cbind(mean(E.maR[,3]),quantile(E.maR[,3],.025),quantile(E.maR[,3],.975))

#   r1 r2 r3 
#   bf bf bf
#   gp gp gp
#   hs hs hs 
#   ag ag ag
#   ma ma ma

# head(iterboot1_mean,25)
# library(ggplot2)
# p1 <- ggplot(data = iterboot1_mean, aes(x = iter, y = GETPRENA, group = imput)) + geom_line() + guides(colour=FALSE) + aes(alpha=.01,colour = factor(imput)) + theme(legend.position="none")
# p2 <- ggplot(data = iterboot1_mean, aes(x = iter, y = PNCAREWK, group = imput)) + geom_line() + guides(colour=FALSE) + aes(alpha=.01,colour = factor(imput)) + theme(legend.position="none")
# p3 <- ggplot(data = iterboot1_mean, aes(x = iter, y = MATERNLV, group = imput)) + geom_line() + guides(colour=FALSE) + aes(alpha=.01,colour = factor(imput)) + theme(legend.position="none")
# sum(as.numeric(pYtilde_white==99))

## DISPARITY MEASURES
# FIGURE 1
#split screen in two columns:
#pdf(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/F1.pdf",width=11.5,height=8)
col1<-"black"#rgb(.1,.1,.1,0.5)
col2<-"white"#rgb(.9,.9,.9,0.7)
ylims<-c(0,30);xlims_rd<-c(-.25,.1);xlims_rr<-c(-1,.2)
y.point<-25
#split.screen(c(1,2))
#screen(1)
## risk difference
hist(DM_RDbw,
     xaxs="i",yaxs="i",main="",col=col1,
     ylim=ylims,xlim=xlims_rd,
     xlab="Risk Difference",las=1,tcl=-.1) ## DARK
points(mean(DM_RDbw),y.point,col=col1,pch=20,cex=2)
segments(quantile(DM_RDbw,.025),y.point,quantile(DM_RDbw,.975),y.point,col=col1)
segments(quantile(DM_RDbw,.025),y.point+.5,quantile(DM_RDbw,.025),y.point-.5,col=col1)
segments(quantile(DM_RDbw,.975),y.point+.5,quantile(DM_RDbw,.975),y.point-.5,col=col1)
hist(DM_RDhw,
     ylim=ylims,xlim=xlims_rd,
     col=col2,add=T) ## LIGHT
col2<-"black"
points(mean(DM_RDhw),y.point,col=col2,pch=20,cex=2)
segments(quantile(DM_RDhw,.025),y.point,quantile(DM_RDhw,.975),y.point,col=col2)
segments(quantile(DM_RDhw,.025),y.point+.5,quantile(DM_RDhw,.025),y.point-.5,col=col2)
segments(quantile(DM_RDhw,.975),y.point+.5,quantile(DM_RDhw,.975),y.point-.5,col=col2)
box()
#screen(2)
## risk ratio
hist(log(DM_RRbw),
     xaxs="i",yaxs="i",main="",col=col1,
     ylim=ylims,xlim=xlims_rr,
     xlab="log Risk Ratio",las=1,tcl=-.1) ## DARK
points(mean(log(DM_RRbw)),y.point,col=col1,pch=20,cex=2)
segments(quantile(log(DM_RRbw),.025),y.point,quantile(log(DM_RRbw),.975),y.point,col=col1)
segments(quantile(log(DM_RRbw),.025),y.point+.5,quantile(log(DM_RRbw),.025),y.point-.5,col=col1)
segments(quantile(log(DM_RRbw),.975),y.point+.5,quantile(log(DM_RRbw),.975),y.point-.5,col=col1)
col2<-"white"
hist(log(DM_RRhw),
     ylim=ylims,xlim=xlims_rr,
     col=col2,add=T) ## LIGHT
col2<-"black"
points(mean(log(DM_RRhw)),y.point,col=col2,pch=20,cex=2)
segments(quantile(log(DM_RRhw),.025),y.point,quantile(log(DM_RRhw),.975),y.point,col=col2)
segments(quantile(log(DM_RRhw),.025),y.point+.5,quantile(log(DM_RRhw),.025),y.point-.5,col=col2)
segments(quantile(log(DM_RRhw),.975),y.point+.5,quantile(log(DM_RRhw),.975),y.point-.5,col=col2)
box()
#close.screen(all = TRUE) 
#dev.off()

# FIGURE 2
#split screen in two columns:
#pdf(file="~/Dropbox/Documents/Research/Papers/TestingFundamentalCause/F2.pdf",width=11.5,height=8)
col1<-rgb(.1,.1,.1,0.25)
col2<-rgb(.9,.9,.9,0.25)
col3<-rgb(.1,.9,.9,0.25)
ylims<-c(0,30);xlims_rd<-c(-.15,.1);xlims_rr<-c(-1,.2)
y.point<-25
#split.screen(c(1,2))
#screen(1)
## risk difference
hist(CDM1_RDw,
     xaxs="i",yaxs="i",main="",col=col1,
     ylim=ylims,xlim=xlims_rd,
     xlab="Risk Difference",las=1,tcl=-.1) ## DARK
points(mean(CDM1_RDw),y.point,col=col1,pch=20,cex=2)
segments(quantile(CDM1_RDw,.025),y.point,quantile(CDM1_RDw,.975),y.point,col=col1)
segments(quantile(CDM1_RDw,.025),y.point+.5,quantile(CDM1_RDw,.025),y.point-.5,col=col1)
segments(quantile(CDM1_RDw,.975),y.point+.5,quantile(CDM1_RDw,.975),y.point-.5,col=col1)
hist(CDM1_RDh,
     ylim=ylims,xlim=xlims_rd,
     col=col2,add=T) ## LIGHT
points(mean(CDM1_RDh),y.point,col=col2,pch=20,cex=2)
segments(quantile(CDM1_RDh,.025),y.point,quantile(CDM1_RDh,.975),y.point,col=col2)
segments(quantile(CDM1_RDh,.025),y.point+.5,quantile(CDM1_RDh,.025),y.point-.5,col=col2)
segments(quantile(CDM1_RDh,.975),y.point+.5,quantile(CDM1_RDh,.975),y.point-.5,col=col2)
hist(CDM1_RDb,
     ylim=ylims,xlim=xlims_rd,
     col=col3,add=T) ## LIGHT
points(mean(CDM1_RDb),y.point,col=col3,pch=20,cex=2)
segments(quantile(CDM1_RDb,.025),y.point,quantile(CDM1_RDb,.975),y.point,col=col3)
segments(quantile(CDM1_RDb,.025),y.point+.5,quantile(CDM1_RDb,.025),y.point-.5,col=col3)
segments(quantile(CDM1_RDb,.975),y.point+.5,quantile(CDM1_RDb,.975),y.point-.5,col=col3)
box()
#screen(2)
## risk ratio
hist(log(DM_RRbw),
     xaxs="i",yaxs="i",main="",col=col1,
     ylim=ylims,xlim=xlims_rr,
     xlab="log Risk Ratio",las=1,tcl=-.1) ## DARK
points(mean(log(DM_RRbw)),y.point,col=col1,pch=20,cex=2)
segments(quantile(log(DM_RRbw),.025),y.point,quantile(log(DM_RRbw),.975),y.point,col=col1)
segments(quantile(log(DM_RRbw),.025),y.point+.5,quantile(log(DM_RRbw),.025),y.point-.5,col=col1)
segments(quantile(log(DM_RRbw),.975),y.point+.5,quantile(log(DM_RRbw),.975),y.point-.5,col=col1)
col2<-"white"
hist(log(DM_RRhw),
     ylim=ylims,xlim=xlims_rr,
     col=col2,add=T) ## LIGHT
col2<-"black"
points(mean(log(DM_RRhw)),y.point,col=col2,pch=20,cex=2)
segments(quantile(log(DM_RRhw),.025),y.point,quantile(log(DM_RRhw),.975),y.point,col=col2)
segments(quantile(log(DM_RRhw),.025),y.point+.5,quantile(log(DM_RRhw),.025),y.point-.5,col=col2)
segments(quantile(log(DM_RRhw),.975),y.point+.5,quantile(log(DM_RRhw),.975),y.point-.5,col=col2)
box()
#close.screen(all = TRUE) 
#dev.off()
