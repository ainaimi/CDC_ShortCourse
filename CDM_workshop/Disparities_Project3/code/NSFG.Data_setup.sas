********************************************************************************
******** Setting up NSFG Data for Testing Fundamental Cause Analysis
******** Program created Jan 2016
******** Author: Ashley Isaac Naimi
;
ods select all;
%inc "c:\ain\header2.txt";
%inc "Y:\Dropbox\Documents\Research\Data\NSFG\2006_2010_FemPregSetup.sas";
%inc "Y:\Dropbox\Documents\Research\Data\NSFG\2006_2010_FemRespSetup.sas";
%inc "Y:\Dropbox\Documents\Research\Data\NSFG\2011_2013_FemPregSetup.sas";
%inc "Y:\Dropbox\Documents\Research\Data\NSFG\2011_2013_FemRespSetup.sas";

libname NSFG "Y:\Dropbox\Documents\Research\Data\NSFG";
*create dataset from female respondent file to merge with female pregnancy file;
data FemResp_0610;
	set NSFG.FemResp_0610;
	keep caseid educmom cmhsgrad cmbagrad menarche cmpgvis1 BRNOUT yrstrus
		 ANYPRGHP cmpidlst;
run;
*create dataset from female respondent file to merge with female pregnancy file;
data FemResp_1113;
	set NSFG.FemResp_1113;
	keep caseid educmom cmhsgrad cmbagrad menarche cmpgvis1 BRNOUT yrstrus
		 ANYPRGHP cmpidlst;
run;
proc print data=FemResp_0610 (obs=10);
proc print data=FemResp_1113 (obs=10);run;
proc sort data=FemResp_0610;by caseid;
proc sort data=FemResp_1113;by caseid;
proc sort data=NSFG.FemPreg_0610;by caseid;
proc sort data=NSFG.FemPreg_1113;by caseid;
data NSFG.FemPreg_0610;
	merge NSFG.FemPreg_0610 (in=a) FemResp_0610;
	by caseid;
	if a;
	nsfg=0;
	keep caseid educmom cmhsgrad cmbagrad menarche cmpgvis1 BRNOUT yrstrus cmpidlst cmpgvis1 caseid pregordr 
		 datend outcome multbrth birthwgt_lb1 HPAGELB birthplc getprena prglngth datecon  bpa_bdscheck1
		 paydeliv CMJAN5YR wantresp MENARCHE PRIORSMK hisprace wgtq1q16 agepreg ager cmintvw sest secu 
		 POSTSMKS cmkidied1 cmkidied2 cmkidied3 fmarcon5 educmom PNCAREWK nsfg pregend1  pregend2  pregend3
		 BFEEDWKS BIRTHORD ANYPRGHP alivenow1 alivenow2 alivenow3 MATERNLV cmbirth;
data NSFG.FemPreg_1113;
	merge NSFG.FemPreg_1113 (in=a) FemResp_1113;
	by caseid;
	if a;
	nsfg=1;
	PREGEND3=.;
	keep caseid educmom cmhsgrad cmbagrad menarche cmpgvis1 BRNOUT yrstrus cmpidlst cmpgvis1 caseid pregordr 
		 datend outcome multbrth birthwgt_lb1 HPAGELB birthplc getprena prglngth datecon  bpa_bdscheck1
		 paydeliv CMJAN5YR wantresp MENARCHE PRIORSMK hisprace WGT2011_2013 agepreg ager cmintvw sest secu 
		 POSTSMKS cmkidied1 cmkidied2 cmkidied3 fmarcon5 educmom PNCAREWK nsfg  pregend1  pregend2  pregend3
		 BFEEDWKS BIRTHORD ANYPRGHP alivenow1 alivenow2 alivenow3 MATERNLV cmbirth;
	rename WGT2011_2013=wgtq1q16;
run;quit;run;

*NSFG.FemPreg was obtained from: ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/sas/;
proc sort data=NSFG.FemPreg_0610;by caseid pregordr;run;
proc sort data=NSFG.FemPreg_1113;by caseid pregordr;run;
data nsfg;	set NSFG.FemPreg_0610 NSFG.FemPreg_1113;run;
data nsfg;
	set nsfg;
	if cmhsgrad=. then cmhsgrad=99999;
	if cmbagrad=. then cmbagrad=99999;
	highschool = (datend ge cmhsgrad);
	university = (datend ge cmbagrad);
	if datend = . then do;
		highschool=.;university=.;
	end;
	multbrth = (MULTBRTH=1);
	livebirth = (outcome=1);
	recallb=(datecon>=CMJAN5YR);
	yearcon=1900+int((datecon-1)/12);
	intyear=1900+int((CMINTVW-1)/12);
	b1=intyear-yearcon;
	recallb1 = (b1 le 10);
	yearend = 1900+int((datend-1)/12);
run;
data nsfg;
	set nsfg;
	*create lagged variables;
	by caseid pregordr;
	retain hdatend datendl1 
		   houtcome outcomel1 
		   hmultbrth multbrthl1 
		   hBFEEDWKS BFEEDWKSl1 
		   hPNCAREWK PNCAREWKl1
		   hprglngth prglngthl1
		   hbirthwgt birthwgtl1;
	if first.caseid then do;
		hdatend=.; datendl1=.;
		houtcome=.; outcomel1=.;
		hmultbrth=.; multbrthl1=.;
		hBFEEDWKS=.; BFEEDWKSl1=.;
		hPNCAREWK=.; PNCAREWKl1=.;
		hprglngth=.; prglngthl1=.;
		hbirthwgt=.; birthwgtl1=.;
	end;
		datendl1 = hdatend;
		hdatend = datend;
		outcomel1 = houtcome;
		houtcome = outcome;
		multbrthl1 = hmultbrth;
		hmultbrth = multbrth;
		BFEEDWKSl1 = hBFEEDWKS;
		hBFEEDWKS = BFEEDWKS;
		PNCAREWKl1 = hPNCAREWK;
		hPNCAREWK = PNCAREWK;
		prglngthl1 = hprglngth;
		hprglngth = prglngth;
		birthwgtl1 = hbirthwgt;
		hbirthwgt = birthwgt_lb1;
run;
data nsfg;
	set nsfg;
	by caseid pregordr;
	if first.caseid then do;
		datendl1=0;outcomel1=0;multbrthl1=0;bfeedwksl1=0;pncarewkl1=0;prglngthl1=0;birthwgtl1=0;
	end;
	usbirth=1;
	if brnout=1 and (yearend<yrstrus) then usbirth=0;

	preghelp=0;
	if cmpgvis1 ne . and datecon > cmpgvis1 then preghelp=1;
	if cmpgvis1=9999 then preghelp=.;

	if hpagelb in (98,99) then hpagelb=.;

	if birthwgt_lb1=. then birthwgt_lb1=98;
	if birthwgt_lb1=99 then birthwgt_lb1=.;
	if birthwgt_lb1=98 then birthwgt_lb1=99;
	if outcome = 1 and birthwgt_lb1=99 then birthwgt_lb1=.;

	if priorsmk=. then priorsmk=99;
	if priorsmk=8 then priorsmk=.;
	if outcome ne 2 and bpa_bdscheck1 ne 1 and priorsmk=99 then PRIORSMK=.;

	if postsmks=. then postsmks=99;
	if postsmks in (8,9) then postsmks=.;
	if outcome ne 2 and bpa_bdscheck1 ne 1 and postsmks=99 then postsmks=.;

	if paydeliv=. then paydeliv=99;
	if outcome=1 and paydeliv=99 then paydeliv=.;

	if cmkidied1=. then cmkidied1=99;
	if alivenow1=9 then cmkidied1=.;

	if cmkidied2=. then cmkidied2=99;
	if alivenow2=9 then cmkidied2=.;

	if cmkidied3=. then cmkidied3=99;
	if alivenow3=9 then cmkidied3=.;

	if educmom=95 then educmom=99;

	if BFEEDWKS=. then BFEEDWKS=9999;
	if BFEEDWKS=995 then BFEEDWKS=0;
	*if BFEEDWKS=994 then BFEEDWKS=9999;

	if BFEEDWKSl1=. then BFEEDWKSl1=9999;
	if BFEEDWKSl1=995 then BFEEDWKSl1=0;
	*if BFEEDWKS=994 then BFEEDWKS=9999;

	*if getprena=. then getprena=99;
	if getprena in (8,9) then getprena=.;
	*if (pregend1 ne 3|pregend2 ne 3|pregend3 ne 3) and BPA_BDScheck1 ne 1 and getprena=99 then getprena=.;

	if MATERNLV=. then MATERNLV=99;
	if outcome=1 and bpa_bdscheck1 ne 1 and bpa_bdscheck1 ne 2 and maternlv=99 then maternlv=.;

	if getprena=1 and pncarewk=95 then do; getprena=.;pncarewk=.;end;

	death_age1 = cmkidied1;
	if cmkidied1=99 then death_age1=9999;
	if cmkidied1 ne 99 and cmkidied1 ne . then death_age1 = cmkidied1-datend;

	*indicator of infant and child death;
	inf_death1=0;
	if death_age1 ne . and death_age1 le 12 then inf_death1=1;
	if death_age1=. then inf_death1=.;

	child_death1=0;
	if death_age1 ne . and death_age1 ne 9999 and death_age1 > 12 then child_death1=1;
	if death_age1 =. then child_death1=.;


	death_age2 = cmkidied2;
	if cmkidied2=99 then death_age2=9999;
	if cmkidied2 ne 99 and cmkidied2 ne . then death_age2 = cmkidied2-datend;

	*indicator of infant and child death;
	inf_death2=0;
	if death_age2 ne . and death_age2 le 12 then inf_death2=1;
	if death_age2=. then inf_death2=.;

	child_death2=0;
	if death_age2 ne . and death_age2 ne 9999 and death_age2 > 12 then child_death2=1;
	if death_age2 =. then child_death2=.;


	death_age3 = cmkidied3;
	if cmkidied3=99 then death_age3=9999;
	if cmkidied3 ne 99 and cmkidied3 ne . then death_age3 = cmkidied3-datend;

	*indicator of infant and child death;
	inf_death3=0;
	if death_age3 ne . and death_age3 le 12 then inf_death3=1;
	if death_age3=. then inf_death3=.;

	child_death3=0;
	if death_age3 ne . and death_age3 ne 9999 and death_age3 > 12 then child_death3=1;
	if death_age3 =. then child_death3=.;

	drop hdatend houtcome hBFEEDWKS hPNCAREWK hprglngth hbirthwgt hmultbrth;
run;
data cd1;
	set nsfg;
	where cmkidied1 ne 99 and cmkidied1 ne .;
	keep caseid cmkidied1 inf_death1 child_death1;
proc sort data=cd1;by caseid;run;
data cd1;
	set cd1;
	by caseid;
	retain count;
	if first.caseid then count=1;
	else count+1;
data cd10;
	array dd0[3] cmdied1_1-cmdied1_3;
	array dd1[3] infd1_1-infd1_3;
	array dd2[3] chd1_1-chd1_3;
	do i = 1 to 3 until (last.caseid);
		set cd1;
		by caseid;
		dd0[count] = cmkidied1;
		dd1[count] = inf_death1;
		dd2[count] = child_death1;
		if last.caseid then output;
	end;
	keep caseid cmdied1_1-cmdied1_3 infd1_1-infd1_3 chd1_1-chd1_3;
data cd2;
	set nsfg;
	where cmkidied2 ne 99 and cmkidied2 ne .;
	keep caseid cmkidied2 inf_death2 child_death2;
proc sort data=cd2;by caseid;run;
data cd2;
	set cd2;
	by caseid;
	retain count;
	if first.caseid then count=1;
	else count+1;
data cd20;
	array dd0[3] cmdied2_1-cmdied2_3;
	array dd1[3] infd2_1-infd2_3;
	array dd2[3] chd2_1-chd2_3;
	do i = 1 to 3 until (last.caseid);
		set cd2;
		by caseid;
		dd0[count] = cmkidied2;
		dd1[count] = inf_death2;
		dd2[count] = child_death2;
		if last.caseid then output;
	end;
	keep caseid cmdied2_1 infd2_1 chd2_1;
data cd30;
	set nsfg;
	where cmkidied3 ne 99 and cmkidied3 ne .;
	keep caseid cmkidied3 inf_death3 child_death3;
	rename cmkidied3=cmdied3_1 inf_death3=infd3_1 child_death3=chd3_1;
proc print data=cd10;
proc print data=cd20;
proc print data=cd30;
run;quit;run;
data nsfg;
	merge nsfg cd10 cd20 cd30;
	by caseid;
	array dd[*] cmdied1_1-cmdied1_3 infd1_1-infd1_3 chd1_1-chd1_3 cmdied2_1 infd2_1 chd2_1 cmdied3_1 infd3_1 chd3_1;
	do ii = 1 to 15;
		if dd[ii] = . and cmkidied1 ne . then dd[ii]=99;
		if cmkidied1 = . then dd[ii]=.;
	end;

	id12=0;
	if datecon - cmdied1_1 le 12 and datecon - cmdied1_1 ge 0 and infd1_1 = 1 then id12=1;
		if datecon - cmdied1_2 le 12 and datecon - cmdied1_2 ge 0 and infd1_2 = 1 then id12=1;
			if datecon - cmdied1_3 le 12 and datecon - cmdied1_3 ge 0 and infd1_3 = 1 then id12=1;
	if datecon - cmdied2_1 le 12 and datecon - cmdied2_1 ge 0 and infd2_1 = 1 then id12=1;
		if datecon - cmdied2_2 le 12 and datecon - cmdied2_2 ge 0 and infd2_2 = 1 then id12=1;
			if datecon - cmdied2_3 le 12 and datecon - cmdied2_3 ge 0 and infd2_3 = 1 then id12=1;
	if datecon - cmdied3_1 le 12 and datecon - cmdied3_1 ge 0 and infd3_1 = 1 then id12=1;
		if datecon - cmdied3_2 le 12 and datecon - cmdied3_2 ge 0 and infd3_2 = 1 then id12=1;
			if datecon - cmdied3_3 le 12 and datecon - cmdied3_3 ge 0 and infd3_3 = 1 then id12=1;
	if death_age1=. then id12=.;

	cd12=0;
	if datecon - cmdied1_1 le 12 and datecon - cmdied1_1 ge 0 and chd1_1 = 1 then cd12=1;
		if datecon - cmdied1_2 le 12 and datecon - cmdied1_2 ge 0 and chd1_2 = 1 then cd12=1;
			if datecon - cmdied1_3 le 12 and datecon - cmdied1_3 ge 0 and chd1_3 = 1 then cd12=1;
	if datecon - cmdied2_1 le 12 and datecon - cmdied2_1 ge 0 and chd2_1 = 1 then cd12=1;
		if datecon - cmdied2_2 le 12 and datecon - cmdied2_2 ge 0 and chd2_2 = 1 then cd12=1;
			if datecon - cmdied2_3 le 12 and datecon - cmdied2_3 ge 0 and chd2_3 = 1 then cd12=1;
	if datecon - cmdied3_1 le 12 and datecon - cmdied3_1 ge 0 and chd3_1 = 1 then cd12=1;
		if datecon - cmdied3_2 le 12 and datecon - cmdied3_2 ge 0 and chd3_2 = 1 then cd12=1;
			if datecon - cmdied3_3 le 12 and datecon - cmdied3_3 ge 0 and chd3_3 = 1 then cd12=1;
	if death_age1=. then cd12=.;
	drop ii cmdied1_1-cmdied1_3 infd1_1-infd1_3 chd1_1-chd1_3 cmdied2_1 infd2_1 chd2_1 cmdied3_1 infd3_1 chd3_1
		 cmdied2_2 infd2_2 cmdied2_3 infd2_3 cmdied3_2 infd3_2 cmdied3_3 infd3_3
		  chd2_2 chd2_3 chd3_2 chd3_3;
run;quit;run;
data nsfg;
	set nsfg;
	if outcome=1;if multbrth=0;if usbirth=1;

	if bfeedwks=9999 or bfeedwks=994 then delete;
	if bfeedwksl1=9999 or bfeedwksl1=994 then bfeedwksl1=0;
	*if pncarewk=95 then pncarewk=0;

	time_arrive=0;
	if brnout=1 then time_arrive = yearcon-YRSTRUS;
	if brnout in (8,9) then time_arrive = .;
	if time_arrive<0 then time_arrive=0;

	ipi=0;
	if pregordr>1 then ipi = datecon-datendl1;
	if ipi < 0 then ipi=0;

	drop pregend1-pregend3 livebirth ALIVENOW1-ALIVENOW3 CMKIDIED1-CMKIDIED3 brnout YRSTRUS CMINTVW CMJAN5YR CMHSGRAD CMBAGRAD MENARCHE CMPGVIS1 CMPIDLST
	ANYPRGHP yearcon intyear b1 yearend death_age1 inf_death1 child_death1 death_age2 inf_death2 child_death2 death_age3 inf_death3
	child_death3 BPA_BDSCHECK1;
run;

data nsfg;
	set nsfg;
	by caseid;
	if first.caseid then count_uid=1;else count_uid=0;
	count_cid=1;run;
proc means data=nsfg sum;
title "Total Number of Women / Pregnancies in Restrited Data";
var count_uid count_cid;run;
run;quit;run;
data nsfg;
	set nsfg;
	if hisprace=4 then delete;
data nsfg;
	set nsfg;
	by caseid;
	if first.caseid then count_uid=1;else count_uid=0;
	count_cid=1;run;
proc means data=nsfg sum;
title "Total Number of Women / Pregnancies in Restrited Data";
var count_uid count_cid;run;
data nsfg;
	set nsfg;
	if recallb1=1;
	drop recallb1;
data nsfg;
	set nsfg;
	by caseid;
	if first.caseid then count_uid=1;else count_uid=0;
	count_cid=1;run;
proc means data=nsfg sum;
title "Total Number of Women / Pregnancies in Restrited Data";
var count_uid count_cid;run;
proc means data=nsfg sum;where recallb=1;
title "Total Number of Women / Pregnancies in Restrited Data, recallb=1";
var count_uid count_cid;
run;quit;run;

proc contents data=nsfg;run;
proc surveymeans data=nsfg;
strata SEST;
cluster SECU;
weight WGTQ1Q16;
var agepreg;
run;
proc surveyfreq data=nsfg;
strata SEST;
cluster SECU;
weight WGTQ1Q16;
table hisprace;
table FMARCON5;
table highschool;
run;

proc means data=nsfg min max sum nmiss maxdec=2;run;
proc univariate data=nsfg;var time_arrive ipi;histogram;run;

libname dd "X:\Documents\Research\Papers\TestingFundamentalCause";
data dd.nsfg_data2;set work.nsfg;run;

/**Export to impute in R using MICE;*/
proc export data=work.nsfg
   outfile='X:\Documents\Research\Papers\TestingFundamentalCause\nsfg_dataimp_01Feb16.txt'
   dbms=tab replace; 
run;







