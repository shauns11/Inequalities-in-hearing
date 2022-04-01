* Encoding: UTF-8.

*Socioeconomic differences in hearing among middle-aged and older adults:
*cross-sectional analyses using the Health Survey for England

dataset close all.
GET FILE='N:\Hearing\hse2014ai.sav'
/keep pserial age90 sex 
psu cluster
Hear hrwrry
BestEar hrgrp hrtv
difhraid hrRight hrleft
HrModGt
HrModDeaf
MaxDiff
HrTest12
HrAidUse
LeftSumm1khz
RightSumm1khz
LeftSumm3khz
RightSumm3khz
HrBetter1KHZ
HrBetter3KHZ Wemwbs10 age85 wt_nurse HeHNoise NUROUTC
HeHCI	HeHEI eqv5 qimd genhelf
ILL12m
ill12m1
ill12m2
ill12m3
ill12m4
ill12m5
ill12m6 bmival2 bmivg5 medcnjd
NoVig
DaysVigCX1
DaysVigCX2
DaysVigCX3
DaysVigCX4
DaysVigCX5
DaysVigCX6
DaysVigCX7
TVigHou
TVigMin
NoMod
DaysModCX1
DaysModCX2
DaysModCX3
DaysModCX4
DaysModCX5
DaysModCX6
DaysModCX7
TModHou
TModMin
NoWalk
DaysWalCX1
DaysWalCX2
DaysWalCX3
DaysWalCX4
DaysWalCX5
DaysWalCX6
DaysWalCX7
TWalHou
TWalMin
TSitHou
TSitMin cigsta3 wemwbs WrkNs MVPAMWKG maxdiff topqual3 
topqual4 gor1 hy140om2 diabete2 bmivg5 porftvg cholval2 lipid2 cholval12 origin2 alclimit07b
eqv3 glyhbval.
rename variables (age90 cluster = age strata).
exe.

compute HighChol2=-2.
if lipid2=0 & range(cholval12,0.01,4.999) HighChol2=0.
if lipid2=0 & range(cholval12,5.000,144.999) HighChol2=1.
if lipid2=1 HighChol2=1.
exe.
val labels HighChol2 
-2 "not applicable"
0 "no"
1 "yes".
exe.
compute HighChol3=-2.
*if lipid2=0 & range(cholval12,0.01,4.999) HighChol3=0.
*if lipid2=0 & range(cholval12,5.001,144.999) HighChol3=1.
*if lipid2=1 HighChol2=1.
if range(cholval12,0.01,4.999) HighChol3=0.
if range(cholval12,5.000,144.999) HighChol3=1.
exe.
variable label HighChol3 "irrespective of lipid".
exe.
val labels HighChol3 
-2 "not applicable"
0 "no"
1 "yes".
exe.
missing values all ().
exe.
select if age>=16.
exe.

*Self-reported hearing.
*fre hear.

*Conversing in a quiet room.
*fre bestear.
compute bestear2=-2.
if bestear=1 bestear2=1.
if bestear=2 bestear2=2.
if bestear=3 bestear2=3.
if bestear=4 bestear2=4.
if bestear=5 bestear2=4.
val labels bestear2
1 "no difficulty"
2 "slight difficulty"
3 "moderate difficulty"
4 "great difficulty / cannot hear at all".
exe.

*Conversing in a group.
*fre hrgrp.

*Following TV programmes.
*fre hrtv.

****************************************************
*Global summary (used for Table 4.8 in HSE Report).
****************************************************

compute maxdiff2 = max(bestear,hrtv,hrgrp).
EXECUTE.
if (difhraid=1) & (hrRight=1) & (hrleft=1) & (hrTV=1) & (HrGrp=1) maxdiff2=2.
exe.
do if maxdiff2=1.
if bestear=1 & (hrright~=hrleft) & (hrtv=1) & (hrgrp=1) maxdiff2=2.
end if.
recode maxdiff2 (4 thru 5=4).
exe.
if any(HrRight,-9,-8,6)|any(Hrleft,-9,-8,6)|any(HrTv,-9,-8,5)|any(HrGRp,-8,-9,5)|any(DifHraid,-8,3) maxdiff2=-1.
exe.
val labels maxdiff2 
1 "None"
2 "Slight"
3 "Moderate"
4 "Great".

SEL IF NUROUTC=81.
select if HrBetter3KHZ>=0.
select if any(HeHNoise,1,2).
select if age>=45.
exe.
compute low10=-2.
if range(wemwbs,40,70) low10=0.
if range(wemwbs,14,39) low10=1.
exe.
if (glyhbval>3.5 & glyhbval<6.3) glyhbval=glyhbval+0.1.
if (glyhbval>6.2 & glyhbval<9.0) glyhbval=glyhbval+0.2.
if (glyhbval>8.9) glyhbval=glyhbval+0.3.
EXECUTE.
compute diabete3=-2.
if (glyhbval>0.0 & glyhbval LE 6.5) diabete3=0.
if (glyhbval>6.5) diabete3=1.
exe.
variable label diabete3 "diabetes".
value labels diabete3 0 "no" 1 "yes".
exe.

SAVE TRANSLATE OUTFILE="N:\Hearing\Hearing_dataset.dta"
  /TYPE=STATA
  /VERSION=8
  /EDITION=SE
  /MAP
  /REPLACE
/keep pserial sex age wt_nurse HrBetter3KHZ psu strata wemwbs hraiduse low10 eqv5 qimd genhelf ill12m cigsta3 WrkNs MVPAMWKG
bestear2 hrgrp hrtv maxdiff2 hear topqual3 topqual4 gor1 hy140om2 diabete2 bmivg5 porftvg highchol2 origin2 alclimit07b
eqv3 highchol3 diabete3.







