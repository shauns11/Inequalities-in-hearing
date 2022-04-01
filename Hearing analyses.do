*************************************************
***** Socioeconomic differences in hearing among middle-aged and older adults:
***** cross-sectional analyses using the Health Survey for England
***************************************************


use "N:\Hearing\Hearing_dataset.dta", clear
renvars, lower
mvdecode low10, mv(-2)
mvdecode eqv5, mv(-90 -1)
mvdecode eqv3, mv(-90 -1)
mvdecode wemwbs,mv(-9 -8 -1)
mvdecode ill12m, mv(-8)
mvdecode wrkns,mv(-8 5)
mvdecode mvpamwkg, mv(-9 -1)
mvdecode bestear2,mv(-2)
mvdecode hrgrp,mv(-8)
mvdecode hrtv,mv(-8 5)
mvdecode maxdiff2,mv(-1)
generate men= sex==1
generate women= sex==2
generate agegroup=0
replace agegroup=1 if inrange(age,45,54)
replace agegroup=2 if inrange(age,55,64)
replace agegroup=3 if inrange(age,65,74)
replace agegroup=4 if inrange(age,75,184)
generate region=0
replace region=1 if (gor==1|gor==2|gor==3)
replace region=2 if (gor==4|gor==5|gor==6)
replace region=3 if (gor==7)
replace region=4 if (gor==8|gor==9)
label define regionlbl 1 "North" 2 "East" 3 "London" 4 "South"
label values region regionlbl
generate objloss= inrange(hrbetter3khz,2,4)
recode hrbetter3khz (4=3)

generate genhelf2=-2
replace genhelf2=0 if inrange(genhelf,1,3)
replace genhelf2=1 if inrange(genhelf,4,5)
mvdecode genhelf2,mv(-2)

* Binge drinking.
generate binge2=-2
replace binge2=0 if (alclimit07b==0|alclimit07b==1|alclimit07b==2)
replace binge2=1 if (alclimit07b==3)

generate eqv5a = eqv5
recode eqv5a (1=5) (2=4) (3=3) (4=2) (5=1)
label define albl 1 "highest" 2 "2" 3 "3" 4 "4" 5 "lowest"
label values eqv5a albl
generate eqv3a = eqv3
recode eqv3a (1=3) (2=2) (3=1)
label define blbl 1 "highest" 2 "2" 3 "lowest"
label values eqv3a blbl

save "N:\Hearing\Hearing_dataset_v2.dta", replace


****************************
*Table 1: age-std prevalence
*****************************.

use "N:\Hearing\Hearing_dataset_v2.dta", clear

preserve
collapse (sum) wt_nurse, by(sex agegroup) 
rename wt_nurse numsxag
keep sex agegroup numsxag
sort sex agegroup
save "N:\Temp\Temp1.dta",replace
restore

sort sex agegroup
merge m:1 sex agegroup using "N:\Temp\Temp1.dta"
drop _merge

preserve
collapse (sum) wt_nurse, by(sex) 
rename wt_nurse numsx
keep sex numsx
sort sex
save "N:\Temp\Temp2.dta",replace
restore

sort sex
merge m:1 sex using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)

replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* By sex.
svy:mean objloss,over(sex)

* By age.
svy:mean objloss,over(sex agegroup)
* P-value.
svy,subpop(men):tab agegroup objloss, row 
svy,subpop(women):tab agegroup objloss, row 

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex)
svy:mean objloss1,over(sex agegroup)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex)
svy:mean objloss2,over(sex agegroup)

*********************************************.
* Noise at work (3-categories).
* None; Less than 5-yrs; 5yrs or more.
*********************************************.

use "N:\Hearing\Hearing_dataset_v2.dta", replace

recode wrkns (3=2)

* Standardised estimates using direct standardisation.

preserve
collapse (sum) wt_nurse, by(sex agegroup wrkns) 
rename wt_nurse numsxag
keep sex agegroup wrkns numsxag
sort sex agegroup wrkns
save "N:\Temp\Temp1.dta",replace
restore

sort sex agegroup wrkns
merge m:1 sex agegroup wrkns using "N:\Temp\Temp1.dta"
drop _merge

preserve
collapse (sum) wt_nurse, by(sex wrkns) 
rename wt_nurse numsx
keep sex wrkns numsx
sort sex wrkns
save "N:\Temp\Temp2.dta",replace
restore

sort sex wrkns
merge m:1 sex wrkns using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* unable to hear 35dBHL.
svy:mean objloss,over(sex wrkns)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex wrkns)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex wrkns)

* P-value.
svy,subpop(men):tab wrkns objloss, row 
svy,subpop(women):tab wrkns objloss, row 

****************
*Income.
****************

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if (eqv3a==1|eqv3a==2|eqv3a==3)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup eqv3a) 
rename wt_nurse numsxag
keep sex agegroup eqv3a numsxag
sort sex agegroup eqv3a
save "N:\Temp\Temp1.dta",replace
restore

sort sex agegroup eqv3a
merge m:1 sex agegroup eqv3a using "N:\Temp\Temp1.dta"
drop _merge

preserve
collapse (sum) wt_nurse, by(sex eqv3a) 
rename wt_nurse numsx
keep sex eqv3a numsx
sort sex eqv3a
save "N:\Temp\Temp2.dta",replace
restore

sort sex eqv3a
merge m:1 sex eqv3a using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101) (214153=214152) (214159=214158) (214171 = 214170) (214197 = 214198) (214204=214203)
recode strata(214234=214233) (214235=214236) (214258=214259)

* unable to hear 35dBHL.
svy:mean objloss,over(sex eqv3a)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex eqv3a)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex eqv3a)

* P-value.
svy,subpop(men):tab eqv3a objloss, row 
svy,subpop(women):tab eqv3a objloss, row 


***************
* IMD.
***************

use "N:\Hearing\Hearing_dataset_v2.dta", replace

* Standardised estimates using direct standardisation.

preserve
collapse (sum) wt_nurse, by(sex agegroup qimd) 
rename wt_nurse numsxag
keep sex agegroup qimd numsxag
sort sex agegroup qimd
save "N:\Temp\Temp1.dta",replace
restore

sort sex agegroup qimd
merge m:1 sex agegroup qimd using "N:\Temp\Temp1.dta"
drop _merge

preserve
collapse (sum) wt_nurse, by(sex qimd) 
rename wt_nurse numsx
keep sex qimd numsx
sort sex qimd
save "N:\Temp\Temp2.dta",replace
restore

sort sex qimd
merge m:1 sex qimd using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)

replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* unable to hear 35dBHL.
svy:mean objloss,over(sex qimd)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex qimd)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex qimd)

* P-value.
svy,subpop(men):tab qimd objloss, row 
svy,subpop(women):tab qimd objloss, row 


*****************.
*Topqual.
*****************

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if inrange(topqual4,1,3)

* Standardised estimates using direct standardisation.

preserve
collapse (sum) wt_nurse, by(sex agegroup topqual4) 
rename wt_nurse numsxag
keep sex agegroup topqual4 numsxag
sort sex agegroup topqual4
save "N:\Temp\Temp1.dta",replace
restore

sort sex agegroup topqual4
merge m:1 sex agegroup topqual4 using "N:\Temp\Temp1.dta"
drop _merge

preserve
collapse (sum) wt_nurse, by(sex topqual4) 
rename wt_nurse numsx
keep sex topqual4 numsx
sort sex topqual4
save "N:\Temp\Temp2.dta",replace
restore

sort sex topqual4
merge m:1 sex topqual4 using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)

replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* unable to hear 35dBHL.
svy:mean objloss,over(sex topqual4)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex topqual4)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex topqual4)

* P-value.
svy,subpop(men):tab topqual4 objloss, row 
svy,subpop(women):tab topqual4 objloss, row 

****************************
*Supplementary Table 2.
****************************

* 4 regions.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
* Standardised estimates using direct standardisation.

preserve
collapse (sum) wt_nurse, by(sex agegroup region) 
rename wt_nurse numsxag
keep sex agegroup region numsxag
sort sex agegroup region
save "N:\Temp\Temp1.dta",replace
restore

sort sex agegroup region
merge m:1 sex agegroup region using "N:\Temp\Temp1.dta"
drop _merge

preserve
collapse (sum) wt_nurse, by(sex region) 
rename wt_nurse numsx
keep sex region numsx
sort sex region
save "N:\Temp\Temp2.dta",replace
restore

sort sex region
merge m:1 sex region using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* unable to hear 35dBHL.
svy:mean objloss,over(sex region)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex region)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex region)

* P-value.
svy,subpop(men):tab region objloss, row 
svy,subpop(women):tab region objloss, row 

**********.
* Smoking.
***********.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if inrange(cigsta3,1,4)
generate smokex=0
replace smokex=1 if cigsta3==3
replace smokex=2 if cigsta3==2
replace smokex=3 if cigsta3==1
label define xxlbl 1 "never" 2 "ex" 3 "current"
label values smokex xxlbl

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup cigsta3) 
rename wt_nurse numsxag
keep sex agegroup cigsta3 numsxag
sort sex agegroup cigsta3
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup cigsta3
merge m:1 sex agegroup cigsta3 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex cigsta3) 
rename wt_nurse numsx
keep sex cigsta3 numsx
sort sex cigsta3
save "N:\Temp\Temp2.dta",replace
restore
sort sex cigsta3
merge m:1 sex cigsta3 using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* unable to hear 35dBHL.
svy:mean objloss,over(sex smokex)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex smokex)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex smokex)

* P-value.
svy,subpop(men):tab smokex objloss, row 
svy,subpop(women):tab smokex objloss, row 


*****************************.
*Obesity.
*****************************.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
* Obese as a separate category (3 categories: normal; overweight; obese).
keep if inrange(bmivg5,2,5)
generate bmi3=0
replace bmi3=1 if bmivg5==2
replace bmi3=2 if bmivg5==3
replace bmi3=3 if bmivg5==4|bmivg5==5
label define bmi3lbl 1 "Normal" 2 "Overweight" 3 "Obese"
label values bmi3 bmi3lbl

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup bmi3) 
rename wt_nurse numsxag
keep sex agegroup bmi3 numsxag
sort sex agegroup bmi3
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup bmi3
merge m:1 sex agegroup bmi3 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex bmi3) 
rename wt_nurse numsx
keep sex bmi3 numsx
sort sex bmi3
save "N:\Temp\Temp2.dta",replace
restore
sort sex bmi3
merge m:1 sex bmi3 using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101)

* unable to hear 35dBHL.
svy:mean objloss,over(sex bmi3)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex bmi3)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex bmi3)

* P-value.
svy,subpop(men):tab bmi3 objloss, row 
svy,subpop(women):tab bmi3 objloss, row 


**************************.
* Diagnosed Diabetes.
**************************.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if diabete2==1|diabete2==2
generate diabetex = 0
replace diabetex = 1 if diabete2==1
label define xlbl 0 "no" 1 "yes"
label values diabetex xlbl 

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup diabete2) 
rename wt_nurse numsxag
keep sex agegroup diabete2 numsxag
sort sex agegroup diabete2
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup diabete2
merge m:1 sex agegroup diabete2 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex diabete2) 
rename wt_nurse numsx
keep sex diabete2 numsx
sort sex diabete2
save "N:\Temp\Temp2.dta",replace
restore
sort sex diabete2
merge m:1 sex diabete2 using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)

replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101)

* unable to hear 35dBHL.
svy:mean objloss,over(sex diabetex)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex diabetex)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex diabetex)

* P-value.
svy,subpop(men):tab diabetex objloss, row 
svy,subpop(women):tab diabetex objloss, row 


***************************.
* Diabetes (raised Hb1Ac).
***************************.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
* Diabetes.
keep if diabete3==0|diabete3==1
* Standardised estimates using direct standardisation.

preserve
collapse (sum) wt_nurse, by(sex agegroup diabete3) 
rename wt_nurse numsxag
keep sex agegroup diabete3 numsxag
sort sex agegroup diabete3
save "N:\Temp\Temp1.dta",replace
restore

sort sex agegroup diabete3
merge m:1 sex agegroup diabete3 using "N:\Temp\Temp1.dta"
drop _merge

preserve
collapse (sum) wt_nurse, by(sex diabete3) 
rename wt_nurse numsx
keep sex diabete3 numsx
sort sex diabete3
save "N:\Temp\Temp2.dta",replace
restore

sort sex diabete3
merge m:1 sex diabete3 using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101)
recode strata (214160 = 214159) (214203 = 214202) (214234=214233) (214241=214242) (214258=214257)
recode strata (214118=214117) (214132=214133) (214223=214222) (214228=214229) (214235=214236) (214250=214249) (214117=214116) (214206=214205)
recode strata (214117 = 214120) (214171 = 214180)

* unable to hear 35dBHL.
svy:mean objloss,over(sex diabete3)
estat size

* mild to moderate

gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex diabete3)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex diabete3)

* P-value.
svy,subpop(men):tab diabete3 objloss, row 
svy,subpop(women):tab diabete3 objloss, row 

***********************************.
* Hypertension.
***********************************.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if inrange(hy140om2,1,4)
gen highbp=0
replace highbp=1 if inrange(hy140om2,2,4)
label define hibplbl 0 "no" 1 "hypertensive"
label values highbp hibplbl

* Standardised estimates using direct standardisation.

preserve
collapse (sum) wt_nurse, by(sex agegroup highbp) 
rename wt_nurse numsxag
keep sex agegroup highbp numsxag
sort sex agegroup highbp
save "N:\Temp\Temp1.dta",replace
restore

sort sex agegroup highbp
merge m:1 sex agegroup highbp using "N:\Temp\Temp1.dta"
drop _merge

preserve
collapse (sum) wt_nurse, by(sex highbp) 
rename wt_nurse numsx
keep sex highbp numsx
sort sex highbp
save "N:\Temp\Temp2.dta",replace
restore

sort sex highbp
merge m:1 sex highbp using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101)
recode strata (214160 = 214159) (214203 = 214202) (214234=214233) (214241=214242) (214258=214257)

* unable to hear 35dBHL.
svy:mean objloss,over(sex highbp)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex highbp)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex highbp)

* P-value.
svy,subpop(men):tab highbp objloss, row 
svy,subpop(women):tab highbp objloss, row 


***********************************************************.
* High cholesterol (irrespective of medication use).
***********************************************************.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if highchol3==0|highchol3==1
label define hicplbl 0 "no" 1 "high chol"
label values highchol3 hiclbl

* Standardised estimates using direct standardisation.

preserve
collapse (sum) wt_nurse, by(sex agegroup highchol3) 
rename wt_nurse numsxag
keep sex agegroup highchol3 numsxag
sort sex agegroup highchol3
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup highchol3
merge m:1 sex agegroup highchol3 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex highchol3) 
rename wt_nurse numsx
keep sex highchol3 numsx
sort sex highchol3
save "N:\Temp\Temp2.dta",replace
restore
sort sex highchol3
merge m:1 sex highchol3 using "N:\Temp\Temp2.dta"
drop _merge


generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101)
recode strata (214160 = 214159) (214203 = 214202) (214234=214233) (214241=214242) (214258=214257)
recode strata (214118=214117) (214132=214133) (214223=214222) (214228=214229) (214235=214236) (214250=214249)
recode strata (214117=214116)
recode strata (214171=214174)  

*unable to hear 35dBHL.
svy:mean objloss,over(sex highchol3)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex highchol3)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex highchol3)

* P-value.

svy,subpop(men):tab highchol3 objloss, row 
svy,subpop(women):tab highchol3 objloss, row 

******************.
* MVPA.
******************.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if (mvpamwkg==1|mvpamwkg==2)
rename mvpamwkg mvpa
label define MVPAlbl 1 "inactive" 2 "active"
label values mvpa MVPAlbl
generate mvpax=0
replace mvpax=1 if mvpa==1
label define mvpaxlbl 0 "active" 1 "inactive"
label values mvpax mvpaxlbl

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup mvpa) 
rename wt_nurse numsxag
keep sex agegroup mvpa numsxag
sort sex agegroup mvpa
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup mvpa
merge m:1 sex agegroup mvpa using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex mvpa) 
rename wt_nurse numsx
keep sex mvpa numsx
sort sex mvpa
save "N:\Temp\Temp2.dta",replace
restore
sort sex mvpa
merge m:1 sex mvpa using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) strata(strata)
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101)
recode strata (214160 = 214159) (214203 = 214202) (214234=214233) (214241=214242) (214258=214257)
recode strata (214118=214117) (214132=214133) (214223=214222) (214228=214229) (214235=214236) (214250=214249) (214117=214116) (214206=214205)

* unable to hear 35dBHL.
svy:mean objloss,over(sex mvpax)
estat size

* mild to moderate
gen objloss1 = hrbetter3khz==2
svy:mean objloss1,over(sex mvpax)

* moderate
gen objloss2 = hrbetter3khz==3
svy:mean objloss2,over(sex mvpax)

* P-value.
svy,subpop(men):tab mvpax objloss, row 
svy,subpop(women):tab mvpax objloss, row 


*************************************************
* Figure 2 (odds of hearing loss).
* Separate models for income; IMD; education
*************************************************

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if (eqv3a==1|eqv3a==2|eqv3a==3)

*Missing category (99) for exposure to noise at work and CVD RF.
*Noise at work.
*Smoking.
*BMI.
*Diabetes.
*Hypertension.
*Cholesterol.
*PA.

mvencode wrkns, mv(99)
recode diabete2 (-8=99)
generate bmi3=99
replace bmi3=1 if bmivg5==2
replace bmi3=2 if bmivg5==3
replace bmi3=3 if bmivg5==4|bmivg5==5
label define bmi3lbl 1 "Normal" 2 "Overweight" 3 "Obese"
label values bmi3 bmi3lbl
gen highbp=99
replace highbp=1 if hy140om2==1
replace highbp=2 if inrange(hy140om2,2,4)
label define hibplbl 0 "no" 1 "hypertensive"
label values highbp hibplbl
recode highchol3 (-2=99)
mvencode mvpamwkg, mv(99)
rename mvpamwkg mvpa

svyset [pweight=wt_nurse],psu(psu) strata(strata)
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101)
recode strata (214160 = 214159) (214203 = 214202) (214234=214233) (214241=214242) (214258=214257)
recode strata (214118=214117) (214132=214133) (214223=214222) (214228=214229) (214235=214236) (214250=214249) (214117=214116) (214206=214205)
recode strata (214153=214152) (214171=214170) (214197=214198) (214204=214202)

* Age-adjusted.
svy,subpop(men): logit objloss i.eqv3a c.age, or
estimates store m1
testparm i.eqv3a
svy,subpop(women): logit objloss i.eqv3a c.age, or
estimates store f1
testparm i.eqv3a

*Multivariate adjusted.
svy,subpop(men): logit objloss i.eqv3a c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpa, or
estimates store m2
testparm i.eqv3a
svy,subpop(women): logit objloss i.eqv3a c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpa, or
estimates store f2
testparm i.eqv3a

* Plot for Men (age- and fully-adjusted).
coefplot (m1,label(Age-adjusted) msymbol(o) offset(0.1) pstyle(p1) mlabposition(1)) ///
(m2,label(Fully-Adjusted) msymbol(o) offset(-0.1) pstyle(p2) mlabposition(6)) ///
,drop(_cons) keep(2.eqv3a 3.eqv3a) ///
ytitle("") eform xtitle(Odds Ratio) xline(1,lcolor(black)) mlabel format(%9.2f) ///
coeflabels(2.eqv3a = "2 {it:vs.} highest" 3.eqv3a = "3 {it:vs.} highest")  ///
xlabel(,format(%9.1f)) ///
text(0.7 3.5 "Men: Fully-Adjusted: {it:P}=0.999",size(small) placement(right)) ///
title("Odds of hearing loss",pos(11) size(medsmall))

* Plot for Women (age- and fully-adjusted).

coefplot (f1,label(Age-adjusted) msymbol(o) offset(0.1) pstyle(p1) mlabposition(1)) ///
(f2,label(Fully-Adjusted) msymbol(o) offset(-0.1) pstyle(p2) mlabposition(6)) ///
,drop(_cons) keep(2.eqv3a 3.eqv3a) ///
ytitle("") eform xtitle(Odds Ratio) xline(1,lcolor(black)) mlabel format(%9.2f) ///
coeflabels(2.eqv3a = "2 {it:vs.} highest" 3.eqv3a = "3 {it:vs.} highest")  ///
xlabel(,format(%9.1f)) ///
text(0.7 3.5 "Women: Fully-Adjusted: {it:P}=0.999",size(small) placement(right)) ///
title("Odds of hearing loss",pos(11) size(medsmall))


*IMD.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
mvencode wrkns, mv(99)
recode diabete2 (-8=99)
generate bmi3=99
replace bmi3=1 if bmivg5==2
replace bmi3=2 if bmivg5==3
replace bmi3=3 if bmivg5==4|bmivg5==5
label define bmi3lbl 1 "Normal" 2 "Overweight" 3 "Obese"
label values bmi3 bmi3lbl
gen highbp=99
replace highbp=1 if hy140om2==1
replace highbp=2 if inrange(hy140om2,2,4)
label define hibplbl 0 "no" 1 "hypertensive"
label values highbp hibplbl
mvencode highchol2, mv(99)
recode highchol3 (-2=99)
mvencode mvpamwkg, mv(99)
rename mvpamwkg mvpa

*Age-adjusted.
svyset [pweight=wt_nurse],psu(psu) strata(strata)
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101)
recode strata (214160 = 214159) (214203 = 214202) (214234=214233) (214241=214242) (214258=214257)
recode strata (214118=214117) (214132=214133) (214223=214222) (214228=214229) (214235=214236) (214250=214249) (214117=214116) (214206=214205)
recode strata (214153=214152) (214171=214170) (214197=214198) (214204=214202)
svydes, single
svy,subpop(men): logit objloss i.qimd c.age, or
estimates store m3
testparm i.qimd
svy,subpop(women): logit objloss i.qimd c.age, or
estimates store f3
testparm i.qimd

* Multivariate adjusted.
svy,subpop(men): logit objloss i.qimd c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpa, or
estimates store m4
testparm i.qimd
svy,subpop(women): logit objloss i.qimd c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpa, or
estimates store f4
testparm i.qimd

* Plot for Men (age- and fully-adjusted).

coefplot (m3,label(Age-adjusted) msymbol(o) offset(0.1) pstyle(p1) mlabposition(1)) ///
(m4,label(Fully-Adjusted) msymbol(o) offset(-0.1) pstyle(p2) mlabposition(6)) ///
,drop(_cons) keep(2.qimd 3.qimd 4.qimd 5.qimd) ///
ytitle("") eform xtitle(Odds Ratio) xline(1,lcolor(black)) mlabel format(%9.2f) ///
coeflabels(2.qimd = "2 {it:vs.} least" 3.qimd = "3 {it:vs.} least"  4.qimd = "4  {it:vs.} least"  5.qimd = "most  {it:vs.} least")  ///
xlabel(,format(%9.1f)) ///
text(0.7 3.5 "Men: Fully-Adjusted: {it:P}=0.999",size(small) placement(right)) ///
title("Odds of hearing loss",pos(11) size(medsmall))

* Plot for Women (age- and fully-adjusted).

coefplot (f3,label(Age-adjusted) msymbol(o) offset(0.1) pstyle(p1) mlabposition(1)) ///
(f4,label(Fully-Adjusted) msymbol(o) offset(-0.1) pstyle(p2) mlabposition(6)) ///
,drop(_cons) keep(2.qimd 3.qimd 4.qimd 5.qimd) ///
ytitle("") eform xtitle(Odds Ratio) xline(1,lcolor(black)) mlabel format(%9.2f) ///
coeflabels(2.qimd = "2 {it:vs.} least" 3.qimd = "3 {it:vs.} least"  4.qimd = "4  {it:vs.} least"  5.qimd = "most  {it:vs.} least")  ///
xlabel(,format(%9.1f)) ///
text(0.7 3.5 "Women: Fully-Adjusted: {it:P}=0.999",size(small) placement(right)) ///
title("Odds of hearing loss",pos(11) size(medsmall))


* Highest Educational Qualification.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if inrange(topqual4,1,3)
mvencode wrkns, mv(99)
recode diabete2 (-8=99)
generate bmi3=99
replace bmi3=1 if bmivg5==2
replace bmi3=2 if bmivg5==3
replace bmi3=3 if bmivg5==4|bmivg5==5
label define bmi3lbl 1 "Normal" 2 "Overweight" 3 "Obese"
label values bmi3 bmi3lbl
gen highbp=99
replace highbp=1 if hy140om2==1
replace highbp=2 if inrange(hy140om2,2,4)
label define hibplbl 0 "no" 1 "hypertensive"
label values highbp hibplbl
mvencode highchol2, mv(99)
recode highchol3 (-2=99)
mvencode mvpamwkg, mv(99)
rename mvpamwkg mvpa

svyset [pweight=wt_nurse],psu(psu) strata(strata)
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
recode strata (214102=214101)
recode strata (214160 = 214159) (214203 = 214202) (214234=214233) (214241=214242) (214258=214257)
recode strata (214118=214117) (214132=214133) (214223=214222) (214228=214229) (214235=214236) (214250=214249) (214117=214116) (214206=214205)
recode strata (214153=214152) (214171=214170) (214197=214198) (214204=214202)
svydes, single

* Age-adjusted.
svy,subpop(men): logit objloss i.topqual4 c.age, or
estimates store m5
testparm i.topqual4
svy,subpop(women): logit objloss i.topqual4 c.age, or
estimates store f5
testparm i.topqual4

* Multivariate adjusted.
svy,subpop(men): logit objloss i.topqual4 c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpa, or
estimates store m6
testparm i.topqual4
svy,subpop(women): logit objloss i.topqual4 c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpa, or
estimates store f6
testparm i.topqual4

* Plot for Men (age- and fully-adjusted).
coefplot (m5,label(Age-adjusted) msymbol(o) offset(0.1) pstyle(p1) mlabposition(1)) ///
(m6,label(Fully-Adjusted) msymbol(o) offset(-0.1) pstyle(p2) mlabposition(6)) ///
,drop(_cons) keep(2.topqual4 3.topqual4) ///
ytitle("") eform xtitle(Odds Ratio) xline(1,lcolor(black)) mlabel format(%9.2f) ///
coeflabels(2.topqual4 = "below degree {it:vs.} degree" 3.topqual4 = "none {it:vs.} degree" )  ///
xlabel(,format(%9.1f)) ///
text(0.7 3.5 "Men: Fully-Adjusted: {it:P}=0.999",size(small) placement(right)) ///
title("Odds of hearing loss",pos(11) size(medsmall))


* Plot for Women (age- and fully-adjusted).
coefplot (f5,label(Age-adjusted) msymbol(o) offset(0.1) pstyle(p1) mlabposition(1)) ///
(f6,label(Fully-Adjusted) msymbol(o) offset(-0.1) pstyle(p2) mlabposition(6)) ///
,drop(_cons) keep(2.topqual4 3.topqual4) ///
ytitle("") eform xtitle(Odds Ratio) xline(1,lcolor(black)) mlabel format(%9.2f) ///
coeflabels(2.topqual4 = "below degree {it:vs.} degree" 3.topqual4 = "none {it:vs.} degree" )  ///
xlabel(,format(%9.1f)) ///
text(0.7 3.5 "Women: Fully-Adjusted: {it:P}=0.999",size(small) placement(right)) ///
title("Odds of hearing loss",pos(11) size(medsmall))


************************************************************************************.
* Table 3 (prevalence of hearing aid use, conditional on objective hearing loss).
************************************************************************************.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if objloss==1
tab hrbetter3khz
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode agegroup (1=2)

*Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup hrbetter3khz) 
rename wt_nurse numsxag
keep sex agegroup hrbetter3khz numsxag
sort sex agegroup hrbetter3khz
save "N:\Temp\Temp1.dta",replace
restore

sort sex agegroup hrbetter3khz
merge m:1 sex agegroup hrbetter3khz using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex hrbetter3khz) 
rename wt_nurse numsx
keep sex hrbetter3khz numsx
sort sex hrbetter3khz
save "N:\Temp\Temp2.dta",replace
restore
sort sex hrbetter3khz
merge m:1 sex hrbetter3khz using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* hearing aid use.
svy:mean use,over(sex)
svy:mean use,over(sex hrbetter3khz)

* P-value.
svy,subpop(men):tab hrbetter3khz use, row 
svy,subpop(women):tab hrbetter3khz use, row 



*age-group.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode agegroup (1=2)

*Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup) 
rename wt_nurse numsxag
keep sex agegroup numsxag
sort sex agegroup
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup
merge m:1 sex agegroup using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex) 
rename wt_nurse numsx
keep sex numsx
sort sex
save "N:\Temp\Temp2.dta",replace
restore
sort sex
merge m:1 sex using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* hearing aid use.
svy:mean use,over(sex)
svy:mean use,over(sex agegroup)

* P-value.
svy,subpop(men):tab agegroup use, row 
svy,subpop(women):tab agegroup use, row 


*Aid use by Exposure to Noise at work.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode wrkns (3=2) 
recode wrkns (2=1)
label define noiselbl 1 "no" 4 "yes"
label values wrkns noiselbl
recode agegroup (1=2)

* Standardised estimates using direct standardisation.

preserve
collapse (sum) wt_nurse, by(sex agegroup wrkns) 
rename wt_nurse numsxag
keep sex agegroup wrkns numsxag
sort sex agegroup wrkns
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup wrkns
merge m:1 sex agegroup wrkns using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex wrkns) 
rename wt_nurse numsx
keep sex wrkns numsx
sort sex wrkns
save "N:\Temp\Temp2.dta",replace
restore
sort sex wrkns
merge m:1 sex wrkns using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* Hearing aid use.
svy:mean use,over(sex wrkns)
estat size

* P-value.
svy,subpop(men):tab wrkns use, row 
svy,subpop(women):tab wrkns use, row 

*Income 
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if (eqv3a==1|eqv3a==2|eqv3a==3)
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode agegroup (1=2)

*Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup eqv3a) 
rename wt_nurse numsxag
keep sex agegroup eqv3a numsxag
sort sex agegroup eqv3a
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup eqv3a
merge m:1 sex agegroup eqv3a using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex eqv3a) 
rename wt_nurse numsx
keep sex eqv3a numsx
sort sex eqv3a
save "N:\Temp\Temp2.dta",replace
restore
sort sex eqv3a
merge m:1 sex eqv3a using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

*Hearing aid use.
svy:mean use,over(sex eqv3a)
estat size

* P-value.
svy,subpop(men):tab eqv3a use, row 
svy,subpop(women):tab eqv3a use, row 


*Aid use by IMD.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode qimd (1=1) (2=1) (3=2) (4=3) (5=3)
recode agegroup (1=2)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup qimd) 
rename wt_nurse numsxag
keep sex agegroup qimd numsxag
sort sex agegroup qimd
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup qimd
merge m:1 sex agegroup qimd using "N:\Temp\Temp1.dta"
drop _merge

preserve
collapse (sum) wt_nurse, by(sex qimd) 
rename wt_nurse numsx
keep sex qimd numsx
sort sex qimd
save "N:\Temp\Temp2.dta",replace
restore
sort sex qimd
merge m:1 sex qimd using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* Hearig aid use.
svy:mean use,over(sex qimd)

* P-value.
svy,subpop(men):tab qimd use, row 
svy,subpop(women):tab qimd use, row 

* binary (educational qualifications).
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if inrange(topqual4,1,4)
recode topqual4 (2=1)
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode agegroup (1=2)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup topqual4) 
rename wt_nurse numsxag
keep sex agegroup topqual4 numsxag
sort sex agegroup topqual4
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup topqual4
merge m:1 sex agegroup topqual4 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex topqual4) 
rename wt_nurse numsx
keep sex topqual4 numsx
sort sex topqual4
save "N:\Temp\Temp2.dta",replace
restore
sort sex topqual4
merge m:1 sex topqual4 using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* Hearing aid use.
svy:mean use,over(sex topqual4)
estat size

* P-value.
svy,subpop(men):tab topqual4 use, row 
svy,subpop(women):tab topqual4 use, row 

***********************************************************************
*Supplementary Table 3.
************************************************************************.

*Aid use by smoking.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if inrange(cigsta3,1,4)
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
generate smokex=0
replace smokex=1 if cigsta3==3
replace smokex=2 if cigsta3==2
replace smokex=3 if cigsta3==1
label define xxlbl 1 "never" 2 "ex" 3 "current"
label values smokex xxlbl
recode agegroup (1=2)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup cigsta3) 
rename wt_nurse numsxag
keep sex agegroup cigsta3 numsxag
sort sex agegroup cigsta3
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup cigsta3
merge m:1 sex agegroup cigsta3 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex cigsta3) 
rename wt_nurse numsx
keep sex cigsta3 numsx
sort sex cigsta3
save "N:\Temp\Temp2.dta",replace
restore
sort sex cigsta3
merge m:1 sex cigsta3 using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.3455*(numsx/numsxag) if (agegroup==1) & (sex==1)
replace standwt=wt_nurse*0.2758*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.3239*(numsx/numsxag) if (agegroup==1) & (sex==2)
replace standwt=wt_nurse*0.2612*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* Hearing aid use.
svy:mean use,over(sex smokex)

* P-value.
svy,subpop(men):tab smokex use, row 
svy,subpop(women):tab smokex use, row 


* Aid use by obesity.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if inrange(bmivg5,2,5)
generate bmi3=0
replace bmi3=1 if bmivg5==2
replace bmi3=2 if bmivg5==3
replace bmi3=3 if bmivg5==4|bmivg5==5
label define bmi3lbl 1 "Normal" 2 "Overweight" 3 "Obese"
label values bmi3 bmi3lbl
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode agegroup (1=2)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup bmi3) 
rename wt_nurse numsxag
keep sex agegroup bmi3 numsxag
sort sex agegroup bmi3
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup bmi3
merge m:1 sex agegroup bmi3 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex bmi3) 
rename wt_nurse numsx
keep sex bmi3 numsx
sort sex bmi3
save "N:\Temp\Temp2.dta",replace
restore
sort sex bmi3
merge m:1 sex bmi3 using "N:\Temp\Temp2.dta"
drop _merge
generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)
svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* Hearing aid use.
svy:mean use,over(sex bmi3)
estat size

* P-value.
svy,subpop(men):tab bmi3 use, row 
svy,subpop(women):tab bmi3 use, row

* Aid use by diagnosed diabetes.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if diabete2==1|diabete2==2
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
generate diabetex = diabete2==1
label define xlbl 0 "no" 1 "yes"
label values diabetex xlbl 
recode agegroup (1=2)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup diabete2) 
rename wt_nurse numsxag
keep sex agegroup diabete2 numsxag
sort sex agegroup diabete2
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup diabete2
merge m:1 sex agegroup diabete2 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex diabete2) 
rename wt_nurse numsx
keep sex diabete2 numsx
sort sex diabete2
save "N:\Temp\Temp2.dta",replace
restore
sort sex diabete2
merge m:1 sex diabete2 using "N:\Temp\Temp2.dta"
drop _merge
generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)
svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* Hearing aid use.
svy:mean use,over(sex diabetex)

* P-value.

svy,subpop(men):tab diabetex use, row 
svy,subpop(women):tab diabetex use, row 

* Aid use by Glycated haemoglobin.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if diabete3==0|diabete3==1
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode agegroup (1=2)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup diabete3) 
rename wt_nurse numsxag
keep sex agegroup diabete3 numsxag
sort sex agegroup diabete3
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup diabete3
merge m:1 sex agegroup diabete3 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex diabete3) 
rename wt_nurse numsx
keep sex diabete3 numsx
sort sex diabete3
save "N:\Temp\Temp2.dta",replace
restore
sort sex diabete3
merge m:1 sex diabete3 using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)

replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* Hearing aid use.
svy:mean use,over(sex diabete3)

* P-value.
svy,subpop(men):tab diabete3 use, row 
svy,subpop(women):tab diabete3 use, row

* Aid use by hypertension.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if inrange(hy140om2,1,4)
gen highbp = inrange(hy140om2,2,4)
label define hibplbl 0 "no" 1 "hypertensive"
label values highbp hibplbl
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode agegroup (1=2)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup highbp) 
rename wt_nurse numsxag
keep sex agegroup highbp numsxag
sort sex agegroup highbp
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup highbp
merge m:1 sex agegroup highbp using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex highbp) 
rename wt_nurse numsx
keep sex highbp numsx
sort sex highbp
save "N:\Temp\Temp2.dta",replace
restore
sort sex highbp
merge m:1 sex highbp using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)
svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* hearing aid use.
svy:mean use,over(sex highbp)

* P-value.
svy,subpop(men):tab highbp use, row 
svy,subpop(women):tab highbp use, row

* HighCholesterol.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if highchol3==0|highchol3==1
label define hicplbl 0 "no" 1 "high chol"
label values highchol3 hiclbl
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode agegroup (1=2)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup highchol3) 
rename wt_nurse numsxag
keep sex agegroup highchol3 numsxag
sort sex agegroup highchol3
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup highchol3
merge m:1 sex agegroup highchol3 using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex highchol3) 
rename wt_nurse numsx
keep sex highchol3 numsx
sort sex highchol3
save "N:\Temp\Temp2.dta",replace
restore
sort sex highchol3
merge m:1 sex highchol3 using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* Hearing aid use.
svy:mean use,over(sex highchol3)

* P-value.
svy,subpop(men):tab highchol3 use, row 
svy,subpop(women):tab highchol3 use, row 


* Aid use by MVPA.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if (mvpamwkg==1|mvpamwkg==2)
rename mvpamwkg mvpa
label define MVPAlbl 1 "inactive" 2 "active"
label values mvpa MVPAlbl
generate mvpax = mvpa==1
label define mvpaxlbl 0 "active" 1 "inactive"
label values mvpax mvpaxlbl
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
recode agegroup (1=2)

* Standardised estimates using direct standardisation.
preserve
collapse (sum) wt_nurse, by(sex agegroup mvpa) 
rename wt_nurse numsxag
keep sex agegroup mvpa numsxag
sort sex agegroup mvpa
save "N:\Temp\Temp1.dta",replace
restore
sort sex agegroup mvpa
merge m:1 sex agegroup mvpa using "N:\Temp\Temp1.dta"
drop _merge
preserve
collapse (sum) wt_nurse, by(sex mvpa) 
rename wt_nurse numsx
keep sex mvpa numsx
sort sex mvpa
save "N:\Temp\Temp2.dta",replace
restore
sort sex mvpa
merge m:1 sex mvpa using "N:\Temp\Temp2.dta"
drop _merge

generate standwt=0
replace standwt=wt_nurse*0.6212*(numsx/numsxag) if (agegroup==2) & (sex==1)
replace standwt=wt_nurse*0.2219*(numsx/numsxag) if (agegroup==3) & (sex==1)
replace standwt=wt_nurse*0.1569*(numsx/numsxag) if (agegroup==4) & (sex==1)
replace standwt=wt_nurse*0.5851*(numsx/numsxag) if (agegroup==2) & (sex==2)
replace standwt=wt_nurse*0.2196*(numsx/numsxag) if (agegroup==3) & (sex==2)
replace standwt=wt_nurse*0.1952*(numsx/numsxag) if (agegroup==4) & (sex==2)

svyset,clear
svyset [pweight=standwt],psu(psu) 
svydes, single
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* hearing aid use.
svy:mean use,over(sex mvpax)

* P-value.
svy,subpop(men):tab mvpax use, row 
svy,subpop(women):tab mvpax use, row 



*************************************************
* Figure 3 (odds of hearing aid use).
* Separate models for income; IMD; education
*************************************************

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
svyset, clear
svyset [pweight=wt_nurse],psu(psu) 
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)
keep if (eqv3a==1|eqv3a==2|eqv3a==3)

* Missing category (99) for exposure to noise at work and CVD RF.
* Noise at work.
* Smoking.
* BMI.
* Diabetes.
* Hypertension.
* Cholesterol.
* PA.

* Exposure to noise at work.
recode wrkns (3=2) 
recode wrkns (2=1)
label define noiselbl 1 "no" 4 "yes"
label values wrkns noiselbl
mvencode wrkns, mv(99)

* Diabetes.
recode diabete2 (-8=99)

* BMI.
generate bmi3=99
replace bmi3=1 if bmivg5==2
replace bmi3=2 if bmivg5==3
replace bmi3=3 if bmivg5==4|bmivg5==5
label define bmi3lbl 1 "Normal" 2 "Overweight" 3 "Obese"
label values bmi3 bmi3lbl

* Hypertension.
gen highbp=99
replace highbp=1 if hy140om2==1
replace highbp=2 if inrange(hy140om2,2,4)
label define hibplbl 0 "no" 1 "hypertensive"
label values highbp hibplbl

* High Cholesterol (irrespective of medication use).
recode highchol3 (-2=99)

* PA.
mvencode mvpamwkg, mv(99)

* Age-adjusted.
svy,subpop(men): logit use i.eqv3a c.age, or
testparm i.eqv3a
svy,subpop(women): logit use i.eqv3a c.age, or
testparm i.eqv3a

* Multivariate adjusted.
svy,subpop(men): logit use i.eqv3a c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpamwkg, or
testparm i.eqv3a
svy,subpop(women): logit use i.eqv3a c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpamwkg, or
testparm i.eqv3a

* Logistic regression (hearing aid use): by IMD.
use "N:\Hearing\Hearing_dataset_v2.dta", replace
recode qimd (1=1) (2=1) (3=2) (4=3) (5=3)
recode agegroup (1=2)
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
svyset, clear
svyset [pweight=wt_nurse],psu(psu) 
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

* Missing category (99) for exposure to noise at work and CVD RF.
* Noise at work.
* Smoking.
* BMI.
* Diabetes.
* Hypertension.
* Cholesterol.
* PA.

* Exposure to noise at work.
recode wrkns (3=2) 
recode wrkns (2=1)
label define noiselbl 1 "no" 4 "yes"
label values wrkns noiselbl
mvencode wrkns, mv(99)

* Diabetes.
recode diabete2 (-8=99)

* BMI.
generate bmi3=99
replace bmi3=1 if bmivg5==2
replace bmi3=2 if bmivg5==3
replace bmi3=3 if bmivg5==4|bmivg5==5
label define bmi3lbl 1 "Normal" 2 "Overweight" 3 "Obese"
label values bmi3 bmi3lbl

* Hypertension.
gen highbp=99
replace highbp=1 if hy140om2==1
replace highbp=2 if inrange(hy140om2,2,4)
label define hibplbl 0 "no" 1 "hypertensive"
label values highbp hibplbl

* High Cholesterol (irrespective of medication use).
recode highchol3 (-2=99)

* PA.
mvencode mvpamwkg, mv(99)

* Age-adjusted.
svy,subpop(men): logit use i.qimd c.age, or
testparm i.qimd

svy,subpop(women): logit use i.qimd c.age, or
testparm i.qimd

* Multivariate adjusted.
svy,subpop(men): logit use i.qimd c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpamwkg, or
testparm i.qimd
svy,subpop(women): logit use i.qimd c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpamwkg, or
testparm i.qimd

* Logistic regression (hearing aid use): by Educational qualification.
** binary for highest educational qualification.

use "N:\Hearing\Hearing_dataset_v2.dta", replace
keep if objloss==1
generate use = hraiduse==1
label define a2lbl 0 "no use" 1 "current use"
label values use a2lbl
keep if inrange(topqual4,1,3)
recode topqual4 (2=1)
svyset, clear
svyset [pweight=wt_nurse],psu(psu) 
recode strata (214117=214116) (214126=214125) (214162=214163) (214178=214179) (214192=214193) (214227=214226)

recode wrkns (3=2) 
recode wrkns (2=1)
label define noiselbl 1 "no" 4 "yes"
label values wrkns noiselbl
mvencode wrkns, mv(99)

* Diabetes.
recode diabete2 (-8=99)

* BMI.
generate bmi3=99
replace bmi3=1 if bmivg5==2
replace bmi3=2 if bmivg5==3
replace bmi3=3 if bmivg5==4|bmivg5==5
label define bmi3lbl 1 "Normal" 2 "Overweight" 3 "Obese"
label values bmi3 bmi3lbl

* Hypertension.
gen highbp=99
replace highbp=1 if hy140om2==1
replace highbp=2 if inrange(hy140om2,2,4)
label define hibplbl 0 "no" 1 "hypertensive"
label values highbp hibplbl

* High Cholesterol (irrespective of medication use).
recode highchol3 (-2=99)

* PA.
mvencode mvpamwkg, mv(99)

* Age-adjusted.
svy,subpop(men): logit use i.topqual4 c.age, or
testparm i.topqual4
svy,subpop(women): logit use i.topqual4 c.age, or
testparm i.topqual4

* Multivariate adjusted.

svy,subpop(men): logit use i.topqual4 c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpamwkg, or
testparm i.topqual4
svy,subpop(women): logit use i.topqual4 c.age i.wrkns i.cigsta3 i.bmi3 i.diabete2 i.highbp i.highchol3 i.mvpamwkg, or
testparm i.topqual4






