* Replication file for Fafchamps et al. JDE paper

/*
*** This is some of the code used for variable construction/cleaning
* Keep experimental sample
keep if experimentsample==1
xtset sheno wave

* correct one sheno which has atreat misentered (but atreatcash and atreatequip correct)
replace atreat=0 if sheno==140834003

* Define group of high baseline profits groups to drop (baseline 2 rounds profits>1500)
gen trimgroup=1 if groupnum==992|groupnum==50002|groupnum==100005|groupnum==512|groupnum==10004|groupnum==301

*** Define Time since Treatment
gen timecash=0
replace timecash=1 if (wave==3 & atreatcash==1)|(wave==4 & earlytreat==0 & atreatcash==1 & latetreat==0)|(wave==5 & atreatcash==1 & latetreat==1)
replace timecash=2 if (wave==4 & atreatcash==1 & earlytreat==1)|(wave==5 & earlytreat==0 & latetreat==0 & atreatcash==1)|(wave==6 & latetreat==1 & atreatcash==1)
replace timecash=3 if (wave==5 & atreatcash==1 & earlytreat==1)|(wave==6 & earlytreat==0 & latetreat==0 & atreatcash==1)
replace timecash=4 if (wave==6 & atreatcash==1 & earlytreat==1)
gen timeequip=0
replace timeequip=1 if (wave==3 & atreatequip==1)|(wave==4 & earlytreat==0 & atreatequip==1 & latetreat==0)|(wave==5 & atreatequip==1 & latetreat==1)
replace timeequip=2 if (wave==4 & atreatequip==1 & earlytreat==1)|(wave==5 & earlytreat==0 & latetreat==0 & atreatequip==1)|(wave==6 & latetreat==1 & atreatequip==1)
replace timeequip=3 if (wave==5 & atreatequip==1 & earlytreat==1)|(wave==6 & earlytreat==0 & latetreat==0 & atreatequip==1)
replace timeequip=4 if (wave==6 & atreatequip==1 & earlytreat==1)
gen timetreat=timecash+timeequip

for num 1/4: gen atreatcash_timeX=timecash==X
for num 1/4: gen atreatequip_timeX=timeequip==X
for num 1/4: gen atreat_timeX=atreatcash_timeX+atreatequip_timeX

** Capital Stock data
* Note Simon's revised data from checks don't take account of corrections from text fields
* e.g. I manually coded GH2 as 2 in my do file, he has it coded as missing.
* So only correct cases where revisions differ from my measures and revisions not missing
gen inventories1=inventories
replace inventories1=InventoriesRevised if inventories~=InventoriesRevised & InventoriesRevised~=.
bysort wave: sum inventories inventories1
gen cashonhand1=cashonhand
replace cashonhand1=CashRevised if cashonhand~=CashRevised & CashRevised~=.
bysort wave: sum cashonhand cashonhand1

* replace error
replace inventories=. if SHENO==110500502 & (wave==1|wave==2)
replace inventories1=. if SHENO==110500502 & (wave==1|wave==2)
replace inventories=. if inventories==350000
replace inventories1=. if inventories1==350000
* trim at 99th percentile
gen inventoriescap=inventories1 if inventories1<=3500



gen totalK=OtherCapital_OwnedFixed+inventories1
gen totalcap=OtherCapital_OwnedFixed+inventoriescap
* Replace one large outlier with missing
replace totalK=. if sheno==110402708 & wave==4

gen control=cashtreat==0 & equiptreat==0
gen paidemployee=paidemployeehours>0 & paidemployeehours~=.

gen businesshome=location==2
replace educ_years=. if educ_years>=999
gen akanspeaker=language_home==1
gen gaspeaker=language_home==3
gen everloan=mfi_loan==1|bank_loan==1
gen fem=female_female==1|female_mixed==1

*** Principal Component of Baseline Household Assets
pca OwnsLandline-OwnsCar if wave==1
predict assetindex
egen aindex=max(assetindex), by(sheno)


gen baseprof=realfinalprof if wave==1
egen mbaseprof=max(baseprof), by(sheno)
gen wave2prof=realfinalprof if wave==2
egen mwave2prof=max(wave2prof), by(sheno)

gen female=gender=="female"
gen male=gender=="male"
egen maxprofgroup=max(realfinalprof) if wave==2, by(groupnum)
egen minprofgroup=min(realfinalprof) if wave==2, by(groupnum)
gen lowgroup=maxprofgroup<92
egen mlowgroup=max(lowgroup), by(sheno)
gen highgroup=mlowgroup==0
gen lowcapture=highcapture==0
gen lowcapital=highcapital==0

gen atreatcashfemale=atreatcash*female
gen atreatequipfemale=atreatequip*female
gen atreatcashmale=atreatcash*male
gen atreatequipmale=atreatequip*male
for num 2/6: gen waveX_female=waveX*female

*/

cap log close
use ReplicationDataGhanaJDE.dta, clear
xtset sheno wave

**** Figures
*** Figure 1: Post-treatment profits for Males
cumul realfinalprofit if (wave==5|wave==6) & atreat==0 & female==0 & trimgroup~=1, gen(cum1m) equal
cumul realfinalprofit if (wave==5|wave==6) & cashtreat==1 & female==0 & trimgroup~=1, gen(cum2m) equal
cumul realfinalprofit if (wave==5|wave==6) & equiptreat==1 & female==0 & trimgroup~=1, gen(cum3m) equal
label var cum1m "Control"
label var cum2m "Cash"
label var cum3m "In-kind"
sort realfinalprofit
twoway line cum1m cum2m cum3m realfinalprofit if (wave==5|wave==6) & realfinalprofit<1500 & female==0
* test of equality of distributions
ranksum realfinalprofit if (wave==5|wave==6) & female==0 & trimgroup~=1 & equiptreat==0, by(cashtreat)
ranksum realfinalprofit if (wave==5|wave==6) & female==0 & trimgroup~=1 & cashtreat==0, by(equiptreat)
ranksum realfinalprofit if (wave==5|wave==6) & female==0 & trimgroup~=1 & (cashtreat==1|equiptreat==1), by(equiptreat)

*** Figure 2: Post-treatment profits for Females
cumul realfinalprofit if (wave==5|wave==6) & atreat==0 & female==1 & trimgroup~=1, gen(cum1f) equal
cumul realfinalprofit if (wave==5|wave==6) & cashtreat==1 & female==1 & trimgroup~=1, gen(cum2f) equal
cumul realfinalprofit if (wave==5|wave==6) & equiptreat==1 & female==1 & trimgroup~=1, gen(cum3f) equal
label var cum1f "Control"
label var cum2f "Cash"
label var cum3f "In-kind"
sort realfinalprofit
twoway line cum1f cum2f cum3f realfinalprofit if (wave==5|wave==6) & realfinalprofit<1500 & female==1
* test of equality of distributions
ranksum realfinalprofit if (wave==5|wave==6) & female==1 & trimgroup~=1 & equiptreat==0, by(cashtreat)
ranksum realfinalprofit if (wave==5|wave==6) & female==1 & trimgroup~=1 & cashtreat==0, by(equiptreat)
ranksum realfinalprofit if (wave==5|wave==6) & female==1 & trimgroup~=1 & (cashtreat==1|equiptreat==1), by(equiptreat)



*** Figure 3: Post-treatment capital stock for males
cumul totalK if (wave==5|wave==6) & atreat==0 & male==1, gen(cum1dim) equal
cumul totalK if (wave==5|wave==6) & cashtreat==1 & male==1, gen(cum2dim) equal
cumul totalK if (wave==5|wave==6) & equiptreat==1 & male==1, gen(cum3dim) equal
label var cum1dim "Control"
label var cum2dim "Cash"
label var cum3dim "In-kind"
sort totalK
twoway line cum1dim cum2dim cum3dim totalK if (wave==5|wave==6) & totalK<5000 & male==1
* test of equality of distributions
ranksum totalK if (wave==5|wave==6) & female==0 &  equiptreat==0, by(cashtreat)
ranksum totalK if (wave==5|wave==6) & female==0 &  cashtreat==0, by(equiptreat)
ranksum totalK if (wave==5|wave==6) & female==0 &  (cashtreat==1|equiptreat==1), by(equiptreat)


*** Figure 4: Post-treatment capital stock for females
cumul totalK if (wave==5|wave==6) & atreat==0 & female==1, gen(cum1dif) equal
cumul totalK if (wave==5|wave==6) & cashtreat==1 & female==1, gen(cum2dif) equal
cumul totalK if (wave==5|wave==6) & equiptreat==1 & female==1, gen(cum3dif) equal
label var cum1dif "Control"
label var cum2dif "Cash"
label var cum3dif "In-kind"
sort totalK
twoway line cum1dif cum2dif cum3dif totalK if (wave==5|wave==6) & totalK<5000 & female==1
* test of equality of distributions
ranksum totalK if (wave==5|wave==6) & female==1 &  equiptreat==0, by(cashtreat)
ranksum totalK if (wave==5|wave==6) & female==1 &  cashtreat==0, by(equiptreat)
ranksum totalK if (wave==5|wave==6) & female==1 &  (cashtreat==1|equiptreat==1), by(equiptreat)


**************************** TABLES *********************************
*** Table 2: Verifying Randomization
*** Means for full experimental subsample and trimmed subsample
** first do for variables we stratified or matched on
#delimit ;
log using balance.log, replace;
foreach lhs of varlist realfinalprofit fem highcapture highcapital male_male male_mixed female_female female_mixed 
 finalsales  inventories  hourslastweek totalK useasusu businesshome married
educ_years digitspan akanspeaker gaspeaker age firmage everloan business_taxnumber  
{;
if "`lhs'" == "realfinalprofit" {; local ra "replace"; };
else {; local ra "append"; };

* OLS estimation without strata controls;
reg `lhs' control cashtreat equiptreat if wave==2, noc robust;
outreg2 control cashtreat equiptreat using ghanatreat.out, nonotes bdec(2) nose noaster `ra';
test cashtreat==equiptreat==control;
reg `lhs' control cashtreat equiptreat if wave==2 & trimgroup~=1, noc robust;
outreg2 control cashtreat equiptreat using ghanatreat1.out, nonotes bdec(2) nose noaster `ra';
test cashtreat==equiptreat==control;
};
log close;

#delimit cr
* Also do wave 1 profits
reg realfinalprof control cashtreat equiptreat if wave==1, noc robust
test cashtreat==equiptreat==control
reg realfinalprof control cashtreat equiptreat if wave==1 & trimgroup~=1, noc robust
test cashtreat==equiptreat==control


**** Table 3 - main treatment effects on Profits
log using newtab2.log, replace
*** OLS with strata dummies and clustering
areg realfinalprof atreatcash atreatequip wave2-wave6, abs(groupnum) cluster(sheno)
outreg2 atreatcash atreatequip using table2.out, replace nonotes bdec(2)
test atreatcash=atreatequip
*** OLS with strata dummies and clustering and trimming
areg realfinalprof atreatcash atreatequip wave2-wave6 if trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcash atreatequip using table2.out, append nonotes bdec(2)
test atreatcash=atreatequip
*** Fixed effects with all rounds
xtreg realfinalprof atreatcash atreatequip wave2-wave6, fe cluster(sheno)
outreg2 atreatcash atreatequip using table2.out, append nonotes bdec(2)
test atreatcash=atreatequip
*** Fixed effects with all rounds and trimming
xtreg realfinalprof atreatcash atreatequip wave2-wave6 if trimgroup~=1, fe cluster(sheno)
outreg2 atreatcash atreatequip using table2.out, append nonotes bdec(2)
test atreatcash=atreatequip

**then divide by gender
areg realfinalprof atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female, abs(groupnum) cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table2.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
areg realfinalprof atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table2.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
xtreg realfinalprof atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female, fe cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table2.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
xtreg realfinalprof atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, fe cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table2.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
areg realfinalprof atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if wave>=5, abs(groupnum) cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table2.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
areg realfinalprof atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1 & wave>=5, abs(groupnum) cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table2.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
log close


*** APPENDIX 3 TABLE: TIME SINCE TREATMENT
areg realfinalprofit atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 wave2-wave6, abs(groupnum) cluster(sheno)
outreg2 atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 using timesince1.out, replace nonotes bdec(2)
test atreatcash_time1=atreatcash_time2=atreatcash_time3=atreatcash_time4
test atreatequip_time1=atreatequip_time2=atreatequip_time3=atreatequip_time4
areg realfinalprofit atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 wave2-wave6 if trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 using timesince1.out, append nonotes bdec(2)
test atreatcash_time1=atreatcash_time2=atreatcash_time3=atreatcash_time4
test atreatequip_time1=atreatequip_time2=atreatequip_time3=atreatequip_time4
xtreg realfinalprofit atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 wave2-wave6, fe cluster(sheno)
outreg2 atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 using timesince1.out, append nonotes bdec(2)
test atreatcash_time1=atreatcash_time2=atreatcash_time3=atreatcash_time4
test atreatequip_time1=atreatequip_time2=atreatequip_time3=atreatequip_time4
xtreg realfinalprofit atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 wave2-wave6 if trimgroup~=1, fe cluster(sheno)
outreg2 atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 using timesince1.out, append nonotes bdec(2)
test atreatcash_time1=atreatcash_time2=atreatcash_time3=atreatcash_time4
test atreatequip_time1=atreatequip_time2=atreatequip_time3=atreatequip_time4
* by gender
areg realfinalprofit atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 wave2-wave6 if trimgroup~=1 & gender=="male", abs(groupnum) cluster(sheno)
outreg2 atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 using timesince1.out, append nonotes bdec(2)
test atreatcash_time1=atreatcash_time2=atreatcash_time3=atreatcash_time4
test atreatequip_time1=atreatequip_time2=atreatequip_time3=atreatequip_time4
xtreg realfinalprofit atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 wave2-wave6 if trimgroup~=1 & gender=="male", fe cluster(sheno)
outreg2 atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 using timesince1.out, append nonotes bdec(2)
test atreatcash_time1=atreatcash_time2=atreatcash_time3=atreatcash_time4
test atreatequip_time1=atreatequip_time2=atreatequip_time3=atreatequip_time4
areg realfinalprofit atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 wave2-wave6 if trimgroup~=1 & gender=="female", abs(groupnum) cluster(sheno)
outreg2 atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 using timesince1.out, append nonotes bdec(2)
test atreatcash_time1=atreatcash_time2=atreatcash_time3=atreatcash_time4
test atreatequip_time1=atreatequip_time2=atreatequip_time3=atreatequip_time4
xtreg realfinalprofit atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 wave2-wave6 if trimgroup~=1 & gender=="female" , fe cluster(sheno)
outreg2 atreatcash_time1-atreatcash_time4 atreatequip_time1-atreatequip_time4 using timesince1.out, append nonotes bdec(2)
test atreatcash_time1=atreatcash_time2=atreatcash_time3=atreatcash_time4
test atreatequip_time1=atreatequip_time2=atreatequip_time3=atreatequip_time4


*** TABLE 4: HETEROGENEITY ACCORDING TO VARIABLES USED FOR STRATIFICATION/MATCHING

#delimit ;
foreach lhs of varlist male_male male_mixed female_female female_mixed highcapture lowcapture highcapital lowcapital highgroup mlowgroup 
{;
gen atreatcash_`lhs'=atreatcash*`lhs';
gen atreatequip_`lhs'=atreatequip*`lhs';
for num 2/6: gen waveX_`lhs'=waveX*`lhs';
};

#delimit cr
log using hetero.log, replace

*** Panel A: Heterogeneity in Female Sample
** By industry
areg realfinalprof atreatcash_female_female atreatcash_female_mixed atreatequip_female_female atreatequip_female_mixed wave2-wave6 wave2_female_female-wave6_female_female if trimgroup~=1 & female==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_female_female atreatcash_female_mixed atreatequip_female_female atreatequip_female_mixed  using table3f.out, replace nonotes bdec(2)
test atreatcash_female_female=atreatcash_female_mixed
test atreatequip_female_female=atreatequip_female_mixed
test atreatcash_female_female=atreatequip_female_female
test atreatcash_female_mixed=atreatequip_female_mixed, acc
xtreg realfinalprof atreatcash_female_female atreatcash_female_mixed atreatequip_female_female atreatequip_female_mixed wave2-wave6 wave2_female_female-wave6_female_female if trimgroup~=1 & female==1, cluster(sheno) fe
outreg2 atreatcash_female_female atreatcash_female_mixed atreatequip_female_female atreatequip_female_mixed  using table3f.out, append nonotes bdec(2)
test atreatcash_female_female=atreatcash_female_mixed
test atreatequip_female_female=atreatequip_female_mixed
test atreatcash_female_female=atreatequip_female_female
test atreatcash_female_mixed=atreatequip_female_mixed, acc
** By capture
areg realfinalprof atreatcash_lowcapture atreatcash_highcapture atreatequip_lowcapture atreatequip_highcapture  wave2-wave6 wave2_lowcapture-wave6_lowcapture if trimgroup~=1 & female==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture  using table3f.out, append nonotes bdec(2)
test atreatcash_lowcapture=atreatcash_highcapture
test atreatequip_lowcapture=atreatequip_highcapture
test atreatcash_lowcapture=atreatequip_lowcapture
test atreatcash_highcapture=atreatequip_highcapture, acc
xtreg realfinalprof atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture  wave2-wave6 wave2_lowcapture-wave6_lowcapture if trimgroup~=1 & female==1, cluster(sheno) fe
outreg2 atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture   using table3f.out, append nonotes bdec(2)
test atreatcash_lowcapture=atreatcash_highcapture
test atreatequip_lowcapture=atreatequip_highcapture
test atreatcash_lowcapture=atreatequip_lowcapture
test atreatcash_highcapture=atreatequip_highcapture, acc
** By baseline capital
areg realfinalprof atreatcash_lowcapital atreatcash_highcapital atreatequip_lowcapital atreatequip_highcapital  wave2-wave6 wave2_lowcapital-wave6_lowcapital if trimgroup~=1 & female==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital  using table3f.out, append nonotes bdec(2)
test atreatcash_lowcapital=atreatcash_highcapital
test atreatequip_lowcapital=atreatequip_highcapital 
test  atreatcash_lowcapital=atreatequip_lowcapital
test atreatcash_highcapital=atreatequip_highcapital, acc
xtreg realfinalprof atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital  wave2-wave6 wave2_lowcapital-wave6_lowcapital if trimgroup~=1 & female==1, cluster(sheno) fe
outreg2 atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital   using table3f.out, append nonotes bdec(2)
test atreatcash_lowcapital=atreatcash_highcapital
test atreatequip_lowcapital=atreatequip_highcapital 
test  atreatcash_lowcapital=atreatequip_lowcapital
test atreatcash_highcapital=atreatequip_highcapital, acc 
** By baseline profits group
areg realfinalprof atreatcash_mlowgroup atreatcash_highgroup atreatequip_mlowgroup atreatequip_highgroup  wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if trimgroup~=1 & female==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  using table3f.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup 
test atreatequip_mlowgroup=atreatequip_highgroup
test atreatcash_mlowgroup=atreatequip_mlowgroup
test atreatcash_highgroup=atreatequip_highgroup, acc
xtreg realfinalprof atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if trimgroup~=1 & female==1, cluster(sheno) fe
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  using table3f.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup 
test atreatequip_mlowgroup=atreatequip_highgroup
test atreatcash_mlowgroup=atreatequip_mlowgroup
test atreatcash_highgroup=atreatequip_highgroup, acc
log close


*** Panel B: Heterogeneity in Male Sample
** By industry
areg realfinalprof atreatcash_male_male atreatcash_male_mixed atreatequip_male_male atreatequip_male_mixed wave2-wave6 wave2_male_male-wave6_male_male if trimgroup~=1 & male==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_male_male atreatcash_male_mixed atreatequip_male_male atreatequip_male_mixed  using table3.out, replace nonotes bdec(2)
test atreatcash_male_male=atreatcash_male_mixed
test atreatequip_male_male=atreatequip_male_mixed
test atreatcash_male_male=atreatequip_male_male
test atreatcash_male_mixed=atreatequip_male_mixed, acc

xtreg realfinalprof atreatcash_male_male atreatcash_male_mixed atreatequip_male_male atreatequip_male_mixed wave2-wave6 wave2_male_male-wave6_male_male if trimgroup~=1 & male==1, cluster(sheno) fe
outreg2 atreatcash_male_male atreatcash_male_mixed atreatequip_male_male atreatequip_male_mixed  using table3.out, append nonotes bdec(2)
test atreatcash_male_male=atreatcash_male_mixed
test atreatequip_male_male=atreatequip_male_mixed
test atreatcash_male_male=atreatequip_male_male
test atreatcash_male_mixed=atreatequip_male_mixed, acc
** By capture
areg realfinalprof atreatcash_lowcapture atreatcash_highcapture atreatequip_lowcapture atreatequip_highcapture  wave2-wave6 wave2_lowcapture-wave6_lowcapture if trimgroup~=1 & male==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture  using table3.out, append nonotes bdec(2)
test atreatcash_lowcapture=atreatcash_highcapture
test atreatequip_lowcapture=atreatequip_highcapture
test atreatcash_lowcapture=atreatequip_lowcapture
test atreatcash_highcapture=atreatequip_lowcapture, acc
xtreg realfinalprof atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture  wave2-wave6 wave2_lowcapture-wave6_lowcapture if trimgroup~=1 & male==1, cluster(sheno) fe
outreg2 atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture   using table3.out, append nonotes bdec(2)
test atreatcash_lowcapture=atreatcash_highcapture
test atreatequip_lowcapture=atreatequip_highcapture
test atreatcash_lowcapture=atreatequip_lowcapture
test atreatcash_highcapture=atreatequip_lowcapture, acc
** By baseline capital
areg realfinalprof atreatcash_lowcapital atreatcash_highcapital atreatequip_lowcapital atreatequip_highcapital  wave2-wave6 wave2_lowcapital-wave6_lowcapital if trimgroup~=1 & male==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital  using table3.out, append nonotes bdec(2)
test atreatcash_lowcapital=atreatcash_highcapital
test atreatequip_lowcapital=atreatequip_highcapital 
test  atreatcash_lowcapital=atreatequip_lowcapital
test atreatcash_highcapital=atreatequip_highcapital, acc
xtreg realfinalprof atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital  wave2-wave6 wave2_lowcapital-wave6_lowcapital if trimgroup~=1 & male==1, cluster(sheno) fe
outreg2 atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital   using table3.out, append nonotes bdec(2)
test atreatcash_lowcapital=atreatcash_highcapital
test atreatequip_lowcapital=atreatequip_highcapital  
test  atreatcash_lowcapital=atreatequip_lowcapital
test atreatcash_highcapital=atreatequip_highcapital, acc
** By baseline profits group
areg realfinalprof atreatcash_mlowgroup atreatcash_highgroup atreatequip_mlowgroup atreatequip_highgroup  wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if trimgroup~=1 & male==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  using table3.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup 
test atreatequip_mlowgroup=atreatequip_highgroup
test atreatcash_mlowgroup=atreatequip_mlowgroup
test atreatcash_highgroup=atreatequip_highgroup, acc
xtreg realfinalprof atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if trimgroup~=1 & male==1, cluster(sheno) fe
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  using table3.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup 
test atreatequip_mlowgroup=atreatequip_highgroup
test atreatcash_mlowgroup=atreatequip_mlowgroup
test atreatcash_highgroup=atreatequip_highgroup, acc



#delimit cr


*** TABLE 6: WHERE DID THE GRANTS GO?
*** CAPITAL STOCK
tab groupnum if totalK>5000 & totalK~=. & wave<=2
gen captrimgroup=groupnum==104|groupnum==303|groupnum==512|groupnum==709|groupnum==1101|groupnum==1304|groupnum==1308|groupnum==1309|groupnum==30001|groupnum==60003
egen maxgroup=max(totalK) if wave<=2, by(groupnum)
egen mgroup=max(maxgroup), by(sheno)

** replace some errors with missing
* two firms who claim to have daily susu of 140 and 5000 cedi in wave 1, and then 1 or 2 cedi thereafter
destring SHENO, replace
replace dailysusu=. if wave==1 & (SHENO==140700505|SHENO==300103404)

* generate indicator of other business income
gen haveotherbus=incomeotherbus>0 & incomeotherbus~=.
replace haveotherbus=. if incomeotherbus==.

* replace missing transfers as missing instead of zero
replace amounttransferout=. if madetransfersout==.

* replace a few massive outliers as missing in expenditure
* one value of 1000000 in education
replace expend_education_3months=. if expend_education_3months>3000
replace expend_total_3months=. if expend_total_3months>5000
replace expend_total=. if expend_total>20000
replace expend_logtotal=. if expend_logtotal>10

*** Aggregate expenditure categories
* Food
replace expend_fooddrink_outside_week=0 if expend_fooddrink_outside_week==. & expend_fooddrink_home_week ~=.
gen weeklyfood=expend_fooddrink_home_week +expend_fooddrink_outside_week
* Clothing and footwear
gen clothingshoes=expend_clothing_3months+expend_footwear_3months
* Ceremonies - lots of missing which are most likely zeros, replace if not missing for other expenditure categories
replace expend_ceremonies_3months=0 if expend_ceremonies_3months==.
replace expend_ceremonies_3months=. if expend_health_3months==. & expend_education_3months==. & weeklyfood==.
* Education and health
gen healthedn=expend_health_3months+expend_education_3months

#delimit ;
foreach lhs of varlist weeklyfood clothingshoes expend_ceremonies_3months healthedn expend_total_month expend_total_3months expend_total expend_logtotal  {;
egen top995_`lhs'=pctile(`lhs') if trimgroup~=1, p(99.5);
gen `lhs'_trunc=`lhs';
replace `lhs'_trunc=top995_`lhs' if `lhs'>top995_`lhs' & `lhs'~=.;
};

#delimit cr
egen top995_totalK=pctile(totalK) if trimgroup~=1, p(99.5)
gen totalK_trunc=totalK
replace totalK_trunc=top995_totalK if totalK>top995_totalK & totalK~=.

#delimit cr
egen top99_totalK=pctile(totalK) if trimgroup~=1, p(99)
gen totalK_trunc1=totalK
replace totalK_trunc1=top99_totalK if totalK>top99_totalK & totalK~=.

#delimit cr
egen top95_totalK=pctile(totalK) if trimgroup~=1, p(95)
gen totalK_trunc2=totalK
replace totalK_trunc2=top95_totalK if totalK>top95_totalK & totalK~=.


log using table4.log, replace
****
** Capital Stock
xtreg totalK atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, fe cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table4.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
xtreg totalK  atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if female==1 & trimgroup~=1, fe cluster(sheno)
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup using table4a.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup
test atreatequip_mlowgroup=atreatequip_highgroup

*** truncated capital stock
xtreg totalK_trunc atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, fe cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table4.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
xtreg totalK_trunc  atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if female==1 & trimgroup~=1, fe cluster(sheno)
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup using table4a.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup
test atreatequip_mlowgroup=atreatequip_highgroup

*** Transfers out
** Made transfers out and amount made
areg madetransfersout atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table4.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
areg amounttransferout atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale  using table4.out, append nonotes bdec(2)
test atreatcashfemale=atreatequipfemale
test atreatcashmale=atreatequipmale
test atreatcashmale=atreatcashfemale
test atreatequipmale=atreatequipfemale
areg madetransfersout  atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if female==1 & trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup using table4a.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup
test atreatequip_mlowgroup=atreatequip_highgroup
areg amounttransferout  atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if female==1 & trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup using table4a.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup
test atreatequip_mlowgroup=atreatequip_highgroup

log close

log using expendlog.log, replace
#delimit ;
*foreach lhs of varlist weeklyfood clothingshoes expend_ceremonies_3months healthedn expend_total_month expend_total_3months expend_total expend_logtotal  weeklyfood_trunc-expend_logtotal_trunc   {;
foreach lhs of varlist weeklyfood clothingshoes expend_ceremonies_3months healthedn expend_total_month expend_total_3months expend_total expend_logtotal   {;
if "`lhs'" == "weeklyfood" {; local ra "replace"; };
else {; local ra "append"; };
/*
* OLS estimation with randomization strata controls;
areg `lhs' atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 if trimgroup~=1, robust cluster(sheno) a(groupnum);
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale using table4p2.out, nonotes bdec(2) `ra';
test atreatcashfemale=atreatequipfemale;
test atreatcashmale=atreatequipmale;*/
* FE estimation;
xtreg `lhs'_trunc atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 if trimgroup~=1, fe cluster(sheno); 
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale using table4p2.out, nonotes bdec(2) append;
test atreatcashfemale=atreatequipfemale;
test atreatcashmale=atreatequipmale;
* by female subgroup;
/*
areg `lhs'  atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if female==1 & trimgroup~=1, abs(groupnum) cluster(sheno);
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup using table4p2a.out, `ra' nonotes bdec(2);
test atreatcash_mlowgroup=atreatcash_highgroup;
test atreatequip_mlowgroup=atreatequip_highgroup;*/
xtreg `lhs'_trunc  atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if female==1 & trimgroup~=1, fe cluster(sheno);
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup using table4p2a.out, append nonotes bdec(2);
test atreatcash_mlowgroup=atreatcash_highgroup;
test atreatequip_mlowgroup=atreatequip_highgroup;
};
log close;

#delimit cr

*** Table 7 : How do the low profits women compare to the high profits
gen g1=1 if gender=="male"
replace g1=2 if gender=="female" & mlowgroup==1
replace g1=3 if gender=="female" & mlowgroup==0
gen spousesupports=spouse_support_bus>=4 & spouse_support_bus~=.
replace spousesupports=. if spouse_support==.
gen spousegive=spouse_give_money>=4 & spouse_give_money~=.
replace spousegive=. if spouse_give_money==.
* collapse (mean) everloan educ_years digitspan sectchoice_balance sectchoice_profits spousesupports spousegive willingness_to_take_risks spouse_compell aindex, by(g1)

for var everloan educ_years digitspan sectchoice_balance sectchoice_profits spousesupports spousegive willingness_to_take_risks spouse_compell aindex realfinalprof totalK finalsales: ttest X if wave==2 & female==1 & (g1==2|g1==3) & trimgroup~=1, by(g1)
gen saveregular=save_reg==4|save_reg==5
replace saveregular=. if save_reg==.
for var businesshome keepsaccounts age firmage OwnsMobile saveregular: ttest X if wave==2 & female==1 & (g1==2|g1==3) & trimgroup~=1, by(g1)

for var sectchoice_familyexp- sectchoice_social : ttest X if wave==2 & female==1 & (g1==2|g1==3) & trimgroup~=1, by(g1)

bysort g1: sum keepsaccounts if wave==2 & trimgroup~=1

bysort g1: sum realfinalprofit totalK finalsales if wave==2 & trimgroup~=1, de
bysort g1: sum age firmage if wave==2 & trimgroup~=1
median realfinalprofit if wave==2 & female==1 & (g1==2|g1==3) & trimgroup~=1, by(g1)
median totalK if wave==2 & female==1 & (g1==2|g1==3) & trimgroup~=1, by(g1)
median finalsales if wave==2 & female==1 & (g1==2|g1==3) & trimgroup~=1, by(g1)


**** Appendix Table 2: Sensitivity to Attrition
**** Now look at attrition and how it varies with treatment status
*** attrition as measured by whether not surveyed/refused
replace attrited=1 if attrited==.
replace attrited=0 if wave==1
egen maxattrit=sum(attrited), by(sheno)
gen everattrit=maxattrit>0
bysort wave: sum attrited if trimgroup~=1
bysort wave: sum attrited if cashtreat==0 & equiptreat==0 & trimgroup~=1
bysort wave: sum attrited if cashtreat==1 & trimgroup~=1
bysort wave: sum attrited if equiptreat==1 & trimgroup~=1
gen treatgroup=1 if cashtreat==0 & equiptreat==0
replace treatgroup=2 if cashtreat==1
replace treatgroup=3 if equiptreat==1
sum everattrit if wave==1 & trimgroup~=1
bysort treatgroup: sum everattrit if wave==1 & trimgroup~=1
for num 1/6: areg attrited cashtreat equiptreat if trimgroup~=1 & wave==X, robust a(groupnum)
areg everattrit cashtreat equiptreat if wave==1 & trimgroup~=1, robust a(groupnum)

*** attrition as measured by whether doesn't report profits in wave
gen missprofit=realfinalprofit==.
bysort wave: sum missprofit if trimgroup~=1
egen evermissprofit=max(missprofit), by(sheno)
sum evermissprofit if trimgroup~=1
for num 1/3: bysort wave: sum missprofit if treatgroup==X
bysort treatgroup: sum evermissprofit if wave==1 & trimgroup~=1
for num 1/6: areg missprofit cashtreat equiptreat if trimgroup~=1 & wave==X, robust a(groupnum)
areg evermissprofit cashtreat equiptreat baseprof if wave==1 & trimgroup~=1, robust a(groupnum)

cap drop wave2prof
gen wave2prof=realfinalprof if wave==2
egen mwave2=max(wave2prof), by(sheno)
gen lowprof2=mwave2<68

areg missprofit atreatcash atreatequip female lowprof2 wave2-wave6 if  trimgroup~=1, cluster(sheno) a(groupnum)

gen lowprof2atreatcash=lowprof2*atreatcash
gen lowprof2atreatequip=lowprof2*atreatequip
for num 2/6: gen waveX_lowprof2=waveX*lowprof2

areg missprofit atreatcash atreatequip lowprof2atreatcash lowprof2atreatequip lowprof2 wave2-wave6 wave2_lowprof2-wave6_lowprof2 if  trimgroup~=1, cluster(sheno) a(groupnum)

*** Lee bounds
* Baseline specification for comparison
areg realfinalprof atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale using tablea1.out, replace
xtreg realfinalprof atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, fe cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale using tablea1.out, append

** trimming top profits from treatment groups according to difference with control
gen realprofleehigh=realfinalprof
gen realprofleelow=realfinalprof
egen lh3=pctile(realfinalprof) if wave==3 & cashtreat==1, p(98.4)
egen ll3=pctile(realfinalprof) if wave==3 & cashtreat==1, p(1.6)
egen lh4=pctile(realfinalprof) if wave==4 & cashtreat==1, p(94.8)
egen ll4=pctile(realfinalprof) if wave==4 & cashtreat==1, p(5.2)
egen lh5=pctile(realfinalprof) if wave==5 & cashtreat==1, p(96.9)
egen ll5=pctile(realfinalprof) if wave==5 & cashtreat==1, p(3.1)
egen lh6=pctile(realfinalprof) if wave==6 & cashtreat==1, p(94.0)
egen ll6=pctile(realfinalprof) if wave==6 & cashtreat==1, p(6.0)
egen lh3a=pctile(realfinalprof) if wave==3 & equiptreat==1, p(99.5)
egen ll3a=pctile(realfinalprof) if wave==3 & equiptreat==1, p(0.5)
egen lh4a=pctile(realfinalprof) if wave==4 & equiptreat==1, p(94.3)
egen ll4a=pctile(realfinalprof) if wave==4 & equiptreat==1, p(5.7)
egen lh5a=pctile(realfinalprof) if wave==5 & equiptreat==1, p(95.2)
egen ll5a=pctile(realfinalprof) if wave==5 & equiptreat==1, p(4.8)
egen lh6a=pctile(realfinalprof) if wave==6 & equiptreat==1, p(94.0)
egen ll6a=pctile(realfinalprof) if wave==6 & equiptreat==1, p(6.0)
for num 3/6: replace realprofleehigh=. if realfinalprof>lhX & wave==X & cashtreat==1
for num 3/6: replace realprofleehigh=. if realfinalprof>lhXa & wave==X & equiptreat==1
for num 3/6: replace realprofleelow=. if realfinalprof<llX & wave==X & cashtreat==1
for num 3/6: replace realprofleelow=. if realfinalprof<llXa & wave==X & equiptreat==1

areg realprofleelow atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale using tablea1.out, append
areg realprofleehigh atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, abs(groupnum) cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale using tablea1.out, append
xtreg realprofleelow atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, fe cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale using tablea1.out, append
xtreg realprofleehigh atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale wave2-wave6 wave2_female-wave6_female if trimgroup~=1, fe cluster(sheno)
outreg2 atreatcashfemale atreatequipfemale atreatcashmale atreatequipmale using tablea1.out, append



****** Table 8
* Looking at Interactions : creating interaction variables

* when have money people request
gen norm7=norms_7=="Agree"|norms_7=="Strongly agree"
gen norm8=norms_8=="Agree"|norms_8=="Strongly agree"
gen norm9=norms_9=="Agree"|norms_9=="Strongly agree"
gen norm12=norms_12=="Agree"|norms_12=="Strongly agree"

* Use a susu at baseline
gen bsusu=useasusu if wave==1
egen basesusu=max(bsusu), by(SHENO)
* Spouse compels to give money
gen spcompel=spouse_compel==1
replace spcompel=. if spouse_compel==.
* Spend without consulting spouse
gen spendfreely=spend_wo_consult==4|spend_wo_consult==5
replace spendfreely=. if spend_wo_consult==.
* Spouse supportive
gen supspouse=spouse_support_bus==4|spouse_support_bus==5
replace supspouse=. if spouse_support_bus==.
* Is the household head
gen hhhead=rel_hhhead==1
* Save regularly
*gen saveregular=save_reg==4|save_reg==5
*replace saveregular=. if save_reg==.
* Spouse will know if profits high
gen spouseknow=knowprof_spouse==1
replace spouseknow=. if knowprof_spouse==4|knowprof_spouse==.
* Parents or Children know if profits high (only 3% say extended family would know)
gen othersknow=knowprof_parents==1|knowprof_children==1
replace othersknow=. if (knowprof_parents==4 & knowprof_children==4)|(knowprof_parents==. & knowprof_children==.)
* Pressure to share with other household members
gen pressurehh=pressure_share_otherhh==1|pressure_share_otherhh==2
replace pressurehh=. if pressure_share_otherhh==.
* Pressure to share with non-household members=  only 9% say some or lots of pressure
* Would spend extra profit on self
gen spendself=xtraprof_yourself==1
replace spendself=. if xtraprof_yourself==.
* would spend extra profit on hh
gen spendhh=xtraprof_hhold==1
replace spendhh=. if xtraprof_hhold==.
* would spend extra profit on business assets
gen spendbus=xtraprof_busequip==1
replace spendbus=. if xtraprof_busequip==.
* Amount of 100 lottery would give away (demeaned)
gen amount100give=lottery_giveaway-19.71
* Would share so that assure others will share
gen shareret=share_return==4|share_return==5
replace shareret=. if share_return==.
* Would share to improve position in community
gen sharecomm=share_stature==4|share_stature==5
replace sharecomm=. if share_stature==.
* Would share so can borrow in emergencies
gen shareborrow=share_borrow==4|share_borrow==5
replace shareborrow=. if share_borrow==.
* Would share so others come to funeral
gen sharefuneral=share_funeral==4|share_funeral==5
replace sharefuneral=. if share_funeral==.
* Give Discount and Hyperbolicity shorter names
gen dis1=Discount_MedianRevised
gen dis2=Discount_MedianTrimRevised
gen hyp1=Discount_HyperbolicRevised
gen hyp2=Discount_HyperbolicTrimRevised
* demeaned digitspan
gen dspan=digitspan-5.080729 
* demeaned education
gen ednyears=educ_years-8.885161
* Other hh members selfemployed at baseline
gen otherse=Roster_SelfEmp>0
replace otherse=. if Roster_SelfEmp==.
* Other hh members wage earners at baseline
gen otherwage=Roster_Wage>0
replace otherwage=. if Roster_Wage==.
* Number of children demeaned
gen children=Children_Total
replace children=0 if Children_Total==.
replace children=children-2.160992
* Baseline household size
gen hhsize=Household_Members+1
replace hhsize=hhsize-3.834879
* Number of kids 0 to 6
gen kids=Children_Age0to6
replace kids=0 if kids==.
replace kids=kids-.4846574
* shorter names of the new variables
gen hh1=HouseholdSize
replace hh1=hh1-2.491094 
gen ohb=OtherHouseholdBusiness
gen sma=SpouseMoreAssets
gen s50=Spouse50
* Construct principal component of savings/hyperbolicity/self-control
pca useasusu saveregular dis1 hyp1 if wave==1
predict selfcontrolpca
egen mselfcontrolpca=max(selfcontrolpca), by(SHENO)
 rename mselfcontrolpca mpca

exit

 * Construct hyperbolicity/discount only pca
pca  dis1 hyp1 if wave==1
predict selfcontrolpca3
egen mselfcontrolpca3=max(selfcontrolpca3), by(SHENO)
 rename mselfcontrolpca3 mpca3
 
gen agediff=OwnMarriageAge-SpouseMarriageAge
gen assetshare=OwnAssetsBrought/(OwnAssetsBrought+SpouseAssetsBrought)
rename SpouseOperatingBusiness spousebus

gen r1expend=expend_total if wave==1
gen r1profit=realfinalprofit if wave==1
gen r2expend=expend_total if wave==2
gen r2profit=realfinalprofit if wave==2
for var r1expend-r2profit: egen mX=max(X), by(sheno)
gen profshareofcons=((mr1profit+mr2profit)*3)/(mr1expend+mr2expend)
sum profshareofcons, de
gen lowshareprof=profshareofcons<0.25
gen wave2lowK=inventories<50 & wave==2
egen mwave2lowK=max(wave2lowK), by(sheno)

gen incshare=((mr1profit+mr2profit)/2)/(((mr1profit+mr2profit)/2)+OtherHouseholdInc)

**** Broad External pressure principal component for everyone
pca pressurehh HouseholdSize norm7 norm8 norm9  sibsAccra married if wave==1
predict extpresspca
egen mextpresspca=max(extpresspca), by(SHENO)
 rename mextpresspca mextp1

 **** Narrow External pressure measure - norms only
 pca pressurehh  norm7 norm8 norm9  if wave==1
predict extpresspca2
egen mextpresspca2=max(extpresspca2), by(SHENO)
 rename mextpresspca2 mextp2
 

 
 




*** Column 1: Treatment heterogeneity by digitspan 
gen lowdigitspan=digitspan<=5
replace lowdigitspan=. if digitspan==.
#delimit ;
foreach lhs of varlist lowdigitspan 
{;
gen atreatcash_`lhs'=atreatcash*`lhs';
gen atreatequip_`lhs'=atreatequip*`lhs';
for num 2/6: gen waveX_`lhs'=waveX*`lhs';
xtreg realfinalprof atreatcash atreatequip atreatcash_`lhs' atreatequip_`lhs' 
wave2-wave6 wave2_`lhs'-wave6_`lhs' if trimgroup~=1, fe cluster(sheno);
outreg2 atreatcash atreatequip atreatcash_`lhs' atreatequip_`lhs' using table7jderevise.out, replace; 
};
test atreatcash_lowd==atreatequip_lowd

 ***** Treatment heterogeneity by these pcas (also can do for mmc1 mmc2 mmc3)
 
 #delimit ;
 foreach lhs of varlist mpca mextp1 mextp2 mpca3
 {;
gen atreatcash_`lhs'=atreatcash*`lhs';
gen atreatequip_`lhs'=atreatequip*`lhs';
for num 2/6: gen waveX_`lhs'=waveX*`lhs';
};

*** TABLE 7: Adding both external pressure and self-control
** column 2
#delimit ;
xtreg realfinalprof atreatcash atreatequip atreatcash_mpca atreatequip_mpca
 atreatcash_mextp2 atreatequip_mextp2
wave2-wave6 wave2_mpca-wave6_mpca wave2_mextp2-wave6_mextp2 if trimgroup~=1, fe cluster(sheno);
outreg2 atreatcash atreatequip atreatcash_mpca atreatequip_mpca atreatcash_mextp2 atreatequip_mextp2 using tables1.out, append;
test atreatcash_mpca==atreatequip_mpca;
test atreatcash_mextp2 atreatequip_mextp2;
*** column 3
#delimit ;
xtreg realfinalprof atreatcash atreatequip atreatcash_mpca atreatequip_mpca
 atreatcash_mextp1 atreatequip_mextp1
wave2-wave6 wave2_mpca-wave6_mpca wave2_mextp1-wave6_mextp1 if trimgroup~=1, fe cluster(sheno);
outreg2 atreatcash atreatequip atreatcash_mpca atreatequip_mpca atreatcash_mextp1 atreatequip_mextp1 using tables1.out, replace;
test atreatcash_mpca==atreatequip_mpca;
test atreatcash_mextp1 atreatequip_mextp1;

*** Columns 4 and 5 - high profit women
#delimit ;
xtreg realfinalprof atreatcash atreatequip atreatcash_mpca atreatequip_mpca
 atreatcash_mextp2 atreatequip_mextp2
wave2-wave6 wave2_mpca-wave6_mpca wave2_mextp2-wave6_mextp2 if trimgroup~=1 & female==1 & highgroup==1, fe cluster(sheno);
outreg2 atreatcash atreatequip atreatcash_mpca atreatequip_mpca atreatcash_mextp2 atreatequip_mextp2 using tables1.out, append;
test atreatcash_mpca==atreatequip_mpca;
test atreatcash_mextp2 atreatequip_mextp2;
#delimit ;
xtreg realfinalprof atreatcash atreatequip atreatcash_mpca atreatequip_mpca
 atreatcash_mextp1 atreatequip_mextp1
wave2-wave6 wave2_mpca-wave6_mpca wave2_mextp1-wave6_mextp1 if trimgroup~=1 & female==1 & highgroup==1, fe cluster(sheno);
outreg2 atreatcash atreatequip atreatcash_mpca atreatequip_mpca atreatcash_mextp1 atreatequip_mextp1 using tables1.out, append;
test atreatcash_mpca==atreatequip_mpca;
test atreatcash_mextp1 atreatequip_mextp1;

*** Columns 6 and 7- low profit women
#delimit ;
xtreg realfinalprof atreatcash atreatequip atreatcash_mpca atreatequip_mpca
 atreatcash_mextp2 atreatequip_mextp2
wave2-wave6 wave2_mpca-wave6_mpca wave2_mextp2-wave6_mextp2 if trimgroup~=1 & female==1 & mlowgroup==1, fe cluster(sheno);
outreg2 atreatcash atreatequip atreatcash_mpca atreatequip_mpca atreatcash_mextp2 atreatequip_mextp2 using tables1.out, append;
test atreatcash_mpca==atreatequip_mpca;
test atreatcash_mextp2 atreatequip_mextp2;
#delimit ;
xtreg realfinalprof atreatcash atreatequip atreatcash_mpca atreatequip_mpca
 atreatcash_mextp1 atreatequip_mextp1
wave2-wave6 wave2_mpca-wave6_mpca wave2_mextp1-wave6_mextp1 if trimgroup~=1 & female==1 & mlowgroup==1, fe cluster(sheno);
outreg2 atreatcash atreatequip atreatcash_mpca atreatequip_mpca atreatcash_mextp1 atreatequip_mextp1 using tables1.out, append;
test atreatcash_mpca==atreatequip_mpca;
test atreatcash_mextp1 atreatequip_mextp1;

*** Columns 8 and 9 - males
#delimit ;
xtreg realfinalprof atreatcash atreatequip atreatcash_mpca atreatequip_mpca
 atreatcash_mextp2 atreatequip_mextp2
wave2-wave6 wave2_mpca-wave6_mpca wave2_mextp2-wave6_mextp2 if trimgroup~=1 & male==1, fe cluster(sheno);
outreg2 atreatcash atreatequip atreatcash_mpca atreatequip_mpca atreatcash_mextp2 atreatequip_mextp2 using tables1.out, append;
test atreatcash_mpca==atreatequip_mpca;
test atreatcash_mextp2 atreatequip_mextp2;
#delimit ;
xtreg realfinalprof atreatcash atreatequip atreatcash_mpca atreatequip_mpca
 atreatcash_mextp1 atreatequip_mextp1
wave2-wave6 wave2_mpca-wave6_mpca wave2_mextp1-wave6_mextp1 if trimgroup~=1 & male==1, fe cluster(sheno);
outreg2 atreatcash atreatequip atreatcash_mpca atreatequip_mpca atreatcash_mextp1 atreatequip_mextp1 using tables1.out, append;
test atreatcash_mpca==atreatequip_mpca;
test atreatcash_mextp1 atreatequip_mextp1;


**** appendix Table A6 - table 4 with no trimming

 /*
 #delimit ;
foreach lhs of varlist male_male male_mixed female_female female_mixed highcapture lowcapture highcapital lowcapital highgroup mlowgroup 
{;
gen atreatcash_`lhs'=atreatcash*`lhs';
gen atreatequip_`lhs'=atreatequip*`lhs';
for num 2/6: gen waveX_`lhs'=waveX*`lhs';
};
*/

*** Panel A: Heterogeneity in Female Sample
** By industry
areg realfinalprof atreatcash_female_female atreatcash_female_mixed atreatequip_female_female atreatequip_female_mixed wave2-wave6 wave2_female_female-wave6_female_female if  female==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_female_female atreatcash_female_mixed atreatequip_female_female atreatequip_female_mixed  using table3f.out, replace nonotes bdec(2)
test atreatcash_female_female=atreatcash_female_mixed
test atreatequip_female_female=atreatequip_female_mixed
test atreatcash_female_female=atreatequip_female_female
test atreatcash_female_mixed=atreatequip_female_mixed, acc
xtreg realfinalprof atreatcash_female_female atreatcash_female_mixed atreatequip_female_female atreatequip_female_mixed wave2-wave6 wave2_female_female-wave6_female_female if  female==1, cluster(sheno) fe
outreg2 atreatcash_female_female atreatcash_female_mixed atreatequip_female_female atreatequip_female_mixed  using table3f.out, append nonotes bdec(2)
test atreatcash_female_female=atreatcash_female_mixed
test atreatequip_female_female=atreatequip_female_mixed
test atreatcash_female_female=atreatequip_female_female
test atreatcash_female_mixed=atreatequip_female_mixed, acc
** By capture
areg realfinalprof atreatcash_lowcapture atreatcash_highcapture atreatequip_lowcapture atreatequip_highcapture  wave2-wave6 wave2_lowcapture-wave6_lowcapture if  female==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture  using table3f.out, append nonotes bdec(2)
test atreatcash_lowcapture=atreatcash_highcapture
test atreatequip_lowcapture=atreatequip_highcapture
test atreatcash_lowcapture=atreatequip_lowcapture
test atreatcash_highcapture=atreatequip_highcapture, acc
xtreg realfinalprof atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture  wave2-wave6 wave2_lowcapture-wave6_lowcapture if  female==1, cluster(sheno) fe
outreg2 atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture   using table3f.out, append nonotes bdec(2)
test atreatcash_lowcapture=atreatcash_highcapture
test atreatequip_lowcapture=atreatequip_highcapture
test atreatcash_lowcapture=atreatequip_lowcapture
test atreatcash_highcapture=atreatequip_highcapture, acc
** By baseline capital
areg realfinalprof atreatcash_lowcapital atreatcash_highcapital atreatequip_lowcapital atreatequip_highcapital  wave2-wave6 wave2_lowcapital-wave6_lowcapital if  female==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital  using table3f.out, append nonotes bdec(2)
test atreatcash_lowcapital=atreatcash_highcapital
test atreatequip_lowcapital=atreatequip_highcapital 
test  atreatcash_lowcapital=atreatequip_lowcapital
test atreatcash_highcapital=atreatequip_highcapital, acc
xtreg realfinalprof atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital  wave2-wave6 wave2_lowcapital-wave6_lowcapital if  female==1, cluster(sheno) fe
outreg2 atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital   using table3f.out, append nonotes bdec(2)
test atreatcash_lowcapital=atreatcash_highcapital
test atreatequip_lowcapital=atreatequip_highcapital 
test  atreatcash_lowcapital=atreatequip_lowcapital
test atreatcash_highcapital=atreatequip_highcapital, acc 
** By baseline profits group
areg realfinalprof atreatcash_mlowgroup atreatcash_highgroup atreatequip_mlowgroup atreatequip_highgroup  wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if  female==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  using table3f.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup 
test atreatequip_mlowgroup=atreatequip_highgroup
test atreatcash_mlowgroup=atreatequip_mlowgroup
test atreatcash_highgroup=atreatequip_highgroup, acc
xtreg realfinalprof atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if  female==1, cluster(sheno) fe
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  using table3f.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup 
test atreatequip_mlowgroup=atreatequip_highgroup
test atreatcash_mlowgroup=atreatequip_mlowgroup
test atreatcash_highgroup=atreatequip_highgroup, acc
log close

*** Panel B: Heterogeneity in Male Sample
** By industry
areg realfinalprof atreatcash_male_male atreatcash_male_mixed atreatequip_male_male atreatequip_male_mixed wave2-wave6 wave2_male_male-wave6_male_male if   male==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_male_male atreatcash_male_mixed atreatequip_male_male atreatequip_male_mixed  using table3.out, replace nonotes bdec(2)
test atreatcash_male_male=atreatcash_male_mixed
test atreatequip_male_male=atreatequip_male_mixed
test atreatcash_male_male=atreatequip_male_male
test atreatcash_male_mixed=atreatequip_male_mixed, acc

xtreg realfinalprof atreatcash_male_male atreatcash_male_mixed atreatequip_male_male atreatequip_male_mixed wave2-wave6 wave2_male_male-wave6_male_male if   male==1, cluster(sheno) fe
outreg2 atreatcash_male_male atreatcash_male_mixed atreatequip_male_male atreatequip_male_mixed  using table3.out, append nonotes bdec(2)
test atreatcash_male_male=atreatcash_male_mixed
test atreatequip_male_male=atreatequip_male_mixed
test atreatcash_male_male=atreatequip_male_male
test atreatcash_male_mixed=atreatequip_male_mixed, acc
** By capture
areg realfinalprof atreatcash_lowcapture atreatcash_highcapture atreatequip_lowcapture atreatequip_highcapture  wave2-wave6 wave2_lowcapture-wave6_lowcapture if   male==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture  using table3.out, append nonotes bdec(2)
test atreatcash_lowcapture=atreatcash_highcapture
test atreatequip_lowcapture=atreatequip_highcapture
test atreatcash_lowcapture=atreatequip_lowcapture
test atreatcash_highcapture=atreatequip_lowcapture, acc
xtreg realfinalprof atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture  wave2-wave6 wave2_lowcapture-wave6_lowcapture if   male==1, cluster(sheno) fe
outreg2 atreatcash_lowcapture atreatcash_highcapture  atreatequip_lowcapture atreatequip_highcapture   using table3.out, append nonotes bdec(2)
test atreatcash_lowcapture=atreatcash_highcapture
test atreatequip_lowcapture=atreatequip_highcapture
test atreatcash_lowcapture=atreatequip_lowcapture
test atreatcash_highcapture=atreatequip_lowcapture, acc
** By baseline capital
areg realfinalprof atreatcash_lowcapital atreatcash_highcapital atreatequip_lowcapital atreatequip_highcapital  wave2-wave6 wave2_lowcapital-wave6_lowcapital if   male==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital  using table3.out, append nonotes bdec(2)
test atreatcash_lowcapital=atreatcash_highcapital
test atreatequip_lowcapital=atreatequip_highcapital 
test  atreatcash_lowcapital=atreatequip_lowcapital
test atreatcash_highcapital=atreatequip_highcapital, acc
xtreg realfinalprof atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital  wave2-wave6 wave2_lowcapital-wave6_lowcapital if   male==1, cluster(sheno) fe
outreg2 atreatcash_lowcapital atreatcash_highcapital  atreatequip_lowcapital atreatequip_highcapital   using table3.out, append nonotes bdec(2)
test atreatcash_lowcapital=atreatcash_highcapital
test atreatequip_lowcapital=atreatequip_highcapital  
test  atreatcash_lowcapital=atreatequip_lowcapital
test atreatcash_highcapital=atreatequip_highcapital, acc
** By baseline profits group
areg realfinalprof atreatcash_mlowgroup atreatcash_highgroup atreatequip_mlowgroup atreatequip_highgroup  wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if   male==1, cluster(sheno) abs(groupnum)
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  using table3.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup 
test atreatequip_mlowgroup=atreatequip_highgroup
test atreatcash_mlowgroup=atreatequip_mlowgroup
test atreatcash_highgroup=atreatequip_highgroup, acc
xtreg realfinalprof atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  wave2-wave6 wave2_mlowgroup-wave6_mlowgroup if   male==1, cluster(sheno) fe
outreg2 atreatcash_mlowgroup atreatcash_highgroup  atreatequip_mlowgroup atreatequip_highgroup  using table3.out, append nonotes bdec(2)
test atreatcash_mlowgroup=atreatcash_highgroup 
test atreatequip_mlowgroup=atreatequip_highgroup
test atreatcash_mlowgroup=atreatequip_mlowgroup
test atreatcash_highgroup=atreatequip_highgroup, acc


***** Table 5: Longer-term results
append using r7tomerge.dta

tsset sheno wave
foreach var in atreat cashtreat equiptreat atreatcash atreatequip female trimgroup wave2 wave3 wave4 wave5 wave6 groupnum highgroup mlowgroup {
replace `var'=L.`var' if wave==7
replace `var'=L2.`var' if wave==7 & `var'==.
}

*** OLS for round 7
areg realfinalprof atreatcash atreatequip if wave==7, abs(groupnum) 
outreg2 atreatcash atreatequip using longrunresults.out, replace
test atreatcash==atreatequip
areg realfinalprof atreatcash atreatequip if  wave==7 & female==0, abs(groupnum) 
outreg2 atreatcash atreatequip using longrunresults.out, append
test atreatcash==atreatequip
areg realfinalprof atreatcash atreatequip if  wave==7 & female==1, abs(groupnum) 
outreg2 atreatcash atreatequip using longrunresults.out, append
test atreatcash==atreatequip
areg realfinalprof atreatcash atreatequip if  wave==7 & female==1 & highgroup==1, abs(groupnum) 
outreg2 atreatcash atreatequip using longrunresults.out, append
test atreatcash==atreatequip
areg realfinalprof atreatcash atreatequip if  wave==7 & female==1 & mlowgroup==1, abs(groupnum) 
outreg2 atreatcash atreatequip using longrunresults.out, append
test atreatcash==atreatequip
