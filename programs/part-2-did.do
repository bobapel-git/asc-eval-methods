clear
cd "C:\Users\lambc\Desktop\ASC Eval Methods/Data"
use "conviction-work.dta"

desc

drop if wave1convic==1
count

gen treated=wave1convic~=.
tab treated

reshape long age_ work_ arrest_ convic_, i(id) j(t)
rename *_ *
xtset id t

gen time1treated=t-wave1convic
gen group=0
replace group=1 if treated==1 & time1treated<0
replace group=2 if treated==1 & time1treated>=0
twoway (lpoly work t if group==0) (lpoly work t if group==1) (lpoly work t if group==2), legend(subtitle("Treatment Group") order(1 "Never Convicted" 2 "Ever Convicted, Pretest" 3 "Ever Convicted, Posttest") pos(5) ring(0) cols(1)) ytitle("Proportion Employed") xtitle("Interview Round") title("Distribution of Employment, by Time and Conviction Status")

* two-way fixed effects *

gen tvtreated=time1treated>=0 & treated==1

*ssc install reghdfe, replace
xtreg work i.t tvtreated, fe vce(cluster id)
reghdfe work tvtreated, absorb(id t) vce(cluster id)
xtdidregress (work) (tvtreated), group(id) time(t) vce(cluster id)

reghdfe work F(0/2).tvtreated, absorb(id t) vce(cluster id)
test F1.tvtreated F2.tvtreated

*ssc install coefplot, replace
gen time1treated100=time1treated+100
replace time1treated100=85 if treated==0
reghdfe work o99.time1treated100, absorb(id t) vce(cluster id)
coefplot, vertical drop(_cons) omitted yline(0) xline(15) ylabel(-0.4(0.1)0.1, format(%03.1f)) xlabel(0 "-15" 5 "-10" 10 "-5" 15 "0" 20 "5" 25 "10") ytitle("Average Treatment Effect on the Treated") xtitle("Time to First Conviction") title("Event Study Estimates of the Impact of Conviction on Employment")

* group-time estimator *

xthdidregress aipw (work) (tvtreated), group(id) controlgroup(never) vce(cluster id) 
estat ptrends

estat aggregation, overall sci(reps(1000) rseed(20231114))

*recode wave1convic (. = 0), gen(wave1treated)
*ssc install csdid, replace
*csdid work tvtreated, ivar(id) time(t) gvar(wave1treated) method(dripw)
*estat pretrend
*estat simple

* heterogeneous DID *

estat aggregation, dynamic sci(reps(1000) rseed(20231114)) graph(yline(0) xline(0) legend(off) ytitle("Average Treatment Effect on the Treated") xtitle("Time to First Conviction") title("Dynamic Impact of Conviction on Employment"))

estat aggregation, cohort sci(reps(1000) rseed(20231114)) graph(yline(0) legend(off) ytitle("Average Treatment Effect on the Treated") xtitle("Time to First Conviction") title("Cohort-Specific Impact of Conviction on Employment"))

estat aggregation, time sci(reps(1000) rseed(20231114)) graph(yline(0) legend(off) ytitle("Average Treatment Effect on the Treated") xtitle("Time to First Conviction") title("Calendar-Specific Impact of Conviction on Employment"))

* adjustment for covariates and anticipation *

xthdidregress aipw (work i.(male minrty)##c.cohort) (tvtreated i.(male minrty)##c.cohort), group(id) controlgroup(never) vce(cluster id)
estat aggregation, overall sci(reps(1000) rseed(20231114))

