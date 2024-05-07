clear
cd "C:\Users\lambc\Desktop\ASC Eval Methods/Data"
use "supported-work.dta"

desc
tab exp
tab exp if out_earned~=.

twoway (hist out_earned if exp==0, discrete start(0) width(1000) color(stred%30)) (hist out_earned if exp==1, discrete start(0) width(1000) color(stblue%30)), legend(subtitle("Treatment Group") order(1 "Control" 2 "Experimental") pos(1) ring(0)) ytitle("Density") xtitle("Total Earnings") title("Distribution of Earnings, by Treatment Assignment")

cumul out_earned if exp==0 & out_earned<=12000, gen(cum0_out_earned) equal
cumul out_earned if exp==1 & out_earned<=12000, gen(cum1_out_earned) equal
twoway (line cum0_out_earned out_earned if out_earned<=12000, sort lwidth(medthick)) (line cum1_out_earned out_earned if out_earned<=12000, sort lwidth(medthick)), ylabel(0.4(0.2)1, format(%03.1f)) xlabel(0 "$0" 3000 "$3,000" 6000 "$6,000" 9000 "$9,000" 12000 "$12,000") legend(subtitle("Treatment Group") order(1 "Control" 2 "Experimental") pos(5) ring(0)) ytitle("Empirical CDF") xtitle("Total Earnings") title("Cumulative Distribution of Earnings, by Treatment Assignment")

reg out_earned exp, vce(hc3)

gen trt_worked=trt_months>0 if trt_months~=.
gen complier=0
replace complier=1 if exp==1 & trt_worked==1
replace complier=1 if exp==0 & trt_worked==0
replace complier=. if trt_months==.
tabstat trt_worked complier, by(exp)

reg out_earned exp if complier==1, vce(hc3)

reg out_earned trt_worked, vce(hc3)

ivregress 2sls out_earned (trt_worked = exp), vce(robust)

*ssc install estout, replace
qui: reg out_earned exp, vce(hc3)
est store ITT
qui: reg out_earned exp if complier==1, vce(hc3)
est store PP
qui: reg out_earned trt_worked, vce(hc3)
est store AT
qui: ivregress 2sls out_earned (trt_worked = exp), vce(robust)
est store LATE
estout ITT PP AT LATE, cells(b(star fmt(1)) se(par fmt(1))) stats(N)

* effect size *

esize twosample out_earned, by(exp) cohensd unequal

qui: reg out_earned exp, vce(hc3)
global itt=_b[exp]
qui: ivregress 2sls out_earned (trt_worked = exp), vce(robust)
global late=_b[trt_worked]
qui: esize twosample out_earned, by(exp) cohensd unequal
di r(d)*($late/$itt)

* randomization inference *

*net install ritest, from(https://raw.githubusercontent.com/simonheb/ritest/master/)
ritest exp _b[exp], reps(1000) seed(20231114) kdensityplot saving(ri_b, replace): regress out_earned exp

qui: reg out_earned exp, vce(hc3)
global itt=_b[exp]
frame create ri_dist
frame change ri_dist
use "ri_b.dta"
qui: sum _pm_1
global mn=r(mean)
global sd=r(sd)
twoway (hist _pm_1, width(20)) (function y = normalden(x, $mn, $sd), range(-400 400) lw(medthick) lp(dash)), ylabel(0(0.001)0.004, format(%04.3f)) xlabel(-400 "-$400" -200 "-$200" 0 "$0" 200 "$200" 400 "$400") legend(off) ytitle("Density") xtitle("Impact of Supported Work on Earnings") xline($itt, lp(dash))
frame change default
frame drop ri_dist

* regression adjustment *

gen pre_worked=pre_months>0 if pre_months~=.
gen pre_unemp_ln=ln(pre_unemp)
gen pre_illeg_ln=ln1p(pre_illeg)
gen pre_hours_ln=ln1p(pre_hours)
gen pre_earned_ln=ln1p(pre_earned)

global covars i.pre_age i.pre_male i.pre_race i.pre_marr i.pre_educ i.pre_empl i.pre_activ c.pre_unemp_ln c.pre_illeg_ln i.pre_worked c.pre_months c.pre_hours_ln c.pre_earned_ln

foreach x in $covars {
	qui: reg exp `x'
	di "`x': " _col(20) %9.2f e(F) %9.0f e(df_m) %9.0f e(df_r) %9.4f Ftail(e(df_m),e(df_r),e(F))
}

global covars2 i.pre_age i.pre_male i.pre_race i.pre_marr i.pre_educ i.pre_empl c.pre_unemp_ln c.pre_illeg_ln i.pre_worked c.pre_months c.pre_hours_ln c.pre_earned_ln

teffects ra (out_earned i.stratum $covars2) (exp), vce(robust)

* reweighting estimator *

gen insamp=out_earned~=.

logit insamp i.exp i.stratum $covars2, vce(robust)
predict phat if e(sample), pr
gen invprob=1/(phat*insamp+(1-phat)*(1-insamp))

reg out_earned exp [pw=invprob], vce(robust)

ivregress 2sls out_earned (trt_worked = exp) [pw=invprob], vce(robust)

qui: reg out_earned exp [pw=invprob], vce(robust)
est store ITT_IPW
qui: ivregress 2sls out_earned (trt_worked = exp) [pw=invprob], vce(robust)
est store LATE_IPW
estout ITT ITT_IPW LATE LATE_IPW, cells(b(star fmt(1)) se(par fmt(1))) stats(N)

* appendix: log-linear regression *

poisson out_earned exp, vce(robust)
di 100*(exp(_b[exp])-1)

qui: reg trt_worked exp if out_earned~=.
predict res1st if e(sample), residuals
poisson out_earned trt_worked res1st, vce(boot, reps(250))

