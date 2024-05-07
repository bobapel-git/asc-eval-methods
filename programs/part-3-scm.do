clear
cd "C:\Users\lambc\Desktop\ASC Eval Methods/Data"
use "deprosecution-homicide.dta"

levelsof agency, clean sep(", ")

gen murdrate=murd/pop*100000
gen post=year>=2015
gen treated=agency=="philadelphia PA"
xtset id year
xtline murdrate, overlay legend(off) ylabel(0(20)80, grid) xlabel(1990(5)2020, grid) xmtick(1990(1)2020) ytitle("Homicide Rate per 100k") title("Agency Trends in Homicide, 1990-2020") addplot(line murdrate year if id==102, sort(year) lcolor(black) lwidth(vthick)) xline(2015, lp(dash)) scheme(s1mono)

gen tvtreated=treated==1 & year>=2015
gen year1treated=2015 if treated==1
replace year1treated=0 if treated==0
gen time1treated=year-2015 if treated==1
replace time1treated=0 if treated==0

reghdfe murdrate tvtreated, absorb(id year) vce(cluster id)
reghdfe murdrate F(0/3).tvtreated, absorb(id year) vce(cluster id)

gen time1treated100=time1treated+100
replace time1treated100=74 if treated==0
reghdfe murdrate o99.time1treated100, absorb(id year) vce(cluster id)
coefplot, vertical drop(_cons) omitted yline(0) xline(26) xlabel(1 "-25" 6 "-20" 11 "-15" 16 "-10" 21 "-5" 26 "0" 31 "5") ytitle("Estimate of the ATT") xtitle("Time to De-Prosecution Policy") title("Event Study Estimates of the Impact of De-Prosecution on Homicide")

*ssc install synth2, replace
*net install synth_runner, from(https://raw.github.com/bquistorff/synth_runner/master/) replace
synth murdrate murdrate, trunit(102) trperiod(2015) unitnames(agency) figure
synth_runner murdrate murdrate, trunit(102) trperiod(2015)

synth murdrate murdrate murdrate(2006) murdrate(2011/2013), trunit(102) trperiod(2015) unitnames(agency) figure
synth_runner murdrate murdrate(2006) murdrate(2011/2013), trunit(102) trperiod(2015)

/*
synth2 murdrate murdrate murdrate(2006) murdrate(2011/2013), trunit(102) trperiod(2015) placebo(unit) figure
*/
