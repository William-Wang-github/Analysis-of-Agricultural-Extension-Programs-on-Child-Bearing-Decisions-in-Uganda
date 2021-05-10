*******************************************************************************
*	ORIGINAL SETUP CODE, SLIGHTLY MODIFIED 
*******************************************************************************
* Closes any previously opened log files and craetes/replaces a new one
cap log close _all 
log using "ECO403 Our Regression Results.log", replace text
set more off
clear // Removes any data from memory every time this script is run.

* Imports dataset, replace with wherever you have saved the data
use "C:\Users\hatho\Desktop\University Stuff\Economics\ECO403\Research Paper\Data for agricultural extension\programs\data\ready_hh.dta",clear

* If you have not already done so please install the following STATA packages to be able to properly run the code, rdrobust (using "net install rdrobust, from (https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace")

* Generates identifiers for each villiage
egen vid=group(c_code s_code p_code lc1_code)
label var vid "Village identifier"
bys vid: gen villhh=_N

* Creates a global variable for each bandwidth, 2 km, 1.5 km, and 3 km, as well as the controls/covariates used in the regressions 
global default "c(0) h(2) vce(nncluster vid 17)"
global small "c(0) h(1.5) vce(nncluster vid 17)"
global large "c(0) h(3) vce(nncluster vid 17)"
global control "hhheadage hhheadlit member distance hhr_gender imonth" 

* Creates a dummy for controls
gen baldata=1
foreach j of varlist $control {
	replace baldata=0 if `j'==.
	}
	
* Generates the bins for the distance 
egen bins = cut(mdistance), at(-6(0.1)6) 
replace bins=bins+0.05

*******************************************************************************
*	ECO403 OUR HEALTH REGRESSIONS
*******************************************************************************

* OUR Health Awareness Regressions without covariates/controls
rdrobust health mdistance,$default
rdrobust health mdistance,$small
rdrobust health mdistance,$large

* # of obs for this health regression without covariates/controls
sum health if abs(mdistance)<2 
sum health if abs(mdistance)<1.5
sum health if abs(mdistance)<3

* Control mean for this health regression without covariates/controls
sum health if mdistance>0 & mdistance<2
sum health if mdistance>0 & mdistance<1.5
sum health if mdistance>0 & mdistance<3

*OUR Health Awareness Regressions with covariates/controls
rdrobust health mdistance,covs($control)$default
rdrobust health mdistance,covs($control)$small
rdrobust health mdistance,covs($control)$large

* # of obs for this health regression with covariates/controls
sum health if abs(mdistance)<2 & baldata==1  
sum health if abs(mdistance)<1.5 & baldata==1 
sum health if abs(mdistance)<3 & baldata==1 

* Control mean for this health regression with covariates/controls
sum health if mdistance>0 & mdistance<2 & baldata==1 
sum health if mdistance>0 & mdistance<1.5 & baldata==1 
sum health if mdistance>0 & mdistance<3 & baldata==1 

*******************************************************************************
*	ECO403 OUR CHILD0 REGRESSIONS
*******************************************************************************

* Summary for the number of children under 5
sum num5

* OUR Regressions for children less than 1 without covariates/controls
rdrobust child0 mdistance,$default
rdrobust child0 mdistance,$small
rdrobust child0 mdistance,$large

* # of obs for this child0 regression without covariates/controls
sum child0 if abs(mdistance)<2 
sum child0 if abs(mdistance)<1.5
sum child0 if abs(mdistance)<3

* Control mean for this child0 regression without covariates/controls
sum child0 if mdistance>0 & mdistance<2
sum child0 if mdistance>0 & mdistance<1.5
sum child0 if mdistance>0 & mdistance<3

* OUR Regressions for children less than 1 with covariates/controls
rdrobust child0 mdistance,covs($control)$default
rdrobust child0 mdistance,covs($control)$small
rdrobust child0 mdistance,covs($control)$large

* # of obs for this child0 regression with covariates/controls
sum child0 if abs(mdistance)<2 & baldata==1  
sum child0 if abs(mdistance)<1.5 & baldata==1 
sum child0 if abs(mdistance)<3 & baldata==1 

* Control mean for this child0 regression with covariates/controls
sum child0 if mdistance>0 & mdistance<2 & baldata==1 
sum child0 if mdistance>0 & mdistance<1.5 & baldata==1 
sum child0 if mdistance>0 & mdistance<3 & baldata==1 

*******************************************************************************
*	ECO403 OUR CHILD2 REGRESSIONS
*******************************************************************************

* OUR Regressions for children less than 5 without covariates/controls
rdrobust child2 mdistance,$default
rdrobust child2 mdistance,$small
rdrobust child2 mdistance,$large

* # of obs for this child2 regression without covariates/controls
sum child2 if abs(mdistance)<2 
sum child2 if abs(mdistance)<1.5
sum child2 if abs(mdistance)<3

* Control mean for this child2 regression without covariates/controls
sum child2 if mdistance>0 & mdistance<2 
sum child2 if mdistance>0 & mdistance<1.5
sum child2 if mdistance>0 & mdistance<3

* OUR Regressions for children less than 5 with covariates/controls
rdrobust child2 mdistance,covs($control)$default
rdrobust child2 mdistance,covs($control)$small
rdrobust child2 mdistance,covs($control)$large

* # of obs for this child2 regression with covariates/controls
sum child2 if abs(mdistance)<2 & baldata==1  
sum child2 if abs(mdistance)<1.5 & baldata==1 
sum child2 if abs(mdistance)<3 & baldata==1 

* Control mean for this child2 regression with covariates/controls
sum child2 if mdistance>0 & mdistance<2 & baldata==1 
sum child2 if mdistance>0 & mdistance<1.5 & baldata==1 
sum child2 if mdistance>0 & mdistance<3 & baldata==1 


*******************************************************************************
*	ECO403 OUR HEALTH FIGURE DISCONTINUITY IN COVARIATES
*******************************************************************************

* Household health discontinuity graph*

* Generates the mean of the health variable, sorting by our bins variable we made
bysort bins: egen mean_health = mean (health) 

* Does a local polynomial regression weighted by kernel for left of bandwidth for health variable, i.e. the trend, and generates a variable for the confidence interval
lpoly health mdistance if mdistance<=0 & mdistance>=-3, degree(1) kernel(triangle) bwidth(2) /// 
 nograph generate(x_left y_left) se(shat_left) n(100) 
 quietly gen yhigh_left=y_left+1.96*shat_left
 quietly gen ylow_left=y_left-1.96*shat_left 

* Does a local polynomial regression weighted by kernel for right of bandwidth for health variable, i.e. the trend, and generates a variable for the confidence interval
lpoly health mdistance if mdistance>0 & mdistance<=3, degree(1) kernel(triangle) bwidth(2)  ///
nograph generate(x_right y_right)  se(shat_right)  n(100) 
 quietly gen yhigh_right=y_right+1.96*shat_right
 quietly gen ylow_right=y_right-1.96*shat_right

* Generates the health bandwidth graph
#delimit ;
twoway (scatter yhigh_left y_left ylow_left x_left if x_left<=0 & x_left>=-3, m(none none none) connect(l l l) lcolor(gray black gray) lpat(shortdash solid shortdash))
(scatter yhigh_right y_right ylow_right x_right if x_right>0 & x_right<=3, m(none none none) connect(l l l) lcolor(gray black gray) lpat(shortdash solid shortdash))
(scatter mean_health bins if abs(mdistance)<=3, msize(medsmall) msymbol(circle) mfcolor(white) mlcolor(black) mlwidth(thin))
(scatteri 0 0 0.6 0, recast(line) lcolor(cranberry) lpattern(dash)), 
ytitle("Household health awareness rate", size(medsmall) margin(small)) ylabel(0(0.2)0.6,labsize(medsmall)  format(%9.1f)) xtitle("Distance from Cutoff", size(medsmall) margin(small)) xlabel(-3(1)3, labsize(medsmall)) // Generates axis labels
graphregion(fcolor(white) lcolor(white)) legend(order(2 "Local polynomial fit" 3 "95% C.I."  7 "0.1-km bin" 8 "Cutoff value" )) plotregion(lcolor(black) lwidth(thin));// Generates the graph legend
#delimit cr
graph export "healthfighead.eps", as(eps) preview(off) replace
drop x_left y_left shat_left yhigh_left ylow_left x_right y_right shat_right yhigh_right ylow_right  // Saves the graph and drops the variables created to be used in other plots


*******************************************************************************
*	ECO403 OUR CHILDREN FIGURES DISCONTINUITY IN COVARIATES
*******************************************************************************

* Household has children less than 1 discontinuity graph*

* Generates the mean of the child0 variable, sorting by our bins variable
bysort bins: egen mean_child0 = mean (child0) 

* Does a local polynomial regression weighted by kernel for left of bandwidth for child0 variable, i.e. the trend, and generates a variable for the confidence interval
lpoly child0 mdistance if mdistance<=0 & mdistance>=-3, degree(1) kernel(triangle) bwidth(2) /// 
 nograph generate(x_left y_left) se(shat_left) n(100) 
 quietly gen yhigh_left=y_left+1.96*shat_left
 quietly gen ylow_left=y_left-1.96*shat_left
 
* Does a local polynomial regression weighted by kernel for right of bandwidth for child0 variable, i.e. the trend, and generates a variable for the confidence interval
lpoly child0 mdistance if mdistance>0 & mdistance<=3, degree(1) kernel(triangle) bwidth(2)  ///
nograph generate(x_right y_right)  se(shat_right)  n(100) 
 quietly gen yhigh_right=y_right+1.96*shat_right
 quietly gen ylow_right=y_right-1.96*shat_right

* Generates the child0 bandwidth graph
#delimit ;
twoway (scatter yhigh_left y_left ylow_left x_left if x_left<=0 & x_left>=-3, m(none none none) connect(l l l) lcolor(gray black gray) lpat(shortdash solid shortdash))
(scatter yhigh_right y_right ylow_right x_right if x_right>0 & x_right<=3, m(none none none) connect(l l l) lcolor(gray black gray) lpat(shortdash solid shortdash))
(scatter mean_child0 bins if abs(mdistance)<=3, msize(medsmall) msymbol(circle) mfcolor(white) mlcolor(black) mlwidth(thin))
(scatteri 0 0 0.6 0, recast(line) lcolor(cranberry) lpattern(dash)), 
ytitle("Household with children under 1", size(medsmall) margin(small)) ylabel(0(0.2)0.6,labsize(medsmall)  format(%9.1f)) xtitle("Distance from Cutoff", size(medsmall) margin(small)) xlabel(-3(1)3, labsize(medsmall)) // Generates axis labels
graphregion(fcolor(white) lcolor(white)) legend(order(2 "Local polynomial fit" 3 "95% C.I."  7 "0.1-km bin" 8 "Cutoff value" )) plotregion(lcolor(black) lwidth(thin)); // Generates the graph legend
#delimit cr
graph export "child0fighead.eps", as(eps) preview(off) replace
drop x_left y_left shat_left yhigh_left ylow_left x_right y_right shat_right yhigh_right ylow_right  // Saves the graph and drops the variables created to be used in other plots

*******************************************************************************

* Household has children less than 5 discontinuity graph*

* Generates the mean of the child2 variable, sorting by our bins variable
bysort bins: egen mean_child2 = mean (child2) 

* Does a local polynomial regression weighted by kernel for left of bandwidth for child2 variable, i.e. the trend, and generates a variable for the confidence interval
lpoly child2 mdistance if mdistance<=0 & mdistance>=-3, degree(1) kernel(triangle) bwidth(2) /// 
 nograph generate(x_left y_left) se(shat_left) n(100) 
 quietly gen yhigh_left=y_left+1.96*shat_left
 quietly gen ylow_left=y_left-1.96*shat_left

* Does a local polynomial regression weighted by kernel for right of bandwidth for child2 variable, i.e. the trend, and generates a variable for the confidence interval
lpoly child2 mdistance if mdistance>0 & mdistance<=3, degree(1) kernel(triangle) bwidth(2)  ///
nograph generate(x_right y_right)  se(shat_right)  n(100) 
 quietly gen yhigh_right=y_right+1.96*shat_right
 quietly gen ylow_right=y_right-1.96*shat_right
 
* Generates the child2 bandwidth graph
#delimit ;
twoway (scatter yhigh_left y_left ylow_left x_left if x_left<=0 & x_left>=-3, m(none none none) connect(l l l) lcolor(gray black gray) lpat(shortdash solid shortdash))
(scatter yhigh_right y_right ylow_right x_right if x_right>0 & x_right<=3, m(none none none) connect(l l l) lcolor(gray black gray) lpat(shortdash solid shortdash))
(scatter mean_child2 bins if abs(mdistance)<=3, msize(medsmall) msymbol(circle) mfcolor(white) mlcolor(black) mlwidth(thin))
(scatteri 0 0 0.6 0, recast(line) lcolor(cranberry) lpattern(dash)), 
ytitle("Household with children under 5", size(medsmall) margin(small)) ylabel(0(0.2)0.6,labsize(medsmall)  format(%9.1f)) xtitle("Distance from Cutoff", size(medsmall) margin(small)) xlabel(-3(1)3, labsize(medsmall)) // Generates axis labels
graphregion(fcolor(white) lcolor(white)) legend(order(2 "Local polynomial fit" 3 "95% C.I."  7 "0.1-km bin" 8 "Cutoff value" )) plotregion(lcolor(black) lwidth(thin)); // Generates the graph legend
#delimit cr
graph export "child2fighead.eps", as(eps) preview(off) replace
drop x_left y_left shat_left yhigh_left ylow_left x_right y_right shat_right yhigh_right ylow_right // Saves the graph and drops the variables created to be used in other plots
 
 
log close 