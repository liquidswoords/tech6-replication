

*This file produces all tables in the paper "Islamic Rule and Empowerment of the Poor and Pious" by Erik Meyersson
*Programs used in this do-file
*RDOB - RD estimator created by Imbens and Kalyanaraman. The included ado has a small addition which saves RD estimates as three scalars to be used for making tables.



global root "{Insert Root Directory}"

set matsize 11000 
clear all
set mem 400m
set more off

cd $root/data
use regdata0.dta
 

forval x=1/3 {

gen vi99_`x'=l5vmgn_islam^`x'
gen vi99_`x'_i=l5i1* l5vmgn_islam^`x'
}

********************************DEFINE GLOBALS********************************


global contr="vshr_islam1994 partycount shhs lpop1994 ageshr19 ageshr60 sexr merkezi merkezp subbuyuk buyuk pd_*"
global contrs="vshr_islam1994 partycount shhs lpop1994 ageshr19 ageshr60 sexr merkezi merkezp subbuyuk buyuk"
global contrx="shhs lpop1994 ageshr19 ageshr60 sexr merkezi merkezp subbuyuk buyuk pd_*"


global contr99 "shhs l5vshr_islam lpop ageshr19 ageshr60 sexr merkezi merkezp subbuyuk buyuk pd_*"
***********************************

global se "cluster pcode"	
global slvl "starlevels(* 0.10 ** 0.05 *** 0.01)"


***********************************************************************************************
**************************************	Regressions	 ***********************************
***********************************************************************************************


*******TABLE 1 SUMMARY STATISTICS
use regdata0.dta, clear


global sumstats "hischshr1520f hischshr1520m i94 vshr_islam1994 partycount lpop1994 ageshr19 ageshr60 sexr merkezi merkezp subbuyuk buyuk"


reg iwm94 $sumstats
keep if e(sample)==1
****


 capt prog drop myttests

*! version 1.0.0  14aug2007  Ben Jann
program myttests, eclass
	version 8
	syntax varlist [if] [in], by(varname) [ * ]
	marksample touse
	markout `touse' `by'
	tempname mu sd mu_1 mu_2 sd_1 sd_2 d d_se d_t d_p N_1 N_2 N
	foreach var of local varlist {
		qui ttest `var' if `touse', by(`by') `options'
		mat `mu_1' = nullmat(`mu_1'), r(mu_1)
		mat `mu_2' = nullmat(`mu_2'), r(mu_2)
		mat `sd_1' = nullmat(`sd_1'), r(sd_1)
		mat `sd_2' = nullmat(`sd_2'), r(sd_2)
		mat `d'    = nullmat(`d'   ), r(mu_1)-r(mu_2)
		mat `d_se' = nullmat(`d_se'), r(se)
		mat `d_t'  = nullmat(`d_t' ), r(t)
		mat `d_p'  = nullmat(`d_p' ), r(p)
		mat `sd' = nullmat(`sd'), r(sd)
		mat `N_1' =nullmat(`N_1'), r(N_1)
		mat `N_2' =nullmat(`N_1'), r(N_2)
		sum `var' if `touse'
		mat `mu'  = nullmat(`mu' ), r(mean)
		mat `N' =nullmat(`N_1'), r(N)
}
foreach mat in mu_1 mu_2 mu sd sd_1 sd_2 d d_se d_t d_p N_1 N_2 N {
	mat coln ``mat'' = `varlist'
}
tempname b V
mat `b' = `mu_1'*0
mat `V' = `b''*`b'
eret post `b' `V'
eret local cmd "myttests"
foreach mat in mu_1 mu_2 mu sd sd_1 sd_2 d d_se d_t d_p N_1 N_2 N {
    eret mat `mat' = ``mat''
}
end


gen I94=1-i94

myttests $sumstats, by(I94)



estout using $root/output/sums.tex, replace ///
     cells("mu(fmt(%9.3f)) mu_1 mu_2 d(star pvalue(d_p))" "sd(par) sd_1(par) sd_2(par) d_se(par)") ///
     style(tex) nonumber label collabels(, none) mlabels(, none) stat(N)
***

myttests hischshr1520f hischshr1520m, by(I94)



estout using $root/output/sums_out.tex, replace ///
     cells("mu(fmt(%9.3f)) mu_1 mu_2 d(star pvalue(d_p))" "sd(par) sd_1(par) sd_2(par) d_se(par)") ///
     style(tex) nonumber label collabels(, none) mlabels(, none)


myttests i94, by(I94)

estout using $root/output/sums_exp.tex, replace ///
     cells("mu(fmt(%9.3f)) mu_1 mu_2 d(star pvalue(d_p))" "sd(par) sd_1(par) sd_2(par) d_se(par)") ///
     style(tex) nonumber label collabels(, none) mlabels(, none)


myttests vshr_islam1994 partycount lpop1994 ageshr19 ageshr60 sexr shhs merkezi merkezp subbuyuk, by(I94)

estout using $root/output/sums_cov.tex, replace ///
     cells("mu(fmt(%9.3f)) mu_1 mu_2 d(star pvalue(d_p))" "sd(par) sd_1(par) sd_2(par) d_se(par)") ///
     style(tex) nonumber label collabels(, none) mlabels(, none)


mat sumsN=(.,.,.,.)
sum hischshr1520f
mat sumsN[1,1]=r(N)
mat sumsN[1,4]=r(N)

sum hischshr1520f if i94==1
mat sumsN[1,2]=r(N)
sum hischshr1520f if i94==0
mat sumsN[1,3]=r(N)
mat rownames sumsN = "Observations"
estout matrix(sumsN, fmt(%9.0g)) using $root/output/SumsN.tex, replace title("") style(tex) ///
	collabels(, none) mlabels(, none) nolabel
			
			
			
********************TABLE 2 MAIN RESULT********************

use regdata0.dta, clear
set more off

qui reg hischshr1520f i94 $contr iwm94 hischshr1520m
keep if e(sample)==1


label var vshr_islam1994 "Islamic vote share 1994"



	

foreach g in f m {

	
	regress hischshr1520`g' i94, vce($se)
		estimates store r_olsN_hischshr1520`g'
	mat dolsn_`g'=e(V)
	sum hischshr1520`g'
	estadd scalar be=r(mean)
	estadd scalar h=1
		qui regress hischshr1520`g' i94,
			estimates store olsN_hischshr1520`g'

	regress hischshr1520`g' i94 $contr, vce($se)
		estimates store r_olsY_hischshr1520`g'
	sum hischshr1520`g'
	estadd scalar be=r(mean)
	estadd scalar h=1

		qui regress hischshr1520`g' i94 $contr
			estimates store olsY_hischshr1520`g'
	rdob hischshr1520`g' iwm94, c(0)
	
***
	regress hischshr1520`g' i94 vi1* if abs(iwm94)<shopt, vce($se)
		estimates store r_locN_hischshr1520`g'
	sum hischshr1520`g' if abs(iwm94)<shopt
	estadd scalar be=r(mean)
	estadd scalar h=shopt
		qui regress hischshr1520`g' i94 vi1* if abs(iwm94)<shopt
			estimates store locN_hischshr1520`g'


	regress hischshr1520`g' i94 vi1* $contr if abs(iwm94)<shopt, vce($se)
		estimates store r_locY_hischshr1520`g'
	sum hischshr1520`g' if abs(iwm94)<shopt
	estadd scalar be=r(mean)
	estadd scalar h=shopt
		qui regress hischshr1520`g' i94 vi1* $contr if abs(iwm94)<shopt
			estimates store locY_hischshr1520`g'


****


*	rdob hischshr1520`g' iwm94, c(0)	
	regress hischshr1520`g' i94 vi1* $contr if abs(iwm94)<shopt/2 , vce($se)
		estimates  store r_loc10_hischshr1520`g'
	sum hischshr1520`g' if abs(iwm94)<shopt/2 
	estadd scalar be=r(mean)
	estadd scalar h= shopt/2
		qui regress hischshr1520`g' i94 vi1* $contr if abs(iwm94)<shopt/2 
			estimates store loc10_hischshr1520`g'


	regress hischshr1520`g' i94 vi1* $contr if abs(iwm94)<shopt*2, vce($se)
		estimates store r_loc50_hischshr1520`g'
		sum hischshr1520`g' if abs(iwm94)<shopt*2 
		estadd scalar be=r(mean)
		estadd scalar h= shopt*2
		qui regress hischshr1520`g' i94 vi1* $contr if abs(iwm94)<shopt*2 
			estimates store loc50_hischshr1520`g'
		
	
	regress hischshr1520`g' i94 vi1* vi2* $contr if abs(iwm94)<shopt, vce($se)
		estimates store r_loc25q_hischshr1520`g'

	sum hischshr1520`g' if abs(iwm94)<shopt
		estadd scalar be=r(mean)
		estadd scalar h= shopt
		qui regress hischshr1520`g' i94 vi1* vi2* $contr if abs(iwm94)<shopt
			estimates store loc25q_hischshr1520`g'
			
	
	regress hischshr1520`g' i94 vi1* vi2* vi3* $contr if abs(iwm94)<shopt, vce($se)
		estimates store r_loc25c_hischshr1520`g'

	sum hischshr1520`g' if abs(iwm94)<shopt
		estadd scalar be=r(mean)
		estadd scalar h= shopt
		qui regress hischshr1520`g' i94 vi1* vi2* vi3* $contr if abs(iwm94)<shopt
			estimates store loc25c_hischshr1520`g'			
			

	rdob studshr1530`g' iwm94, c(0)
	regress studshr1530`g' i94 vi1* if abs(iwm94)<shopt, vce($se)
		estimates store r_locN_studshr1530`g'


	regress studshr1530`g' i94 vi1* $contr if abs(iwm94)<shopt, vce($se)
		estimates store r_locY_studshr1530`g'

	sum studshr1530`g' if abs(iwm94)<shopt
	estadd scalar be=r(mean)
	estadd scalar h= shopt
		qui regress studshr1530`g' i94 vi1* $contr if abs(iwm94)<shopt
			estimates store locY_studshr1530`g'	


		
		
	estout ///
		r_olsN_hischshr1520`g' r_olsY_hischshr1520`g' ///
			r_locN_hischshr1520`g' r_locY_hischshr1520`g' ///
				r_loc10_hischshr1520`g' r_loc50_hischshr1520`g' ///
					r_loc25q_hischshr1520`g' r_loc25c_hischshr1520`g' ///
		 	 	 	 		r_locY_studshr1530`g'  ///
		using "$root/output/table2_`g'.tex", ///
			replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
			keep(i94) mlabels(, none) ///
			stats(h r2 N , fmt( %9.3f %9.2f %9.0g) labels( "Bandwidth" "\$R^2\$" "Observations" ) layout(@ @)) ///
			$slvl 
			
			*indicate(Controls = pd_2 pd_3 pd_4 pd_5 pd_6 )

	estout ///
		r_olsN_hischshr1520`g' r_olsY_hischshr1520`g' ///
			r_locN_hischshr1520`g' r_locY_hischshr1520`g' ///
				r_loc10_hischshr1520`g' r_loc50_hischshr1520`g' ///
					r_loc25q_hischshr1520`g' r_loc25c_hischshr1520`g' ///
		 	 	 	 		r_locY_studshr1530`g'  ///
		 	 	 	 		using "$root/output/tablesumtest_`g'.tex", ///
		 	 	 	 		 replace style(tex) cells(none) collabels(, none) mlabels(, none) ///
		 	 	 	 		 stats(be, fmt( %9.3f) labels("Outcome mean"))
}	
	

	
*Different male female estimates

foreach y in hischshr1520 {
	foreach r in ols loc  {
		foreach c in Y N {
			set more off
			suest `r'`c'_`y'f `r'`c'_`y'm, vce($se)
			test [`r'`c'_`y'f_mean]i94 = [`r'`c'_`y'm_mean]i94 
			mat eqcoeff_`r'`c'_`y'=(.)
			mat eqcoeff_`r'`c'_`y'[1,1]=r(p)
		}
	}
}

set more off
foreach y in hischshr1520 {
	foreach c in Y  10 50 25q 25c {
		suest loc`c'_`y'f loc`c'_`y'm, vce($se)
		test [loc`c'_`y'f_mean]i94 = [loc`c'_`y'm_mean]i94 
		mat eqcoeff_loc`c'_`y'=(.)
		mat eqcoeff_loc`c'_`y'[1,1]=r(p)
	}
}

foreach y in studshr1530 {
	foreach c in Y  {
		suest loc`c'_`y'f loc`c'_`y'm, vce($se)
		test [loc`c'_`y'f_mean]i94 = [loc`c'_`y'm_mean]i94 
		mat eqcoeff_loc`c'_`y'=(.)
		mat eqcoeff_loc`c'_`y'[1,1]=r(p)
	}
}





**************************

matrix eqcoeff=(eqcoeff_olsN_hischshr1520) , (eqcoeff_olsY_hischshr1520) ,  (eqcoeff_locN_hischshr1520) , (eqcoeff_locY_hischshr1520) , (eqcoeff_loc10_hischshr1520)  , (eqcoeff_loc50_hischshr1520) , (eqcoeff_loc25q_hischshr1520) ,   (eqcoeff_loc25c_hischshr1520) ,  (eqcoeff_locY_studshr1530)
mat rownames eqcoeff = "p-value"
estout matrix(eqcoeff, fmt(%9.3f)) using $root/output/table2_eqcoeff.tex, replace title("") style(tex) ///
	collabels(, none) mlabels(, none) nolabel


estimates clear




******************************************************************


*********** TABLE 3 - Alternative non-parametric*******************


******************************************************************




estimates drop _all
mat drop _all
foreach g in f m {

use regdata0.dta, clear

qui reg hischshr1520f i94 $contr iwm94
gen regsamp=1 if e(sample)
keep if regsamp==1

foreach b in 100 50 25 10 5 2 1 {
	set more off
	local i =`b'/100




	reg hischshr1520`g' i94 $contr  if iwm94<`i' & iwm94>-`i', cluster(pcode)
		estimates store altsp_`b'_0`g'


	reg hischshr1520`g' i94 vi1*  $contr  if iwm94<`i' & iwm94>-`i', cluster(pcode)
		estimates store altsp_`b'_1`g'


	reg hischshr1520`g' i94 vi1* vi2* $contr if iwm94<`i' & iwm94>-`i', cluster(pcode)
		estimates store altsp_`b'_2`g'


	reg hischshr1520`g' i94 vi1* vi2* vi3* $contr   if iwm94<`i' & iwm94>-`i', cluster(pcode)
		estimates store altsp_`b'_3`g'


	reg hischshr1520`g' i94 vi* $contr   if iwm94<`i' & iwm94>-`i', cluster(pcode)
		estimates store altsp_`b'_4`g'


}

	label var i94 "None"

estout ///
	altsp_100_0`g' altsp_50_0`g' altsp_25_0`g' altsp_10_0`g' altsp_5_0`g'   ///
	using "$root/output/table_altrob_0`g'.tex", ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) $slvl
	label var i94 "Linear"

estout ///
	altsp_100_1`g' altsp_50_1`g' altsp_25_1`g' altsp_10_1`g' altsp_5_1`g'   ///
	using "$root/output/table_altrob_1`g'.tex", ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) $slvl
	label var i94 "Quadratic"

estout ///
	altsp_100_2`g' altsp_50_2`g' altsp_25_2`g' altsp_10_2`g' altsp_5_2`g'   ///
	using "$root/output/table_altrob_2`g'.tex", ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) $slvl

	label var i94 "Cubic"

estout ///
	altsp_100_3`g' altsp_50_3`g' altsp_25_3`g' altsp_10_3`g' altsp_5_3`g'   ///
	using "$root/output/table_altrob_3`g'.tex", ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) $slvl
			label var i94 "Quartic"

estout ///
	altsp_100_4`g' altsp_50_4`g' altsp_25_4`g' altsp_10_4`g' altsp_5_4`g'   ///
	using "$root/output/table_altrob_4`g'.tex", ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) $slvl
		
estout ///
	altsp_100_4`g' altsp_50_4`g' altsp_25_4`g' altsp_10_4`g' altsp_5_4`g'   ///
	using "$root/output/table_altrob_obs`g'.tex", ///
		replace style(tex) collabels(, none) label cells(none)  ///
		 mlabels(, none) ///
		stats(N, fmt(%9.0g ) labels("Observations")  )		
}







******************* TABLE 4 ROBUSTNESS CHECKS ********************



use regdata0.dta, replace

label var vshr_islam1994 "Islamic vote share 1994"

set more off




foreach g in f m {
	preserve
		qui reg hischshr1520`g' i94 vi1*  $contr 
			gen sample=e(sample)
			keep if sample==1
			rdob hischshr1520`g' iwm94 , c(0)
			local o=shopt
	restore

		reg hischshr1520`g' i94 vi1*  $contr  if iwm94<`o' & iwm94>-`o', vce($se)
			estimates store base`g'		
			estadd scalar h=`o'
			gen sample=e(sample)
			sum hischshr1520`g' if sample==1
			estadd scalar be = r(mean)
			drop sample

		qui reg hischshr1520`g' i94 vi1*  $contr  if iwm94<`o' & iwm94>-`o', 
			estimates store rob_base`g'		
	preserve
		qui reg hischshr1520`g' i94 vi1*  merkez* subbuyuk  vshr_islam1994  anyc pd_* c90hischshr1520f c90hischshr1520m c90empf c90mrf c90sexr c90ageshr19 c90ageshr60 c90lpop
			gen sample=e(sample)
			keep if sample==1
			rdob hischshr1520`g' iwm94 , c(0)
			local o=shopt
	restore
		reg hischshr1520`g' i94 vi1*  merkez* subb vshr_islam1994 anyc pd_*  c90hischshr1520f c90hischshr1520m c90empf c90mrf c90sexr c90ageshr19 c90ageshr60 c90lpop if iwm94<`o' & iwm94>-`o', vce($se)
			estimates store c90`g'		
			estadd scalar h=`o'		
			gen sample=e(sample)
			sum hischshr1520`g' if sample==1
			estadd scalar be = r(mean)
			drop sample

		
		reg hischshr1520`g' i94 vi1*  merkez* subb  vshr_islam1994 anyc pd_* c90hischshr1520f c90hischshr1520m c90empf c90mrf c90sexr c90ageshr19 c90ageshr60 c90lpop if iwm94<`o' & iwm94>-`o', 
			estimates store rob_c90`g'		
	
	preserve
	
	
		qui reg hischshr1520`g' i94 vi1*   $contr if islamleft1994==0 
			gen sample=e(sample)
			keep if sample==1
			rdob hischshr1520`g' iwm94 , c(0)
			local o=shopt
	restore
		reg hischshr1520`g' i94 vi1*  $contr   if iwm94<`o' & iwm94>-`o' & islamleft1994==0, vce($se)
			estimates store islleft0`g'
			estadd scalar h=`o'
			gen sample=e(sample)
			sum hischshr1520`g' if sample==1
			estadd scalar be = r(mean)
			drop sample

		qui reg hischshr1520`g' i94 vi1*  $contr   if iwm94<`o' & iwm94>-`o' & islamleft1994==0, 
			estimates store rob_islleft0`g'
	

	preserve
		qui reg hischshr1520`g' i94 vi1*  $contr if islamleft1994==1
			gen sample=e(sample)
			keep if sample==1
			rdob hischshr1520`g' iwm94 , c(0)
			local o=shopt
	restore	
		reg hischshr1520`g' i94 vi1*   $contr   if iwm94<`o' & iwm94>-`o' & islamleft1994==1, vce($se)
			estimates store islleft1`g'
			estadd scalar h=`o'
			gen sample=e(sample)
			sum hischshr1520`g' if sample==1
			estadd scalar be = r(mean)
			drop sample

		
		reg hischshr1520`g' i94 vi1*  $contr   if iwm94<`o' & iwm94>-`o' & islamleft1994==1,  
			estimates store rob_islleft1`g'
	
	
	preserve
		qui reg hischshr1520`g' i94 vi1*  $contr eduarshr90 lpopdens90 lareapre ownshr* 
			gen sample=e(sample)
			keep if sample==1
			rdob hischshr1520`g' iwm94 , c(0)
			local o=shopt
	restore
		reg hischshr1520`g' i94 vi1*  $contr eduarshr90 lpopdens90 lareapre ownshr*  if iwm94<`o' & iwm94>-`o', vce($se)
			estimates store build`g'
			estadd scalar h=`o'
			gen sample=e(sample)
			sum hischshr1520`g' if sample==1
			estadd scalar be = r(mean)
			drop sample

		
		reg hischshr1520`g' i94 vi1*  $contr eduarshr90 lpopdens90 lareapre ownshr*  if iwm94<`o' & iwm94>-`o',  
			estimates store rob_build`g'
	
	
	preserve
		qui reg hischshr1520`g' i94 vi1*  $contr i89
			gen sample=e(sample)
			keep if sample==1
			rdob hischshr1520`g' iwm94 , c(0)
			local o=shopt
	restore
		reg hischshr1520`g' i94 vi1*  $contr i89 if iwm94<`o' & iwm94>-`o', vce($se)
			estimates store inc`g'
			estadd scalar h=`o'
			gen sample=e(sample)
			sum hischshr1520`g' if sample==1
			estadd scalar be = r(mean)
			drop sample

		
		reg hischshr1520`g' i94 vi1*  $contr i89 if iwm94<`o' & iwm94>-`o',  
			estimates store rob_inc`g'
	

	preserve
	
		qui reg hischshr1520`g' i94 vi1*  $contr if anyc==1
			gen sample=e(sample)
			keep if sample==1
			rdob hischshr1520`g' iwm94 , c(0)
			local o=shopt
	restore
		reg hischshr1520`g' i94 vi1*  $contr  if iwm94<`o' & iwm94>-`o' & anyc ==1, vce($se)
			estimates store urban`g'
			estadd scalar h=`o'
			gen sample=e(sample)
			sum hischshr1520`g' if sample==1
			estadd scalar be = r(mean)
			drop sample

		
		reg hischshr1520`g' i94 vi1*  $contr  if iwm94<`o' & iwm94>-`o' & anyc ==1, 
			estimates store rob_urban`g'
	

	preserve
	
		qui reg hischshr1520`g' i94 vi1*  $contr if anyc ==0
			gen sample=e(sample)
			keep if sample==1
			rdob hischshr1520`g' iwm94 , c(0)
			local o=shopt
	restore
		reg hischshr1520`g' i94 vi1*  $contr  if iwm94<`o' & iwm94>-`o' & anyc ==0, vce($se)
			estimates store rural`g'
			estadd scalar h=`o'
			gen sample=e(sample)
			sum hischshr1520`g' if sample==1
			estadd scalar be = r(mean)
			drop sample

		
		reg hischshr1520`g' i94 vi1*  $contr  if iwm94<`o' & iwm94>-`o' & anyc ==0,  
			estimates store rob_rural`g'
			
		
		
	
		
	estout ///
		base`g'  ///
			c90`g'  ///
		 		 islleft0`g' ///
		 	 		 islleft1`g'   ///
		 	 	 	 		build`g'  ///
		 	 	 	 			inc`g'  ///
		 	 	 	 				urban`g' ///
		 	 	 	 					rural`g' ///
		using "$root/output/table3i_`g'.tex", ///
			replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
			keep(i94) mlabels(, none) ///
			stats(h r2 N , fmt(%9.3f %9.2f %9.0g ) labels("Bandwidth" "\$R^2\$"   "Observations") layout(@ @)) ///
			$slvl 		
		
	estout ///
		base`g'  ///
			c90`g'  ///
		 		 islleft0`g' ///
		 	 		 islleft1`g'   ///
		 	 	 	 		build`g'  ///
		 	 	 	 			inc`g'  ///
		 	 	 	 				urban`g' ///
		 	 	 	 					rural`g' ///
		using "$root/output/table3i_sumstats`g'.tex", ///
			replace style(tex) collabels(, none) label cells(none)  ///
			keep() mlabels(, none) ///
			stats(be, fmt(%9.3f) labels("Outcome mean" ) layout(@)) 		
			
	
		
}		




*Different male female estimates

foreach y in base c90 islleft0 islleft1   build inc urban rural {
	set more off
	suest rob_`y'f rob_`y'm, vce($se)
	test [rob_`y'f_mean]i94 = [rob_`y'm_mean]i94 
	mat eqcoeff_rob_`y'=(.) 
	mat eqcoeff_rob_`y'[1,1]=r(p)
}


mat rob_eqcoeff = eqcoeff_rob_base , eqcoeff_rob_c90 , eqcoeff_rob_islleft0 , eqcoeff_rob_islleft1  , eqcoeff_rob_build , eqcoeff_rob_inc , eqcoeff_rob_urban , eqcoeff_rob_rural 
mat rownames rob_eqcoeff = "p-value"
estout matrix(rob_eqcoeff, fmt(%9.3f)) using $root/output/table3i_p.tex,  ///
	replace title("") style(tex) ///
	collabels(, none) mlabels(, none) nolabel




estimates clear


**************************************************************************



******************TABLE 5 -  HETEROGENOUS TREATMENT EFFECTS*************


**************************************************************************


estimates clear



use regdata0.dta, clear
set more off

foreach z in relarshr00 litshrt vshr_islam1994 {

foreach g in f m {





label var vshr_islam1994 "Islamic vote share 1994"



*preserve

local l=.25

qui reg hischshr1520`g'  i94 vi1* `z'  $contr if iwm94<`l' & iwm94>-`l' 

gen `z'`g'samp=1 if e(sample)==1


sum `z' if `z'`g'samp==1, d
local m=r(p50)
display `m'

qui reg hischshr1520`g' i94     $contr `z' if `z'`g'samp==1 &  iwm94<`l' & iwm94>-`l' & `z'>`m' , vce($se)
	estimates store H`g'`z'0_ols
	sum hischshr1520`g' if e(sample)==1
	estadd scalar hmean=r(mean)
	estadd scalar h=`l'
	qui reg hischshr1520`g' i94     $contr `z' if `z'`g'samp==1 & iwm94<`l' & iwm94>-`l' & `z'>`m' ,  
    estimates store HS`g'`z'0_ols

qui reg hischshr1520`g' i94    $contr `z' if `z'`g'samp==1 & iwm94<`l' & iwm94>-`l' & `z'<`m', vce($se)
	estimates store H`g'`z'1_ols
	sum hischshr1520`g' if e(sample)==1
	estadd scalar hmean=r(mean)
	estadd scalar h=`l'

	qui reg hischshr1520`g' i94    $contr `z' if `z'`g'samp==1 & iwm94<`l' & iwm94>-`l' & `z'<`m',  
    estimates store HS`g'`z'1_ols



qui reg hischshr1520`g' i94 vi1*   $contr `z' if `z'`g'samp==1 & iwm94<`l' & iwm94>-`l' & `z'>`m' , vce($se)
	estimates store H`g'`z'0_rd
	sum hischshr1520`g' if e(sample)==1
	estadd scalar hmean=r(mean)
	estadd scalar h=`l'
	
	qui reg hischshr1520`g' i94 vi1*   $contr `z' if `z'`g'samp==1 & iwm94<`l' & iwm94>-`l' & `z'>`m' , 
	estimates store HS`g'`z'0_rd

qui reg hischshr1520`g' i94 vi1*   $contr `z' if `z'`g'samp==1 & iwm94<`l' & iwm94>-`l' & `z'<`m', vce($se)
	estimates store H`g'`z'1_rd
	sum hischshr1520`g' if e(sample)==1
	estadd scalar hmean=r(mean)
	estadd scalar h=`l'

	qui reg hischshr1520`g' i94 vi1*   $contr `z' if `z'`g'samp==1 & iwm94<`l' & iwm94>-`l' & `z'<`m', 
	estimates store HS`g'`z'1_rd


qui suest HS`g'`z'0_rd  HS`g'`z'1_rd , vce($se)
	test [HS`g'`z'0_rd_mean]i94 = [HS`g'`z'1_rd_mean]i94 
	estadd scalar sp=r(p) : H`g'`z'0_rd
display r(p)


*restore
}

}

foreach g in f m {

qui suest HS`g'vshr_islam19940_rd  HS`g'vshr_islam19941_rd ///
	HS`g'relarshr000_rd  HS`g'relarshr001_rd ///
	HS`g'litshrt0_rd  HS`g'litshrt1_rd , vce($se)

*[c90lpop_mean]i94

test ([HS`g'vshr_islam19940_rd_mean]i94 = [HS`g'vshr_islam19941_rd_mean]i94) ///
	([HS`g'relarshr000_rd_mean]i94 = [HS`g'relarshr001_rd_mean]i94) ///
	([HS`g'litshrt0_rd_mean]i94 = [HS`g'litshrt1_rd_mean]i94)
}

foreach z in relarshr00 litshrt vshr_islam1994 {

estout ///
	Hf`z'0_ols  Hf`z'1_ols  Hm`z'0_ols  Hm`z'1_ols   ///
	using "$root/output/table_H`z'olsmean.tex", ///
 	replace style(tex) cells(none) collabels(, none) mlabels(, none) ///
	 stats(hmean, fmt( %9.3f ) labels("Outcome mean"))		

estout ///
	Hf`z'0_ols  Hf`z'1_ols  Hm`z'0_ols  Hm`z'1_ols   ///
	using "$root/output/table_Hols`z'.tex", ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(N h, fmt(%9.0g %9.2f ) labels("Observations" "Bandwidth") layout(@ @)) $slvl

estout ///
	Hf`z'0_rd  Hf`z'1_rd  Hm`z'0_rd  Hm`z'1_rd   ///
	using "$root/output/table_H`z'mean.tex", ///
 	replace style(tex) cells(none) collabels(, none) mlabels(, none) ///
	 stats(hmean, fmt( %9.3f ) labels("Outcome mean"))		

estout ///
	Hf`z'0_rd  Hf`z'1_rd  Hm`z'0_rd  Hm`z'1_rd   ///
	using "$root/output/table_H`z'.tex", ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(sp h N , fmt(%9.3f %9.3f %9.0g  ) labels("p-value" "Bandwidth" "Observations" ) layout(@ @ @)) $slvl

}





***************************************************************************
**************************** TABLE 6: Buildings ***************************
***************************************************************************


use regdata0.dta, clear

mat drop _all
foreach y in eduarshr00 eduprivshr00 eduvakifshr00 edumunshr00 edugovshr00 bp_edushr bp_eduprvshr  {
	set more off
	global contrub "lareapre eduarshr90"

	preserve




	reg `y' i94 $contrub $contr, vce($se)
	keep if e(sample)==1
	keep `y' i94 $contrub $contr iwm94 vi* pcode
		estimates store ols_`y'
	sum `y'
		estadd scalar be=r(mean)
		estadd scalar h=1
		
	reg `y' i94 , vce($se)
		estimates store olsN_`y'
	sum `y' if e(sample)==1
		estadd scalar be=r(mean)	
	 
	rdob `y' iwm94 $contrub $contrs pd_2-pd_81, c(0)
		local o = shopt
	reg `y' i94 vi1*   $contrub $contr if abs(iwm94)<`o', vce($se)
		estimates store loc1_`y'
		estadd scalar bw = `o'
				sum `y' if e(sample)==1
				estadd scalar be=r(mean)
	reg `y' i94 vi1*   $contrub $contr if abs(iwm94)<`o'/2, vce($se)
		estimates store loc2_`y'
				estadd scalar bw = `o'/2
	reg `y' i94 vi1*   $contrub $contr if abs(iwm94)<`o'*2 , vce($se)
		estimates store loc3_`y'
				estadd scalar bw = `o'*2


	restore
}



estout olsN_eduarshr00 olsN_eduprivshr00 olsN_eduvakifshr00 olsN_edumunshr00 olsN_edugovshr00 olsN_bp_edushr olsN_bp_eduprvshr using $root/output/table_BLD_meanN.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) 

estout olsN_eduarshr00 olsN_eduprivshr00 olsN_eduvakifshr00 olsN_edumunshr00 olsN_edugovshr00 olsN_bp_edushr olsN_bp_eduprvshr using $root/output/table_BLD_olsN.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
	 $slvl 
*	stats(r2 , fmt(%9.2f    ) labels("\$R^2\$" ) layout(@))

estout ols_eduarshr00 ols_eduprivshr00 ols_eduvakifshr00 ols_edumunshr00 ols_edugovshr00 ols_bp_edushr ols_bp_eduprvshr using $root/output/table_BLD_mean.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) 

estout ols_eduarshr00 ols_eduprivshr00 ols_eduvakifshr00 ols_edumunshr00 ols_edugovshr00 ols_bp_edushr ols_bp_eduprvshr using $root/output/table_BLD_ols.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		 $slvl 
		
estout ols_eduarshr00 ols_eduprivshr00 ols_eduvakifshr00 ols_edumunshr00 ols_edugovshr00 ols_bp_edushr ols_bp_eduprvshr using $root/output/table_BLD_ols_obs.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(h N, fmt(%9.2f %9.0g  ) labels("Bandwidth" "Observations") layout(@))		


estout loc1_eduarshr00 loc1_eduprivshr00 loc1_eduvakifshr00 loc1_edumunshr00 loc1_edugovshr00 loc1_bp_edushr loc1_bp_eduprvshr using $root/output/table_BLD_loc1_mean.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) 


estout loc1_eduarshr00 loc1_eduprivshr00 loc1_eduvakifshr00 loc1_edumunshr00 loc1_edugovshr00 loc1_bp_edushr loc1_bp_eduprvshr  using $root/output/table_BLD_loc1.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		 stats(bw  N, fmt(%9.3f %9.0g ) labels("Bandwidth" "Observations" ) layout(@)) $slvl


estout loc2_eduarshr00 loc2_eduprivshr00 loc2_eduvakifshr00 loc2_edumunshr00 loc2_edugovshr00 loc2_bp_edushr loc2_bp_eduprvshr  using $root/output/table_BLD_loc2.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(bw  N, fmt(%9.3f %9.0g ) labels("Bandwidth" "Observations" ) layout(@)) $slvl
		

	
estout loc3_eduarshr00 loc3_eduprivshr00 loc3_eduvakifshr00 loc3_edumunshr00 loc3_edugovshr00 loc3_bp_edushr loc3_bp_eduprvshr  using $root/output/table_BLD_loc3.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(bw  N, fmt(%9.3f %9.0g ) labels("Bandwidth" "Observations" ) layout(@)) $slvl
		
	
		

estout loc1_eduarshr00 loc1_eduprivshr00 loc1_eduvakifshr00 loc1_edumunshr00 loc1_edugovshr00 loc1_bp_edushr loc1_bp_eduprvshr  using $root/output/table_BLD_loc1_obs.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(bw  N, fmt(%9.3f %9.0g ) labels("Bandwidth" "Observations" ) layout(@))  $slvl		
				

*************************************************************************



 ******************************************
 
 ********TABLE 7 LONG-RUN EDUCATION EFFECTS
 
 ******************************************
 estimates clear
 use regdata0.dta, clear
 set more off

foreach y in  ahischshr15_19  ahischshr15_29 nevm15_19  {
foreach g in f m {


reg `y'`g' i94  $contr, vce($se)
	estimates store ST94ols_`y'`g'
	sum `y'`g' if e(sample)==1
	estadd scalar be=r(mean)
	estadd scalar h=1

reg `y'`g' i94 if e(sample)==1, vce($se)
	estimates store ST94olsN_`y'`g'
	sum `y'`g' if e(sample)==1
	estadd scalar be=r(mean)
	estadd scalar h=1	
	
preserve
qui reg `y'`g' i94 vi1*  $contr, vce($se)
		
	keep if e(sample)==1
	keep `y'`g' i94 iwm94 $contr pcode vi1* 
	rdob `y'`g' iwm94 , c(0) 
	scalar h=shopt
	reg `y'`g'  i94 vi1* $contr if abs(iwm94)<shopt , vce($se)
	estimates store ST94rd_`y'`g'
	sum `y'`g' if e(sample)==1
	estadd scalar be=r(mean)
	estadd scalar h=shopt
	reg `y'`g'  i94 vi1*  $contr if abs(iwm94)<shopt/2, vce($se)
	estimates store ST94rd2_`y'`g'
	sum `y'`g' if e(sample)==1
	estadd scalar be=r(mean)
	estadd scalar h=shopt/2
	reg `y'`g'  i94 vi1*  $contr if abs(iwm94)<shopt*2, vce($se)
	estimates store ST94rd3_`y'`g'
	sum `y'`g' if e(sample)==1
	estadd scalar be=r(mean)
	estadd scalar h=shopt*2
	
restore


}
}

*OLS

	estout  ///
	ST94olsN_ahischshr15_19f ST94olsN_ahischshr15_19m ST94olsN_ahischshr15_29f ST94olsN_ahischshr15_29m ST94olsN_nevm15_19f ST94olsN_nevm15_19m  ///
	using "$root/output/table_ST94olsmN.tex", replace ///
		style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(be, fmt( %9.3f  ) labels("Outcome mean" ) layout(@ )) 

	estout  ///
	ST94olsN_ahischshr15_19f ST94olsN_ahischshr15_19m ST94olsN_ahischshr15_29f ST94olsN_ahischshr15_29m ST94olsN_nevm15_19f ST94olsN_nevm15_19m  ///
	using "$root/output/table_ST94olsN.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		 	$slvl	
*stats( , fmt( %9.2f  ) labels( "\$R^2\$" ) layout(@ ))


	estout  ///
	ST94ols_ahischshr15_19f ST94ols_ahischshr15_19m ST94ols_ahischshr15_29f ST94ols_ahischshr15_29m ST94ols_nevm15_19f ST94ols_nevm15_19m  ///
	using "$root/output/table_ST94olsm.tex", replace ///
		style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(be, fmt( %9.3f  ) labels("Outcome mean" ) layout(@ )) 

	estout  ///
	ST94ols_ahischshr15_19f ST94ols_ahischshr15_19m ST94ols_ahischshr15_29f ST94ols_ahischshr15_29m ST94ols_nevm15_19f ST94ols_nevm15_19m  ///
	using "$root/output/table_ST94ols.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
			$slvl	
		
	estout  ///
	ST94ols_ahischshr15_19f ST94ols_ahischshr15_19m ST94ols_ahischshr15_29f ST94ols_ahischshr15_29m ST94ols_nevm15_19f ST94ols_nevm15_19m  ///
	using "$root/output/table_ST94olsobs.tex", replace ///
		style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(h N , fmt(%9.2f %9.0g   ) labels( "Bandwidth" "Observations" ) layout(@ )) 	 	

	estout  ///
	ST94rd_ahischshr15_19f ST94rd_ahischshr15_19m ST94rd_ahischshr15_29f ST94rd_ahischshr15_29m ST94rd_nevm15_19f ST94rd_nevm15_19m  ///
	using "$root/output/table_ST94rdm.tex", replace ///
		style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(be, fmt( %9.3f  ) labels("Outcome mean" ) layout(@ )) 

	estout  ///
	ST94rd_ahischshr15_19f ST94rd_ahischshr15_19m ST94rd_ahischshr15_29f ST94rd_ahischshr15_29m ST94rd_nevm15_19f ST94rd_nevm15_19m  ///
	using "$root/output/table_ST94rd.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		 stats(h  N, fmt(%9.3f %9.0g ) labels("Bandwidth" "Observations" ) layout(@)) 	$slvl	



	estout  ///
	ST94rd_ahischshr15_19f ST94rd_ahischshr15_19m ST94rd_ahischshr15_29f ST94rd_ahischshr15_29m ST94rd_nevm15_19f ST94rd_nevm15_19m  ///
	using "$root/output/table_ST94rdobs.tex", replace ///
		style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(h N, fmt(%9.2f %9.0g  ) labels("Bandwidth" "Observations" ) layout(@ )) 

	estout  ///
	ST94rd2_ahischshr15_19f ST94rd2_ahischshr15_19m ST94rd2_ahischshr15_29f ST94rd2_ahischshr15_29m ST94rd2_nevm15_19f ST94rd2_nevm15_19m  ///
	using "$root/output/table_ST94rd2m.tex", replace ///
		style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(be, fmt( %9.3f  ) labels("Outcome mean" ) layout(@ )) 

	estout  ///
	ST94rd2_ahischshr15_19f ST94rd2_ahischshr15_19m ST94rd2_ahischshr15_29f ST94rd2_ahischshr15_29m ST94rd2_nevm15_19f ST94rd2_nevm15_19m  ///
	using "$root/output/table_ST94rd2.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		 stats(h  N, fmt(%9.3f %9.0g ) labels("Bandwidth" "Observations" ) layout(@)) 	$slvl	


	estout  ///
	ST94rd3_ahischshr15_19f ST94rd3_ahischshr15_19m ST94rd3_ahischshr15_29f ST94rd3_ahischshr15_29m ST94rd3_nevm15_19f ST94rd3_nevm15_19m  ///
	using "$root/output/table_ST94rd3m.tex", replace ///
		style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(be, fmt( %9.3f  ) labels("Outcome mean" ) layout(@ )) 

	estout  ///
	ST94rd3_ahischshr15_19f ST94rd3_ahischshr15_19m ST94rd3_ahischshr15_29f ST94rd3_ahischshr15_29m ST94rd3_nevm15_19f ST94rd3_nevm15_19m  ///
	using "$root/output/table_ST94rd3.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		 stats(h  N, fmt(%9.3f %9.0g ) labels("Bandwidth" "Observations" ) layout(@)) 	$slvl	



***************************************************************************
************************* TABLE 8  - ISLAMIC PREFERENCES
***************************************************************************


estimates clear
mat drop _all
scalar drop _all
use regdata0.dta, clear




	foreach y in   vshr_islam1999 vshr_islam2004 vshr_islam2002 vshr_islam2007 vshr_islam2009  femshr wshr_islam wshr_noislam  {
		set more off
		sum `y'

	
		
		reg `y' i94  $contr , vce($se)
			*gen sample=e(sample)
			estimates store cols_`y'
			sum `y' if e(sample)==1
			estadd scalar be=r(mean)
			estadd scalar h=1

			reg `y' i94  $contr ,  
				estimates store r_cols_`y'
				
		reg `y' i94  if e(sample)==1  , vce($se)
			estimates store colsN_`y'
			sum `y' if e(sample)==1
			estadd scalar be=r(mean)
			reg `y' i94  $contr ,  
				estimates store r_colsN_`y'	
				

			rdob `y' iwm94 , c(0)
			local shopt2=shopt
			
		reg `y' i94 vi1* $contr if abs(iwm94)<shopt  , vce($se)
			estimates store crd_`y'
			estadd scalar bw = shopt
			sum `y' if e(sample)==1
			estadd scalar be=r(mean)
			reg `y' i94 vi1* $contr if abs(iwm94)<shopt  ,  
				estimates store r_crd_`y'


		reg `y' i94 vi1*   $contr if abs(iwm94)<shopt/2  , vce($se)
			estimates store crda_`y'
			estadd scalar bw = shopt/2
			sum `y' if e(sample)==1
			estadd scalar be=r(mean)
			reg `y' i94 vi1* $contr if abs(iwm94)<shopt/2  ,  
				estimates store r_crda_`y'


		reg `y' i94 vi1*   $contr if abs(iwm94)<shopt*2   , vce($se)
			estimates store crdb_`y'
			estadd scalar bw = shopt*2
			sum `y' if e(sample)==1
			estadd scalar be=r(mean)
			reg `y' i94 vi1* $contr if abs(iwm94)<shopt*2   ,  
				estimates store r_crdb_`y'
			
	}


	
set more off

suest  r_colsN_vshr_islam1999 r_colsN_vshr_islam2002  r_colsN_vshr_islam2004 r_colsN_vshr_islam2007 r_colsN_vshr_islam2009 r_colsN_femshr r_colsN_wshr_islam r_colsN_wshr_noislam, vce($se)
	test i94
	estadd scalar pval=r( p ) : colsN_vshr_islam1999

suest  r_cols_vshr_islam1999 r_cols_vshr_islam2002  r_cols_vshr_islam2004 r_cols_vshr_islam2007 r_cols_vshr_islam2009 r_cols_femshr r_cols_wshr_islam r_cols_wshr_noislam, vce($se)
	test i94
	estadd scalar pval=r( p ) : cols_vshr_islam1999

suest  r_crd_vshr_islam1999 r_crd_vshr_islam2002  r_crd_vshr_islam2004 r_crd_vshr_islam2007 r_crd_vshr_islam2009 r_crd_femshr r_crd_wshr_islam r_crd_wshr_noislam, vce($se)	
	test i94
	estadd scalar pval=r( p ) : crd_vshr_islam1999

suest r_crda_vshr_islam1999 r_crda_vshr_islam2002 r_crda_vshr_islam2004  r_crda_vshr_islam2007 r_crda_vshr_islam2009 r_crda_femshr r_crda_wshr_islam r_crda_wshr_noislam, vce($se)
	test i94
	estadd scalar pval=r( p ) : crda_vshr_islam1999


suest r_crdb_vshr_islam1999 r_crdb_vshr_islam2002 r_crdb_vshr_islam2004  r_crdb_vshr_islam2007 r_crdb_vshr_islam2009 r_crdb_femshr r_crdb_wshr_islam r_crdb_wshr_noislam, vce($se)
	test i94
	estadd scalar pval=r( p ) : crdb_vshr_islam1999



estout  colsN_vshr_islam1999 colsN_vshr_islam2002  colsN_vshr_islam2004 colsN_vshr_islam2007 colsN_vshr_islam2009 colsN_femshr colsN_wshr_islam colsN_wshr_noislam ///
		using $root/output/table_OTHNmean.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		keep()  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) 

estout  colsN_vshr_islam1999 colsN_vshr_islam2002  colsN_vshr_islam2004 colsN_vshr_islam2007 colsN_vshr_islam2009 colsN_femshr colsN_wshr_islam colsN_wshr_noislam ///
		using $root/output/table_OTHNols.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(pval , fmt(%9.3f  ) labels("Joint p-value" ) layout(@)) $slvl
		

estout  cols_vshr_islam1999 cols_vshr_islam2002  cols_vshr_islam2004 cols_vshr_islam2007 cols_vshr_islam2009 cols_femshr cols_wshr_islam cols_wshr_noislam ///
		using $root/output/table_OTHmean.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		keep()  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) 

estout  cols_vshr_islam1999 cols_vshr_islam2002  cols_vshr_islam2004 cols_vshr_islam2007 cols_vshr_islam2009 cols_femshr cols_wshr_islam cols_wshr_noislam ///
		using $root/output/table_OTHols.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(pval , fmt(%9.3f  ) labels("Joint p-value" ) layout(@)) $slvl
		
		

estout  cols_vshr_islam1999 cols_vshr_islam2002  cols_vshr_islam2004 cols_vshr_islam2007 cols_vshr_islam2009 cols_femshr cols_wshr_islam cols_wshr_noislam ///
		using $root/output/table_OTHolsobs.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		keep()  mlabels(, none) ///
		stats(h N, fmt(%9.2f %9.0g ) labels("Bandwidth" "Observations") layout(@)) 

		
		
estout  crd_vshr_islam1999 crd_vshr_islam2002  crd_vshr_islam2004 crd_vshr_islam2007 crd_vshr_islam2009 crd_femshr crd_wshr_islam crd_wshr_noislam ///
		using $root/output/table_OTHrdmean.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		keep()  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) 

estout  crd_vshr_islam1999 crd_vshr_islam2002  crd_vshr_islam2004 crd_vshr_islam2007 crd_vshr_islam2009 crd_femshr crd_wshr_islam crd_wshr_noislam ///
		using $root/output/table_OTHrd.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(pval bw N , fmt(%9.3f %9.3f %9.0g  ) labels("Joint p-value" "Bandwidth" "Observations" ) layout(@)) $slvl

estout  crda_vshr_islam1999 crda_vshr_islam2002 crda_vshr_islam2004  crda_vshr_islam2007 crda_vshr_islam2009 crda_femshr crda_wshr_islam crda_wshr_noislam ///
		using $root/output/table_OTHrda.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(pval bw N , fmt(%9.3f %9.3f %9.0g  ) labels("Joint p-value" "Bandwidth" "Observations" ) layout(@)) $slvl


estout  crdb_vshr_islam1999 crdb_vshr_islam2002 crdb_vshr_islam2004  crdb_vshr_islam2007 crdb_vshr_islam2009 crdb_femshr crdb_wshr_islam crdb_wshr_noislam ///
		using $root/output/table_OTHrdb.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(pval bw N , fmt(%9.3f %9.3f %9.0g  ) labels("Joint p-value" "Bandwidth" "Observations" ) layout(@)) $slvl


**********




*************************************************************************

***************APPENDIX TABLES********************************************

*************************************************************************


**************************************************

*********Table A1: Treatment effects on covariates**********

**************************************************



set more off
estimates clear
mat drop _all
scalar drop _all
use regdata0.dta, clear


reg hischshr1520f i94 vi1* $contr
keep if e(sample)==1


foreach y in lpop1994 vshr_islam1994 partycount shhs ageshr19 ageshr60 sexr anyc {

rdob `y' iwm94, c(0)
reg `y' i94 vi1* if abs(iwm94)<shopt, vce($se)
	estimates store cb1_`y'
	estadd scalar bw = shopt

qui reg `y' i94 vi1* if abs(iwm94)<shopt,
	estimates store Bcb1_`y'
	
reg `y' i94 vi1* if abs(iwm94)<shopt/2, vce($se)
	estimates store cb1_`y'2
	estadd scalar bw = shopt/2

qui reg `y' i94 vi1* if abs(iwm94)<shopt/2,
	estimates store Bcb1_`y'2
}	

suest Bcb1_lpop1994 Bcb1_vshr_islam1994 Bcb1_partycount Bcb1_shhs Bcb1_ageshr19 Bcb1_ageshr60 Bcb1_sexr Bcb1_anyc, vce($se)
test i94
	estadd scalar sp=r(p) : cb1_lpop1994
	
suest Bcb1_lpop19942 Bcb1_vshr_islam19942 Bcb1_partycount2 Bcb1_shhs2 Bcb1_ageshr192 Bcb1_ageshr602 Bcb1_sexr2 Bcb1_anyc2, vce($se)
test i94
	estadd scalar sp=r(p) : cb1_lpop19942

estout cb1_lpop1994 cb1_vshr_islam1994 cb1_partycount cb1_shhs cb1_ageshr19 cb1_ageshr60 cb1_sexr cb1_anyc ///
		using $root/output/table_cb1.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(sp bw  N, fmt(%9.3f %9.2f %9.0g  ) labels("p-value"  "Bandwidth"  "Observations" ) layout(@ @)) $slvl

estout cb1_lpop19942 cb1_vshr_islam19942 cb1_partycount2 cb1_shhs2 cb1_ageshr192 cb1_ageshr602 cb1_sexr2 cb1_anyc2 ///
		using $root/output/table_cb12.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(sp bw  N, fmt(%9.3f %9.2f %9.0g  ) labels("p-value"  "Bandwidth"  "Observations" ) layout(@ @)) $slvl




**************************************************

*********Table A2: Treatment effects on 1990 Census Outcomes ********************

**************************************************
estimates clear
scalar drop _all

use regdata0.dta, clear
set matsize 10000
set more off

label var c90mrf "Share Married"
label var c90lpop "Log Population"
label var c90ageshr19 "Age share below 19"
label var c90ageshr60 "Age share above 60"
label var c90hischshr1520f "Share of 15-20 Women with High School"
label var c90hischshr1520m "Share of 15-20 Men with High School"
label var c90sexr "Gender ratio"

reg hischshr1520f i94 vi1* $contr


keep if e(sample)==1

foreach y in mrf lpop empf ageshr19 ageshr60 hischshr1520f hischshr1520m sexr  {


	rdob c90`y' iwm94  , c(0)
	reg  c90`y' i94 vi1*    if abs(iwm94)<shopt, vce($se)
		estimates store sc90`y'
		sum c90`y' if e(sample)==1
		estadd scalar be=r(mean)
		estadd scalar h=shopt
	qui reg  c90`y' i94 vi1*  if abs(iwm94)<shopt  
		estimates store c90`y'
		
	reg  c90`y' i94 vi1*    if abs(iwm94)<shopt/2, vce($se)
		estimates store sc90`y'2
		sum c90`y' if e(sample)==1
		estadd scalar be=r(mean)
		estadd scalar h=shopt/2
	qui reg  c90`y' i94 vi1*  if abs(iwm94)<shopt/2  
		estimates store c90`y'2
	
 	
 
		
}




suest c90lpop c90ageshr19 c90ageshr60 c90hischshr1520f c90hischshr1520m c90sexr c90mrf, vce($se)
test i94
estadd scalar pval=r(p) : sc90hischshr1520f



suest c90lpop2 c90ageshr192 c90ageshr602 c90hischshr1520f2 c90hischshr1520m2 c90sexr2 c90mrf2, vce($se)
test i94
estadd scalar pval=r(p) : sc90hischshr1520f2




estout sc90hischshr1520f sc90hischshr1520m sc90lpop sc90ageshr19 sc90ageshr60 sc90mrf   sc90sexr sc90empf  ///
	using "$root/output/t90a.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(pval h N, fmt(%9.3f %9.2f %9.0g  ) labels("p-value" "Bandwidth" "Observations" ) layout(@ @))  $slvl

estout sc90hischshr1520f2 sc90hischshr1520m2 sc90lpop2 sc90ageshr192 sc90ageshr602 sc90mrf2   sc90sexr2 sc90empf2  ///
	using "$root/output/t90a2.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(pval h N, fmt(%9.3f %9.2f %9.0g  ) labels("p-value" "Bandwidth" "Observations" ) layout(@ @))  $slvl







*********************************************************************************

************************ Table A3 Effects on types of Education ******************

*********************************************************************************


use regdata0.dta, clear
set more off



reg hischshr1520f i94 iwm94 vi1* $contr 
keep if e(sample)==1

foreach g in f m {
foreach y in hischshr1520 pshr1520 hvshr1520 jhivshr1520 studshr1530 jhischshr1520 {

			rdob `y'`g' iwm94 , c(0)


		qui reg `y'`g' i94 vi1*   if abs(iwm94)<shopt, vce($se)
			estimates store oedN_`y'`g'		
			estadd scalar h=shopt
			qui sum `y'`g' if e(sample)==1
			estadd scalar be = r(mean)

		qui reg `y'`g' i94 vi1* $contr  if abs(iwm94)<shopt, vce($se)
			estimates store oedY_`y'`g'		
			estadd scalar h=shopt
			qui sum `y'`g' if e(sample)==1
			estadd scalar be = r(mean)
}


		
		
	estout ///
		oedY_pshr1520`g' ///
			oedY_jhischshr1520`g' ///
				oedY_jhivshr1520`g' ///
					oedY_hischshr1520`g' ///
						oedY_hvshr1520`g' ///
							oedY_studshr1530`g' ///
		using "$root/output/othed_`g'.tex", ///
			replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
			keep(i94) mlabels(, none) ///
			stats(h  N , fmt( %9.3f  %9.0g) labels( "Bandwidth" "Observations" ) layout(@ @)) ///
			$slvl 
			

	estout ///
		oedY_pshr1520`g' ///
			oedY_jhischshr1520`g' ///
				oedY_jhivshr1520`g' ///
					oedY_hischshr1520`g' ///
						oedY_hvshr1520`g' ///
							oedY_studshr1530`g' ///
	 	 	 		using "$root/output/othedsum_`g'.tex", ///
		 	 	 	 		 replace style(tex) cells(none) collabels(, none) mlabels(, none) ///
		 	 	 	 		 stats(be, fmt( %9.3f) labels("Outcome mean"))
}	
	


**********************************************************************************

************************* TABLE A4 - ADDITIONAL OUTCOMES *************************

**********************************************************************************


estimates clear
mat drop _all
scalar drop _all
use regdata0.dta, clear


	foreach y in  hwfshr1530f hiedshr2630f hiedshr2630m nonagshr1530f nonagshr1530m msshr1530marf msshr1530marm {
		set more off
		sum `y'

		mat OTH_`y' = (.,.,.,.) \ (.,.,.,.) \ (.,.,.,.) 
		

		
		reg `y' i94  $contr , vce($se)
			estimates store cols_`y'
			sum `y' if e(sample)==1
			estadd scalar be=r(mean)
			reg `y' i94  $contr ,  
				estimates store r_cols_`y'
				
		reg `y' i94  if e(sample)==1  , vce($se)
			estimates store colsN_`y'
			sum `y' if e(sample)==1
			estadd scalar be=r(mean)
			reg `y' i94  $contr ,  
				estimates store r_colsN_`y'	
				
			rdob `y' iwm94 , c(0)
			local shopt2=shopt
			
		reg `y' i94 vi1* $contr if abs(iwm94)<shopt , vce($se)
			estimates store crd_`y'
			estadd scalar bw = shopt
			sum `y' if e(sample)==1
			estadd scalar be=r(mean)
			reg `y' i94 vi1* $contr if abs(iwm94)<shopt ,  
				estimates store r_crd_`y'



			
	}



mat OTH_hwfshr1530f=OTH_hwfshr1530f , OTH_hiedshr2630f , OTH_hiedshr2630m , OTH_nonagshr1530f , OTH_nonagshr1530m , OTH_msshr1530marf , OTH_msshr1530marm  

mat rownames OTH_hwfshr1530f = "p-value"

estout matrix(OTH_hwfshr1530f, fmt("%9.3f" )) using   $root/output/tableOTHpx.tex,  ///
	replace title("") style(tex) ///
	collabels(, none) mlabels(, none) nolabel



estout  cols_hiedshr2630f cols_hiedshr2630m cols_nonagshr1530f cols_nonagshr1530m cols_hwfshr1530f cols_msshr1530marf cols_msshr1530marm  ///
		using $root/output/table_OTHmeanx.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		keep()  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) $slvl

estout  cols_hiedshr2630f cols_hiedshr2630m cols_nonagshr1530f cols_nonagshr1530m cols_hwfshr1530f cols_msshr1530marf cols_msshr1530marm   ///
		using $root/output/table_OTHolsx.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(r2 N, fmt(%9.2f %9.0g ) labels("\$R^2\$" "Observations") layout(@)) $slvl
		
estout crd_hiedshr2630f crd_hiedshr2630m crd_nonagshr1530f crd_nonagshr1530m crd_hwfshr1530f  crd_msshr1530marf crd_msshr1530marm   ///
		using $root/output/table_OTHrdmeanx.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		keep()  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) $slvl

estout crd_hiedshr2630f crd_hiedshr2630m crd_nonagshr1530f crd_nonagshr1530m crd_hwfshr1530f  crd_msshr1530marf crd_msshr1530marm   ///
		using $root/output/table_OTHrdx.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats( bw r2 N, fmt(%9.3f %9.2f %9.0g  ) labels("Bandwidth" "\$R^2\$" "Observations" ) layout(@ @)) $slvl




***************************************************************************************

*************************TABLE A5: BASIC OLS ROBUSTNESS TESTS *************************

***************************************************************************************


set more off

use regdata0.dta, clear
label var vshr_islam1994 "Islamic vote share 1994"
estimates clear
reg hischshr1520f hischshr1520m i94 $contr vshr_islam1994
keep if e(sample)==1

label var vi1 "Islamic winning margin"
label var vi1_i "Isl.win.margin * Islamic mayor"
label var partycount "Number of parties"

foreach g in f m {
reg hischshr1520`g' i94  $contrx partycount  , vce($se) 
	estimates store x0_`g'
	estadd scalar h=1


reg hischshr1520`g' i94  $contr , vce($se)
	estimates store x1_`g'
	estadd scalar h=1

rdob hischshr1520`g' iwm94 , c(0)
reg hischshr1520`g' i94 vi1* $contrx partycount if abs(iwm94)<shopt , vce($se)
	estimates store x2_`g'
	estadd scalar h=shopt

reg hischshr1520`g' i94 vi1* $contr if abs(iwm94)<shopt    , vce($se)
	estimates store x3_`g'
	estadd scalar h=shopt

	estout  ///
	x0_`g' x1_`g' x2_`g' x3_`g'  ///
	using "$root/output/table_xols`g'.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94 vi1 vi1_i vshr_islam1994 partycount ageshr19 ageshr60 sexr merkezi merkezp subbuyuk)  mlabels(, none) ///
		stats(h r2 N, fmt(%9.2f %9.2f %9.0g   ) labels("Bandwidth" "\$R^2\$" "Observations"  ) layout(@ )) 	$slvl
	
}
 



 

************************************************************************************

************ Table A6: OTHER COHORTS ************************************************

************************************************************************************



estimates clear
use regdata0.dta, clear
set more off


gen hsrff=hischshr1520f-hischshr3164f
gen hsrmf=hischshr1520m-hischshr3164f
gen hsrmm=hischshr1520m-hischshr3164m
gen hsrfm=hischshr1520f-hischshr3164m



global qr "2"

foreach y in hischshr1520f hischshr1520m hischshr2125f hischshr2125m hischshr2630f hischshr2630m hischshr3164f hischshr3164m hsrff hsrfm hsrmm hsrmf {


		
		reg `y' i94 iwm94 $contr  , vce($se)

		rdob `y' iwm94  , c(0) 
		scalar h=shopt
		reg `y'  i94 vi1* $contr if iwm94<=shopt & iwm94>=-shopt, vce($se)
			estimates store relrd_`y'
		qui sum `y' if e(sample)==1
			estadd scalar hmean=r(mean)
			estadd scalar h=shopt
}



	
	estout relrd_hischshr1520f relrd_hischshr2125f relrd_hischshr2630f relrd_hischshr3164f relrd_hsrff relrd_hsrfm ///
	using "$root/output/table_MH_wmnm.tex", replace ///
		style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(hmean, fmt( %9.3f ) labels("Outcome mean") layout(@ ))  

	estout relrd_hischshr1520f relrd_hischshr2125f relrd_hischshr2630f relrd_hischshr3164f relrd_hsrff relrd_hsrfm ///
	using "$root/output/table_MH_wmn.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(N h, fmt(%9.0g  %9.2f ) labels("Observations" "Bandwidth") layout(@ @)) $slvl

	estout relrd_hischshr1520m relrd_hischshr2125m relrd_hischshr2630m relrd_hischshr3164m relrd_hsrmf relrd_hsrmm ///
	using "$root/output/table_MH_menm.tex", replace ///
		style(tex) collabels(, none) label cells(none)  ///
		  mlabels(, none) ///
		stats(hmean, fmt( %9.3f ) labels("Outcome mean") layout(@ ))  

	estout relrd_hischshr1520m relrd_hischshr2125m relrd_hischshr2630m relrd_hischshr3164m relrd_hsrmf relrd_hsrmm  ///
	using "$root/output/table_MH_men.tex", replace ///
		style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(N h, fmt(%9.0g  %9.2f ) labels("Observations" "Bandwidth") layout(@ @)) $slvl



**********************************************************
******** TABLE A7: TREATMENT EFFECTS ON DEMOGRAPHICS *****
**********************************************************

set more off
estimates clear
mat drop _all
scalar drop _all
use regdata0.dta, clear


reg i94 popshr* vi1*

keep if e(sample)==1

foreach c in 614 1520 2125 2630 3164 65 {
foreach g in f m {
rdob popshr`c'`g' iwm94, c(0)
reg popshr`c'`g' i94 vi1* if abs(iwm94)<shopt, vce($se)
	estimates store shrpop`c'`g'
	estadd scalar bw = shopt
	sum popshr`c'`g' if e(sample)==1
	estadd scalar be=r(mean)
reg popshr`c'`g' i94 vi1* if abs(iwm94)<shopt,
	estimates store Bshrpop`c'`g'




}
gen gr_`c'=popshr`c'f/popshr`c'm

rdob gr_`c' iwm94, c(0)
reg gr_`c' i94 vi1* if abs(iwm94)<shopt, vce($se)
	estimates store rpop`c'
	estadd scalar bw = shopt
	sum gr_`c' if e(sample)==1
	estadd scalar be=r(mean)
reg gr_`c' i94 vi1* if abs(iwm94)<shopt, 
	estimates store Brpop`c'

}


suest Bshrpop614f Bshrpop1520f Bshrpop2125f Bshrpop2630f Bshrpop3164f Bshrpop65f, vce($se)
test i94
	estadd scalar sp=r(p) : shrpop614f

suest Bshrpop614m Bshrpop2125m Bshrpop2630m Bshrpop3164m Bshrpop65m, vce($se)
test i94
	estadd scalar sp=r(p) : shrpop614m

suest Brpop614 Brpop2125 Brpop2630 Brpop3164 Brpop65, vce($se)
test i94
	estadd scalar sp=r(p) : rpop614



estout shrpop614f shrpop1520f  shrpop2125f shrpop2630f shrpop3164f shrpop65f ///
		using $root/output/table_demfm.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		keep()  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) $slvl

estout shrpop614f shrpop1520f  shrpop2125f shrpop2630f shrpop3164f shrpop65f ///
		using $root/output/table_demf.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(sp bw  N, fmt(%9.3f %9.2f %9.0g  ) labels("p-value" "Bandwidth"   "Observations" ) layout(@ @)) $slvl


estout shrpop614m shrpop1520m  shrpop2125m shrpop2630m shrpop3164m shrpop65m ///
		using $root/output/table_demmm.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		keep()  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) $slvl

estout shrpop614m shrpop1520m  shrpop2125m shrpop2630m shrpop3164m shrpop65m ///
		using $root/output/table_demm.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(sp bw  N, fmt(%9.3f %9.2f %9.0g  ) labels("p-value" "Bandwidth"   "Observations" ) layout(@ @)) $slvl



estout rpop614 rpop1520  rpop2125 rpop2630 rpop3164 rpop65 ///
		using $root/output/table_demRm.tex, ///
		replace style(tex) collabels(, none) label cells(none)  ///
		keep()  mlabels(, none) ///
		stats(be, fmt( %9.3f ) labels("Outcome mean") layout(@)) $slvl

estout rpop614 rpop1520  rpop2125 rpop2630 rpop3164 rpop65 ///
		using $root/output/table_demR.tex, ///
		replace style(tex) collabels(, none) label cells(b(star fmt(%9.3f)) se(par))  ///
		keep(i94)  mlabels(, none) ///
		stats(sp bw  N, fmt(%9.3f %9.2f %9.0g  ) labels("p-value"  "Bandwidth"  "Observations" ) layout(@ @)) $slvl

**************

