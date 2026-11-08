
*Programs used in this do-file
*rdprog_cols1_se2.do - Custom-made program for RD graphs by Erik Meyersson
*DCDENSITY.ado - Program by Justin McCrary (see readme for source) for testing smooth density of forcing variable at cutoff.


global root "{Insert Root director}"

clear all
set mem 1G
set matsize 11000

set more off

cd $root/data



*******GLOBAL

*global contr="vshr_islam1994 partycount lpop1994 ageshr19 ageshr60 sexr merkezi merkezp subbuyuk buyuk"
 global contr="vshr_islam1994 partycount shhs lpop1994 ageshr19 ageshr60 sexr merkezi merkezp subbuyuk buyuk pd_*"

global grfont "Georgia"

**************************



													

*************************************************************************

*FIGURE 1 ISLAMIC VOTE SHARE AND Islamic win margin

set more off
use regdata0.dta, clear
reg hischshr1520f i94 $contr
keep if e(sample)==1



global lim ".75"
global ffc "gray"
global nnc "black"	
	
twoway scatter iwm94  vshr_islam1994 if vshr_islam1994<$lim, ///
	msize(.75) msym(circle) mcolor($ffc) legend(off) xlabel(0(.25)$lim, nogrid)  ///
		xtitle(Islamic vote share) ytitle(Islamic win margin) ylabel(-1(.5).5, nogrid) ///
					yline(0, lcolor(gs10) lwidth(vthin))  ///
						graphregion(color(white) lcolor(white) lwidth(thick)) ///
							|| scatter iwm94 vshr_islam1994  if abs(iwm94)<.02 ///
								, msize(.75) msym(circle) mcolor($nnc)  ///
										|| function y=-1+2*x  ,  range(0 $lim) lwidth(thin) lcolor(gs6) lpattern()
												graph export $root/output/Figure1.eps, replace font($grfont)

*******************************************



*****FIGURE 2:  HISTOGRAM AND DENSITY TEST


***A 


use regdata0.dta, clear

sort iwm94

hist iwm94, ///
	bin(99)  graphregion(color(white) lcolor(white) lwidth(vthick)) ylabel(, nogrid) ///
		xline(0, lpattern(shortdash) lcolor(gs6)) percent fcolor(gs10) lcolor(gs2) xtitle("Islamic win margin") ///
				xscale(lcolor(none)) yscale(lcolor(none)) plotregion(lpattern(solid) lcolor(black))

*graph export /Users/egmeyera/Documents/Research/TURKELEC/STATA/graphout/Figure3.emf, replace
graph export $root/output/Figure2a.eps, replace font($grfont)

**** B MCCRARY DENSITY TEST 

use regdata0.dta, clear

reg hischshr1520f iwm94
keep if e(sample)==1

keep if abs(iwm94)<.6


gen Z=iwm94
sort Z

DCdensity Z , breakpoint(0) generate(Xj Yj r0 fhat se_fhat)  nograph


keep Z Yj Xj r0 fhat se_fhat


gen fu=fhat+1.96*se_fhat
gen fl=fhat-1.96*se_fhat

global rvX "Xj>-.5 & Xj<.5"
global rvR "r0>-.5 & r0<.5"
global gr "gray"


local theta2: display %5.3f r(theta)
local tse: display %5.3f r(se)

twoway scatter Yj Xj if $rvX, msymbol()   color($gr) ///
	|| line fhat r0 if r0>0 & $rvR, lcolor(black) lwidth(medthick) ///
		|| line fhat r0 if r0<0 & $rvR, lcolor(black) lwidth(medthick) ///
			|| line fu r0 if r0<0 & $rvR, lwidth(vthin) lpattern(longdash) lcolor($gr) ///
				|| line fu r0 if r0>0 & $rvR, lwidth(vthin) lpattern(longdash) lcolor($gr) ///
					|| line fl r0 if r0<0 & $rvR, lwidth(vthin) lpattern(longdash) lcolor($gr) ///
						|| line fl r0 if r0>0 & $rvR, lwidth(vthin) lpattern(longdash) lcolor($gr) ///
							 legend(off) ylabel(0(1.25)2.5, nogrid) xline(0, lwidth(vthin) lpattern(shortdash) lcolor(gs8)) ///
								plotregion(lpattern(solid) lcolor(gs4)) ///
									xscale(lcolor(none)) yscale(lcolor(none)) ///
										graphregion(color(white) lcolor(white) lwidth(thick)) saving(grDCall.gph, replace) ///
												note("Discontinuity est. = `theta2', s.e. =  `tse'") ///
													xtitle("Islamic win margin 1994") ytitle("Density")
														graph export $root/output/Figure2b.eps, replace font($grfont)




********FIGURE 4 MAIN RESULT GRAPH

use regdata0.dta, clear
set more off

global blim ".10"

cap prog drop _all

run $root/do/rdprog_cols1_se2

global gbw ".08"
global bin "50"
global lim "iwm94 <=.5 & iwm94>=-.5"
global xlabel "-.5(.25).5,"
global ylabel "-.05(.05).25,"


rdintgraphtlim hischshr1520f iwm94 $bin "$lim" "" "Islamic win margin 1994, size(1) color(white)" "$ylabel" "$xlabel" "" "A. Women in 2000, size(3.5) color(black)" "" "$gbw" 
graph save alt_hischf.gph, replace

rdintgraphtlim hischshr1520m iwm94 $bin "$lim" "" "Islamic win margin 1994, size(1) color(white)" "$ylabel" "$xlabel" "alt" "B. Men in 2000, size(3.5) color(black)" "" "$gbw" 
graph save alt_hischm.gph, replace

rdintgraphtlim c90hischshr1520f iwm94 $bin "$lim" "" "Islamic win margin 1994, size(1) color(white)" "$ylabel" "$xlabel" "" "C. Women in 1990, size(3.5) color(black)" "" "$gbw" 
graph save alt_c90hischf.gph, replace

rdintgraphtlim c90hischshr1520m iwm94 $bin "$lim" "" "Islamic win margin 1994, size(1) color(white)" "$ylabel" "$xlabel" "alt" "D. Men in 1990, size(3.5) color(black)" "" "$gbw" 
graph save alt_c90hischm.gph, replace


graph combine alt_hischf.gph alt_hischm.gph alt_c90hischf.gph alt_c90hischm.gph, col(2) imargin(1 1 1 1) graphregion(color(white) lcolor(white) lwidth(thick))  plotregion(lpattern(solid) lcolor(white)) ///
	ysize(4) xsize(3) l1title("Share of 15-20-year-olds with High School Degree", size(2.5) color(black)) saving(g.gph, replace) ///
	b1title("Islamic win margin 1994", size(2.5) color(black))


graph export $root/output/Figure4.eps, replace font($grfont)


****************
*FIGURE 5: EDUCATION TYPES




global range1 "iwm94 <.32 & iwm94>-.32"
global labrange "-.25(.25).25"
global bin "25"

rdintgraphtlim hischshr1520f iwm94 $bin "$range1" "" "" "0(.125).25," "$labrange" "" "Women, size(3.5) color(black)" "" "$gbw"
graph save alt_1.gph, replace
rdintgraphtlim hischshr1520m iwm94 $bin "$range1" "" "" "0(.125).25," "$labrange" "alt" "Men, size(3.5) color(black)" "" "$gbw"
graph save alt_2.gph, replace
graph combine alt_1.gph alt_2.gph , col(2) imargin(1 1 1 1) graphregion(color(white)) ///
	ysize(3) xsize(5)  l2title("", size(3)) saving(g1.gph, replace) b1title("", size(2.6) color(black)) title(C. Completed High School, size(3) color(black))
*b1title("Islamic win margin", size(2.6))
rdintgraphtlim pshr1520f iwm94 $bin "$range1" "" "" ".6(.3).9," "$labrange" "" "Women, size(3.5) color(black)" "" "$gbw"
graph save alt_1.gph, replace
rdintgraphtlim pshr1520m  iwm94 $bin "$range1" "" "" ".6(.3).9," "$labrange" "alt" "Men, size(3.5) color(black)" "" "$gbw"
graph save alt_2.gph, replace
graph combine alt_1.gph alt_2.gph , col(2) imargin(1 1 1 1) graphregion(color(white)) ///
	ysize(3) xsize(5) l2title("", size(3)) saving(g2.gph, replace) b1title("", size(2.6) color(black)) title(B. Completed Primary School, size(3) color(black))

	
rdintgraphtlim studshr1530f iwm94 $bin "$range1" "" "" ".1(.1).3," "$labrange" "" "Women, size(3.5) color(black)" "" "$gbw"
graph save alt_1.gph, replace
rdintgraphtlim studshr1530m iwm94 $bin "$range1" "" "" ".1(.1).3," "$labrange" "alt" "Men, size(3.5) color(black)" "" "$gbw"
graph save alt_2.gph, replace
graph combine alt_1.gph alt_2.gph , col(2) imargin(1 1 1 1) graphregion(color(white)) ///
	ysize(3) xsize(5) l2title("", size(3)) saving(g3a.gph, replace) b1title("", size(2.6) color(black)) title(A. Enrolled, size(3) color(black))

rdintgraphtlim hvshr1520f iwm94 $bin "$range1" "" "" "0(.125).25," "$labrange" "" "Women, size(3.5) color(black)" "" "$gbw"
graph save alt_1.gph, replace
rdintgraphtlim hvshr1520m iwm94 $bin "$range1" "" "" "0(.125).25," "$labrange" "alt" "Men, size(3.5) color(black)" "" "$gbw"
graph save alt_2.gph, replace
graph combine alt_1.gph alt_2.gph , col(2) imargin(1 1 1 1) graphregion(color(white)) ///
	ysize(3) xsize(5) l2title("", size(3)) saving(g4.gph, replace) b1title("", size(2.6) color(black)) title("D. Completed Vocational High Sch.", size(3) color(black))


rdintgraphtlim jhivshr1520f iwm94 $bin "$range1" "" "" "0(.25).5," "$labrange" "" "Women, size(3.5) color(black)" "" "$gbw"
graph save alt_1.gph, replace
rdintgraphtlim jhivshr1520m iwm94 $bin "$range1" "" "" "0(.25).5," "$labrange" "alt" "Men, size(3.5) color(black)" "" "$gbw"
graph save alt_2.gph, replace
graph combine alt_1.gph alt_2.gph , col(2) imargin(1 1 1 1) graphregion(color(white)) ///
	ysize(3) xsize(5) l2title("", size(3)) saving(g5.gph, replace) b1title("", size(2.6) color(black)) title("F. Completed Jr. Vocational Middle Sch", size(3) color(black))


rdintgraphtlim jhischshr1520f iwm94 $bin "$range1" "" "" "0(.25).5," "$labrange" "" "Women, size(3.5) color(black)" "" "$gbw"
graph save alt_1.gph, replace
rdintgraphtlim jhischshr1520m iwm94 $bin "$range1" "" "" "0(.25).5," "$labrange" "alt" "Men, size(3.5) color(black)" "" "$gbw"
graph save alt_2.gph, replace
graph combine alt_1.gph alt_2.gph , col(2) imargin(1 1 1 1) graphregion(color(white)) ///
	ysize(3) xsize(5) l2title("", size(3)) saving(g6.gph, replace) b1title("", size(2.6) color(black)) title("E. Completed Middle School", size(3) color(black))



graph combine g3a.gph g2.gph  g1.gph g4.gph g6.gph g5.gph   , col(2) imargin(small) graphregion(color(white) lcolor(white) lwidth(thick)) ///
	ysize(4) xsize(3.5) b1title("Islamic win margin", size(2) color(black)) l1title("Cohort share", size(2) color(black)) r1title("Cohort share", size(2) color(black) orientation(rvertical))


 graph export $root/output/Figure5.eps, replace font($grfont)

***********************************



*********FIGURE 3 CONTROL GRAPHS********************


global range1 "iwm94<.3 & iwm94>-.3"
global xlabel "-.25(.25).25,"
global gbw ".06"

global bin "25"

cap prog drop _all
 
run $root/do/rdprog_cols1_se2

set  more off

rdintgraphtlim ageshr19 iwm94 $bin "$range1" "Age 19-, size(3) color(black)" "Islamic win margin" ".35(.1).55," "$xlabel" "" "" "off" "$gbw"  
graph save contr_ageshr19.gph, replace

rdintgraphtlim ageshr60 iwm94 $bin "$range1" "Age 60+, size(3) color(black)" "Islamic win margin" ".05(.02).11," "$xlabel" "alt" "" "off" "$gbw"  
graph save contr_ageshr60.gph, replace

rdintgraphtlim lpop1994 iwm94 $bin "$range1" "Log Population, size(3) color(black)" "Islamic win margin" "6(2)10," "$xlabel" "alt" "" "off" "$gbw"   
graph save contr_lpop1994.gph, replace

rdintgraphtlim sexr iwm94 $bin "$range1"  "Gender, size(3) color(black)" "Islamic win margin" ".9(.1)1.2," "$xlabel" "" "" "off" "$gbw"  
graph save contr_sexr.gph, replace

rdintgraphtlim anyc iwm94 $bin "$range1" "Any center municipality, size(3) color(black)" "Islamic win margin" "0(.2).8," "$xlabel" "alt" "" "off" "$gbw"  
graph save contr_anyc.gph, replace
 

rdintgraphtlim vshr_islam1994 iwm94 $bin "$range1"  "Islamic vote share, size(3) color(black)" "Islamic win margin" "," "$xlabel" "" "" "off" "$gbw"  
graph save contr_vshr_islam1994.gph, replace


rdintgraphtlim partycount iwm94 $bin "$range1" "Number of parties, size(3) color(black)" "Islamic win margin" "2(2)8," "$xlabel" "" "" "" "$gbw"  
graph save contr_partycount.gph, replace

rdintgraphtlim shhs iwm94 $bin "$range1 " "Household size, size(3) color(black)" "Islamic win margin" "4(2)10," "$xlabel" "alt" "" "" "$gbw"  
graph save contr_shhs.gph, replace




graph combine contr_vshr_islam1994.gph  contr_ageshr60.gph contr_ageshr19.gph  contr_lpop1994.gph contr_sexr.gph contr_anyc.gph contr_partycount.gph contr_shhs.gph , cols(2) ///
	ysize(4.5) xsize(4) imargin(small) graphregion(color(white) lcolor(white) lwidth(thick))


 graph export $root/output/Figure3.eps, replace font($grfont)






***************************************************
***********************FIGURE A1: Alternative discontinuities

use regdata0.dta, clear
set more off

global yt "hischshr1520f"

keep  hischshr1520f  iwm94 i94 vi* pcode $contr  pd_*

sort iwm94
matrix p_x=(.,.)			

forval k=0/200 {
	local k2=`k'/100-1			
	matrix p_`k'=(.,.,.,.)			
	gen rc_`k'=`k'/100-1	
	matrix p_`k'[1,1]=`k2'

	gen di_`k'=0 if iwm94<=rc_`k'
	replace di_`k'=1 if iwm94>rc_`k'

	forval x=1/4 {
		gen dix_`k'_`x'=iwm94^`x'
		gen dix_`k'_`x'_di=iwm94^`x'*di_`k'
	}

	gen dpi=di_`k'
	qui reg $yt dpi dix_`k'_1* dix_`k'_2* $contr , cluster(pcode)
	matrix p_`k'[1,2]=abs(_b[dpi]/_se[dpi])
	
	qui reg $yt dpi dix_`k'_1* dix_`k'_2* dix_`k'_3* $contr , cluster(pcode)
	matrix p_`k'[1,3]=abs(_b[dpi]/_se[dpi])
	
	qui reg $yt dpi dix_`k'_* $contr, cluster(pcode)
	matrix p_`k'[1,4]=abs(_b[dpi]/_se[dpi])
	
	drop dpi



	drop   dix_* di_*  rc_*
}

forval k=0/200 {
	matrix p_0=p_0 \ p_`k'
}

mat list p_0
svmat p_0, names(D_)
keep D_*
egen D_m=rowmean(D_2 D_3 D_4)

matrix drop _all

	
twoway line D_m D_1 if D_1<=.4 & D_1>=-.8, ///
	xtitle("Possible discontinuities (Islamic win margin)", size(4)) xlabel(-.8(.2).4) lcolor(black) legend(off) ///
		ytitle("Absolute value of t-statistic", size(4)) ylabel(0(2)4, nogrid) ///
			graphregion(color(white)) ///
				saving(polm.gph, replace)

twoway hist D_m if D_1>-.8 & D_1<.4,   density   horiz  ///
	fcolor(gs12) lcolor(gs9) ///
		saving(g2, replace) fxsize(20)  legend(off) xtitle("Density", size(4)) xlabel(0(1)1) yscale(off)    ///
			graphregion(color(white)) ylabel(0(2)4, nogrid) 

			
graph combine polm.gph g2.gph, holes(3 4) imargin(small) graphregion(color(white) lcolor(white) lwidth(thick)) 
	graph export $root/output/FigureA1.eps, replace font($grfont)


	
****************************************************************************
