
*define program


capture program drop rdintgraphtlim
program rdintgraphtlim
	args outcome  diff bin lim ytit xtit ylabel xlabel yscale title xscale bw

gen int`diff'0=  int(`diff'*`bin'/2) if  `diff'<=0
gen int`diff'1=  int(`diff'*`bin'/2) if  `diff'>0
*gen int`diff'0=  int(`diff'*`bin'/2) if `lim' & `diff'<=0
*gen int`diff'1=  int(`diff'*`bin'/2) if `lim' & `diff'>0
	
*		sort int`diff'
		by int`diff'0, sort: egen mi`diff'0=mean(`diff') if int`diff'0!=.
		by int`diff'1, sort: egen mi`diff'1=mean(`diff') if int`diff'1!=.

		by int`diff'0, sort: egen mi`diff'outcome0=mean(`outcome') if int`diff'0!=.
		by int`diff'1, sort: egen mi`diff'outcome1=mean(`outcome') if int`diff'1!=.

*		
gen mi`diff'=mi`diff'0 
replace mi`diff'=mi`diff'1 if mi`diff'==.

gen mi`diff'outcome=mi`diff'outcome0 
replace mi`diff'outcome=mi`diff'outcome1 if mi`diff'outcome==.


gen ii1=0 if mi`diff'<0
replace ii1=1 if mi`diff'>0 & mi`diff'<.


forval x=1/4{
gen l10vi`x'=mi`diff'^`x'
gen l10vi`x'_i=mi`diff'^`x'*ii1
}



gen binc=int`diff'0 if mi`diff'<0
replace binc=int`diff'1 if mi`diff'>0 & mi`diff'<.



lpoly `outcome' `diff' if `diff'<0 & `lim' , bw(`bw') gen(x0 s0) se(se0) nograph
lpoly `outcome' `diff' if `diff'>0 & `lim' , bw(`bw') gen(x1 s1) se(se1) nograph


*quietly reg mi`diff'outcome ii1 l10vi*, cluster(binc)
*predict fit
*predict stderror, stdp
gen fit1=s0 if mi`diff'<0
gen fit2=s1 if mi`diff'>0 & mi`diff'<.


g stderror1 = se0 if mi`diff' <0
g stderror2 = se1 if mi`diff' >0 & mi`diff'<.

g int1U = fit1 + 2*stderror1
g int1L = fit1 - 2*stderror1
g int2U = fit2 + 2*stderror2
g int2L = fit2 - 2*stderror2


sort mi`diff'




#delimit ;
twoway scatter mi`diff'outcome mi`diff' if `lim',  mcolor(gs6) msymbol(o) msize(1.5) 
	|| lpolyci `outcome' `diff' if mi`diff'<0 & `lim', lpattern(solid) lwidth(vvthin) lcolor(gs6) bw(`bw') degree(1) kernel()  ciplot(rline)
	|| lpolyci `outcome'  `diff' if mi`diff'>0 & `lim', lpattern(solid) lwidth(vvthin) lcolor(gs6) bw(`bw') degree(1) kernel()   ciplot(rline)
	|| lpoly `outcome' `diff' if mi`diff'<0 & `lim', lpattern(solid) lwidth(medthin) lcolor(black) bw(`bw') degree(1) kernel()
	|| lpoly `outcome'  `diff' if mi`diff'>0 & `lim', lpattern(solid) lwidth(medthin) lcolor(black) bw(`bw') degree(1) kernel()
	ylabel(`ylabel' nogrid labsize(3)) xlabel(`xlabel') ytitle(`ytit') yscale(`yscale') xscale(`xscale') 
	graphregion(color(white) fcolor(white)) plotregion(color(white)) title(`title') 
	xtitle(`xtit') legend(off) xline(0, lwidth(.2) lpattern(shortdash) lcolor(gs6));
#delimit cr



drop int`diff'0-int2L s0 s1 x0 x1 se0 se1
end


