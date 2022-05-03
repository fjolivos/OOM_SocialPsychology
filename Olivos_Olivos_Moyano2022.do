********************************************************************************
***                  Understanding other in times of crisis                  ***
***                             Olivos, Olivos & Moyano                      ***
***                               February 2021                              ***
********************************************************************************

version 14
clear all
set more off

global root   = "C:\Users\Francisco\Dropbox\Data\Barometro de la Felicidad"
global output = "C:\Users\Francisco\Dropbox\210209 SI Understanding others in times of crisis\Results"

use "${root}\basefinal - felicidad - Wide.dta", clear

**Events 

sum P51_A P51_B P51_C P51_D P51_E P51_F P51_G P51_H P51_I P51_J P51_K P51_L P51_M P51_N P51_O P51_P P51_Q

clonevar event1 = P51_A
lab var event1 "Decrease of earnings"

clonevar event2 = P51_B
lab var event2 "Married, partnered or fall in love"

clonevar event3 = P51_C
lab var event3 "Cut-off basic services"

clonevar event4 = P51_D
lab var event4 "Mental illness"

clonevar event5 = P51_E
lab var event5 "Graduation of children or grandchildren"

clonevar event6 = P51_F
lab var event6 "Job loss of household head"

clonevar event7 = P51_G
lab var event7 "Natural disaster victim"

clonevar event8 = P51_H
lab var event8 "Birth of a child"

clonevar event9 = P51_I
lab var event9 "Serious illness"

clonevar event10 = P51_J
lab var event10 "Ending of a important relationship"

clonevar event11 = P51_K
lab var event11 "Promotion or earning increase"

clonevar event12 = P51_L
lab var event12 "Lack of urgent medial assistance"

clonevar event13 = P51_M
lab var event13 "Dropping out education for financial reason"

clonevar event14 = P51_N
lab var event14 "Become addicted to drugs or alcohol"

clonevar event15 = P51_O
lab var event15 "Purchase of a house"

clonevar event16 = P51_P
lab var event16 "Having an illness that destabilize family budget"

clonevar event17 = P51_Q
lab var event17 "Death of a loved one"

lab def event 1 "Yes" 0 "No"
foreach var of varlist event1-event17{
recode `var' (9=.) (2=0)
lab val `var' event
} 

gen famipo = event2 + event5 + event8
gen famneg = event9 + event10 + event16
gen econeg = event1 + event3 + event6 + event7 + event13
gen ecopos = event11 + event15
gen helneg = event4 + event12 + event14

foreach var of varlist famipo famneg econeg ecopos helneg{
recode `var' (0=0) (1/5=1), gen(`var'2)
lab val `var' event
}



sum event1-event17

gen positive = event2 + event5 + event8 + event11 + event15 
gen negative = event1 + event3 + event4 + event6 + event7 + event9 + event10 + event12 + event13 + event14 + event16 + event17

recode positive (0=0) (1/5=1), gen(positive2)
recode negative (0=0) (1/12=1), gen(negative2)

egen mpositive = rowmean(event2 event5 event8 event11 event15) 
egen mnegative = rowmean(event1 event3 event4 event6 event7 event9 event10 event12 event13 event14 event16 event17)

gen lnpositive = ln(1+(mpositive*10))
gen lnnegative = ln(1+(mnegative*10))

*Covariates & Entropy Balance

egen swlsm = rowmean(P5_A P5_B P5_C P5_D P5_E)
sum swlsm

encode COMUNA, gen(city)
tab city
clonevar gender = SEXO_ENCUESTADO 
clonevar age = EDAD_ENCUESTADO
clonevar edures = educ_enc
clonevar eduhohe = educ_JH
clonevar ses = NSE4
clonevar edufa = P72_A_PADRE

*Controls

clonevar happy = P1
recode happy (9=.)

ebalance event1 city gender age edures eduhohe ses edufa
rename _webal wevent1

ebalance event2 city gender age edures eduhohe ses edufa
rename _webal wevent2

ebalance event3 city gender age edures eduhohe ses edufa
rename _webal wevent3

ebalance event4 city gender age edures eduhohe ses edufa
rename _webal wevent4

ebalance event5 city gender age edures eduhohe ses edufa
rename _webal wevent5

ebalance event6 city gender age edures eduhohe ses edufa
rename _webal wevent6

ebalance event7 city gender age edures eduhohe ses edufa
rename _webal wevent7

ebalance event8 city gender age edures eduhohe ses edufa
rename _webal wevent8

ebalance event9 city gender age edures eduhohe ses edufa
rename _webal wevent9

ebalance event10 city gender age edures eduhohe ses edufa
rename _webal wevent10

ebalance event11 city gender age edures eduhohe ses edufa
rename _webal wevent11

ebalance event12 city gender age edures eduhohe ses edufa
rename _webal wevent12

ebalance event13 city gender age edures eduhohe ses edufa
rename _webal wevent13

ebalance event14 city gender age edures eduhohe ses edufa
rename _webal wevent14

ebalance event15 city gender age edures eduhohe ses edufa
rename _webal wevent15

ebalance event16 city gender age edures eduhohe ses edufa
rename _webal wevent16

ebalance event17 city gender age edures eduhohe ses edufa
rename _webal wevent17

ebalance positive2 city gender age edures eduhohe ses edufa
rename _webal wpositive2

ebalance negative2 city gender age edures eduhohe ses edufa
rename _webal wnegative2

ebct city gender age edures eduhohe ses edufa, treatvar(lnnegative) 
rename _weight wlnnegative

ebct city gender age edures eduhohe ses edufa, treatvar(lnpositive) 
rename _weight wlnpositive

*Descriptive statistics

recode educ_JH (1/3=1) (4/5=2) (6/10=3), gen(eduJH_3)
recode educ_enc (1/3=1) (4/5=2) (6/10=3), gen(eduenc_3)
tab eduJH_3, gen(eduJH)
tab eduenc_3, gen(eduEN)
recode gender (2=1) (1=0), gen(female)
tab ses, gen(ses)
mark sample 
tab zona, gen(zone)

markout sample female age eduJH1-eduEN3 ses1 ses2 ses3 ses4 zone1 zone2 zone3 zone4 Diener_escala happy lnpositive lnnegative 

tab sample

tabstat female age eduJH1-eduEN3 ses4 ses3 ses2 ses1 zone1 zone2 zone3 zone4 swlsm happy lnpositive lnnegative [aw=POND_B] if sample==1, stat(mean sd min max N) c(s)

*****Model vignette dimensions  

*General vignette model

reshape long P7_V id_vig sexo edad relacion calidad ingresos salud estilo avance, ///
        i(FOLIO) j(vi)
		
lab def  sexo 1 "Male" ///
              2 "Female"
lab val  sexo sexo

lab def edad 1 "20" ///
             2 "30" ///
			 3 "40" ///
			 4 "50" ///
			 5 "60" ///
			 6 "70"
lab val edad edad

lab def relacion 1 "Without couple for 5 years" ///
                 2 "Without couple for 1 year" ///
				 3 "Recent break up" ///
				 4 "With couple for 1 year and" ///
				 5 "With couple for 5 years"
lab val relacion relacion

lab def calidad 1 "Very bad" ///
                2 "Bad" ///
				3 "Good" ///
				4 "Very good"
lab val calidad calidad
revrs calidad

lab def ingresos 1 "210,000" ///
                 2 "400,000" ///
				 3 "600,000" ///
				 4 "800,000" ///
				 5 "1,000,000" ///
				 6 "2,000,000" ///
				 7 "3,000,000" ///
				 8 "5,000,000"
lab val ingresos ingresos

lab def salud 1 "Very bad" ///
              2 "Bad" ///
			  3 "Good" ///
			  4 "Very good"
lab val salud salud

lab def estilo 1 "Exciting" ///
               2 "Quiet" ///
			   3 "Boring" ///
			   4 "Stressful"
lab val estilo estilo

lab def avance 1 "Much worse" ///
               2 "Worse" 3 "Same" ///
			   4 "Better" ///
			   5 "Much better"
			   
lab val avance avance

*recode salud (1=4) (2=3) (3=2) (4=1)
*lab def salud2 1 "Very good" ///
*               2 "Good" ///
*			   3 "Bad" ///
*			   4 "Very bad"
*lab val salud salud2

tab relacion, gen(rela)
tab estilo, gen(estilo)

lab def  dic 0 "No" ///
             1 "Yes"
lab val  rela1 dic
lab val  rela2 dic
lab val  rela3 dic
lab val  rela4 dic
lab val  rela5 dic

lab val estilo1 dic
lab val estilo2 dic
lab val estilo3 dic
lab val estilo4 dic
xx

*******************Analysis

set more off
xtmixed P7_V ///
        i.sexo c.edad rela1 rela2 rela3 rela4 calidad ingresos salud estilo2 estilo3 estilo4 avance ///
		|| FOLIO:, variance  covariance(unstructured)
mltrsq
sleep 1000
outreg2 using "${output}\multivel_R&R", dec(3) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) replace

foreach var of varlist event1-event17{
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.`var' ///
		i.sexo#i.`var' c.edad#i.`var' /// 
		i.rela1#i.`var' i.rela2#i.`var' i.rela3#i.`var' i.rela4#i.`var' ///
		c.calidad#i.`var' c.ingresos#i.`var' c.salud#i.`var' ///
		i.estilo2#i.`var' i.estilo3#i.`var' i.estilo4#i.`var' ///
		c.avance#i.`var' ///
		|| FOLIO:, pweight(w`var') pwscale(size) variance  covariance(unstructured)
mltrsq
sleep 1000
outreg2 using "${output}multivel_R&R", dec(3) label excel ct(Model `var') alpha(0.001, 0.01, 0.05) append
}
xx
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.event1 ///
		i.sexo#i.event1 c.edad#i.event1 /// 
		i.rela1#i.event1 i.rela2#i.event1 i.rela3#i.event1 i.rela4#i.event1 ///
		c.calidad#i.event1 c.ingresos#i.event1 c.salud#i.event1 ///
		i.estilo2#i.event1 i.estilo3#i.event1 i.estilo4#i.event1 ///
		c.avance#i.event1 ///
		|| FOLIO:, pweight(we1) pwscale(size) variance  covariance(unstructured)
mltrsq
sleep 1000
outreg2 using "${output}multivel_R&R", dec(3)  label excel ct(Model E1) alpha(0.001, 0.01, 0.05) append

*Multilevel dimensionality reduction 

foreach var of varlist famipo2 famneg2 econeg2 ecopos2 helneg2{
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.famipo2 i.famneg2 i.econeg2 i.ecopos2 i.helneg2 ///
		i.sexo#i.`var' c.edad#i.`var' /// 
		i.rela1#i.`var' i.rela2#i.`var' i.rela3#i.`var' i.rela4#i.`var' ///
		c.calidad#i.`var' c.ingresos#i.`var' c.salud#i.`var' ///
		i.estilo2#i.`var' i.estilo3#i.`var' i.estilo4#i.`var' ///
		c.avance#i.`var' ///
		|| FOLIO:, variance  covariance(unstructured)
mltrsq
sleep 2000
outreg2 using "${output}multivel_dim", dec(3) label excel ct(Model `var') alpha(0.001, 0.01, 0.05) append
}

*Marginal predictions 

xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.famipo2 i.famneg2 i.econeg2 i.ecopos2 i.helneg2 ///
		i.sexo#i.helneg2 c.edad#i.helneg2 /// 
		i.rela1#i.helneg2 i.rela2#i.helneg2 i.rela3#i.helneg2 i.rela4#i.helneg2 ///
		c.calidad#i.helneg2 c.ingresos#i.helneg2 c.salud#i.helneg2 ///
		i.estilo2#i.helneg2 i.estilo3#i.helneg2 i.estilo4#i.helneg2 ///
		c.avance#i.helneg2 ///
		|| FOLIO:, variance  covariance(unstructured)
		
margins, dydx(salud) over(helneg2 ) atmeans vsquish 
marginsplot, xtitle(" ") title("Health Event"" ",position(11)) ytitle("Effect of health dimension") scheme(s2mono) name("healthXhealth", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(-.6 0)) ylabel(0 "0" -0.2 "-0.2" -0.4 "-0.4" -0.6 "-0.6") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\healthXhealth.png", as(png) replace

xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.revcalidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.famipo2 i.famneg2 i.econeg2 i.ecopos2 i.helneg2 ///
		i.sexo#i.econeg2 c.edad#i.econeg2 /// 
		i.rela1#i.econeg2 i.rela2#i.econeg2 i.rela3#i.econeg2 i.rela4#i.econeg2 ///
		c.revcalidad#i.econeg2 c.ingresos#i.econeg2 c.salud#i.econeg2 ///
		i.estilo2#i.econeg2 i.estilo3#i.econeg2 i.estilo4#i.econeg2 ///
		c.avance#i.econeg2 ///
		|| FOLIO:, variance  covariance(unstructured)

margins, dydx(revcalidad) over(econeg2) atmeans vsquish 
marginsplot, xtitle(" ") title("Economic Negative Event"" ",position(11)) ytitle("Effect of family dimension") scheme(s2mono) name("econegXfamily", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(-.3 0)) ylabel(0 "0" -0.1 "-0.1" -0.2 "-0.2" -0.3 "-0.3") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\econegXfamily.png", as(png) replace	


xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.revcalidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.famipo2 i.famneg2 i.econeg2 i.ecopos2 i.helneg2 ///
		i.sexo#i.famneg2 c.edad#i.famneg2 /// 
		i.rela1#i.famneg2 i.rela2#i.famneg2 i.rela3#i.famneg2 i.rela4#i.famneg2 ///
		c.revcalidad#i.famneg2 c.ingresos#i.famneg2 c.salud#i.famneg2 ///
		i.estilo2#i.famneg2 i.estilo3#i.famneg2 i.estilo4#i.famneg2 ///
		c.avance#i.famneg2 ///
		|| FOLIO:, variance  covariance(unstructured)

margins, dydx(revcalidad) over(famneg2) atmeans vsquish 
marginsplot, xtitle(" ") title("Familiar Negative Event"" ",position(11)) ytitle("Effect of family dimension") scheme(s2mono) name("famnegXfamily", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(-.3 0)) ylabel(0 "0" -0.1 "-0.1" -0.2 "-0.2" -0.3 "-0.3") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\famnegXfamily.png", as(png) replace	

margins, dydx(salud) over(famneg2) atmeans vsquish 
marginsplot, xtitle(" ") title("Familiar Negative Event"" ",position(11)) ytitle("Effect of health dimension") scheme(s2mono) name("famnegXsalud", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(-.6 0)) ylabel(0 "0" -0.2 "-0.2" -0.4 "-0.4" -0.6 "-0.6") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\famnegXsalud.png", as(png) replace

xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.revcalidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.famipo2 i.famneg2 i.econeg2 i.ecopos2 i.helneg2 ///
		i.sexo#i.famipo2 c.edad#i.famipo2 /// 
		i.rela1#i.famipo2 i.rela2#i.famipo2 i.rela3#i.famipo2 i.rela4#i.famipo2 ///
		c.revcalidad#i.famipo2 c.ingresos#i.famipo2 c.salud#i.famipo2 ///
		i.estilo2#i.famipo2 i.estilo3#i.famipo2 i.estilo4#i.famipo2 ///
		c.avance#i.famipo2 ///
		|| FOLIO:, variance  covariance(unstructured)
		
margins, dydx(ingresos) over(famipo2) atmeans vsquish 
marginsplot, xtitle(" ") title("Familiar Positive Event"" ",position(11)) ytitle("Effect of income dimension") scheme(s2mono) name("famposXsalud", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(0 .3)) ylabel(0 "0" .1 "0.1" .2 "0.2" 0.3 "0.3") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\famposXsalud.png", as(png) replace

xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.revcalidad c.ingresos c.salud i.estilo c.avance ///
		i.famipo2 i.famneg2 i.econeg2 i.ecopos2 i.helneg2 ///
		i.sexo#i.famipo2 c.edad#i.famipo2 /// 
		i.rela1#i.famipo2 i.rela2#i.famipo2 i.rela3#i.famipo2 i.rela4#i.famipo2 ///
		c.revcalidad#i.famipo2 c.ingresos#i.famipo2 c.salud#i.famipo2 ///
		i.estilo#i.famipo2  ///
		c.avance#i.famipo2 ///
		|| FOLIO:, variance  covariance(unstructured)
		
margins estilo, over(famipo2) atmeans vsquish 
marginsplot, xtitle(" ") title("Familiar Positive Event"" ",position(11)) ytitle("LP of style dimension") scheme(s2mono) name("famposXLs", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(4 6)) ylabel(4 "4.0" 4.5 "4.5" 5 "5.0" 5.5 "5.5" 6 "6.0") graphregion(margin(6 8 8 8)) ///
			legend(order(3 4) label(3 "No") label(4 "Yes")) // 3 and 4 to refer to lines and not ci
			
graph export "${output}\famposXLs.png", as(png) replace



***Integrated interaction

reg P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.revcalidad c.ingresos c.salud i.estilo c.avance [w=POND_B], vce(robust) 
outreg2 using "${output}linear", dec(3)  label excel ct(Model E1) alpha(0.001, 0.01, 0.05) replace


xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.revcalidad c.ingresos c.salud i.estilo c.avance ///
		i.famipo2 i.famneg2 i.econeg2 i.ecopos2 i.helneg2 ///
		|| FOLIO:, variance  
outreg2 using "${output}multivel_R&R", dec(3)  label excel ct(Model E1) alpha(0.001, 0.01, 0.05) replace
		
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.revcalidad c.ingresos c.salud i.estilo c.avance ///
		i.famipo2 i.famneg2 i.econeg2 i.ecopos2 i.helneg2 ///
		i.sexo#i.famipo2 c.edad#i.famipo2 /// 
		i.rela1#i.famipo2 i.rela2#i.famipo2 i.rela3#i.famipo2 i.rela4#i.famipo2 ///
		c.revcalidad#i.famipo2 c.ingresos#i.famipo2 c.salud#i.famipo2 ///
		i.estilo#i.famipo2  ///
		c.avance#i.famipo2 ///
		i.sexo#i.famneg2 c.edad#i.famneg2 /// 
		i.rela1#i.famneg2 i.rela2#i.famneg2 i.rela3#i.famneg2 i.rela4#i.famneg2 ///
		c.revcalidad#i.famneg2 c.ingresos#i.famneg2 c.salud#i.famneg2 ///
		i.estilo#i.famneg2 ///
		c.avance#i.famneg2 ///
		i.sexo#i.ecopos2 c.edad#i.ecopos2 /// 
		i.rela1#i.ecopos2 i.rela2#i.ecopos2 i.rela3#i.ecopos2 i.rela4#i.ecopos2 ///
		c.revcalidad#i.ecopos2 c.ingresos#i.ecopos2 c.salud#i.ecopos2 ///
		i.estilo#i.ecopos2 ///
		c.avance#i.ecopos2 ///
		i.sexo#i.econeg2 c.edad#i.econeg2 /// 
		i.rela1#i.econeg2 i.rela2#i.econeg2 i.rela3#i.econeg2 i.rela4#i.econeg2 ///
		c.revcalidad#i.econeg2 c.ingresos#i.econeg2 c.salud#i.econeg2 ///
		i.estilo#i.econeg2 ///
		c.avance#i.econeg2 ///
		i.sexo#i.helneg2 c.edad#i.helneg2 /// 
		i.rela1#i.helneg2 i.rela2#i.helneg2 i.rela3#i.helneg2 i.rela4#i.helneg2 ///
		c.revcalidad#i.helneg2 c.ingresos#i.helneg2 c.salud#i.helneg2 ///
		i.estilo#i.helneg2 ///
		c.avance#i.helneg2 ///
		|| FOLIO: calidad || FOLIO: salud || FOLIO: ingresos || FOLIO: R.estilo, variance  
	estimate store inter
outreg2 using "${output}multivel_R&R", dec(3)  label excel ct(Model E1) alpha(0.001, 0.01, 0.05) append

margins, dydx(salud) over(helneg2 ) atmeans vsquish 
marginsplot, xtitle(" ") title("Health Event"" ",position(11)) ytitle("Effect of health dimension") scheme(s2mono) name("healthXhealth", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(-.6 0)) ylabel(0 "0" -0.2 "-0.2" -0.4 "-0.4" -0.6 "-0.6") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\healthXhealth.png", as(png) replace

margins, dydx(revcalidad) over(econeg2) atmeans vsquish 
marginsplot, xtitle(" ") title("Economic Negative Event"" ",position(11)) ytitle("Effect of family dimension") scheme(s2mono) name("econegXfamily", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(-.3 0)) ylabel(0 "0" -0.1 "-0.1" -0.2 "-0.2" -0.3 "-0.3") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\econegXfamily.png", as(png) replace	

margins, dydx(revcalidad) over(famneg2) atmeans vsquish 
marginsplot, xtitle(" ") title("Familiar Negative Event"" ",position(11)) ytitle("Effect of family dimension") scheme(s2mono) name("famnegXfamily", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(-.3 0)) ylabel(0 "0" -0.1 "-0.1" -0.2 "-0.2" -0.3 "-0.3") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\famnegXfamily.png", as(png) replace	

margins, dydx(salud) over(famneg2) atmeans vsquish 
marginsplot, xtitle(" ") title("Familiar Negative Event"" ",position(11)) ytitle("Effect of health dimension") scheme(s2mono) name("famnegXsalud", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(-.6 0)) ylabel(0 "0" -0.2 "-0.2" -0.4 "-0.4" -0.6 "-0.6") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\famnegXsalud.png", as(png) replace

	
margins, dydx(ingresos) over(famipo2) atmeans vsquish 
marginsplot, xtitle(" ") title("Familiar Positive Event"" ",position(11)) ytitle("Effect of income dimension") scheme(s2mono) name("famposXsalud", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(0 .3)) ylabel(0 "0" .1 "0.1" .2 "0.2" 0.3 "0.3") graphregion(margin(6 8 8 8)) ///
			xlabel(0 "No" 1 "Yes") 
graph export "${output}\famposXsalud.png", as(png) replace

		
margins estilo, over(famipo2) atmeans vsquish 
marginsplot, xtitle(" ") title("Familiar Positive Event"" ",position(11)) ytitle("LP of style dimension") scheme(s2mono) name("famposXLs", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(4 6)) ylabel(4 "4.0" 4.5 "4.5" 5 "5.0" 5.5 "5.5" 6 "6.0") graphregion(margin(6 8 8 8)) ///
			legend(order(3 4) label(3 "No") label(4 "Yes")) // 3 and 4 to refer to lines and not ci
			
graph export "${output}\famposXLs.png", as(png) replace


*************Models with dummy events


foreach var of varlist positive2 negative2{
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.positive2 i.negative2 ///
		i.sexo#i.`var' c.edad#i.`var' /// 
		i.rela1#i.`var' i.rela2#i.`var' i.rela3#i.`var' i.rela4#i.`var' ///
		c.calidad#i.`var' c.ingresos#i.`var' c.salud#i.`var' ///
		i.estilo2#i.`var' i.estilo3#i.`var' i.estilo4#i.`var' ///
		c.avance#i.`var' ///
		|| FOLIO:, pweight(w`var') pwscale(size) variance  covariance(unstructured)
mltrsq
sleep 1000
outreg2 using "${output}multivel_dummies", dec(3) label excel ct(Model `var') alpha(0.001, 0.01, 0.05) append
}

******Models for the paper 


xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance if sample==1 ///
		|| FOLIO:, variance  covariance(unstructured)
outreg2 using "${output}multivel_draft", dec(3) label excel ct(B) alpha(0.001, 0.01, 0.05) addtext(Individual FE, NO) replace

xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		lnpositive lnnegative c.swlsm c.happy if sample==1 ///
		|| FOLIO:, variance  covariance(unstructured)
outreg2 using "${output}multivel_draft", dec(3) label excel ct(B) alpha(0.001, 0.01, 0.05) drop(i.FOLIO) addtext(Individual FE, NO) append

margins, at(swlsm=(1 (1) 7))
set matsize 3000
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.FOLIO if sample==1 || FOLIO:, variance  
outreg2 using "${output}multivel_draft", dec(3) label excel ct(B) alpha(0.001, 0.01, 0.05) drop(i.FOLIO) addtext(Individual FE, YES) append


*Robustness check 

recode P2 (99=.)

xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		lnpositive lnnegative c.P2 c.happy if sample==1 ///
		|| FOLIO:, variance  covariance(unstructured)

*Decomposition

xi: rego P7_V ///
        i.sexo edad calidad ingresos salud avance (detail) \ i.rela1 i.rela2 i.rela3 i.rela4 (detail) \ i.estilo2 i.estilo3 i.estilo4 (detail)
		
		

xi: rego P7_V ///
        i.sexo edad calidad ingresos salud avance (detail) \ i.rela1 i.rela2 i.rela3 i.rela4 (detail) \ i.estilo2 i.estilo3 i.estilo4 (detail) if lnpositive>0
		
	

foreach var of varlist lnpositive lnnegative{
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		c.lnpositive c.lnnegative ///
		i.sexo#c.`var' c.edad#c.`var' /// 
		i.rela1#c.`var' i.rela2#c.`var' i.rela3#c.`var' i.rela4#c.`var' ///
		c.calidad#c.`var' c.ingresos#c.`var' c.salud#c.`var' ///
		i.estilo2#c.`var' i.estilo3#c.`var' i.estilo4#c.`var' ///
		c.avance#c.`var' ///
		|| FOLIO:, pweight(w`var') pwscale(size) variance  covariance(unstructured)
mltrsq
sleep 1000
outreg2 using "${output}multivel_dummies", dec(3) label excel ct(Model `var') alpha(0.001, 0.01, 0.05) append
}


foreach var of varlist lnpositive lnnegative{
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		c.lnpositive c.lnnegative ///
		i.sexo#c.`var' c.edad#c.`var' /// 
		i.rela1#c.`var' i.rela2#c.`var' i.rela3#c.`var' i.rela4#c.`var' ///
		c.calidad#c.`var' c.ingresos#c.`var' c.salud#c.`var' ///
		i.estilo2#c.`var' i.estilo3#c.`var' i.estilo4#c.`var' ///
		c.avance#c.`var' ///
		c.Diener_escala c.happy ///
		|| FOLIO:, pweight(w`var') pwscale(size) variance  covariance(unstructured)
mltrsq
sleep 1000
outreg2 using "${output}multivel_dummies", dec(3) label excel ct(Model `var') alpha(0.001, 0.01, 0.05) append
}


mixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		c.lnpositive c.lnnegative ///
		i.sexo#c.lnpositive c.edad#c.lnpositive /// 
		i.rela1#c.lnpositive i.rela2#c.lnpositive i.rela3#c.lnpositive i.rela4#c.lnpositive ///
		c.calidad#c.lnpositive c.ingresos#c.lnpositive c.salud#c.lnpositive ///
		i.estilo2#c.lnpositive i.estilo3#c.lnpositive i.estilo4#c.lnpositive ///
		c.avance#c.lnpositive ///
		c.Diener_escala i.happy if sample==1 ///
		|| FOLIO:, pweight(wlnpositive) pwscale(size) variance  covariance(unstructured)
outreg2 using "${output}multivel_inter1", dec(3) label excel ct(B) alpha(0.001, 0.01, 0.05) replace


margins, dydx(calidad) at(lnpositive = (0(.5)2.5)) 
marginsplot, ytitle("Effect on linear prediction") title("Positive events # Family"" ",position(11)) xtitle("Positive events (ln)") scheme(s2mono) name("posXfamily", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(0 .4)) ylabel(0 "0" 0.1 "0.1" 0.2 "0.2" 0.3 "0.3" 0.4 "0.4") graphregion(margin(6 8 8 8)) 
graph export "${output}\posXfamily.png", as(png) replace	

margins estilo4, at(lnpositive = (0(.5)2.5)) 
marginsplot, xtitle("Positive events (ln)") title("Positive events # Stressed"" ",position(11)) ytitle("Linear prediction") scheme(s2mono) name("posXLs4", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(4 6)) ylabel(4 "4.0" 4.5 "4.5" 5 "5.0" 5.5 "5.5" 6 "6.0") graphregion(margin(6 8 8 8)) ///
			legend(order(3 4) label(3 "No") label(4 "Yes")) // 3 and 4 to refer to lines and not ci	
graph export "${output}\posXLs4.png", as(png) replace

xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		c.lnpositive c.lnnegative ///
		i.sexo#c.lnnegative c.edad#c.lnnegative /// 
		i.rela1#c.lnnegative i.rela2#c.lnnegative i.rela3#c.lnnegative i.rela4#c.lnnegative ///
		c.calidad#c.lnnegative c.ingresos#c.lnnegative c.salud#c.lnnegative ///
		i.estilo2#c.lnnegative i.estilo3#c.lnnegative i.estilo4#c.lnnegative ///
		c.avance#c.lnnegative ///
		c.Diener_escala i.happy if sample==1  ///
		|| FOLIO:, pweight(wlnnegative) pwscale(size) variance  covariance(unstructured)
outreg2 using "${output}multivel_inter1", dec(3) label excel ct(B) alpha(0.001, 0.01, 0.05) append

		
margins, dydx(calidad) at(lnnegative = (0(.5)2.5)) 
marginsplot, ytitle("Effect on linear prediction") title("Negative events # Family"" ",position(11)) xtitle("Negative events (ln)") scheme(s2mono) name("negXfamily", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(0 .6)) ylabel(0 "0" 0.2 "0.2" 0.4 "0.4" 0.6 "0.6") graphregion(margin(6 8 8 8)) 
graph export "${output}\negXfamily.png", as(png) replace	

margins, dydx(ingresos) at(lnnegative = (0(.5)2.5)) 
marginsplot, ytitle("Effect on linear prediction") title("Negative events # Income"" ",position(11)) xtitle("Negative events (ln)") scheme(s2mono) name("negXfamily", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(0 .6)) ylabel(0 "0" 0.2 "0.2" 0.4 "0.4" 0.6 "0.6")  graphregion(margin(6 8 8 8)) 
graph export "${output}\negXinc.png", as(png) replace

margins, dydx(salud) at(lnnegative = (0(.5)2.5)) 
marginsplot, ytitle("Effect on linear prediction") title("Negative events # Health"" ",position(11)) xtitle("Negative events (ln)") scheme(s2mono) name("negXfamily", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(0 .6)) ylabel(0 "0" 0.2 "0.2" 0.4 "0.4" 0.6 "0.6")  graphregion(margin(6 8 8 8)) 
graph export "${output}\negXhealth.png", as(png) replace



*Check of SW moderation


mixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		c.lnpositive c.lnnegative ///
		i.sexo#c.lnpositive c.edad#c.lnpositive /// 
		i.rela1#c.lnpositive i.rela2#c.lnpositive i.rela3#c.lnpositive i.rela4#c.lnpositive ///
		c.calidad#c.lnpositive c.ingresos#c.lnpositive c.salud#c.lnpositive ///
		i.estilo2#c.lnpositive i.estilo3#c.lnpositive i.estilo4#c.lnpositive ///
		c.avance#c.lnpositive ///
		if sample==1 ///
		|| FOLIO:, pweight(wlnpositive) pwscale(size) variance  covariance(unstructured)


*Dimemsions 


xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.ecopos2 i.famipo2 c.lnnegative ///
		i.sexo#i.famipo2 c.edad#i.famipo2 /// 
		i.rela1#i.famipo2 i.rela2#i.famipo2 i.rela3#i.famipo2 i.rela4#i.famipo2 ///
		c.calidad#i.famipo2 c.ingresos#i.famipo2 c.salud#i.famipo2 ///
		i.estilo2#i.famipo2 i.estilo3#i.famipo2 i.estilo4#i.famipo2 ///
		c.avance#i.famipo2 ///
		c.Diener_escala i.happy ///
		|| FOLIO:, variance  covariance(unstructured)

xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 c.calidad c.ingresos c.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		i.ecopos2 i.famipo2 c.lnnegative ///
		i.sexo#i.ecopos2 c.edad#i.ecopos2 /// 
		i.rela1#i.ecopos2 i.rela2#i.ecopos2 i.rela3#i.ecopos2 i.rela4#i.ecopos2 ///
		c.calidad#i.ecopos2 c.ingresos#i.ecopos2 c.salud#i.ecopos2 ///
		i.estilo2#i.ecopos2 i.estilo3#i.ecopos2 i.estilo4#i.ecopos2 ///
		c.avance#i.ecopos2 ///
		c.Diener_escala i.happy ///
		|| FOLIO:, variance  covariance(unstructured)		
		
		
		foreach var of varlist P9_A-P9_I{
recode `var' (8/9=.)
}

alpha P9_A-P9_I, gen(family)

egen family5 = xtile(family), nq(5)
egen family3 = xtile(family), nq(3)
egen family2 = xtile(family), nq(2)

mixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad i.ingresos i.salud i.estilo2 i.estilo3 i.estilo4 c.avance ///
		c.lnpositive c.lnnegative ///
		i.sexo#c.lnpositive c.edad#c.lnpositive /// 
		i.rela1#c.lnpositive i.rela2#c.lnpositive i.rela3#c.lnpositive i.rela4#c.lnpositive ///
		i.calidad#c.lnpositive i.ingresos#c.lnpositive i.salud#c.lnpositive ///
		i.estilo2#c.lnpositive i.estilo3#c.lnpositive i.estilo4#c.lnpositive ///
		c.avance#c.lnpositive ///
		c.Diener_escala i.happy ///
		if sample==1 ///
		|| FOLIO:, variance  covariance(unstructured)

		

		
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad i.ingresos i.salud2 i.estilo2 i.estilo3 i.estilo4 c.avance ///
		c.lnpositive c.lnnegative ///
		i.sexo#c.lnnegative c.edad#c.lnnegative /// 
		i.rela1#c.lnnegative i.rela2#c.lnnegative i.rela3#c.lnnegative i.rela4#c.lnnegative ///
		i.calidad#c.lnnegative i.ingresos#c.lnnegative i.salud2#c.lnnegative ///
		i.estilo2#c.lnnegative i.estilo3#c.lnnegative i.estilo4#c.lnnegative ///
		c.avance#c.lnnegative ///
		c.Diener_escala i.happy ///	
		if sample==1  ///
		|| FOLIO:, variance  covariance(unstructured)
		
		
