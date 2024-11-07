*  ██████   ██████  ██    ██      ██ ██████   ██████   █████  
* ██       ██    ██ ██    ██     ███      ██ ██       ██   ██ 
* ██   ███ ██    ██ ██    ██      ██  █████  ███████   █████  
* ██    ██ ██    ██  ██  ██       ██      ██ ██    ██ ██   ██ 
*  ██████   ██████    ████        ██ ██████   ██████   █████  
*
* Title:		Regression Discontinuity (RD)
* Author:		Mohit Karnani (mkarnani@hks.harvard.edu)
* Created:		October 30, 2024

******************************************************************
* Using Maimonides' Rule to Estimate the Effect of Class Size... *
******************************************************************

* First, let's run this file to compute better standard errors:
do "mmoulton_post.do"

* Let's load the data for 5th graders to replicate Table 4 in the paper:
use final5, clear

* Data cleaning:
replace avgverb= avgverb-100 if avgverb>100
replace avgmath= avgmath-100 if avgmath>100

replace avgverb=. if verbsize==0
replace passverb=. if verbsize==0

replace avgmath=. if mathsize==0
replace passmath=. if mathsize==0

keep if 1<classize & classize<45 & c_size>5
keep if c_leom==1 & c_pik<3
keep if avgverb~=.

g byte disc= (c_size>=36 & c_size<=45) | (c_size>=76 & c_size<=85) | ///
	(c_size>=116 & c_size<=125)

g byte all=1
g c_size2= (c_size^2)/100


* Maimonides' class-size prediction as a function of cohort size (c_size): 
g func1= c_size/(int((c_size-1)/40)+1)

* Replicating(-ish) Figure 1:
tw (sc func1 c_size) (lpoly classize c_size if inrange(c_size,1,40)) ///
(lpoly classize c_size if inrange(c_size,41,80)) ///
(lpoly classize c_size if inrange(c_size,81,120)) ///
(lpoly classize c_size if inrange(c_size,121,160)) ///
(lpoly classize c_size if inrange(c_size,161,200)) ///
(lpoly classize c_size if inrange(c_size,201,240)), legend(off) ///
xline(40 80 120 160 200) yline(40 20.5 27 30.25 32.2 33.5)

* (In case you want to get the "real" figure, run this block all at once)
preserve
	collapse c_size classize, by(schlcode)
	gen func1_c_size= c_size/(int((c_size-1)/40)+1)
	collapse classize func1_c_size, by(c_size)
	sort c_size
	tw (line func1_c_size c_size) (line classize c_size), legend(off)
restore

* Results (Table 4):
mmoulton avgverb (classize=func1) tipuach c_size c_size2, clu(schlcode) 2sls

* Generate trend:
g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160

foreach dvar in avgverb avgmath {

di " "
di "OUTCOME IS `dvar'"
di "FULL SAMPLE"
di " "
mmoulton `dvar' (classize=func1) tipu, clu(schlcode) 2sls
mmoulton `dvar' (classize=func1) tipu c_size, clu(schlcode) 2sls
mmoulton `dvar' (classize=func1) tipu c_size c_size2, clu(schlcode) 2sls
mmoulton `dvar' (classize=func1) trend, clu(schlcode) 2sls

di " "
di "OUTCOME IS `dvar'"
di "DISCONTINUITY SAMPLE"
di " "
mmoulton `dvar' (classize=func1) tipu if disc==1, clu(schlcode) 2sls
mmoulton `dvar' (classize=func1) tipu c_size if disc==1, clu(schlcode) 2sls

}

* But wait! We haven't tested for manipulation around the cutoff...
hist c_size if inrange(c_size,1,80), discrete percent xline(40) xtitle("Enrollment")

* This doesn't look good; we can confirm our suspicions with fancy packages:
ssc install rddensity
rddensity c_size, c(40)

net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace
rddensity c_size, c(40) plot

* 20 years later... *

**************************
* Maimonides' Rule Redux *
**************************


******************
*** Fifth Grade

** Load data
use "final5.dta", clear

** Original Cleaning
replace avgverb= avgverb-100 if avgverb>100
replace avgmath= avgmath-100 if avgmath>100

g func1= c_size/(int((c_size-1)/40)+1)

replace avgverb=. if verbsize==0
replace passverb=. if verbsize==0

replace avgmath=. if mathsize==0
replace passmath=. if mathsize==0

keep if 1<classize & classize<45 & c_size>5
keep if c_leom==1 & c_pik<3
keep if avgverb~=.

g c_size2= (c_size^2)/100

g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160

**** define donuts
g byte donut1 = c_size< 38 | c_size> 42
g byte donut2 = c_size< 37 | c_size> 43
g byte donut3 = c_size< 39 | c_size> 41

*********************************************
****** Regressions
*********************************************

*** Donuts on Linear and Quadratic

** Donut: )39, 41(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut3==1, cluster(schlcode) 
est sto A5
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut3==1, cluster(schlcode) 
est sto B5
* Math
ivreg avgmath (classize=func1) tipu c_size if donut3==1, cluster(schlcode) 
est sto C5
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut3==1, cluster(schlcode) 
est sto D5


esttab A5 B5 C5 D5, nocon se stats(N), using "TableA6.tex", replace
est clear 

** Donut: )38,42(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut1==1, cluster(schlcode) 
est sto E5
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut1==1, cluster(schlcode) 
est sto F5

* Math
ivreg avgmath (classize=func1) tipu c_size if donut1==1, cluster(schlcode) 
est sto G5
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut1==1, cluster(schlcode) 
est sto H5

esttab E5 F5 G5 H5, nocon se stats(N), using "TableA6.tex", append  
est clear 
***** 

** Donut:  )37, 43(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut2==1, cluster(schlcode) 
est sto I5
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut2==1, cluster(schlcode) 
est sto J5
* Math
ivreg avgmath (classize=func1) tipu c_size if donut2==1, cluster(schlcode) 
est sto K5
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut2 == 1, cluster(schlcode) 
est sto L5
**
esttab I5 J5 K5 L5, nocon se stats(N), using "TableA6.tex", append  
est clear 

**
*****************************************************************************************
*****************************************************************************************
*****************************************************************************************
*****************************************************************************************

** Fourth graders, same 

use "final4.dta", clear


replace avgverb= avgverb-100 if avgverb>100
replace avgmath= avgmath-100 if avgmath>100

g func1= c_size/(int((c_size-1)/40)+1)

replace avgverb=. if verbsize==0
replace passverb=. if verbsize==0

replace avgmath=. if mathsize==0
replace passmath=. if mathsize==0

keep if 1<classize & classize<45 & c_size>5
keep if c_leom==1 & c_pik<3
keep if avgverb~=.
g byte all=1
g c_size2= (c_size^2)/100

g trend= c_size if c_size>=0 & c_size<=40
	replace trend= 20+(c_size/2) if c_size>=41 & c_size<=80
	replace trend= (100/3)+(c_size/3) if c_size>=81 & c_size<=120
	replace trend= (130/3)+(c_size/4) if c_size>=121 & c_size<=160


**** define donuts
g byte donut1 = c_size< 38 | c_size> 42
g byte donut2 = c_size< 37 | c_size> 43
g byte donut3 = c_size< 39 | c_size> 41

*********************************************
****** Regressions
*********************************************

*** Donuts on Linear and Quadratic

** Donut: )39, 41(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut3==1, cluster(schlcode) 
est sto A4
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut3==1, cluster(schlcode) 
est sto B4
* Math
ivreg avgmath (classize=func1) tipu c_size if donut3==1, cluster(schlcode) 
est sto C4
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut3==1, cluster(schlcode) 
est sto D4


esttab A4 B4 C4 D4, nocon se stats(N), using "TableA6.tex", append
est clear 

** Donut: )38,42(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut1==1, cluster(schlcode) 
est sto E4
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut1==1, cluster(schlcode) 
est sto F4

* Math
ivreg avgmath (classize=func1) tipu c_size if donut1==1, cluster(schlcode) 
est sto G4
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut1==1, cluster(schlcode) 
est sto H4

esttab E4 F4 G4 H4, nocon se stats(N),  using "TableA6.tex", append
est clear 
***** 

** Donut:  )37, 43(
* Reading
ivreg avgverb (classize=func1) tipu c_size if donut2==1, cluster(schlcode) 
est sto I4
ivreg avgverb (classize=func1) tipu c_size c_size2 if donut2==1, cluster(schlcode) 
est sto J4
* Math
ivreg avgmath (classize=func1) tipu c_size if donut2==1, cluster(schlcode) 
est sto K4 
ivreg avgmath (classize=func1) tipu c_size c_size2 if donut2 == 1, cluster(schlcode) 
est sto L4
**
esttab I4 J4 K4 L4, nocon se stats(N), using "TableA6.tex", append
est clear 
