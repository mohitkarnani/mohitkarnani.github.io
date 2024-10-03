*  ██████   ██████  ██    ██      ██ ██████   ██████   █████  
* ██       ██    ██ ██    ██     ███      ██ ██       ██   ██ 
* ██   ███ ██    ██ ██    ██      ██  █████  ███████   █████  
* ██    ██ ██    ██  ██  ██       ██      ██ ██    ██ ██   ██ 
*  ██████   ██████    ████        ██ ██████   ██████   █████  
*
* Title:		Difference-in-Differences (DiD)
* Author:		Mohit Karnani (mkarnani@hks.harvard.edu)
* Created:		October 2, 2024

clear all
set obs 2000
gen treat = _n<=1000
gen post = mod(_n,2)
gen y = 1 + 2*post*treat + 3*post + 4*treat + rnormal()
reg y treat##post
replace post = post + rnormal()/10
tw (sc y post if treat) (sc y post if !treat) (lfit y post if treat) (lfit y post if !treat)
gen counterfactual = y
replace counterfactual = y-2 if treat & post>0.5
tw (sc y post if treat) (sc y post if !treat) (lfit y post if treat) (lfit y post if !treat) (sc counterfactual post if treat) (lfit counterfactual post if treat)

***********************************************************
* The impact of a large Chilean earthquake on test scores *
***********************************************************

*Let's load and clean some data from Chile
use "scores2011.dta", clear
keep idalumno rbd ptje_lect8b_alu //keep student, school and score
merge m:1 rbd using "schools2011.dta", keep(1 3) keepus(cod_reg_rbd cod_depe2 cod_rural_rbd) nogen //get school location and scharacteristics 
merge 1:1 idalumno using "scores2007.dta", keep(3) keepus(ptje_lect4b_alu) nogen //get pre-period scores
rename (ptje_lect4b_alu ptje_lect8b_alu cod_rural_rbd cod_depe2 idalumno) (score0 score1 rural school_type id) //rename stuff
foreach v of var * { 
	drop if mi(`v') //drop stuff with missing values
}
gen treatment_group=inlist(cod_reg_rbd,9,13,5,6,7,8) //affected regions
drop cod_reg_rbd rbd //drop school characteristics we don't need
reshape long score, i(id) j(post_period) //change data "shape"

*Cross-tabulating the scores in a 2x2 table
tab treatment_group post_period, s(score) //this is all we need to conduct DiD

*There is a regression that directly computes a DiD estimator:
reg score treatment_group##post_period

*If you don't like the ## notation, you can build your own interaction:
gen treatment_post = treatment_group * post_period
reg score treatment_post treatment_group post_period

*What if we study urban and rural schools separately?
reg score treatment_group##post_period if rural==1 //urban schools
reg score treatment_group##post_period if rural==2 //rural schools

*How about private vs public schools?
reg score treatment_group##post_period if school_type==3 //private schools
reg score treatment_group##post_period if school_type!=3 //public schools

***************************************************************
* Replicating Table 2 of Lovenheim and Willén (2019, AEJ:Pol) *
***************************************************************

	* 1. Initiating gender loop (all results are estimated seperately by gender)
	foreach samp in MALE FEMALE {
	clear all
	use "Estimation_`samp'"
			
	* 2. Control variables used in the different specifications. Controls1 represent the controls used in columns i and iv. Controls2 represent the controls used in columns ii and v. Controls3 represent the controls used in columns iii and vi.
		local controls1 "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE"
		local controls2 "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE"
		local controls3 "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR"
	
	* 3. Labelling the treatment variables of interest for ease of interpretation in the output files
		label var rtESV119 "FiveYearEffect"
		label var rtESV124 "TenYearEffect"
		label var rtESV129 "FifteenYearEffect"
		label var totinc "Earnings"
		label var hoursw2 "Hours Worked" 
		label var emp "Employed"
		label var unemp "Unemployed"
		label var notlabforce "Not in Labor Force"
		label var yearseduc "Years of Education"
		
	* 4. Running the regressions and outputting the results 
		
			* i. Table 2
			foreach th in totinc hoursw2 {
			sum `th' [aw=nobs]
			local m = r(mean)
			quietly xi: reg `th' `controls1' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A=""
			local B=""
			outreg2 using "Table2.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls2' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B=""
			outreg2 using "Table2.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls3' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B="x"
			outreg2 using "Table2.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'", "BC-SY FE", "`B'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			}			
			
			}
			

/*
*********************************************************************
* The Long-run Effects of Teacher Collective Bargaining			 	*
* Last update: September 2018										*
* Authors: Michael Lovenheim and Alexander Willén					*
*********************************************************************
* To run this code, adjust path_base to the folder where the data files are stored, and the output path to the folder where you would like the results to be saved. 

loc path_base "/Users/mohitkarnani/Dropbox (MIT)/f2024/GOV1368/s04/116525-V1/Data-files"
global output  "/Users/mohitkarnani/Dropbox (MIT)/f2024/GOV1368/s04/116525-V1"

ssc install outreg2

**********************
* TABLES 2 THROUGH 4 * 
**********************
	
	* 1. Initiating gender loop (all results are estimated seperately by gender)
	foreach samp in MALE FEMALE {
	clear all
	use "`path_base'/Estimation-samples/Estimation_`samp'"
			
	* 2. Control variables used in the different specifications. Controls1 represent the controls used in columns i and iv. Controls2 represent the controls used in columns ii and v. Controls3 represent the controls used in columns iii and vi.
		local controls1 "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE"
		local controls2 "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE"
		local controls3 "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR"
	
	* 3. Labelling the treatment variables of interest for ease of interpretation in the output files
		label var rtESV119 "FiveYearEffect"
		label var rtESV124 "TenYearEffect"
		label var rtESV129 "FifteenYearEffect"
		label var totinc "Earnings"
		label var hoursw2 "Hours Worked" 
		label var emp "Employed"
		label var unemp "Unemployed"
		label var notlabforce "Not in Labor Force"
		label var yearseduc "Years of Education"
		
	* 4. Running the regressions and outputting the results 
		
			* i. Table 2
			foreach th in totinc hoursw2 {
			sum `th' [aw=nobs]
			local m = r(mean)
			quietly xi: reg `th' `controls1' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A=""
			local B=""
			outreg2 using "$output/Table2.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls2' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B=""
			outreg2 using "$output/Table2.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls3' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B="x"
			outreg2 using "$output/Table2.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'", "BC-SY FE", "`B'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			}			
			
			* ii. Table 3
			foreach th in emp unemp notlabforce {
			sum `th' [aw=nobs]
			local m = r(mean)
			quietly xi: reg `th' `controls1' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A=""
			local B=""
			outreg2 using "$output/Table3.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls2' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B=""
			outreg2 using "$output/Table3.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls3' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B="x"
			outreg2 using "$output/Table3.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'", "BC-SY FE", "`B'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			}	
			
			*iii. Table 4
			foreach th in yearseduc {
			sum `th' [aw=nobs]
			local m = r(mean)
			quietly xi: reg `th' `controls1' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A=""
			local B=""
			outreg2 using "$output/Table4.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls2' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B=""
			outreg2 using "$output/Table4.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls3' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B="x"
			outreg2 using "$output/Table4.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'", "BC-SY FE", "`B'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			}	
			}
			
	* The occupational skill estimates in Table 4 are obtained from individual-level data: 

	* [1] Obtaining sample
		foreach samp in MALE FEMALE {
		clear all
		use "`path_base'/Estimation-samples/PERCENTMOSTHS", clear
		gen MALE=0
		replace MALE=1 if male==1
		gen FEMALE=0
		replace FEMALE=1 if male==0
		keep if `samp'==1
		estimates clear
			
	* 3. Control variables used in the different specifications. Controls1 represent the controls used in columns i and iv. Controls2 represent the controls used in columns ii and v. Controls3 represent the controls used in columns iii and vi.
		local controls1 "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE"
		local controls2 "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE"
		local controls3 "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR"
	
	* 4. Labelling the treatment variables of interest for ease of interpretation in the output files
		label var rtESV119 "FiveYearEffect"
		label var rtESV124 "TenYearEffect"
		label var rtESV129 "FifteenYearEffect"
		label var rankpreplow "Occupational Skill" 

	* 5. Running the regressions and outputting the results 		
			foreach th in rankpreplow {
			sum `th' [aw=nobs]
			local m = abs(r(mean))
			quietly xi: reg `th' `controls1' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A=""
			local B=""
			outreg2 using "$output/Table4.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls2' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B=""
			outreg2 using "$output/Table4.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			
			quietly xi: reg `th' `controls3' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			local A="x"
			local B="x"
			outreg2 using "$output/Table4.xls", append  addtext("Percent effect", "`percent'","Sample", "`samp'", "Other Policy Controls", "`A'", "BC-SY FE", "`B'") keep(rtESV119 rtESV124 rtESV129) dec(3) nocons nor2 label
			}	
			}	

***********
* TABLE 5 * 
***********

	* 1. Selecting estimation sample 
	foreach samp in MALE_BHO MALE_WA FEMALE_BHO FEMALE_WA  {
	clear all
	use "`path_base'/Estimation-samples/Estimation_`samp'"

	* 2. Local for the control variables we use in the regressions
	local controls "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR"
	
	* 3. Labelling the treatment variable of interest for ease of interpretation in the output files
		label var rtESV124 "TenYearEffect"
		label var totinc "Earnings"
		label var hoursw2 "Hours Worked" 
		label var emp "Employed"
		label var unemp "Unemployed"
		label var notlabforce "Not in Labor Force"
		label var yearseduc "Years of Education"
		
	* 4. Running the regressions and outputting the results 
			foreach th in totinc hoursw2 emp unemp notlabforce yearseduc{
			sum `th' [aw=nobs]
			local m = r(mean)
			quietly xi: reg `th' `controls' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			outreg2 using "$output/Table5_`samp'.xls", append  addtext("Percent effect", "`percent'") keep(rtESV124) dec(3) nocons nor2 label noobs
			}			
	}	
	
	* The occupational skill estimates are obtained from individual-level data: 

	* 1. Obtaining sample
	foreach samp in MALE_BHO MALE_WA FEMALE_BHO FEMALE_WA {
	clear all 
	use "`path_base'/Estimation-samples/PERCENTMOSTHS", clear
	gen MALE_BHO=0
	replace MALE_BHO=1 if male==1 & RACE==1
	gen MALE_WA=0
	replace MALE_WA=1 if male==1 & RACE==0
	gen FEMALE_BHO=0
	replace FEMALE_BHO=1 if male==0 & RACE==1
	gen FEMALE_WA=0
	replace FEMALE_WA=1 if male==0 & RACE==0
	keep if `samp'==1

	* 2. Local for the control variables we use in the regressions
	local controls "rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR"
	
	* 3. Labelling the treatment variable of interest for ease of interpretation in the output files
	label var rtESV124 "TenYearEffect"
	label var rankpreplow "Occupational Skill" 
		
	* 4. Running the regressions and outputting the results 
			sum rankpreplow [aw=nobs]
			local m = abs(r(mean))
			quietly xi: reg rankpreplow `controls' [aw=nobs], cluster(BIRTHSTATE) 
			local coef10y = _b[rtESV124]
			local percent  = round(`coef10y'/`m' *100,0.01)
			outreg2 using "$output/Table5_`samp'.xls", append  addtext("Percent effect", "`percent'") keep(rtESV124) dec(3) nocons nor2 label noobs
			}			

************
* TABLE 6 * 
***********

	* [1] Selecting estimation sample
	foreach samp in MALE FEMALE {
	use "`path_base'/Estimation-samples/Estimation_`samp'", clear

	* [2] Generating relative time variable
	replace ESV1=0 if ESV1==. 			// These are states that did not implement union laws. Assigned zero as described in text. 

	* [3] Generating post dummy
	gen postr=(ESV1>-1)==1				// Any observation with relative time >-1 is assigned 1 for the post-policy dummy
	replace postr=0 if unionlaw1==. 	// States that did not implement any union law is assigned zero (the command above erroneously assign these states 1
	replace ESV1=ESV1+1					// For ease of interpretation, shifting one 

	* [4] Generating interaction
	gen inter=postr*ESV1

	* [5] Final adjustments 
	drop if ESV1==0					
	replace ESV1=-10 if ESV1<-9 // Note: This is similar to our baseline specification where we combine reltime<-10 (into a -11 dummy), but uses -10 instead because of adjustment 7 rows up
	replace ESV1=22 if ESV1>21	// Note: This is similar to our baseline specification where we combine reltime> 20 (into a  21 dummy), but uses +21 instead because of adjustment 7 rows up
	ren ESV1 RELTIME
	ren postr POST
	ren inter RELTIME_POST 

	* [5b] Labelling variable of interest for ease of interpretation in the output files
	label var RELTIME "Relative Years to DTB Law"
	label var POST "I(DTB Law)"
	label var RELTIME_POST "Relative Years to DTB Law*I(DTB Law)"
	label var totinc "Earnings"
	label var hoursw2 "Hours Worked" 
	label var emp "Employed"
	label var unemp "Unemployed"
	label var notlabforce "Not in Labor Force"
	label var yearseduc "Years of Education"
	
	* [6] Running specification
	global outcomes "totinc hoursw2 emp unemp notlabforce yearseduc"
	foreach dep of global outcomes {
	sum `dep' [aw=nobs]
	local m = r(mean)
	quietly xi: reg `dep' RELTIME POST RELTIME_POST yearsfcor yearsflr aveitc fscontrol asian black hispanic other i.BIRTHYEAR*i.PUS_SURVEY_YEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE [aw=nobs], cluster(BIRTHSTATE) 
	local percent  = round(((_b[POST]+(10*_b[RELTIME_POST]))/`m')*100,0.01)	
	estimates store `dep', title(Model `dep')
	outreg2 using "$output/Table6_`samp'.xls", append addtext("Percent effect", "`percent'") keep(RELTIME POST RELTIME_POST) dec(4) nocons nor2 label noobs
	}
	}
	* The occupational skill estimates are obtained from individual-level data: 

	* [1] Obtaining the data
	use "`path_base'/Estimation-samples/PERCENTMOSTHS", clear 
	gen MALE=0
	replace MALE=1 if male==1
	gen FEMALE=0
	replace FEMALE=1 if male==0

	* [2] Generating relative time variable
	replace ESV1=0 if ESV1==. 
	drop if PUS_SURVEY_YEAR==2000

	* [3] Generating post dummy
	gen postr=(ESV1>-1)==1
	replace postr=0 if unionlaw1==.
	replace ESV1=ESV1+1

	* [4] Interaction
	gen inter=postr*ESV1

	* [5] Final adjustments
	drop if ESV1==0
	replace ESV1=-11 if ESV1<-10
	replace ESV1=21 if ESV1>20
	ren ESV1 RELTIME
	ren postr POST
	ren inter RELTIME_POST 

	* [5b] Labelling variable of interest for ease of interpretation in the output files
	label var RELTIME "Relative Years to DTB Law"
	label var POST "I(DTB Law)"
	label var RELTIME_POST "Relative Years to DTB Law*I(DTB Law)"
	label var rankpreplow "Occupational Skill"
	
	* [6] Running specification
	global outcomes "rankpreplow"
	foreach samp in MALE FEMALE {
	preserve
	keep if `samp'==1
	foreach dep of global outcomes {
	sum `dep' [aw=nobs]
	pause
	local m = r(mean)
	quietly xi: reg `dep' RELTIME POST RELTIME_POST yearsfcor yearsflr aveitc fscontrol asian black hispanic other i.BIRTHYEAR*i.PUS_SURVEY_YEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE [aw=nobs], cluster(BIRTHSTATE) 
	local percent  = round(((_b[POST]+(10*_b[RELTIME_POST]))/`m')*100,0.01)	
	estimates store `dep', title(Model `dep')
	outreg2 using "$output/Table6_`samp'.xls", append addtext("Percent effect", "`percent'") keep(RELTIME POST RELTIME_POST) dec(4) nocons nor2 label noobs
	restore
	}
	}
***********
* TABLE 7 * 
***********

set maxvar 20000

	* A. RANDOMLY ASSIGNING PASSAGE DATES

		* 1. Running the regression
		foreach dep in totinc hoursw2 emp notlabforce yearseduc {
		use "`path_base'/Additional-dta-files/300Simulations_RandomAllocation", clear //
		
		forvalues i=1(1)300 {
		qui: gen COEF_EST_`i'=.
		di "-`dep'_`i'-*" _cont
		
		quietly xi: reg `dep' rtESV_`i'_13-rtESV_`i'_112 rtESV_`i'_114-rtESV_`i'_135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR  [aw=nobs], cluster(BIRTHSTATE)
		qui: replace COEF_EST_`i'=_b[rtESV_`i'_124] 
		}

		* 2. Reshaping the data
			* (1) Keeping only 1 row (all coef, se & tstat are the same for each observation
				duplicates drop COEF_EST_1, force				
			* (2) Keeping the variables of interest
				keep COEF_EST_* 
			* (3) Expanding the data set so that I have 300 rows (equal to the number of simulations)
				gen hold=300
				expand hold
			* (4) replacing values so that Coef 1, se 1 and tstat 1 = 3 columns with all values we are interested in
				gen obs=_n
				forvalues i=1(1)300 {
				replace COEF_EST_1=COEF_EST_`i' if obs==`i'
				}

		* 3. Calculating the p-values
			if "`dep'"=="totinc" {
			count if COEF_EST_1 <-2134.04
			gen TOTINC=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep TOTINC counter
			tempfile A
			save `A' 
			}
			else if "`dep'"=="hoursw2" {
			count if COEF_EST_1 <-0.424
			gen HOURS=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep HOURS counter
			tempfile B
			save `B'
			}
			else if "`dep'"=="emp" {
			count if COEF_EST_1  <-0.010
			gen EMPL=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep EMPL counter
			tempfile C
			save `C'
			}
			else if "`dep'"=="notlabforce" {
			count if COEF_EST_1  < 0.008
			gen NILF=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep NILF counter
			tempfile D
			save `D'
			}
			else if "`dep'"=="yearseduc" {
			count if COEF_EST_1  <-0.051
			gen YEARSEDUC=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep YEARSEDUC counter
			tempfile E
			save `E'
			}
			}
			
		* 4. Adding results for each of the outcomes
			use `A', clear
			merge 1:1 counter using `B'
			drop _merge
			merge 1:1 counter using `C'
			drop _merge
			merge 1:1 counter using `D'
			drop _merge
			merge 1:1 counter using `E'
			drop _merge
			drop counter
		
		* Renaming variables for ease of interpretation when viewing output table
			ren TOTINC Earnings
			ren HOURS Hours_Worked
			ren EMPL Employed
			ren NILF Not_in_Labor_Force
			ren YEARSEDUC Years_of_Education
			
		* Outputting the table in excel form 
			export excel "$output\Table7_PanelA", firstrow(var) replace

		* Occupational skill estimate is based on individual-level data 
	
			* i. Running the regressions
			use "`path_base'/Additional-dta-files/300Simulations_RandomAllocation_OccSort", clear
		
			forvalues i=1(1)300 {
			qui: gen COEF_EST_`i'=.
			di "-`dep'_`i'-*" _cont
		
			quietly xi: reg rankpreplow rtESV_`i'_13-rtESV_`i'_112 rtESV_`i'_114-rtESV_`i'_135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR  [aw=nobs], cluster(BIRTHSTATE)
			qui: replace COEF_EST_`i'=_b[rtESV_`i'_124] 
			drop rtESV_`i'_* UnionLaw_`i' ESV1_`i'
			}
			
			* ii. Reshaping the data
				* (1) Keeping only 1 row (all coef, se & tstat are the same for each observation
					gen holding=_n
					keep if holding==1				
				* (2) Keeping the variables of interest
					keep COEF_EST_*
				* (3) Expanding the data set so that I have 300 rows (equal to the number of simulations)
					gen hold=300
					expand hold
				* (4) replacing values so that Coef 1, se 1 and tstat 1 = 3 columns with all values we are interested in
					gen obs=_n
					forvalues i=1(1)300 {
					replace COEF_EST_1=COEF_EST_`i' if obs==`i'
					}
			* ii. Keeping only these variables
			count if COEF_EST_1 <-0.003
			gen OCCSKILL=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep OCCSKILL counter
			
			* Renaming variables for ease of interpretation when viewing output table
			ren OCCSKILL Occupational_Skill
			
			export excel "$output/Table7_PanelA_OccupationalSkill", firstrow(var) replace
			
	* B. KEEPING THE DISTRIBUTION OF DTB IMPLEMENTATION YEARS CONSTANT

		* 1. Running the regression
			foreach dep in totinc hoursw2 emp notlabforce yearseduc {
			use "`path_base'/Additional-dta-files/300Simulations_MatchDistribution", clear
			
			forvalues i=1(1)300 {
			qui: gen COEF_EST_`i'=.
			di "-`dep'_`i'-*" _cont
			
			quietly xi: reg `dep' rtESV_`i'_13-rtESV_`i'_112 rtESV_`i'_114-rtESV_`i'_135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR  [aw=nobs], cluster(BIRTHSTATE)
			qui: replace COEF_EST_`i'=_b[rtESV_`i'_124] 
			}

		* 2. Reshaping the data
			* (1) Keeping only 1 row (all coef, se & tstat are the same for each observation
				duplicates drop COEF_EST_1, force				
			* (2) Keeping the variable of interest
				keep COEF_EST_* 
			* (3) Expanding the data set so that I have 300 rows (equal to the number of simulations)
				gen hold=300
				expand hold
			* (4) replacing values so that Coef 1, se 1 and tstat 1 = 3 columns with all values we are interested in
				gen obs=_n
				forvalues i=1(1)300 {
				replace COEF_EST_1=COEF_EST_`i' if obs==`i'
				}

		* 3. Calculating the p-values
			if "`dep'"=="totinc" {
			count if COEF_EST_1 <-2134.04
			gen TOTINC=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep TOTINC counter
			tempfile A
			save `A' 
			}
			else if "`dep'"=="hoursw2" {
			count if COEF_EST_1 <-0.424
			gen HOURS=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep HOURS counter
			tempfile B
			save `B'
			}
			else if "`dep'"=="emp" {
			count if COEF_EST_1  <-0.010
			gen EMPL=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep EMPL counter
			tempfile C
			save `C'
			}
			else if "`dep'"=="notlabforce" {
			count if COEF_EST_1  < 0.008
			gen NILF=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep NILF counter
			tempfile D
			save `D'
			}
			else if "`dep'"=="yearseduc" {
			count if COEF_EST_1  <-0.051
			gen YEARSEDUC=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep YEARSEDUC counter
			tempfile E
			save `E'
			}
			}
			
		* 4. Adding results for each of the outcomes
			use `A', clear
			merge 1:1 counter using `B'
			drop _merge
			merge 1:1 counter using `C'
			drop _merge
			merge 1:1 counter using `D'
			drop _merge
			merge 1:1 counter using `E'
			drop _merge
			drop counter
		
		* Renaming variables for ease of interpretation when viewing output table
			ren TOTINC Earnings
			ren HOURS Hours_Worked
			ren EMPL Employed
			ren NILF Not_in_Labor_Force
			ren YEARSEDUC Years_of_Education
			
		* Outputting the table in excel form 
			export excel "$output/Table7_PanelB", firstrow(var) replace
		
		* Occupational skill estimate is based on individual-level data 
	
			* i. Running the regressions
			use "`path_base'/Additional-dta-files/300Simulations_MatchDistribution_OccSort", clear
		
			forvalues i=1(1)300 {
			qui: gen COEF_EST_`i'=.
			di "-`dep'_`i'-*" _cont
		
			quietly xi: reg rankpreplow rtESV_`i'_13-rtESV_`i'_112 rtESV_`i'_114-rtESV_`i'_135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR  [aw=nobs], cluster(BIRTHSTATE)
			qui: replace COEF_EST_`i'=_b[rtESV_`i'_124] 
			drop rtESV_`i'_* UnionLaw_`i' ESV1_`i'
			}
			
			* ii. Reshaping the data
				* (1) Keeping only 1 row (all coef, se & tstat are the same for each observation
					gen holding=_n
					keep if holding==1				
				* (2) Keeping the variable of interest
					keep COEF_EST_*
				* (3) Expanding the data set so that I have 300 rows (equal to the number of simulations)
					gen hold=300
					expand hold
				* (4) replacing values so that Coef 1, se 1 and tstat 1 = 3 columns with all values we are interested in
					gen obs=_n
					forvalues i=1(1)300 {
					replace COEF_EST_1=COEF_EST_`i' if obs==`i'
					}
			* ii. Keeping only these variables
			count if COEF_EST_1 <-0.003
			gen OCCSKILL=r(N)/300
			gen counter=_n
			drop if counter!=1
			keep OCCSKILL counter
			
			* Renaming variables for ease of interpretation when viewing output table
			ren OCCSKILL Occupational_Skill
			
			export excel "$output/Table7_PanelB_OccupationalSkill", firstrow(var) replace

			
***********************
* FIGURES 2 THROUGH 4 * 
***********************

	* [1] Path and preliminary commands
	pause off
	clear all

	* [2] Selecting sample
	foreach samp in MALE FEMALE {
	clear all
	use "`path_base'/Estimation-samples/Estimation_`samp'"
	
	* [3] Generating relative time (x-axis) 
	gen RelTim=.
	replace RelTim=-10 if rtESV14== 1
	replace RelTim=-9  if rtESV15== 1
	replace RelTim=-8  if rtESV16== 1
	replace RelTim=-7  if rtESV17== 1
	replace RelTim=-6  if rtESV18== 1
	replace RelTim=-5  if rtESV19== 1
	replace RelTim=-4  if rtESV110==1
	replace RelTim=-3  if rtESV111==1
	replace RelTim=-2  if rtESV112==1
	replace RelTim= 0  if rtESV114==1
	replace RelTim= 1  if rtESV115==1
	replace RelTim= 2  if rtESV116==1
	replace RelTim= 3  if rtESV117==1
	replace RelTim= 4  if rtESV118==1
	replace RelTim= 5  if rtESV119==1
	replace RelTim= 6  if rtESV120==1
	replace RelTim= 7  if rtESV121==1
	replace RelTim= 8  if rtESV122==1
	replace RelTim= 9  if rtESV123==1
	replace RelTim= 10 if rtESV124==1
	replace RelTim= 11 if rtESV125==1
	replace RelTim= 12 if rtESV126==1
	replace RelTim= 13 if rtESV127==1
	replace RelTim= 14 if rtESV128==1
	replace RelTim= 15 if rtESV129==1
	replace RelTim= 16 if rtESV130==1
	replace RelTim= 17 if rtESV131==1
	replace RelTim= 18 if rtESV132==1
	replace RelTim= 19 if rtESV133==1
	replace RelTim= 20 if rtESV134==1
	
		* i. All observations with reltime less than -10 are combined into one. See data section for further discussion
		forvalues i=1(1)3 {
		replace RelTim=-11 if rtESV1`i'==1
		}
		* ii. All observations with reltime more than 20 are combined into one. See data section for further discussion
		forvalues i=35(1)49 {
		replace RelTim=21 if rtESV1`i'==1
		}

	* [4] Running the specification for each out our outcome variables 
		*i Initiating loop 
			foreach th in totinc hoursw2 emp unemp notlabforce yearseduc {
		* ii. Initiating estimation
			quietly xi: reg `th' rtESV13-rtESV112 rtESV114-rtESV135 yearsfcor yearsflr aveitc fscontrol asian black hispanic other i.PUS_SURVEY_YEAR*i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE [aw=nobs], cluster(BIRTHSTATE) 
		* iii. Collecting point estimates
				matrix B = e(b)'
			* iv. Storing point estimates
				gen PREDCOEF=.
				replace PREDCOEF=B[1,1] if rtESV13==1
				replace PREDCOEF=B[2,1] if rtESV14==1
				replace PREDCOEF=B[3,1] if rtESV15==1
				replace PREDCOEF=B[4,1] if rtESV16==1
				replace PREDCOEF=B[5,1] if rtESV17==1
				replace PREDCOEF=B[6,1] if rtESV18==1
				replace PREDCOEF=B[7,1] if rtESV19==1
				replace PREDCOEF=B[8,1] if rtESV110==1
				replace PREDCOEF=B[9,1] if rtESV111==1
				replace PREDCOEF=B[10,1] if rtESV112==1
				replace PREDCOEF=B[11,1] if rtESV114==1
				replace PREDCOEF=B[12,1] if rtESV115==1
				replace PREDCOEF=B[13,1] if rtESV116==1
				replace PREDCOEF=B[14,1] if rtESV117==1
				replace PREDCOEF=B[15,1] if rtESV118==1
				replace PREDCOEF=B[16,1] if rtESV119==1
				replace PREDCOEF=B[17,1] if rtESV120==1
				replace PREDCOEF=B[18,1] if rtESV121==1
				replace PREDCOEF=B[19,1] if rtESV122==1
				replace PREDCOEF=B[20,1] if rtESV123==1
				replace PREDCOEF=B[21,1] if rtESV124==1
				replace PREDCOEF=B[22,1] if rtESV125==1
				replace PREDCOEF=B[23,1] if rtESV126==1
				replace PREDCOEF=B[24,1] if rtESV127==1
				replace PREDCOEF=B[25,1] if rtESV128==1
				replace PREDCOEF=B[26,1] if rtESV129==1
				replace PREDCOEF=B[27,1] if rtESV130==1
				replace PREDCOEF=B[28,1] if rtESV131==1
				replace PREDCOEF=B[29,1] if rtESV132==1
				replace PREDCOEF=B[30,1] if rtESV133==1
				replace PREDCOEF=B[31,1] if rtESV134==1
				replace PREDCOEF=B[32,1] if rtESV135==1
			* v. Obtaining the standard errors
				quietly xi: reg `th' rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR  [aw=nobs], cluster(BIRTHSTATE) 
				ereturn list
				mat V=e(V)
				loca nv=`e(rank)'
				mat C=J(`nv',1,-9999)
				forval i=1/`nv' {
				mat C[`i',1]=sqrt(V[`i',`i'])
				}
			* vi. Generating confidence interval
				gen CONINTUP=.
				gen CONINTDO=.
				replace CONINTUP=B[1,1]+1.96 *C[1,1]  if rtESV13==1
				replace CONINTDO=B[1,1]-1.96 *C[1,1]  if rtESV13==1
				replace CONINTUP=B[2,1]+1.96 *C[2,1]  if rtESV14==1
				replace CONINTDO=B[2,1]-1.96 *C[2,1]  if rtESV14==1
				replace CONINTUP=B[3,1]+1.96 *C[3,1]  if rtESV15==1
				replace CONINTDO=B[3,1]-1.96 *C[3,1]  if rtESV15==1
				replace CONINTUP=B[4,1]+1.96 *C[4,1]  if rtESV16==1
				replace CONINTDO=B[4,1]-1.96 *C[4,1]  if rtESV16==1
				replace CONINTUP=B[5,1]+1.96 *C[5,1]  if rtESV17==1
				replace CONINTDO=B[5,1]-1.96 *C[5,1]  if rtESV17==1
				replace CONINTUP=B[6,1]+1.96 *C[6,1]  if rtESV18==1
				replace CONINTDO=B[6,1]-1.96 *C[6,1]  if rtESV18==1
				replace CONINTUP=B[7,1]+1.96 *C[7,1]  if rtESV19==1
				replace CONINTDO=B[7,1]-1.96 *C[7,1]  if rtESV19==1
				replace CONINTUP=B[8,1]+1.96 *C[8,1]  if rtESV110==1
				replace CONINTDO=B[8,1]-1.96 *C[8,1]  if rtESV110==1
				replace CONINTUP=B[9,1]+1.96 *C[9,1]  if rtESV111==1
				replace CONINTDO=B[9,1]-1.96 *C[9,1]  if rtESV111==1
				replace CONINTUP=B[10,1]+1.96*C[10,1] if rtESV112==1
				replace CONINTDO=B[10,1]-1.96*C[10,1] if rtESV112==1
				replace CONINTUP=B[11,1]+1.96*C[11,1] if rtESV114==1
				replace CONINTDO=B[11,1]-1.96*C[11,1] if rtESV114==1
				replace CONINTUP=B[12,1]+1.96*C[12,1] if rtESV115==1
				replace CONINTDO=B[12,1]-1.96*C[12,1] if rtESV115==1
				replace CONINTUP=B[13,1]+1.96*C[13,1] if rtESV116==1
				replace CONINTDO=B[13,1]-1.96*C[13,1] if rtESV116==1
				replace CONINTUP=B[14,1]+1.96*C[14,1] if rtESV117==1
				replace CONINTDO=B[14,1]-1.96*C[14,1] if rtESV117==1
				replace CONINTUP=B[15,1]+1.96*C[15,1] if rtESV118==1
				replace CONINTDO=B[15,1]-1.96*C[15,1] if rtESV118==1
				replace CONINTUP=B[16,1]+1.96*C[16,1] if rtESV119==1
				replace CONINTDO=B[16,1]-1.96*C[16,1] if rtESV119==1
				replace CONINTUP=B[17,1]+1.96*C[17,1] if rtESV120==1
				replace CONINTDO=B[17,1]-1.96*C[17,1] if rtESV120==1
				replace CONINTUP=B[18,1]+1.96*C[18,1] if rtESV121==1
				replace CONINTDO=B[18,1]-1.96*C[18,1] if rtESV121==1
				replace CONINTUP=B[19,1]+1.96*C[19,1] if rtESV122==1
				replace CONINTDO=B[19,1]-1.96*C[19,1] if rtESV122==1
				replace CONINTUP=B[20,1]+1.96*C[20,1] if rtESV123==1
				replace CONINTDO=B[20,1]-1.96*C[20,1] if rtESV123==1
				replace CONINTUP=B[21,1]+1.96*C[21,1] if rtESV124==1
				replace CONINTDO=B[21,1]-1.96*C[21,1] if rtESV124==1
				replace CONINTUP=B[22,1]+1.96*C[22,1] if rtESV125==1
				replace CONINTDO=B[22,1]-1.96*C[22,1] if rtESV125==1
				replace CONINTUP=B[23,1]+1.96*C[23,1] if rtESV126==1
				replace CONINTDO=B[23,1]-1.96*C[23,1] if rtESV126==1
				replace CONINTUP=B[24,1]+1.96*C[24,1] if rtESV127==1
				replace CONINTDO=B[24,1]-1.96*C[24,1] if rtESV127==1
				replace CONINTUP=B[25,1]+1.96*C[25,1] if rtESV128==1
				replace CONINTDO=B[25,1]-1.96*C[25,1] if rtESV128==1
				replace CONINTUP=B[26,1]+1.96*C[26,1] if rtESV129==1
				replace CONINTDO=B[26,1]-1.96*C[26,1] if rtESV129==1
				replace CONINTUP=B[27,1]+1.96*C[27,1] if rtESV130==1
				replace CONINTDO=B[27,1]-1.96*C[27,1] if rtESV130==1
				replace CONINTUP=B[28,1]+1.96*C[28,1] if rtESV131==1
				replace CONINTDO=B[28,1]-1.96*C[28,1] if rtESV131==1
				replace CONINTUP=B[29,1]+1.96*C[29,1] if rtESV132==1
				replace CONINTDO=B[29,1]-1.96*C[29,1] if rtESV132==1
				replace CONINTUP=B[30,1]+1.96*C[30,1] if rtESV133==1
				replace CONINTDO=B[30,1]-1.96*C[30,1] if rtESV133==1
				replace CONINTUP=B[31,1]+1.96*C[31,1] if rtESV134==1
				replace CONINTDO=B[31,1]-1.96*C[31,1] if rtESV134==1
				replace CONINTUP=B[32,1]+1.96*C[32,1] if rtESV135==1
				replace CONINTDO=B[32,1]-1.96*C[32,1] if rtESV135==1
				replace PREDCOEF=0 if rtESV113==1
				replace RelTim=-1 if rtESV113==1
				
				* vii. Plotting results
					if "`th'"=="totinc" {
					twoway (scatter PREDCOEF RelTim if RelTim<27 & RelTim>-12) (rcap CONINTUP CONINTDO RelTim if RelTim<27 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim<-1 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim>-1 & 	RelTim<27), scheme(s1color) legend(off) xtitle("Relative Time") xsc(r(-10 20)) xlabel(-10(5)20) ysc(r(-7500 5000)) ylabel(-7500(2500)5000)
					}
					else if "`th'"=="hoursw2" {
					twoway (scatter PREDCOEF RelTim if RelTim<27 & RelTim>-12) (rcap CONINTUP CONINTDO RelTim if RelTim<27 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim<-1 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim>-1 & 	RelTim<27), scheme(s1color) legend(off) xtitle("Relative Time") xsc(r(-10 20)) xlabel(-10(5)20) ysc(r(-3 3)) ylabel(-3(1.5)3)
					}
					else if "`th'"=="emp" {
					twoway (scatter PREDCOEF RelTim if RelTim<27 & RelTim>-12) (rcap CONINTUP CONINTDO RelTim if RelTim<27 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim<-1 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim>-1 & 	RelTim<27), scheme(s1color) legend(off) xtitle("Relative Time") xsc(r(-10 20)) xlabel(-10(5)20) ysc(r(-0.06 0.06)) ylabel(-0.06(0.03)0.06)
					}
					else if "`th'"=="unemp" {
					twoway (scatter PREDCOEF RelTim if RelTim<27 & RelTim>-12) (rcap CONINTUP CONINTDO RelTim if RelTim<27 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim<-1 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim>-1 & 	RelTim<27), scheme(s1color) legend(off) xtitle("Relative Time") xsc(r(-10 20)) xlabel(-10(5)20) ysc(r(-0.02 0.02)) ylabel(-0.02(0.01)0.02)
					}
					else if "`th'"=="notlabforce" {
					twoway (scatter PREDCOEF RelTim if RelTim<27 & RelTim>-12) (rcap CONINTUP CONINTDO RelTim if RelTim<27 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim<-1 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim>-1 & 	RelTim<27), scheme(s1color) legend(off) xtitle("Relative Time") xsc(r(-10 20)) xlabel(-10(5)20) ysc(r(-0.06 0.06)) ylabel(-0.06(0.03)0.06)
					}
					else if "`th'"=="yearseduc" {
					twoway (scatter PREDCOEF RelTim if RelTim<27 & RelTim>-12) (rcap CONINTUP CONINTDO RelTim if RelTim<27 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim<-1 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim>-1 & 	RelTim<27), scheme(s1color) legend(off) xtitle("Relative Time") xsc(r(-10 20)) xlabel(-10(5)20) ysc(r(-.3 .3)) ylabel(-.3(.15).3)
					}
					else {
					twoway (scatter PREDCOEF RelTim if RelTim<27 & RelTim>-12) (rcap CONINTUP CONINTDO RelTim if RelTim<27 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim<-1 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim>-1 & 	RelTim<27), scheme(s1color) legend(off) xtitle("Relative Time") xsc(r(-10 20)) xlabel(-10(5)20)
					}
					drop PREDCOEF CONINTUP CONINTDO
					
				* viii. Saving figure
				graph export "$output/ES_`samp'_`th'.png", replace
				}
				}

	* Occupational skill (figure 4) is based on individual-level data:

	* [1] Obtaining sample
	foreach samp in MALE FEMALE {
	use "`path_base'/Estimation-samples/PERCENTMOSTHS", clear
	gen MALE=0
	replace MALE=1 if male==1
	gen FEMALE=0
	replace FEMALE=1 if male==0
	keep if `samp'==1
			
	* [2] Generating relative time (x-axis in the figures) 
		gen RelTim=.
		replace RelTim=-10 if rtESV14==1
		replace RelTim=-9 if rtESV15==1
		replace RelTim=-8 if rtESV16==1
		replace RelTim=-7 if rtESV17==1
		replace RelTim=-6 if rtESV18==1
		replace RelTim=-5 if rtESV19==1
		replace RelTim=-4 if rtESV110==1
		replace RelTim=-3 if rtESV111==1
		replace RelTim=-2 if rtESV112==1
		replace RelTim=0 if rtESV114==1
		replace RelTim=1 if rtESV115==1
		replace RelTim=2 if rtESV116==1
		replace RelTim=3 if rtESV117==1
		replace RelTim=4 if rtESV118==1
		replace RelTim=5 if rtESV119==1
		replace RelTim=6 if rtESV120==1
		replace RelTim=7 if rtESV121==1
		replace RelTim=8 if rtESV122==1
		replace RelTim=9 if rtESV123==1
		replace RelTim=10 if rtESV124==1
		replace RelTim=11 if rtESV125==1
		replace RelTim=12 if rtESV126==1
		replace RelTim=13 if rtESV127==1
		replace RelTim=14 if rtESV128==1
		replace RelTim=15 if rtESV129==1
		replace RelTim=16 if rtESV130==1
		replace RelTim=17 if rtESV131==1
		replace RelTim=18 if rtESV132==1
		replace RelTim=19 if rtESV133==1
		replace RelTim=20 if rtESV134==1
			* i. All observations with reltime less than -10 are combined into one. See data section for further discussion
				forvalues i=1(1)3 {
				replace RelTim=-11 if rtESV1`i'==1
				}
			* ii. All observations with reltime more than 20 are combined into one. See data section for further discussion
				forvalues i=35(1)49 {
				replace RelTim=21 if rtESV1`i'==1
				}
				  
		 * [3] Creating empty variables to collect the estimates (see bottom rows)
		gen hold=_n
		gen COEF=.
		gen ERR=.
		gen PREDCOEF=.
		gen SE=.
 
 		* [4] Running the specification 
		quietly xi: reg rankpreplow rtESV13-rtESV112 rtESV114-rtESV135 yearsfcor yearsflr aveitc fscontrol asian black hispanic other i.PUS_SURVEY_YEAR*i.BIRTHYEAR i.BIRTHSTATE [aw=nobs], cluster(BIRTHSTATE) 
		matrix B = e(b)'

		replace PREDCOEF=B[1,1] if rtESV13==1
		replace PREDCOEF=B[2,1] if rtESV14==1
		replace PREDCOEF=B[3,1] if rtESV15==1
		replace PREDCOEF=B[4,1] if rtESV16==1
		replace PREDCOEF=B[5,1] if rtESV17==1
		replace PREDCOEF=B[6,1] if rtESV18==1
		replace PREDCOEF=B[7,1] if rtESV19==1
		replace PREDCOEF=B[8,1] if rtESV110==1
		replace PREDCOEF=B[9,1] if rtESV111==1
		replace PREDCOEF=B[10,1] if rtESV112==1
		replace PREDCOEF=B[11,1] if rtESV114==1
		replace PREDCOEF=B[12,1] if rtESV115==1
		replace PREDCOEF=B[13,1] if rtESV116==1
		replace PREDCOEF=B[14,1] if rtESV117==1
		replace PREDCOEF=B[15,1] if rtESV118==1
		replace PREDCOEF=B[16,1] if rtESV119==1
		replace PREDCOEF=B[17,1] if rtESV120==1
		replace PREDCOEF=B[18,1] if rtESV121==1
		replace PREDCOEF=B[19,1] if rtESV122==1
		replace PREDCOEF=B[20,1] if rtESV123==1
		replace PREDCOEF=B[21,1] if rtESV124==1
		replace PREDCOEF=B[22,1] if rtESV125==1
		replace PREDCOEF=B[23,1] if rtESV126==1
		replace PREDCOEF=B[24,1] if rtESV127==1
		replace PREDCOEF=B[25,1] if rtESV128==1
		replace PREDCOEF=B[26,1] if rtESV129==1
		replace PREDCOEF=B[27,1] if rtESV130==1
		replace PREDCOEF=B[28,1] if rtESV131==1
		replace PREDCOEF=B[29,1] if rtESV132==1
		replace PREDCOEF=B[30,1] if rtESV133==1
		replace PREDCOEF=B[31,1] if rtESV134==1
		replace PREDCOEF=B[32,1] if rtESV135==1

		* [5] Obtaining the standard errors and confidence intervals
		quietly xi: reg rankpreplow rtESV13-rtESV112 rtESV114-rtESV135 yearsfcor yearsflr aveitc fscontrol asian black hispanic other i.PUS_SURVEY_YEAR*i.BIRTHYEAR i.BIRTHSTATE [aw=nobs], cluster(BIRTHSTATE) 
		ereturn list
		mat V=e(V)
		loca nv=`e(rank)'
		mat C=J(`nv',1,-9999)
		forval i=1/`nv' {
		mat C[`i',1]=sqrt(V[`i',`i'])
		}
		
		gen CONINTUP=.
		gen CONINTDO=.
		replace CONINTUP=B[1,1]+1.96*C[1,1] if rtESV13==1
		replace CONINTDO=B[1,1]-1.96*C[1,1] if rtESV13==1
		replace CONINTUP=B[2,1]+1.96*C[2,1] if rtESV14==1
		replace CONINTDO=B[2,1]-1.96*C[2,1] if rtESV14==1
		replace CONINTUP=B[3,1]+1.96*C[3,1] if rtESV15==1
		replace CONINTDO=B[3,1]-1.96*C[3,1] if rtESV15==1
		replace CONINTUP=B[4,1]+1.96*C[4,1] if rtESV16==1
		replace CONINTDO=B[4,1]-1.96*C[4,1] if rtESV16==1
		replace CONINTUP=B[5,1]+1.96*C[5,1] if rtESV17==1
		replace CONINTDO=B[5,1]-1.96*C[5,1] if rtESV17==1
		replace CONINTUP=B[6,1]+1.96*C[6,1] if rtESV18==1
		replace CONINTDO=B[6,1]-1.96*C[6,1] if rtESV18==1
		replace CONINTUP=B[7,1]+1.96*C[7,1] if rtESV19==1
		replace CONINTDO=B[7,1]-1.96*C[7,1] if rtESV19==1
		replace CONINTUP=B[8,1]+1.96*C[8,1] if rtESV110==1
		replace CONINTDO=B[8,1]-1.96*C[8,1] if rtESV110==1
		replace CONINTUP=B[9,1]+1.96*C[9,1] if rtESV111==1
		replace CONINTDO=B[9,1]-1.96*C[9,1] if rtESV111==1
		replace CONINTUP=B[10,1]+1.96*C[10,1] if rtESV112==1
		replace CONINTDO=B[10,1]-1.96*C[10,1] if rtESV112==1
		replace CONINTUP=B[11,1]+1.96*C[11,1] if rtESV114==1
		replace CONINTDO=B[11,1]-1.96*C[11,1] if rtESV114==1
		replace CONINTUP=B[12,1]+1.96*C[12,1] if rtESV115==1
		replace CONINTDO=B[12,1]-1.96*C[12,1] if rtESV115==1
		replace CONINTUP=B[13,1]+1.96*C[13,1] if rtESV116==1
		replace CONINTDO=B[13,1]-1.96*C[13,1] if rtESV116==1
		replace CONINTUP=B[14,1]+1.96*C[14,1] if rtESV117==1
		replace CONINTDO=B[14,1]-1.96*C[14,1] if rtESV117==1
		replace CONINTUP=B[15,1]+1.96*C[15,1] if rtESV118==1
		replace CONINTDO=B[15,1]-1.96*C[15,1] if rtESV118==1
		replace CONINTUP=B[16,1]+1.96*C[16,1] if rtESV119==1
		replace CONINTDO=B[16,1]-1.96*C[16,1] if rtESV119==1
		replace CONINTUP=B[17,1]+1.96*C[17,1] if rtESV120==1
		replace CONINTDO=B[17,1]-1.96*C[17,1] if rtESV120==1
		replace CONINTUP=B[18,1]+1.96*C[18,1] if rtESV121==1
		replace CONINTDO=B[18,1]-1.96*C[18,1] if rtESV121==1
		replace CONINTUP=B[19,1]+1.96*C[19,1] if rtESV122==1
		replace CONINTDO=B[19,1]-1.96*C[19,1] if rtESV122==1
		replace CONINTUP=B[20,1]+1.96*C[20,1] if rtESV123==1
		replace CONINTDO=B[20,1]-1.96*C[20,1] if rtESV123==1
		replace CONINTUP=B[21,1]+1.96*C[21,1] if rtESV124==1
		replace CONINTDO=B[21,1]-1.96*C[21,1] if rtESV124==1
		replace CONINTUP=B[22,1]+1.96*C[22,1] if rtESV125==1
		replace CONINTDO=B[22,1]-1.96*C[22,1] if rtESV125==1
		replace CONINTUP=B[23,1]+1.96*C[23,1] if rtESV126==1
		replace CONINTDO=B[23,1]-1.96*C[23,1] if rtESV126==1
		replace CONINTUP=B[24,1]+1.96*C[24,1] if rtESV127==1
		replace CONINTDO=B[24,1]-1.96*C[24,1] if rtESV127==1
		replace CONINTUP=B[25,1]+1.96*C[25,1] if rtESV128==1
		replace CONINTDO=B[25,1]-1.96*C[25,1] if rtESV128==1
		replace CONINTUP=B[26,1]+1.96*C[26,1] if rtESV129==1
		replace CONINTDO=B[26,1]-1.96*C[26,1] if rtESV129==1
		replace CONINTUP=B[27,1]+1.96*C[27,1] if rtESV130==1
		replace CONINTDO=B[27,1]-1.96*C[27,1] if rtESV130==1
		replace CONINTUP=B[28,1]+1.96*C[28,1] if rtESV131==1
		replace CONINTDO=B[28,1]-1.96*C[28,1] if rtESV131==1
		replace CONINTUP=B[29,1]+1.96*C[29,1] if rtESV132==1
		replace CONINTDO=B[29,1]-1.96*C[29,1] if rtESV132==1
		replace CONINTUP=B[30,1]+1.96*C[30,1] if rtESV133==1
		replace CONINTDO=B[30,1]-1.96*C[30,1] if rtESV133==1
		replace CONINTUP=B[31,1]+1.96*C[31,1] if rtESV134==1
		replace CONINTDO=B[31,1]-1.96*C[31,1] if rtESV134==1
		replace CONINTUP=B[32,1]+1.96*C[32,1] if rtESV135==1
		replace CONINTDO=B[32,1]-1.96*C[32,1] if rtESV135==1

		replace PREDCOEF=0 if rtESV113==1
		replace RelTim=-1 if rtESV113==1
		
		twoway (scatter PREDCOEF RelTim if RelTim<27 & RelTim>-12) (rcap CONINTUP CONINTDO RelTim if RelTim<27 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim<-1 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim>-1 & RelTim<27), scheme(s1color) legend(off) xtitle("Relative Time") xsc(r(-10 20)) xlabel(-10(5)20) ysc(r(-0.02 0.02)) ylabel(-0.02(0.01)0.02)
	
		graph export "$output/ES_`samp'_occsort.png", replace
		drop PREDCOEF CONINTUP CONINTDO
		}

************
* FIGURE 5 * 
************

	* [1] Path and preliminary commands
	clear all

	* [2] Selecting sample
	foreach samp in MALE_BHO MALE_WA FEMALE_BHO FEMALE_WA {
	clear all
	use "`path_base'/Estimation-samples/Estimation_`samp'"
		
	* [6] Generating relative time (x-axis) 
	gen RelTim=.
	replace RelTim=-10 if rtESV14== 1
	replace RelTim=-9  if rtESV15== 1
	replace RelTim=-8  if rtESV16== 1
	replace RelTim=-7  if rtESV17== 1
	replace RelTim=-6  if rtESV18== 1
	replace RelTim=-5  if rtESV19== 1
	replace RelTim=-4  if rtESV110==1
	replace RelTim=-3  if rtESV111==1
	replace RelTim=-2  if rtESV112==1
	replace RelTim= 0  if rtESV114==1
	replace RelTim= 1  if rtESV115==1
	replace RelTim= 2  if rtESV116==1
	replace RelTim= 3  if rtESV117==1
	replace RelTim= 4  if rtESV118==1
	replace RelTim= 5  if rtESV119==1
	replace RelTim= 6  if rtESV120==1
	replace RelTim= 7  if rtESV121==1
	replace RelTim= 8  if rtESV122==1
	replace RelTim= 9  if rtESV123==1
	replace RelTim= 10 if rtESV124==1
	replace RelTim= 11 if rtESV125==1
	replace RelTim= 12 if rtESV126==1
	replace RelTim= 13 if rtESV127==1
	replace RelTim= 14 if rtESV128==1
	replace RelTim= 15 if rtESV129==1
	replace RelTim= 16 if rtESV130==1
	replace RelTim= 17 if rtESV131==1
	replace RelTim= 18 if rtESV132==1
	replace RelTim= 19 if rtESV133==1
	replace RelTim= 20 if rtESV134==1
		* i. All observations with reltime less than -10 are combined into one. See data section for further discussion
			forvalues i=1(1)3 {
			replace RelTim=-11 if rtESV1`i'==1
			}
		* ii. All observations with reltime more than 20 are combined into one. See data section for further discussion
			forvalues i=35(1)49 {
			replace RelTim=21 if rtESV1`i'==1
			}
	
	* [8] Running the specification for each out our outcome variables 
		*i Initiating loop 
			foreach th in totinc {
		* ii. Initiating estimation
			quietly xi: reg `th' rtESV13-rtESV112 rtESV114-rtESV135 yearsfcor yearsflr aveitc fscontrol asian black hispanic other i.PUS_SURVEY_YEAR*i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE [aw=nobs], cluster(BIRTHSTATE) 
		* iii. Collecting point estimates
				matrix B = e(b)'
			* iv. Storing point estimates
				gen PREDCOEF=.
				replace PREDCOEF=B[1,1] if rtESV13==1
				replace PREDCOEF=B[2,1] if rtESV14==1
				replace PREDCOEF=B[3,1] if rtESV15==1
				replace PREDCOEF=B[4,1] if rtESV16==1
				replace PREDCOEF=B[5,1] if rtESV17==1
				replace PREDCOEF=B[6,1] if rtESV18==1
				replace PREDCOEF=B[7,1] if rtESV19==1
				replace PREDCOEF=B[8,1] if rtESV110==1
				replace PREDCOEF=B[9,1] if rtESV111==1
				replace PREDCOEF=B[10,1] if rtESV112==1
				replace PREDCOEF=B[11,1] if rtESV114==1
				replace PREDCOEF=B[12,1] if rtESV115==1
				replace PREDCOEF=B[13,1] if rtESV116==1
				replace PREDCOEF=B[14,1] if rtESV117==1
				replace PREDCOEF=B[15,1] if rtESV118==1
				replace PREDCOEF=B[16,1] if rtESV119==1
				replace PREDCOEF=B[17,1] if rtESV120==1
				replace PREDCOEF=B[18,1] if rtESV121==1
				replace PREDCOEF=B[19,1] if rtESV122==1
				replace PREDCOEF=B[20,1] if rtESV123==1
				replace PREDCOEF=B[21,1] if rtESV124==1
				replace PREDCOEF=B[22,1] if rtESV125==1
				replace PREDCOEF=B[23,1] if rtESV126==1
				replace PREDCOEF=B[24,1] if rtESV127==1
				replace PREDCOEF=B[25,1] if rtESV128==1
				replace PREDCOEF=B[26,1] if rtESV129==1
				replace PREDCOEF=B[27,1] if rtESV130==1
				replace PREDCOEF=B[28,1] if rtESV131==1
				replace PREDCOEF=B[29,1] if rtESV132==1
				replace PREDCOEF=B[30,1] if rtESV133==1
				replace PREDCOEF=B[31,1] if rtESV134==1
				replace PREDCOEF=B[32,1] if rtESV135==1
			* v. Obtaining the standard errors
				quietly xi: reg `th' rtESV13-rtESV112 rtESV114-rtESV135 asian black hispanic other yearsfcor yearsflr aveitc fscontrol i.BIRTHYEAR i.PUS_SURVEY_YEAR i.BIRTHSTATE i.PUS_SURVEY_YEAR*i.BIRTHYEAR  [aw=nobs], cluster(BIRTHSTATE) 
				ereturn list
				mat V=e(V)
				loca nv=`e(rank)'
				mat C=J(`nv',1,-9999)
				forval i=1/`nv' {
				mat C[`i',1]=sqrt(V[`i',`i'])
				}
			* vi. Generating confidence interval
				gen CONINTUP=.
				gen CONINTDO=.
				replace CONINTUP=B[1,1]+1.96 *C[1,1]  if rtESV13==1
				replace CONINTDO=B[1,1]-1.96 *C[1,1]  if rtESV13==1
				replace CONINTUP=B[2,1]+1.96 *C[2,1]  if rtESV14==1
				replace CONINTDO=B[2,1]-1.96 *C[2,1]  if rtESV14==1
				replace CONINTUP=B[3,1]+1.96 *C[3,1]  if rtESV15==1
				replace CONINTDO=B[3,1]-1.96 *C[3,1]  if rtESV15==1
				replace CONINTUP=B[4,1]+1.96 *C[4,1]  if rtESV16==1
				replace CONINTDO=B[4,1]-1.96 *C[4,1]  if rtESV16==1
				replace CONINTUP=B[5,1]+1.96 *C[5,1]  if rtESV17==1
				replace CONINTDO=B[5,1]-1.96 *C[5,1]  if rtESV17==1
				replace CONINTUP=B[6,1]+1.96 *C[6,1]  if rtESV18==1
				replace CONINTDO=B[6,1]-1.96 *C[6,1]  if rtESV18==1
				replace CONINTUP=B[7,1]+1.96 *C[7,1]  if rtESV19==1
				replace CONINTDO=B[7,1]-1.96 *C[7,1]  if rtESV19==1
				replace CONINTUP=B[8,1]+1.96 *C[8,1]  if rtESV110==1
				replace CONINTDO=B[8,1]-1.96 *C[8,1]  if rtESV110==1
				replace CONINTUP=B[9,1]+1.96 *C[9,1]  if rtESV111==1
				replace CONINTDO=B[9,1]-1.96 *C[9,1]  if rtESV111==1
				replace CONINTUP=B[10,1]+1.96*C[10,1] if rtESV112==1
				replace CONINTDO=B[10,1]-1.96*C[10,1] if rtESV112==1
				replace CONINTUP=B[11,1]+1.96*C[11,1] if rtESV114==1
				replace CONINTDO=B[11,1]-1.96*C[11,1] if rtESV114==1
				replace CONINTUP=B[12,1]+1.96*C[12,1] if rtESV115==1
				replace CONINTDO=B[12,1]-1.96*C[12,1] if rtESV115==1
				replace CONINTUP=B[13,1]+1.96*C[13,1] if rtESV116==1
				replace CONINTDO=B[13,1]-1.96*C[13,1] if rtESV116==1
				replace CONINTUP=B[14,1]+1.96*C[14,1] if rtESV117==1
				replace CONINTDO=B[14,1]-1.96*C[14,1] if rtESV117==1
				replace CONINTUP=B[15,1]+1.96*C[15,1] if rtESV118==1
				replace CONINTDO=B[15,1]-1.96*C[15,1] if rtESV118==1
				replace CONINTUP=B[16,1]+1.96*C[16,1] if rtESV119==1
				replace CONINTDO=B[16,1]-1.96*C[16,1] if rtESV119==1
				replace CONINTUP=B[17,1]+1.96*C[17,1] if rtESV120==1
				replace CONINTDO=B[17,1]-1.96*C[17,1] if rtESV120==1
				replace CONINTUP=B[18,1]+1.96*C[18,1] if rtESV121==1
				replace CONINTDO=B[18,1]-1.96*C[18,1] if rtESV121==1
				replace CONINTUP=B[19,1]+1.96*C[19,1] if rtESV122==1
				replace CONINTDO=B[19,1]-1.96*C[19,1] if rtESV122==1
				replace CONINTUP=B[20,1]+1.96*C[20,1] if rtESV123==1
				replace CONINTDO=B[20,1]-1.96*C[20,1] if rtESV123==1
				replace CONINTUP=B[21,1]+1.96*C[21,1] if rtESV124==1
				replace CONINTDO=B[21,1]-1.96*C[21,1] if rtESV124==1
				replace CONINTUP=B[22,1]+1.96*C[22,1] if rtESV125==1
				replace CONINTDO=B[22,1]-1.96*C[22,1] if rtESV125==1
				replace CONINTUP=B[23,1]+1.96*C[23,1] if rtESV126==1
				replace CONINTDO=B[23,1]-1.96*C[23,1] if rtESV126==1
				replace CONINTUP=B[24,1]+1.96*C[24,1] if rtESV127==1
				replace CONINTDO=B[24,1]-1.96*C[24,1] if rtESV127==1
				replace CONINTUP=B[25,1]+1.96*C[25,1] if rtESV128==1
				replace CONINTDO=B[25,1]-1.96*C[25,1] if rtESV128==1
				replace CONINTUP=B[26,1]+1.96*C[26,1] if rtESV129==1
				replace CONINTDO=B[26,1]-1.96*C[26,1] if rtESV129==1
				replace CONINTUP=B[27,1]+1.96*C[27,1] if rtESV130==1
				replace CONINTDO=B[27,1]-1.96*C[27,1] if rtESV130==1
				replace CONINTUP=B[28,1]+1.96*C[28,1] if rtESV131==1
				replace CONINTDO=B[28,1]-1.96*C[28,1] if rtESV131==1
				replace CONINTUP=B[29,1]+1.96*C[29,1] if rtESV132==1
				replace CONINTDO=B[29,1]-1.96*C[29,1] if rtESV132==1
				replace CONINTUP=B[30,1]+1.96*C[30,1] if rtESV133==1
				replace CONINTDO=B[30,1]-1.96*C[30,1] if rtESV133==1
				replace CONINTUP=B[31,1]+1.96*C[31,1] if rtESV134==1
				replace CONINTDO=B[31,1]-1.96*C[31,1] if rtESV134==1
				replace CONINTUP=B[32,1]+1.96*C[32,1] if rtESV135==1
				replace CONINTDO=B[32,1]-1.96*C[32,1] if rtESV135==1
				replace PREDCOEF=0 if rtESV113==1
				replace RelTim=-1 if rtESV113==1
				* vii. Plotting results
				twoway (scatter PREDCOEF RelTim if RelTim<27 & RelTim>-12) (rcap CONINTUP CONINTDO RelTim if RelTim<27 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim<-1 & RelTim>-12) (lfit PREDCOEF RelTim if RelTim>-1 & 	RelTim<27), scheme(s1color) legend(off) xtitle("Relative Time") xsc(r(-10 20)) xlabel(-10(5)20) ysc(r(-10000 5000)) ylabel(-10000(2500)5000)
			
				* viii. Saving figure
				graph export "$output/ES_`samp'_`th'.png", replace
				drop PREDCOEF CONINTUP CONINTDO
				}
				}
 
*/
