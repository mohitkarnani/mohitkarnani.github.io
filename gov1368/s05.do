*  ██████   ██████  ██    ██      ██ ██████   ██████   █████  
* ██       ██    ██ ██    ██     ███      ██ ██       ██   ██ 
* ██   ███ ██    ██ ██    ██      ██  █████  ███████   █████  
* ██    ██ ██    ██  ██  ██       ██      ██ ██    ██ ██   ██ 
*  ██████   ██████    ████        ██ ██████   ██████   █████  
*
* Title:		Instrumental Variables (IV)
* Author:		Mohit Karnani (mkarnani@hks.harvard.edu)
* Created:		October 7, 2024

* Correlation is not causation: preschool attendance is endogenous
clear all
set seed 123
set obs 200000
gen lucky=mod(_n,2)
gen preschool=rbinomial(1,0.6+0.2*lucky)
gen score=250+10*preschool+20*lucky+rnormal()*45
sum preschool score
*reg score preschool
ttest score, by(preschool)

* Preschool attendance is still endogenous, but now we have an instrument
clear all
set seed 123
set obs 200000
gen lucky=mod(_n,2)
gen voucher=rbinomial(1,0.5)
gen preschool=rbinomial(1,0.6+0.2*lucky+0.15*voucher)
gen score=250+10*preschool+20*lucky+rnormal()*45
sum preschool score
*reg score preschool
ttest score, by(preschool)

* Instrumental variables solves the selection bias issue!
ttest score, by(voucher)
local numerator=r(mu_2)-r(mu_1)
ttest preschool, by(voucher)
local denominator=r(mu_2)-r(mu_1)
di `numerator'/`denominator'

* Same, but with a regression
reg score voucher // Intent to treat (ITT)
local bscore=_b[voucher]
reg preschool voucher // First stage
di `bscore'/_b[voucher]

* Another framework for IV: 2-stage least squares (2SLS)
reg preschool voucher
predict predpreschool, xb
reg score predpreschool

* One-liner
ivreg score (preschool=voucher)


**********************************************************************
* The Effect of School Spending on Educational and Economic Outcomes *
**********************************************************************

* The data for this paper is not public, so here's ChatGPT's version:
* https://chatgpt.com/share/e/6707df94-41ec-8006-af6d-d1391ca54d78
import delimited "Synthetic_School_Reform_Dataset.csv", clear 

* Let's regress future income on the endogenous spending variable:
reg adult_family_income spending_post_reform //spending is bad?

* Now we instrument the spending using the increase due to the reform:
ivreg adult_family_income (spending_post_reform=spending_increase_due_to_reform)

ivreg years_of_education (spending_post_reform=spending_increase_due_to_reform)

ivreg adult_wages (spending_post_reform=spending_increase_due_to_reform)

ivreg adult_poverty_status (spending_post_reform=spending_increase_due_to_reform)
