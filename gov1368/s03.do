*  ██████   ██████  ██    ██      ██ ██████   ██████   █████  
* ██       ██    ██ ██    ██     ███      ██ ██       ██   ██ 
* ██   ███ ██    ██ ██    ██      ██  █████  ███████   █████  
* ██    ██ ██    ██  ██  ██       ██      ██ ██    ██ ██   ██ 
*  ██████   ██████    ████        ██ ██████   ██████   █████  
*
* Title:		Randomized Controlled Trials (RCTs)
* Author:		Mohit Karnani (mkarnani@hks.harvard.edu)
* Created:		September 26, 2024

***********************************************************************
* New topic: (simulating) the causal effects of pre-schooling / pre-K *
***********************************************************************

* Note: this is a simulation exercise, we're not using real data.
clear all

* Like the paper we covered during lecture, let's work with 3k observations
set obs 3000 //we can change this later to experiment with larger samples

* Let's simulate a random variable describing household income percentiles
set seed 123 //for replication purposes
gen income = floor(runiform()*100) //income percentiles should be uniform
histogram income, percent //they look good! (they look better with more obs)

* Now let's simulate something silly: is blue your favorite color?
gen blue = rbinomial(1, 0.5) //let's say about half of the kids like blue
tab blue //looks good too!

* Let's get serious again: we are randomly assigning a preschool program
gen prek = mod(_n,2) //we alternate who gets the treatment, without any sorting
tabstat income blue, by(prek) s(mean sd N) //there are no systematic differences

* Time passes and kids take a test. Richer kids do better, but pre-K helps
gen score = 5 + 0.8*income + 10*prek + rnormal() //pre-K boosts scores by 10

* Suppose we have no idea about the data generating process.
* Can we still correctly estimate the ATE of pre-K on test scores?
ttest score, by(prek) //yes, we can!
regress score prek //regression yields the same result
regress score prek income //of course, if we know the dgp we can do better

* Now things get hairy: we have non-compliance and richer kids don't like pre-K
gen prekprob = prek
replace prekprob = prekprob-0.5*(income>=80)-0.0001 if prek==1
replace prekprob = prekprob+0.5*(income<80)+0.0001 if prek==0
gen realprek = rbinomial(1, prekprob)
tabstat realprek, by(prek) //similar to what we see in the paper

* Time passes and kids take a test. Richer kids do better, *real* pre-K helps
gen realscore = 5 + 0.8*income + 10*realprek + rnormal() //real pre-K boosts scores by 10

* Can we still correctly estimate the ATE of real pre-K on test scores?
ttest realscore, by(realprek) //no, we can't!
regress realscore realprek //unfortunately, regression doesn't fix this
regress realscore realprek income //but if we know the dgp we can fix this

/*
* Super extra: why did Heckman win a Nobel prize? (I wrote this 10 years ago in Chile)
clear all
set seed 123
set obs 100000                           //100000 observations
gen esc=runiform(0,20)                   //years of schooling
gen exp=runiform(0,50)                   //work experience
gen exp2=exp^2                           //experience squared
matrix C=(2,.4\.4,1)                     //covariance matrix of errors
drawnorm e u, cov(C)                     //normal error draws
gen lnw=1+0.25*esc+0.5*exp-0.01*exp2+e   //log wages
gen z=runiform(-5,5)                     //excluded variable
gen d=-1+0.5*esc-0.25*exp+2*z+u          //latent variable
gen D=(d>0)                              //D is 1 if d>0 and 0 if not
gen y=lnw if D==1                        //wage observed if D=1
probit D esc exp z                       //probit on D
predict xb, xb                           //lienar prediction on probit
gen lambda=normalden(xb)/normal(xb)      //inverse Mills Ratio
reg y esc exp exp2 lambda                //censored regression with lambda
heckman y esc exp exp2, select(esc exp z) two
esttab, s(N rho sigma)
*/

/*
***********************************************************************
* Extra: Tennessee's Student Teacher Achievement Ratio (STAR) project *
***********************************************************************
import spss using "STAR_Students.sav", clear

* Let's take a look at the outcome variables (Y)
sum gktreadss gktmathss gkwordskillss g1treadss g1tmathss g1wordskillss g2treadss g2tmathss g2wordskillss g3treadss g3tmathss g3wordskillss

* Now let's tabulate the treatment indicators
codebook gkclasstype g1classtype g2classtype g3classtype

* Changing "k" to "0" so the loop below works
rename (gkclasstype gktreadss gktmathss gkwordskillss) (g0classtype g0treadss g0tmathss g0wordskillss)

* This loop was taken from a random Github repo (not sure how scores were built)
forvalues i == 0/3 {
	foreach sub in `i'tread `i'tmath `i'wordskill {
		cumul g`sub'ss if inrange(g`i'classtype,2,3), gen(g`sub'xt)
		sort g`sub'ss
		qui replace g`sub'xt=g`sub'xt[_n-1] if g`sub'ss==g`sub'ss[_n-1] & g`i'classtype==1
		qui ipolate g`sub'xt g`sub'ss, gen(ipo)
		qui replace g`sub'xt=ipo if g`i'classtype==1 & mi(g`sub'xt)
		drop ipo
	}
	qui egen g`i'score = rmean(g`i'treadxt g`i'tmathxt g`i'wordskillxt)
	qui replace g`i'score = 100*g`i'score
}

* Replicating (qualitatively) the first column of Table 5
forvalues i == 0/3 {
	reg g`i'score b2.g`i'classtype
}
*/
