*  ██████   ██████  ██    ██      ██ ██████   ██████   █████  
* ██       ██    ██ ██    ██     ███      ██ ██       ██   ██ 
* ██   ███ ██    ██ ██    ██      ██  █████  ███████   █████  
* ██    ██ ██    ██  ██  ██       ██      ██ ██    ██ ██   ██ 
*  ██████   ██████    ████        ██ ██████   ██████   █████  
*
* Title:		Regression Discontinuity (RD)
* Author:		Mohit Karnani (mkarnani@hks.harvard.edu)
* Created:		October 30, 2024

*******************************************************************
* The Impact of IMPACT on Teacher Retention Rates and Performance *
*******************************************************************

* The data for this paper is not public, so here's a simulation:
import delimited "synthetic_teacher_dataset.csv", clear 

* Our treatments are the performance band assigned by the IMPACT program:
gen d_me = (performance_band=="Minimally Effective")
gen d_e = (performance_band=="Effective")
gen d_he = (performance_band=="Highly Effective")

* We don't see evidence of manipulation around the threshold:
tw (hist initial_score, frequency) (kdensity initial_score, yaxis(2)), xline(250 350) leg(pos(6))

* Other variables shouldn't "jump" either:
gen black = (race=="Black")
foreach var in experience_years graduate_degree poverty_rate black {
	tw (lpolyci `var' initial_score if d_me) (lpolyci `var' initial_score if d_e) (lpolyci `var' initial_score if d_he), xline(250 350) leg(off) title(`var')
	graph export `var'.pdf, replace
}

* All good! Now we'll install a package to run this easily
ssc install rdrobust

* LATE estimates of a higher band on retention rates:
rdrobust retention_outcome initial_score, c(250) fuzzy(d_e)
rdrobust retention_outcome initial_score, c(350) fuzzy(d_he)

* LATE estimates of a higher band on future scores:
rdrobust new_impact_score initial_score, c(250) fuzzy(d_e)
rdrobust new_impact_score initial_score, c(350) fuzzy(d_he)

* A picture is worth a thousand words (?):
rdplot retention_outcome initial_score, c(250) p(1)  graph_options(leg(pos(6)))
rdplot retention_outcome initial_score, c(350) p(1)  graph_options(leg(pos(6)))
rdplot new_impact_score initial_score, c(250) p(1)  graph_options(leg(pos(6)))
rdplot new_impact_score initial_score, c(350) p(1)  graph_options(leg(pos(6)))

* Extra: we can approximate these results with local averages:
sum retention_outcome if inrange(initial_score,250,255)
sum retention_outcome if inrange(initial_score,245,250)
di .88-.6938776
