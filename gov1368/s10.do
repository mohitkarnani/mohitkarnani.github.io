clear all
import excel esa_private.xlsx, firstrow clear
preserve
	gen states = 1
	collapse (count) states, by(esa)
	drop if mi(esa)
	gen cummulative = sum(states)
	tw line cummulative esa, xtitle("Year") ytitle("States with ESA laws (total)")
	graph export rollout.pdf, replace
restore
destring y2005 y2017, replace force
reshape long y, i(state) j(year)
frame create public
frame public {
	import excel public.xlsx, firstrow clear
	reshape long y, i(state) j(year)
	rename y public_enr
}
frlink 1:1 state year, frame(public)
frget public_enr, from(public)
gen total_enr=public_enr+y
replace y = y/total_enr
gen treat = !mi(esa)
gen post = year>=esa
gen treatpost = treat*post
encode state, g(state_i)
didregress (y) (treatpost) [aw=total_enr], group(state_i) time(year)
eststo: reg y treatpost i.year i.state_i [aw=total_enr], cluster(state_i)
tw (lpoly y year if treat) (lpoly y year if !treat), xtitle("Year") xline(2011) ///
ytitle("Private Share") legend(label(1 "Treatment") label(2 "Control"))
graph export parallel_trends.pdf, replace
drop if year<2003 //they don't count K private enrollment before 2003
didregress (y) (treatpost) [aw=total_enr], group(state_i) time(year)
eststo: reg y treatpost i.year i.state_i [aw=total_enr], cluster(state_i)
esttab * using results.tex, replace booktabs p keep(treatpost) star(* 0.1 ** 0.05 *** 0.01)
eststo clear
