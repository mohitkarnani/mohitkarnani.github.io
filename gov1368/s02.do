*  ██████   ██████  ██    ██      ██ ██████   ██████   █████  
* ██       ██    ██ ██    ██     ███      ██ ██       ██   ██ 
* ██   ███ ██    ██ ██    ██      ██  █████  ███████   █████  
* ██    ██ ██    ██  ██  ██       ██      ██ ██    ██ ██   ██ 
*  ██████   ██████    ████        ██ ██████   ██████   █████  
*
* Title:		Linear Regression
* Author:		Mohit Karnani (mkarnani@hks.harvard.edu)
* Created:		September 14, 2024

* We will load the (clean) NAEP scores:
import excel "naep.xlsx", firstrow clear

* Let's take a look at our dataset:
describe

* Now let's compute some descriptive statistics on the scores:
summarize reading math

* How do these scores evolve over time? Let's plot them!
twoway line reading year
twoway line math year

* Extra: both together
twoway (line reading year) (line math year)

* Now let's fit a linear regression model for each test:
regress reading year
regress math year

* We are fitting the "best line":
twoway (line reading year) (lfit reading year)
twoway (line math year) (lfit math year)

* What about pre-covid?
regress reading year if year<2020
regress math year if year<2020
twoway (lfit reading year if year<2020) (lfit math year if year<2020)

* Extra: approximating the slope with the correlation and standard deviations
regress reading year
corr reading year
sum reading year if !mi(reading)
di 0.4627 * 1.95796 / 15.11511

* Now let's load the Student_Marks dataset:
import delimited "Student_Marks.csv", clear

* Plotting the scores and hours:
twoway scatter marks time_study

* Simple linear regression:
regress marks time_study

* Graphically:
twoway (scatter marks time_study) (lfit marks time_study)

* Multiple linear regression:
regress marks time_study number_courses
