*  ██████   ██████  ██    ██      ██ ██████   ██████   █████  
* ██       ██    ██ ██    ██     ███      ██ ██       ██   ██ 
* ██   ███ ██    ██ ██    ██      ██  █████  ███████   █████  
* ██    ██ ██    ██  ██  ██       ██      ██ ██    ██ ██   ██ 
*  ██████   ██████    ████        ██ ██████   ██████   █████  
*
* Title:		Descriptive Statistics + AI
* Author:		Mohit Karnani (mkarnani@hks.harvard.edu)
* Created:		August 14, 2024

* We will download the attendance data from MA's Department of Education:
global path "https://www.doe.mass.edu/infoservices/reports"
import excel using "$path/attendance/2024-mar-grade-stugroup.xlsx", firstrow clear

* Mild data cleaning:
drop if SchoolName == "State" // We don't need the state aggregate
keep if Grade == "ALL" // We will work with school-level (not grade-level) data
drop if DistrictCode == SchoolCode // We don't need the district aggregates

* Let's take a look at our dataset:
describe

* Now let's compute some descriptive statistics on the attendance rate:
summarize AttendanceRate

* We can get more details with the ", detail" option:
summarize AttendanceRate, detail

* How do these variables correlate with each other?
correlate AttendanceRate AverageofAbsences Absent10ormoredays ///
ChronicallyAbsent10ormore ChronicallyAbsent20ormore Unexcused9days 

* These results are expected! But how do they correlate with total enrollment?

* Now we'll do something more advanced with a new enrollment dataset:
capture frame create enroll
frame change enroll
import excel using "$path/enroll/2024/school-grade.xlsx", cellrange(A6) firstrow clear

* Mild data cleaning:
drop if SCHOOL == "00000000" | missing(SCHOOL) //these are not schools
drop if School_Total == SPED_Beyond_Grade_12 //these are not in the attendance data

* We have two frames/tables/datasets:
frames dir // the sizes look good!

* Let's merge them based on their SchoolCode:
rename SCHOOL SchoolCode
frame change default
frlink 1:1 SchoolCode, frame(enroll)

fralias add School_Total, from(enroll)
correlate AttendanceRate AverageofAbsences Absent10ormoredays ///
ChronicallyAbsent10ormore ChronicallyAbsent20ormore Unexcused9days School_Total
