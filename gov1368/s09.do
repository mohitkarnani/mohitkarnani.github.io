*  ██████   ██████  ██    ██      ██ ██████   ██████   █████  
* ██       ██    ██ ██    ██     ███      ██ ██       ██   ██ 
* ██   ███ ██    ██ ██    ██      ██  █████  ███████   █████  
* ██    ██ ██    ██  ██  ██       ██      ██ ██    ██ ██   ██ 
*  ██████   ██████    ████        ██ ██████   ██████   █████  
*
* Title:		Difference-in-Differences II (DiD)
* Author:		Mohit Karnani (mkarnani@hks.harvard.edu)
* Created:		November 13, 2024

*******************************************************************
* The Market-Level Effects of Charter Schools on Student Outcomes *
*******************************************************************

* Loading the (synthetic) data:
import delimited Synthetic_Charter_School_Dataset.csv, clear

gen post = (year>=2003)

* Standard regressions
reg grad_rate charter_share post i.district_id
reg math_score charter_share post i.district_id
reg ela_score charter_share post i.district_id

* With controls
reg grad_rate charter_share post i.district_id population urban black_share hispanic_share frl_share
reg math_score charter_share post i.district_id population urban black_share hispanic_share frl_share
reg ela_score charter_share post i.district_id population urban black_share hispanic_share frl_share
