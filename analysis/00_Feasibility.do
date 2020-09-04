/*==============================================================================

DO FILE NAME: 			00_Feasibility			
PROJECT: 				Care Home Mortality 				
DATE: 					16 August 2020 
AUTHOR: 				Anna Schultze 	
STATA VERSION: 			16.1
DESCRIPTION OF FILE: 	Create and output basic tabulations 	
DATASETS USED: 			input.csv
DATASETS CREATED: 		N/A
OTHER OUTPUT: 			table1-8, in analysis/$outdir 							
	
==============================================================================*/

* open a log file

cap log close
log using $logdir/00_Feasibility, replace t

/* Data management============================================================*/ 
*  Preliminary data management to generate descriptives 

* Check population excludes children and people with missing age 

noi di "DROP AGE <18:"
drop if age < 18 

noi di "DROP AGE MISSING:"
drop if age == . 

* Create categorised age
recode age 18/49.9999 = 1 /// 
           50/54.9999 = 2 ///
		   55/59.9999 = 3 ///
	       60/64.9999 = 4 ///
		   65/69.9999 = 5 ///
		   70/74.9999 = 6 ///
		   75/79.9999 = 7 ///
	       80/84.9999 = 8 ///
		   85/89.9999 = 9 ///
		   90/94.9999 = 10 ///
		   95/max = 11, gen(agegroup) 

label define agegroup 	1 "18-<50" ///
						2 "50-<55" ///
						3 "55-<60" ///
						4 "60-<65" ///
						5 "65-<70" ///
						6 "70-<75" ///
						7 "75-<80" ///
						8 "80-<85" ///
						9 "85-<90" ///
						10 "90-<95" ///
						11 "95+"
						
label values agegroup agegroup

tab agegroup, m
label variable agegroup "Age Group"

* Format Care Home Variables
gen care_home_num = 1 if care_home_type == "PC"
replace care_home_num = 2 if care_home_type == "PN"
replace care_home_num = 3 if care_home_type == "PS"
replace care_home_num = 0 if care_home_type == "U"
replace care_home_num = .u if care_home_type == ""

tab care_home_num, missing

* create labels
label define carehome 1 "Care Home"    /// 
					  2 "Nursing Home" ///
					  3 "Care or Nursing Home" /// 
					  0 "Private Home" /// 
					  .u "Unknown"

* apply labels and double check variable creation 
label values care_home_num carehome 
tab care_home_num care_home_type 

* replace with the numeric variable 
drop care_home_type 
rename care_home_num care_home_type 
label variable care_home_type "Property Type"

gen care_home = 1 if care_home_type == 1 | care_home_type == 2 | care_home_type == 3 
replace care_home = 0 if care_home_type == 0 
replace care_home = .u if care_home_type == .u 

label define anycarehome 1 "Yes" /// 
					     0 "No" /// 
						 .u "Unknown"
						 
label values care_home anycarehome 
label variable care_home "Care or Nursing Home"

* Generate count of residents per care home 
* This should just be the household size for the care home 
label variable household_size "Household Size"

* Check this by generating count 
egen nresidents = total(age >= 18), by(household_id) 
replace nresidents = . if care_home != 1 

gen flag = (nresidents != household_size)

* might be different for households with children, but should be the same in care homes
replace flag = . if care_home != 1 

tab flag, m

summarize nresidents, detail
summarize household_size, detail 

* Flag for number of care homes in data
* Flag first occurence of care home, then count number of flags (by table)
bysort household_id: gen care_flag = _n == 1 
* Missing if not care home 
replace care_flag = . if care_home != 1 

* Number of care home residents in data
* Want one entry per care home, so that this can be summed by geographic region
gen care_count_size = household_size
* Missing if not care home and if not first entry per care home
replace care_count_size = . if care_flag == . 

* Counter for number of GP practices per carehome 
* Generate a flag for each unique combination of practice and household
* This will create a flag = 1 for each unique practice within each household 
egen tag = tag(practice_id household_id)
* Sum the number of unique practices, by household 
egen npractices = total(tag), by(household_id)
* Missing if not care home 
replace npractices = . if care_home != 1

tab npractices 

* Count number of care homes and patients by GP practices covering the care home 
* Can't figure out a way to do this without creating a carehome level dataset 
* I do not want to to print anything for levels of households that are not care homes 
* Repeat this when printing the table below 

preserve 
drop if care_home != 1 

levelsof npractices, local(levels)

foreach l of local levels {
  count if care_flag == 1 if npractices == `l'
  display r(N)
  summarize care_count_size if npractices == `l'
  display r(sum)
 }

restore 

* Format and sense check outcome variables 

foreach var of varlist 	ons_covid_death_date ///
						ons_death_date		///
						ons_covid_death_date_main ///
						first_pos_test_sgss ///
						{
						
	confirm string variable `var'
	rename `var' `var'_dstr
	gen `var' = date(`var'_dstr, "YMD")
	drop `var'_dstr
	format `var' %td 
	
}

gen ons_death_any = (ons_death_date		< .)
gen ons_death_covid_main = (ons_covid_death_date_main		< .)
gen ons_death_covid = (ons_covid_death_date		< .)
gen sgss_test_pos = (first_pos_test_sgss		< .)


/* Programs for outputting tables=============================================== 
   from K Baskharan, generic code to output one row of table as txt file 
   
   This can probably be improved because the set-up is a little awkward
   The first program prints a single row for each level of a cat var 
   The second program then repeates the row for each level of another 
   cat var 
   
   This set up is a little awkward; longer term I'm working on improving this 
   into a more general framework for generating descriptive tables. 

*******************************************************************************/
cap prog drop generaterow
program define generaterow
syntax, variable(varname) [level(string)] [condition0(string)] [condition1(string)] [condition2(string)] [condition3(string)]
	
	if ("`level'" != "") { 
			local vlab: label `variable' `level'
			file write tablecontent ("`vlab'") _tab 
			}
	else {
			file write tablecontent ("missing") _tab
			}
	
	quietly cou `condition0'
	local overalldenom=r(N)
	quietly cou if `variable' `condition2' `condition3'
	local colpct = 100*(r(N)/`overalldenom')
	file write tablecontent %9.0gc (r(N))  (" (") %3.1f (`colpct') (")") _tab

	quietly cou if care_home_type == 0 `condition1'
	local rowdenom = r(N)
	quietly cou if care_home_type == 0 & `variable' `condition2' `condition3'
	local pct = 100*(r(N)/`rowdenom') 
	file write tablecontent %9.0gc (r(N)) (" (") %3.1f (`pct') (")") _tab

	quietly cou if care_home_type == 1 `condition1'
	local rowdenom = r(N)
	quietly cou if care_home_type == 1 & `variable' `condition2' `condition3'
	local pct = 100*(r(N)/`rowdenom')
	file write tablecontent %9.0gc (r(N)) (" (") %3.1f  (`pct') (")") _tab

	quietly cou if care_home_type == 2 `condition1'
	local rowdenom = r(N)
	quietly cou if care_home_type == 2 & `variable' `condition2' `condition3'
	local pct = 100*(r(N)/`rowdenom')
	file write tablecontent %9.0gc (r(N)) (" (") %3.1f  (`pct') (")") _tab
	
	quietly cou if care_home_type == 3 `condition1'
	local rowdenom = r(N)
	quietly cou if care_home_type == 3 & `variable' `condition2' `condition3'
	local pct = 100*(r(N)/`rowdenom')
	file write tablecontent %9.0gc (r(N)) (" (") %3.1f  (`pct') (")") _tab


	quietly cou if care_home_type >= . `condition1'
	local rowdenom = r(N)
	quietly cou if care_home_type >= . & `variable' `condition2' `condition3'
	local pct = 100*(r(N)/`rowdenom')
	file write tablecontent %9.0gc (r(N)) (" (") %3.1f (`pct') (")") _n
	
end

********************************************************************************
* Generic code to output one section (varible) within table (calls above)

cap prog drop tabulatevariable
prog define tabulatevariable
syntax, variable(varname) min(real) max(real) [missing] [outcome(string)]

	local lab: variable label `variable'
	file write tablecontent ("`lab'") _n 

	if ("`outcome'" != "") { 
		
		forvalues varlevel = `min'/`max'{ 
				generaterow, variable(`variable') level(`varlevel') condition0("if `variable' ==`varlevel'") ///
																	condition1("& `variable' ==`varlevel'")  ///
																	condition2("==`varlevel'")  			 ///
																	condition3("& `outcome' == 1")
		}			
		if "`missing'"!="" generaterow, variable(`variable') condition2(">=.")
	} 
	
	else { 
		
		forvalues varlevel = `min'/`max'{ 
				generaterow, variable(`variable') level(`varlevel') condition2("==`varlevel'") 
		}	
		if "`missing'"!="" generaterow, variable(`variable') condition2(">=.")
	} 
	
end

********************************************************************************

/* Feasibility Table 1========================================================*/ 
tab agegroup care_home_type, m col

cap file close tablecontent
file open tablecontent using ./$outdir/table1.txt, write text replace

file write tablecontent ("Table 1: Frequency tabulation of OpenSafely population by age and category of place of residence at 1 January 2020") _n

* Exposure labels for columns 

local lab0: label care_home_type 0
local lab1: label care_home_type 1
local lab2: label care_home_type 2 
local lab3: label care_home_type 3 
local labu: label care_home_type .u

file write tablecontent _tab ("Total")  _tab ///
							 ("`lab0'") _tab ///
							 ("`lab1'") _tab ///
							 ("`lab2'") _tab ///
							 ("`lab3'") _tab ///
							 ("`labu'") _n 

tabulatevariable, variable(agegroup) min(1) max(11) missing 

file close tablecontent

/* Feasibility Table 2========================================================*/ 
bysort agegroup: tab ons_death_any care_home_type, m col 

cap file close tablecontent
file open tablecontent using ./$outdir/table2.txt, write text replace

file write tablecontent ("Table 2: Frequency tabulation of OpenSafely deaths (all causes) occurring up until latest date available by age and category of place of residence") _n

* Exposure labels for columns 

local lab0: label care_home_type 0
local lab1: label care_home_type 1
local lab2: label care_home_type 2 
local lab3: label care_home_type 3 
local labu: label care_home_type .u

file write tablecontent _tab ("Total")  _tab ///
							 ("`lab0'") _tab ///
							 ("`lab1'") _tab ///
							 ("`lab2'") _tab ///
							 ("`lab3'") _tab ///
							 ("`labu'") _n 

tabulatevariable, variable(agegroup) min(1) max(11) missing outcome("ons_death_any")

/* Feasibility Table 3========================================================*/ 
bysort agegroup: tab ons_death_covid care_home_type, m col 

cap file close tablecontent
file open tablecontent using ./$outdir/table3.txt, write text replace

file write tablecontent ("Table 3: Frequency tabulation of OpenSafely deaths (COVID-19) occurring up until latest date available by age and category of place of residence") _n

* Exposure labels for columns 

local lab0: label care_home_type 0
local lab1: label care_home_type 1
local lab2: label care_home_type 2 
local lab3: label care_home_type 3 
local labu: label care_home_type .u

file write tablecontent _tab ("Total")  _tab ///
							 ("`lab0'") _tab ///
							 ("`lab1'") _tab ///
							 ("`lab2'") _tab ///
							 ("`lab3'") _tab ///
							 ("`labu'") _n 

tabulatevariable, variable(agegroup) min(1) max(11) missing outcome("ons_death_covid")

file close tablecontent

/* Feasibility Table 4========================================================*/ 
*  Note, current SGSS data received not reliable, propose swapping to other outcome
bysort agegroup: tab sgss_test_pos care_home_type, m col 

cap file close tablecontent
file open tablecontent using ./$outdir/table4.txt, write text replace

file write tablecontent ("Table 4: Frequency tabulation of OpenSafely test positive cases for COVID-19 occurring up until latest date available by age and category of place of residence") _n

* Exposure labels for columns 

local lab0: label care_home_type 0
local lab1: label care_home_type 1
local lab2: label care_home_type 2 
local lab3: label care_home_type 3 
local labu: label care_home_type .u

file write tablecontent _tab ("Total")  _tab ///
							 ("`lab0'") _tab ///
							 ("`lab1'") _tab ///
							 ("`lab2'") _tab ///
							 ("`lab3'") _tab ///
							 ("`labu'") _n 

tabulatevariable, variable(agegroup) min(1) max(11) missing outcome("sgss_test_pos")

file close tablecontent

/* Feasibility Table 5========================================================*/ 
*  Not using programs above as different structure required of tables 

* Table initiation 

cap file close tablecontent
file open tablecontent using ./$outdir/table5.txt, write text replace

file write tablecontent ("Table 5: Number of care homes and people resident in them by NHS region for OpenSafely population at 1 January 2020") _n 

file write tablecontent ("NHS Region") _tab ("Number of Care Homes") _tab ("Number of Residents") _n

cap prog drop countbyregion
prog define countbyregion
syntax, variable(varname) region(string) variable2(varname)

file write tablecontent ("`region'") _tab 
quietly count if `variable' == 1 & region == "`region'" 
file write tablecontent %9.0gc (r(N)) _tab 
quietly summarize `variable2' if region == "`region'"
file write tablecontent %9.0gc (r(sum)) _n 

end 

countbyregion, variable(care_flag) region("East Midlands") variable2(care_count_size)
countbyregion, variable(care_flag) region("East of England") variable2(care_count_size)
countbyregion, variable(care_flag) region("London") variable2(care_count_size)
countbyregion, variable(care_flag) region("North East") variable2(care_count_size)
countbyregion, variable(care_flag) region("North West") variable2(care_count_size)
countbyregion, variable(care_flag) region("South East") variable2(care_count_size)
countbyregion, variable(care_flag) region("West Midlands") variable2(care_count_size)
countbyregion, variable(care_flag) region("Yorkshire and the Humber") variable2(care_count_size)

file close tablecontent

/* Feasibility Table 6========================================================*/ 
*  Not feasible due to large number (100s) of MSOA 


/* Feasibility Table 7========================================================*/ 

cap file close tablecontent
file open tablecontent using ./$outdir/table7.txt, write text replace

file write tablecontent ("Number of care homes and people resident in them according to GP practices covering each care home ") _n 

file write tablecontent ("Number of GP Practices") _tab ("Number of Care Homes") _tab ("Number of Residents") _n

preserve 
drop if care_home != 1 

levelsof npractices, local(levels)

foreach l of local levels {
  file write tablecontent (`l') _tab
  count if care_flag == 1 & npractices == `l'
  display r(N)
  file write tablecontent %9.0gc (r(N)) _tab 
  summarize care_count_size if npractices == `l'
  display r(sum)
  file write tablecontent %9.0gc (r(sum)) _n 
 }

restore 

file close tablecontent


/* Feasibility Table 8========================================================*/ 
* Not feasible due to missing mixedsoftware variable 

* Close log file 
log close

