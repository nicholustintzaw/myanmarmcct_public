/*******************************************************************************
Project Name		: 	MCCT Baseline Kayin + Kayah
Purpose				:	re-do weight calculation based on VI's r file - weights.R
Author				:	Nicholus Tint Zaw
Date				: 	7/09/2022
Modified by			:

*******************************************************************************/

********************************************************************************
** Directory Settings **
********************************************************************************

** Settings for stata ** 
clear all
set more off
set mem 100m
set matsize 11000
set maxvar 32767

********************************************************************************
***SET ROOT DIRECTORY HERE AND ONLY HERE***

// create a local to identify current user
local user = c(username)
di "`user'"

// Set root directory depending on current user
if "`user'" == "Nicholus Tint Zaw" {
    * Nicholus Directory
	
	global dir		"C:/Users/Nicholus Tint Zaw/Documents/GitHub/myanmarmcct_public"
	
}

// Adam, please update your machine directory 
else if "`user'" == "XX" {
    * Adam Directory
	

}

// Ramaa
// pls replicate below `else if' statement based on number of user going to use this analysis dofile  
else if "`user'" == "XX" {
    * CPI team Directory
	
}

global	raw		"$dir/data/stata_dta"
global 	dta		"$raw/data/merged_dta"
global	clean	"$dir/data/cleaning_info"

********************************************************************************
********************************************************************************
** import population data **
import delimited using "$dir/data-raw/pop/popMyanmar.csv", varn(1) clear 

rename ïstate_region state_region
rename geo_locationgeo_rural geo_rural

/*
Nicholus: this part is not included in the R file. The geo_ward_vt_eho alone is 
not a unique var, but the combination of these three variables make unique obs - 
geo_state geo_ward_vt_eho geo_rural. That is the improvement I made from VI's R file 
to fix the duplicated geographical units used for the whole weight calculation process.  
*/

gen geo_state = substr(geo_locationgeo_town, 1, 6)
tostring geo_rural, replace 

distinct geo_state geo_ward_vt_eho geo_rural, joint 

preserve 
keep geo_state geo_ward_vt_eho geo_rural population

distinct geo_ward_vt_eho
distinct geo_state geo_ward_vt_eho geo_rural, joint 


tempfile x
save `x', replace 
restore 


** import HH data **
preserve
use "$raw/hh.dta", clear // 2681 in r package hh.data file, this STATA cleaned file had 2680 

destring sample_component, replace 
keep if sample_component == 1

keep geo_state geo_rural geo_villward 

distinct geo_villward

gen geo_ward_vt_eho = geo_villward

distinct geo_state geo_ward_vt_eho geo_rural, joint

tempfile y 
save `y', replace 
restore 

** creat the dataset to calculate weights **
preserve
use "$raw/hh.dta", clear 

destring sample_component, replace 
keep if sample_component == 1

distinct geo_villward

gen geo_ward_vt_eho = geo_villward

tempfile hhWeight 
save `hhWeight', replace 
restore 


preserve
use `x', clear

merge 1:m geo_state geo_ward_vt_eho geo_rural using `y' // improved with three variable combination as match key 

keep if _merge != 1
drop _merge 

/*
>> this is VI's R file matching appraoch 
. merge 1:m geo_ward_vt_eho using `y'
(note: variable geo_ward_vt_eho was str15, now str28 to accommodate using data's values)

    Result                           # of obs.
    -----------------------------------------
    not matched                           859
        from master                        61  (_merge==1)
        from using                        798  (_merge==2)

    matched                             1,356  (_merge==3)
    -----------------------------------------

*/

keep geo_ward_vt_eho population geo_state geo_rural 

tempfile z 
save `z', replace 
restore 


preserve
use `x', clear

merge 1:m geo_state geo_ward_vt_eho geo_rural using `hhWeight' // improved with three variable combination as match key 

keep if _merge != 1
drop _merge 

/*
>> this is VI's R file matching appraoch 
. merge 1:m geo_ward_vt_eho using `hhWeight'
(note: variable geo_ward_vt_eho was str15, now str28 to accommodate using data's values)

    Result                           # of obs.
    -----------------------------------------
    not matched                           859
        from master                        61  (_merge==1)
        from using                        798  (_merge==2)

    matched                             1,356  (_merge==3)
    -----------------------------------------

*/

tempfile hhWeight 
save `hhWeight', replace 
restore 

** Because some geo units have missing population data, they were replaced with median values. **
// ### get median population size of clusters in a state
use `z', clear  

sum population if geo_state == "MMR002", d
replace population = `r(p50)' if mi(population) & geo_state == "MMR002"  

sum population if geo_state == "MMR003", d
replace population = `r(p50)' if mi(population) & geo_state == "MMR003"  

tempfile z 
save `z', replace 

** Perform population data calculation for each geographical stratum - Kayin/Kayan and Rural/Urban/Hard-to-reach ** 
// ### get weights for MMR002 and geo_rural == 0 (Rural)
preserve 

keep if geo_state == "MMR002" & geo_rural == "0"
keep geo_state geo_rural geo_ward_vt_eho population
distinct geo_ward_vt_eho
distinct geo_state geo_ward_vt_eho geo_rural, joint 
/*
----------------------------------
           |     total   distinct
-----------+----------------------
 (jointly) |       316         45
----------------------------------
in r-weight file, it was only 43 uniques cluster and the total pop was 80195 
in this work, total unique cluster was 45 and total pop was 83925 
*/

bysort geo_ward_vt_eho: keep if _n == 1
egen totalPop = total(population)

tempfile z1 
save `z1', replace 
restore


// ### get weights for MMR002 and geo_rural == 1 (Urban)
preserve 

keep if geo_state == "MMR002" & geo_rural == "1"
keep geo_state geo_rural geo_ward_vt_eho population
distinct geo_ward_vt_eho
distinct geo_state geo_ward_vt_eho geo_rural, joint 

/*
----------------------------------
           |     total   distinct
-----------+----------------------
 (jointly) |       340         32
----------------------------------

in r-weight file, it was only 29 uniques cluster and the total pop was 78569 
in this work, total unique cluster was 32 and total pop was 84164 
*/

bysort geo_ward_vt_eho: keep if _n == 1
egen totalPop = total(population)

tempfile z2 
save `z2', replace 
restore

// ### get weights for MMR002 and geo_rural == 2 (EHO)
preserve 

keep if geo_state == "MMR002" & geo_rural == "2"
keep geo_state geo_rural geo_ward_vt_eho population
distinct geo_ward_vt_eho
distinct geo_state geo_ward_vt_eho geo_rural, joint 
/*
----------------------------------
           |     total   distinct
-----------+----------------------
 (jointly) |       370         36
----------------------------------

same uniaue obs and pop with r file: 36 uniques cluster and the total pop was 73416 
*/

bysort geo_state geo_rural geo_ward_vt_eho population: keep if _n == 1
egen totalPop = total(population)

tempfile z3 
save `z3', replace 
restore

// ### get weights for MMR003 and geo_rural == 0 (Rural)
preserve 

keep if geo_state == "MMR003" & geo_rural == "0"
keep geo_state geo_rural geo_ward_vt_eho population
distinct geo_ward_vt_eho
distinct geo_state geo_ward_vt_eho geo_rural, joint 
/*
----------------------------------
           |     total   distinct
-----------+----------------------
 (jointly) |       346         34
----------------------------------
same uniaue obs and pop with r file: 34 uniques cluster and the total pop was 89896 
*/

bysort geo_ward_vt_eho: keep if _n == 1
egen totalPop = total(population)

tempfile z4 
save `z4', replace 
restore

// ### get weights for MMR003 and geo_rural == 1 (Urban)
preserve 

keep if geo_state == "MMR003" & geo_rural == "1"
keep geo_state geo_rural geo_ward_vt_eho population
distinct geo_ward_vt_eho
distinct geo_state geo_ward_vt_eho geo_rural, joint 
/*
----------------------------------
           |     total   distinct
-----------+----------------------
 (jointly) |       426         27
----------------------------------
in r-weight file, it was only 22 uniques cluster and the total pop was 209273 
in this work, total unique cluster was 27 and total pop was 222493 
*/

bysort geo_ward_vt_eho: keep if _n == 1
egen totalPop = total(population)

tempfile z5 
save `z5', replace 
restore


// ### get weights for MMR003 and geo_rural == 2 (EHO)
preserve 

keep if geo_state == "MMR003" & geo_rural == "2"
keep geo_state geo_rural geo_ward_vt_eho population
distinct geo_ward_vt_eho
distinct geo_state geo_ward_vt_eho geo_rural, joint 
/*
----------------------------------------
                 |     total   distinct
-----------------+----------------------
 geo_ward_vt_eho |       356         40
----------------------------------------
in r-weight file, it was only 38 uniques cluster and the total pop was 115873 
in this work, total unique cluster was 40 and total pop was 121161 
*/

bysort geo_ward_vt_eho: keep if _n == 1
egen totalPop = total(population)

tempfile z6 
save `z6', replace 
restore

** combined all cluster weight files (population) **
use `z1', clear 
append using `z2' `z3' `z4' `z5' `z6' // 202 obs in r-file, 214 in this STATA work 

distinct geo_state geo_ward_vt_eho geo_rural, joint

// ### Kayin pop - 1055359; Kayah pop - 286627
/*
Nicholus: I am not sure what was the reference source for the below replacement 
values. I am just using the replacement cluster size number from the R package file. 
It may be the number of available clusters from sample frames in each geographical stratum.  
*/

gen nClusters = .m 
replace nClusters = 24 if geo_state == "MMR002" & geo_rural == "0"
replace nClusters = 24 if geo_state == "MMR002" & geo_rural == "1"
replace nClusters = 26 if geo_state == "MMR002" & geo_rural == "2"
replace nClusters = 27 if geo_state == "MMR003" & geo_rural == "0"
replace nClusters = 17 if geo_state == "MMR003" & geo_rural == "1"
replace nClusters = 27 if geo_state == "MMR003" & geo_rural == "2"

tempfile zz 
save `zz', replace 


preserve
use `z', clear 

/*
Nicholus - this is VI's approach 
bysort geo_ward_vt_eho: gen size = _N
bysort geo_ward_vt_eho: keep if _n == 1
*/ 

// improved above code with combination of three geo var (which provide distinct uunique geo unit)
bysort geo_state geo_ward_vt_eho geo_rural: gen size = _N
bysort geo_state geo_ward_vt_eho geo_rural: keep if _n == 1

tab size, m 

distinct geo_ward_vt_eho
distinct geo_state geo_ward_vt_eho geo_rural, joint

tempfile clusterSize 
save `clusterSize', replace 

restore 

use `zz', clear
 
distinct geo_ward_vt_eho 
distinct geo_state geo_ward_vt_eho geo_rural, joint

merge m:1 geo_state geo_ward_vt_eho geo_rural using `clusterSize'

/*
Nicholus: I did not understand why only geo_ward_vt_eho was used to merge with 
the cluster size dataset in VI's R file calculation. This is the main issue of 
how the same geo unit has multiple weights in the VI weight dataset. Below is 
the VI r code - it matched all the obs if applied 1:m merging but the population 
data will come from 187 unique geo_ward_vt_eho obs not from 214 obs - which were 
the true unique obs defined by the state, stratum, and village tract/ward unit

. merge 1:1 geo_ward_vt_eho using `clusterSize'
*/

drop _merge 

// cluster weight 
gen cweight = (population * nClusters) / totalPop

// individual weight 
gen iweight = size / population

// final weight 
gen weights = 1 / (cweight * iweight)

// Match with HH Dataset - testing 
preserve
// merge with hh dataset with only sample_component == 1 data 
// working all id from weight dataset  
merge 1:m geo_state geo_ward_vt_eho geo_rural using `hhWeight' // note the unique key was combination of variables 

restore 

save "$raw/svy_weight_STATA.dta", replace 


********************************************************************************
// CHECK the calculated weight with with R-weight dataset 
********************************************************************************

distinct geo_ward_vt_eho // duplicate cases detected
distinct geo_state geo_ward_vt_eho geo_rural, joint 

rename weights weights_stata 

merge 1:1 geo_state geo_ward_vt_eho geo_rural using  "$raw/svy_weight.dta"

gen diff = weights - weights_stata
sum diff,d 

/*
The main improvements made in this new weight calculation in STATA were identifying 
uique geographical units in the population and dataset matching calculation. 
The result of the newly calculated weight had a huge difference for some geographical units. 
*/

