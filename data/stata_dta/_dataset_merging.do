/*******************************************************************************
Project Name		: 	MCCT Baseline Kayin + Kayah
Purpose				:	data mergning and geo infomration cleaning		
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
	
	global dir		"C:/Users/Nicholus Tint Zaw/Documents/GitHub/myanmarmcct_public/data"
	
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

global	raw		"$dir/stata_dta"
global 	dta		"$raw/merged_dta"
global	clean	"$dir/cleaning_info"

********************************************************************************
* import household data *
********************************************************************************

use "$raw/hh.dta", clear

* test - for data cleaning with STATA work

rename cal_respid cal_respid_r
gen instanceID = key

merge 1:1 instanceID using "$clean\household_main.dta", keepusing(cal_respid)

/*

    Result                           # of obs.
    -----------------------------------------
    not matched                            22
        from master                        16  (_merge==1)
        from using                          6  (_merge==2)

    matched                             2,664  (_merge==3)
    -----------------------------------------

The 16 obs from hh dataset should be drop during the data cleaning process. 
but VI r-function fail to do it. See below note for data cleaning. 

Drop if "geo_vill" is "168652" and the "sample_component" variable is "1". Because 
this village was duplicated between two strata, "Rural" and "Hard to Reach Area". 
Finally, CPI decided to keep for "Hard to Reach Area" and to drop survey forms with the "Rural" stratum	  
*/

keep if _merge == 3

drop _merge 
 
* merge with HH level indicators module *

// rcis - coping strategies //
merge 1:1 key 	using "$raw/rcsi.dta", ///
				keepusing(b1 b2 b3 b4 b5 b1w b2w b3w b4w b5w b_total_weighted)

keep if _merge == 3
drop _merge

// lcis - coping strategies //
merge 1:1 key 	using "$raw/lcsi.dta", ///
				keepusing(l1 l2 l5 l6 l7 l8 l9 l11 l12 l14 count_stress ///
				count_crisis count_emergency secure stress crisis emergency)

keep if _merge == 3
drop _merge

// PPI //
merge 1:1 key 	using "$raw/ppiDf.dta", ///
				keepusing(ppi_score nl100 wealth_quintile)

keep if _merge == 3
drop _merge

// ADD Survey weight - using weight data calculated by STATA not from r-package // 
gen geo_ward_vt_eho = geo_villward

distinct geo_state geo_ward_vt_eho geo_rural, joint

merge m:1 geo_state geo_ward_vt_eho geo_rural using "$raw/svy_weight_STATA.dta"

tab _merge sample_component 

/*

    Result                           # of obs.
    -----------------------------------------
    not matched                           241
        from master                       241  (_merge==1)
        from using                          0  (_merge==2)

    matched                             2,423  (_merge==3)
    -----------------------------------------

	
Nicholus: 
Those unmatched observations have come from survey component 2. Survey component 1 
is for cross-sectional design and 2 for RDD design; both have different sampling 
procedures. But VI did not calculate the weight for component 2 (check in the 
"weights.R" file and my note in the STATA weight calculation file). But, not all 
component 2 observations were unmatched in this merging (HH vs. weight dataset). 
And I am not sure why. We need to use only component 1 or both in this secondary analysis. 
*/

drop _merge 

rename key 		parent_key 

tempfile hhupdated
save `hhupdated', replace

save "$dta/hh_ppi_csi.dta", replace

clear

********************************************************************************
* merge with mother health module data *
********************************************************************************
// only use the past pregnancy data as it have all ANC-delivery and PNC infomration

use  "$raw/anc2.dta", clear 

preserve
use "$raw/anc_past.dta", clear

rename key 		parent_key 
rename key_y 	key

tempfile anc_past
save `anc_past', replace

restore

merge 1:m key using `anc_past'

keep if _merge == 3
drop _merge

preserve
use "$raw/delivery.dta", clear

rename key 		parent_key 
rename key_y 	key

tempfile delivery
save `delivery', replace

restore

merge 1:m key using `delivery'
keep if _merge == 3
drop _merge

preserve
use "$raw/pnc.dta", clear

rename key 		parent_key 
rename key_y 	key

tempfile pnc
save `pnc', replace

restore

merge 1:m key using `pnc'
keep if _merge == 3
drop _merge

gen hhroster_id = women_id_pregpast

tempfile mother_module
save `mother_module', replace 

merge m:1 parent_key using `hhupdated'

drop if _merge == 1 
// it is possible that some mothers will not found at HH data becuase we perform 
// data clenaing (like drop some error observation) at HH data only and those mothers
// might be come from those dropped obs HH

save "$dta/hh_mother_health.dta", replace

clear 

********************************************************************************
* merge with child health module data *
********************************************************************************
use  "$raw/chealth.dta", clear

rename key 		parent_key 
rename key_y 	key 
drop parent_key_x

preserve
use "$raw/child_health_all.dta", clear

rename key 		parent_key 
rename key_y 	key

tempfile chealth_all
save `chealth_all', replace

restore

merge 1:1 key using `chealth_all'

keep if _merge == 3
drop _merge

gen hhroster_id = child_id_health

tempfile child_non_anthro
save `child_non_anthro', replace 

merge m:1 parent_key using `hhupdated'

drop if _merge == 1 
// same rationale as mother dataset mearging result 

drop _merge 

save "$dta/hh_child_health.dta", replace 

********************************************************************************
* merge with child anthro module data *
********************************************************************************

use "$raw/child_anthro_all.dta", clear

gen instanceID = instance_id 

rename respid cal_respid_r

merge m:1 instanceID using "$clean\anthro_main.dta", keepusing(cal_respid)

/*

    Result                           # of obs.
    -----------------------------------------
    not matched                            32
        from master                        26  (_merge==1)
        from using                          6  (_merge==2)

    matched                             3,498  (_merge==3)
    -----------------------------------------
those 26 might come from the dropped observations from 16 HH in hh dataset
*/
keep if _merge == 3
drop _merge 


tempfile anthro_update
save `anthro_update', replace
clear


use `hhupdated', clear
distinct cal_respid_r cal_respid

merge 1:m cal_respid	using `anthro_update', keepusing(dob age sex muac ///
						height weight oedema position waz haz whz flag flag_description ///
						global_haz moderate_haz severe_haz global_waz moderate_waz ///
						severe_waz gam_whz mam_whz sam_whz gam_muac mam_muac sam_muac)


		
/*

    Result                           # of obs.
    -----------------------------------------
    not matched                           229
        from master                       143  (_merge==1)
        from using                         86  (_merge==2)

    matched                             3,412  (_merge==3)
    -----------------------------------------

*/

keep if _merge == 3
drop _merge 

save "$dta/hh_child_anthro.dta", replace 

/*
********************************************************************************
* Combined HH + Mom + Child (non-anthro data) *
********************************************************************************

use `mother_module', clear

distinct key 
count if mi(key)


merge m:m parent_key using `child_non_anthro'

&&&

parent_key hhroster_id
use `child_non_anthro', clear

distinct key 
count if mi(key)
&&


// use hh dataset with ppi, csi and weight 
use  "$dta/hh_ppi_csi.dta", clear 

// merge with mom module dataset  
merge 1:m parent_key using `mother_module'

drop if _merge == 2 
drop _merge 

// merge with mom dataset 
merge 1:1 key using `child_non_anthro'
&&&

*/
