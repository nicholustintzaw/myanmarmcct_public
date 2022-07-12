/*******************************************************************************
Project Name		: 	MCCT Baseline Kayin + Kayah
Purpose				:	mapping reporting indicator		
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
* import household + csi + ppi data *
********************************************************************************

use "$dta/hh_ppi_csi.dta", clear 

/*
consumption based coping strategies score
b_total_weighted

livelihood based coping strategies index - hh condition 
secure 
stress 
crisis 
emergency

ppi socre:					 ppi_score 
ppi based wealth quantile: 	wealth_quintile

*/


********************************************************************************
* hh + health module data *
********************************************************************************
use  "$dta/hh_mother_health.dta", clear 

/*
ANC visit - from all type of health care provider
at lest one visit: 	anc1 
at least 4 visits:	anc4

if you want to re-calculate it, pls use the following variable 
specialist_visit doctor_visit nurse_visit ha_visit pdoc_visit lhv_visit midwife_visit amw_visit tba_visit chw_visit ehw_visit


Delivery - you may need to calculate the indicator you want to use based on following variables
## Where delivered?
## Where: home
birth1a
## Where: government hospital
birth1b
## Where: private doctor/clinic
birth1c 
## Where: SRHC/RHC
birth1d 
## Where: Routine ANC location in village/ward
birth1e 
## Where: EHO clinic
birth1f 

## Why choose delivery place?
birth2 
## Why: convenience
birth2a 
## Why: tradition
birth2b 
## Why: close distance
birth2c 
## Why: safety for mother/baby
birth2d 
## Why: affordable cost
birth2e 

## Who assisted in delivery?
birth3 
## Who: doctor
birth3a 
## Who: nurse
birth3b 
## Who: LHV
birth3c 
## Who: midwife
birth3d 
## Who: auxilliary midwife
birth3e 
## Who: traditional birth attendant
birth3f 
## Who: on my own
birth3g 
## Who: relatives
birth3h 
## Who: EHO cadres
birth3i 

## Delivery assisted by a skilled birth attendant
birth3 

## Delivery method
birth4 
## method: normal
birth4a 
## method: caesarian
birth4b 
## method: vacuum
birth4c 
## method: forceps
birth4d 

PNC 
## PNC?
pnc1 

## PNC within 2 days of delivery
pnc2a 
## Who did PNC check?
pnc3 

## Who: doctor
pnc3a 
## Who: nurse
pnc3b 
## Who: LHV
pnc3c 
## Who: midwife
pnc3d 
## Who: auxilliary midwife
pnc3e 
## Who: traditional birth attendant
pnc3f 
## Who: relatives
pnc3g 
## Who: EHO cadres
pnc3h 

## Frequency of doctor PNC check
pnc4a 
## Frequency of nurse PNC check
pnc4b 
## Frequency of lhv PNC check
pnc4c 
## Frequency of midwife PNC check
pnc4d 
## Frequency of auxilliary midwife PNC check
pnc4e 
## Frequency of traditional birth attendant PNC check
pnc4f 
## Frequency of relative PNC check
pnc4g 
## Frequency of eho PNC check
pnc4h 

*/


********************************************************************************
* hh + child health module data *
********************************************************************************
use "$dta/hh_child_health.dta", clear  

/*
## Any vaccination
vac1 
## immunisation card retention
vac2 
  
immunization - age appropriate 
individual immunization
age_bcg age_hep_b age_penta1 age_penta2 age_penta3 age_polio1 age_polio2 age_polio3 age_polio_inj age_measles1 age_measles2 age_rubella 

complete immunization
age_imm

## Vitamin A supplementation
vita 
## Deworming
worm 
  
Childhood illness - you may need to caluclate the indicator based on the following variables

for Diarrhea
  ## Had diarrhoea?
  dia1 
  ## Advice or treatment for diarrhoea?
  dia2 
  
  ## Where advice sought?
  ## Where: Township hospital
  dia5a 
  ## Where: Station hospital
  dia5b 
  ## Where: RHC/Health assistant
  dia5c 
  ## Where: SRHC/Midwife
  dia5d 
  ## Where: Private clinic/doctor
  dia5e 
  ## Where: Community health worker
  dia5f 
  ## Where: Traditional healer
  dia5g 
  ## Where: Untrained health worker
  dia5h
  ## Where: Drug from shop
  dia5i 
  ## Where: EHO clinics/volunteers
  dia5j 
  ## Where: Family members
  dia5k 
  ## Where: NGOs/clinics
  dia5l 
  ## Where: auxilliary midwife
  dia5m 

For Cough/ARI
  ## Had cough/ARI?
  ari1
  ## Advice or treatment for cough/ARI?
  ari2 
  
  ## Where advice sought?
  ## Where: Township hospital
  ari5a 
  ## Where: Station hospital
  ari5b 
  ## Where: RHC/Health assistant
  ari5c 
  ## Where: SRHC/Midwife
  ari5d 
  ## Where: Private clinic/doctor
  ari5e 
  ## Where: Community health worker
  ari5f 
  ## Where: Traditional healer
  ari5g 
  ## Where: Untrained health worker
  ari5h 
  ## Where: Drug from shop
  ari5i 
  ## Where: EHO clinics/volunteers
  ari5j 
  ## Where: Family members
  ari5k 
  ## Where: NGOs/clinics
  ari5l 
  ## Where: auxilliary midwife
  ari5m 
  
For fever
  ## Had fever?
  fev1 
  ## Advice or treatment for fever?
  fev2 
  
  ## Where advice sought?
  ## Where: Township hospital
  fev5a 
  ## Where: Station hospital
  fev5b 
  ## Where: RHC/Health assistant
  fev5c 
  ## Where: SRHC/Midwife
  fev5d 
  ## Where: Private clinic/doctor
  fev5e 
  ## Where: Community health worker
  fev5f 
  ## Where: Traditional healer
  fev5g 
  ## Where: Untrained health worker
  fev5h 
  ## Where: Drug from shop
  fev5i 
  ## Where: EHO clinics/volunteers
  fev5j 
  ## Where: Family members
  fev5k 
  ## Where: NGOs/clinics
  fev5l 
  ## Where: auxilliary midwife
  fev5m 
*/


********************************************************************************
* hh + anthro module data *
********************************************************************************

use "$dta/hh_child_anthro.dta", clear  


/*
child demographic data
age sex 

anthro measurement 
muac height weight oedema position 

z-score
waz haz whz 

stunting - HAZ
global_haz moderate_haz severe_haz 

wasting - WHZ
gam_whz mam_whz sam_whz 

Underweight - WAZ 
global_waz moderate_waz severe_waz 

wasting by MUAC
gam_muac mam_muac sam_muac

*/