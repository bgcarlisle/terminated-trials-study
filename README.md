# terminated-trials-study
# Overview
This repository focuses on processing [ClinicalTrials.gov](https://clinicaltrials.gov/) registered trials with a "Terminated" status, aiming to assess and provide insights for such trials.

# Part 1
## Description
This part focuses on processing historical entries related to ClinicalTrials.gov registered terminated trials and relies on ['cthist'](https://github.com/bgcarlisle/cthist) package developed by [BG Carlisle](https://github.com/bgcarlisle).

## Input
The dataset used for processing should include trial ID  (nctid) of ClinicalTrials.gov registered trials with recruitment status ('recruitment_status') specified as "Terminated" in the registry.

## Dependencies: 
1. The 'cthist' package is utilised to obtain historical entries for the terminated trials
2. Two functions are employed in the script:
  - The `duration_of_enrollment` function calculates the number of days participants were involved in a specific terminated trial, referred to as "patient_days."
  - The `degree_of_enrollment` function calculates the percentage of enrollment achieved for a particular terminated trial, known as "enrollment_percentage."

## Output
The output dataset generated by this repository includes the following variables for each terminated trial:

| variable          |  description|
|-------------------|-------------|
| nctid             |  Trial ID
| start_date        |  Date when the trial was initiated |
| stop_date         |  Date when the trial's overall status was first updated to "Terminated" in the registry |
| why_stopped       |  Reason for trial termination |
| has_summary_result_ctgov |  Boolean indicating the availability of summary results on ClinicalTrials.gov for the trial |
| anticipated_enrollment | Expected number of participants that the trial aimed to enrol |
| actual_enrollment      | Observed number of participants actually enrolled |
| patient_days |  Number of days participants were involved in the trial until termination |
| enrollment_percentage | Percentage of enrollment achieved until termination|

# Part 2 (under work)
## Description
The dataset generated in part 1 is further assessed to find summary result reporting in the [EUCTR registry](https://www.clinicaltrialsregister.eu/). A set of EUCTR trials completed within the same time frame is generated (this method can change based on the dataset). The script relies on a function `euctr_reg_identifiers` [(link)](https://github.com/delwen/euctrscrape/blob/main/R/euctr_reg_identifiers.R) developed in  the forked version of the ["euctrscrape"](https://github.com/delwen/euctrscrape/tree/main/R) repository by [Delwen Franzen](https://github.com/delwen) and originally made by [BG Carlisle](https://github.com/bgcarlisle).

## Input
The EUCTR trial IDs for finding cross-registered trials 

## Dependencies
1. The forked version of the 'euctrscrape' package is utilised to find additional identifiers in the protocol and results section of EUCTR ids and these identifiers are matched to nctids in the original dataset.
2. `get_euctr_result_link.R` function: This function retrieves the result link if the result were posted for EUCTR entry, otherwise returns 'No result found' (is represented by 'has_summary_result_euctr" variable in output dataset).

## Output
The output variable generated for each ClinicalTrails.gov registered trials 
| variable          |  description|
|-------------------|-------------|
| nctid             |  Trial ID   |
| is_crossreg_euctr | Boolean indicating whether or not the 'nctid' has cross-registration in EUCTR |
|   crossreg_euctr_ids| Cross-registered EUCTR ids for ClinicalTrials.gov registered trial |                |
| has_summary_result_euctr | Boolean indicating the availability of summary results on the EUCTR registry for the trial |



# Part 3 (under work)
## Description
The script checks the available  summary result section for cross-registered EUCTR ids to extract information related to 'serious adverse events'.

## Input
The 'crossreg_euctr_ids' having a summary result section.

## Dependencies
`get_reported_events_euctr.R` function: This function retrieves the adverse events section for a EUCTR entry with a result section. 

## Output
| variable          |  description|
|-------------------|-------------|
|   crossreg_euctr_ids| Cross-registered EUCTR ids for ClinicalTrials.gov registered trial |    
| has_adverse_events | Boolean indicating the availability of summary results on the EUCTR registry for the trial|



# References
Carlisle BG. Analysis of clinical trial registry entry histories using the novel R package cthist. PLoS One. 2022;17(7):e0270909. [https://doi.org/10.1371/journal.pone.0270909](https://doi.org/10.1371/journal.pone.0270909)


