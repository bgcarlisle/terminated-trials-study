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
  - The `duration_of_trial` function calculates the number of trial days participants until termination, referred to as "trial_days."
  - The `degree_of_enrollment` function calculates the percentage of enrollment achieved for a particular terminated trial, known as "enrollment_percentage."

## Output
The output dataset generated by this repository includes the following variables for each terminated trial:

| variable          |  description|
|-------------------|-------------|
| nctid             |  Trial ID |
| start_date        |  Date when the trial was initiated |
| stop_date         |  Date when the trial's overall status was first updated to "Terminated" in the registry |
| why_stopped       |  Reason for trial termination |
| has_summary_result_ctgov |  Boolean indicating the availability of summary results on ClinicalTrials.gov for the trial |
| anticipated_enrollment | Expected number of participants that the trial aimed to enrol |
| actual_enrollment      | Observed number of participants actually enrolled |
| trial_days |  Number of days trial was ongoing until termination |
| enrollment_percentage | Percentage of enrollment achieved until termination|

## Formula
trial_days = stop_date - start_date

# Part 2 (dataset specific)
## Description (summary result reporting)
Bi-directional cross-registration checks will be performed using two approaches:

1. Cross-registration from ClinicalTrials.gov to EUCTR: The IntoValue dataset will be used to identify cross-registrations between the ctgov (ClinicalTrials.gov) registry and the EUCTR (European Union Clinical Trials Register) registry. The [cross-registration](https://github.com/maia-sh/intovalue-data/blob/main/data/processed/registries/registry-crossreg.rds) dataset will be utilized to find EUCTR cross-registrations for the corresponding NCT IDs.

2. Cross-registration from EUCTR to ctgov: EUCTR IDs associated with German UMC leads will be extracted from a [JSON dataset](https://raw.githubusercontent.com/ebmdatalab/euctr-tracker-data/master/all_trials.json). The dataset will further be analyzed to identify additional identifiers in the protocol and result pages of the EUCTR registry. The `combine_info function`, sourced from a forked version of the ["euctrscrape"](https://github.com/delwen/euctrscrape) repository by Delwen Franzen and originally created by BG Carlisle, will be used for this purpose. From the extracted additional identifiers, NCT IDs found in the original IntoValue dataset will be identified, thus obtaining cross-registered data from both sources.

Once the cross-registered data is obtained, the EUCTR IDs will be checked to determine if the corresponding trials had results posted in the registry using the `get_euctr_result_link.R` function.

## Input
The EUCTR trial IDs sourced from bi-directional cross-registration checks 

## Dependencies
1. The forked version of the 'euctrscrape' package is utilised to find additional identifiers in the protocol and results section of EUCTR ids and these identifiers are matched to nctids in the original dataset.
2. `get_euctr_result_link.R` function: This function retrieves the result link if the result were posted for EUCTR entry, otherwise returns 'No result found'.

## Output
The output variable generated for each ClinicalTrails.gov registered trial:
| variable          |  description|
|-------------------|-------------|
| nctid | Trial ID  |
| crossreg_euctr_ids| Cross-registered EUCTR ids for ClinicalTrials.gov registered trial |                
| is_crossreg_euctr | Boolean indicating whether or not the 'nctid' has cross-registration in EUCTR |
| has_summary_result_euctr | Boolean indicating the availability of summary results on the EUCTR registry for the trial |
| link|link to available result section page |




# References
Carlisle BG. Analysis of clinical trial registry entry histories using the novel R package cthist. PLoS One. 2022;17(7):e0270909. [https://doi.org/10.1371/journal.pone.0270909](https://doi.org/10.1371/journal.pone.0270909)


