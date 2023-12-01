library(tidyverse)
library(testthat)
library(cthist)

## Read Intovalue NCT's from disk
intovalue <- read_csv("intovalue_raw.csv") %>%
    select(id) %>%
    rename(nctid = id) %>%
    mutate(ctg = grepl("^NCT[0-9]{8}$", nctid)) %>%
    filter(ctg) %>%
    select(nctid)

## Read Cali NCT's from disk
cali <- read_delim("California-trials_2014-2017_exp.csv", ";") %>%
    select(id) %>%
    rename(nctid = id) %>%
    mutate(ctg = grepl("^NCT[0-9]{8}$", nctid)) %>%
    filter(ctg) %>%
    select(nctid)

## Put them together into the same data frame and remove dupes
nctids <- intovalue %>%
    bind_rows(cali) %>%
    distinct(nctid)

test_that(
    "All NCT numbers have been copied to the new data frame with no extras",
{
    ## Number of NCT's in the intovalue df that are not found in
    ## the nctids df
    intovalue_nf <- intovalue %>%
        mutate(found = nctid %in% nctids$nctid) %>%
        filter(! found) %>%
        nrow()
    
    ## Number of NCT's in the cali df that are not found in the
    ## nctids df
    cali_nf <- cali %>%
        mutate(found = nctid %in% nctids$nctid) %>%
        filter(! found) %>%
        nrow()
    
    ## Number of NCT's in the nctids df that are not in one of the
    ## original df's
    nctids_nf <- nctids %>%
        mutate(found = nctid %in% intovalue$nctid | nctid %in% cali$nctid) %>%
        filter(! found) %>%
        nrow()

    ## Check for duplicates
    dups <- nctids %>%
        mutate(dup = duplicated(nctid)) %>%
        filter(dup) %>%
        nrow()
    
    ## If any of the above are greater than 0, that's a problem
    expect_equal(
        intovalue_nf + cali_nf + nctids_nf + dups,
        0
    )
})

## Turn NCT numbers into a list
nctids <- nctids %>%
    pull(nctid)

## Download historical versions
clinicaltrials_gov_download(nctids, "hv.csv")
