#' @title duration_of_trials
#'
#' @description
#' 
#' This function calculates the duration of trial in days for each row
#' of a data frame, based on the provided start and stop date columns.
#' It handles different date formats and provides a warning if the
#' format of the dates has changed.
#' 
#' @param df The data frame containing enrolment data.
#'
#' @param start_date_col Character string. The column name indicating
#'     start start date of the trial.
#'
#' @param stop_date_col Character string. The column name indicating
#'     date on which its corresponding clinical trial record's overall
#'     status was first changed to "Terminated" from any other overall
#'     status in the ClinicalTrials.gov registry.
#'
#' @param inclusive Boolean TRUE or FALSE. If TRUE, it adds one to the
#'     number of trial days calculated to include the last day. This
#'     would mean that a trial with a start date on the 1st of the
#'     month and a stop date on the 31st of the month would have a
#'     duration of 31 days (31-1+1, default behaviour). If FALSE,
#'     nothing is added to the number of trial days calculated. This
#'     would mean that a trial with a start date on the 1st of the
#'     month and a stop date on the 31st of the month would have a
#'     duration of 30 days only (31-1+0).
#'
#' @return The original input data frame with an additional column
#'     named 'trial_days' representing the duration of trial in
#'     days. If missing values are encountered in either the start or
#'     stop date columns, the 'trial_days' value for that row will be
#'     set to "Check missing values".
#'
#' @examples
#'
#' df <- duration_of_trial(df, "start_date", "stop_date")

duration_of_trial <- function(df, start_date_col, stop_date_col,
                              inclusive = TRUE) {

    ## Throw an error if df isn't a data frame
    assertthat::assert_that(
        is.data.frame(df),
        msg="Argument provided (df) is not a valid data frame"
    )

    assertthat::assert_that(
        assertthat::has_name(df, start_date_col),
        msg="Start date column not found"
    )

    assertthat::assert_that(
        assertthat::has_name(df, stop_date_col),
        msg="Stop date column not found"
    )
    
    assertthat::assert_that(
        ! assertthat::has_name(df, "trial_days"),
        msg="The trial_days column already exists"
    )

    assertthat::assert_that(
        is.logical(inclusive),
        msg="The inclusive argument must be TRUE or FALSE"
    )

    ## Create the "trial_days" column and initialize with NA
    df$trial_days <- NA
    
    for (i in 1:nrow(df)) {
        start_date <- df[[start_date_col]][i]
        stop_date <- df[[stop_date_col]][i]
        
        if (is.na(start_date) || is.na(stop_date)) {
            df$trial_days[i] <- "Check missing values"
        } else {
            ## Check the format of start_date and stop_date
            if (! inherits(start_date, "Date")) {
                start_date <- lubridate::parse_date_time(
                                       start_date,
                                       orders = c("dmy", "mdy", "ymd")
                                     )
            } else {
                start_date <- "Date parsing unsuccessful"
            }
            
            if (! inherits(stop_date, "Date")) {
                stop_date <- lubridate::parse_date_time(
                                       stop_date,
                                       orders = c("dmy", "mdy", "ymd")
                                     )
            } else {
                stop_date <- "Date parsing unsuccessful"
            }
            
            df$trial_days[i] <- as.integer(stop_date - start_date) +
                as.integer(inclusive)
        }
    }
    
    return(df)
}
