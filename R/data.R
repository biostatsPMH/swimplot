#' Clinical Trial: Adverse events
#'
#' A dataset containing the adverse event information from a simulated clinical trial.
#'
#' @format A data frame with 11 rows and 6 variables:
#' \describe{
#'   \item{id}{Patient id}
#'   \item{time}{Time of an adverse event (AE)}
#'   \item{event}{Type of adverse event (AE)}
#'   \item{Sex}{Patient Sex}
#'   \item{Age}{Age of patient at trial entry date}
#'   \item{Related}{Likelihood the treatment is related to the adverse event}
#'
#' }
"ClinicalTrial.AE"

#' Clinical Trial: Treatment
#'
#'A dataset containing the treatment arm information from a simulated clinical trial.
#'
#' @format A data frame with 53 rows and 6 variables:
#' \describe{
#' \item{id}{Patient id}
#'   \item{Arm}{Treatment Arm}
#'   \item{End_trt}{Time since enrollment to the end of treatment, in months}
#'   \item{Continued_treatment}{Continued treatment past end of follow up}
#'   \item{Sex}{Patient Sex}
#'   \item{Age}{Age of patient at trial entry date}
#'
#' }
"ClinicalTrial.Arm"

#' Clinical Trial: Response
#'
#' A dataset containing the response information from a simulated clinical trial.
#'
#' @format A data frame with 36 rows and 7 variables:
#' \describe{
#'   \item{id}{Patient id}
#'   \item{Response_start}{Time of starting response, in months since enrollment}
#'   \item{Response_end}{Time of ending response, in months since enrollment}
#'   \item{Response}{Type of response, CR = Complete response, and PR = Partial response}
#'   \item{Continued_response}{Continued response past end of follow up}
#'   \item{Sex}{Patient Sex}
#'   \item{Age}{Age of patient at trial entry date}
#'
#' }
"ClinicalTrial.Response"

#' Clinical Trial: Stage
#'
#' A dataset containing the Stage information from a simulated clinical trial.
#'
#' @format A data frame with 36 rows and 2 variables:
#' \describe{
#'   \item{id}{Patient id}
#'   \item{Stage}{Patients clinical stage at enrollment of the study (either Early Stage or Late Stage)}
#'
#' }
"ClinicalTrial.Stage"

#' Adverse events
#'
#' A dataset containing adverse events from a simulated clinical trial.
#'
#' @format A data frame with 87 rows and 7 variables:
#' \describe{
#'   \item{SUBJECT}{Patient id}
#'   \item{COURSE_NUM}{Treatment course cycle from which adverse event occurred}
#'   \item{EVENT}{Treatment course cycle from which adverse event occurred}
#'   \item{ARM}{Treatment arm for participant}
#'   \item{SITE}{Hospital site for participant}
#'   \item{SEX}{Patient sex}
#'   \item{AGE}{Patient age at AE onset date}
#' }
"ae"

#' Treatment course
#'
#' A dataset containing treatment course from a simulated clinical trial.
#'
#' @format A data frame with 47 rows and 7 variables:
#' \describe{
#'   \item{SUBJECT}{Patient id}
#'   \item{ARM}{Treatment arm for participant}
#'   \item{SITE}{Hospital site for participant}
#'   \item{SEX}{Patient sex}
#'   \item{AGE}{Patient age at AE onset date}
#'   \item{END_TRT}{Last treatment course cycle for patient}
#'   \item{CONTINUED_TREATMENT}{Denotes if patient continued treatment after last treatment cycle on study}
#' }
"arm"

#' Response
#'
#' A dataset containing observed responses in treatment course cycles from a simulated clinical trial.
#'
#' @format A data frame with 67 rows and 9 variables:
#' \describe{
#'   \item{SUBJECT}{Patient id}
#'   \item{ARM}{Treatment arm for participant}
#'   \item{SITE}{Hospital site for participant}
#'   \item{RESPONSE_START}{Treatment course cycle denoting start of response}
#'   \item{RESPONSE_END}{Treatment course cycle denoting end of response}
#'   \item{RESPONSE}{Observed response for patient at treatment course cycle}
#'   \item{RESPONSE}{Observed response for patient at treatment course cycle}
#'   \item{CONTINUED_RESPONSE}{Denotes if response was continued after last observation at treatment course cycle}
#'   \item{AGE}{Patient age at treatment course cycle}
#' }
"res"
