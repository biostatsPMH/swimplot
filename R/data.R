#' Clinical Trial: Adverse events
#'
#' A dataset containing the adverse event information from a simulated clinical trial
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
#'A dataset containing the treatment arm information from a simulated clinical trial
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
#' A dataset containing the response information from a simulated clinical trial
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
