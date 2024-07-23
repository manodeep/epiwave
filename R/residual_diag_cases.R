#' scaled residual quantile diagnostic plots using DHARMa package
#'
#' @param cases_posterior
#' @param cases_true
#'
#' @return
#' @export
#'
#' @examples
residual_diag_cases <- function(cases_posterior,
                                cases_true) {

  cases_posterior <- t(cases_posterior[[1]][,,1])

  cases_posterior_median <- apply(cases_posterior,1,median)

  DHARMa_obj <- DHARMa::createDHARMa(simulatedResponse = cases_posterior,
                             observedResponse = cases_true,
                             fittedPredictedResponse = cases_posterior_median,
                             integerResponse = TRUE)
  DHARMa:::plot.DHARMa(DHARMa_obj)

}
