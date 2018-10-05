#' Predictions
#'
#'Gets the predictions of the models (no newdata needed). It uses the
#'spaMM::predict method which compute, in addition to the fitted value
#'the 95 wald CI.
#'
#'@name get_predictions
#'@param model the model for which we want the predictions
#'@return a DF with prediction and 95 confidence interval
#'@export
#'@examples
#' data(mod_sex_null_diff_PQL)
#' get_predictions(mod_sex_null_diff_PQL)
#' 
get_predictions <- function(model) {
  p <- spaMM::predict.HLfit(model, intervals = "predVar", re.form = NA)
  y <- model$data
  y$pred <- p[,1]
  y$inf <- attr(p, "intervals")[,1]
  y$sup <- attr(p, "intervals")[,2]
  y$mod <- attr(model$fixef_terms, "term.labels")[1]
  return(y)
}
################################################################################
#' AIC
#'
#'Wrapper around the spaMM::AIC method.
#'
#'@note Returns a tibble so be aware of the auto-
#'matic rounding.
#'
#'@name get_AIC
#'@param model model to be used
#'@param name name of the model
#'@return a tibble: be aware of the automatic rounding
#'@export
#'@examples
#'data(mod_social_null_diff_PQL)
#'get_AIC(mod_social_null_diff_PQL, "social_null_diff_PQL")
#'
get_AIC <- function(model, name) {
  dplyr::bind_cols(name = name,
                   AIC = spaMM::AIC.HLfit(model)[1])

}

################################################################################
#'Tjur D
#'
#'Compute the Tjur's coefficient of discrimination D.
#'
#'@name  get_TJUR
#'@param model model to be used
#'@param name name of the model
#'@export
#'@examples
#'data(mod_sex_null_diff_PQL)
#'get_TJUR(mod_sex_null_diff_PQL, "sex_null_PQL")
get_TJUR <- function(model, name) {
  data <- model$data
  data$pred <- spaMM::predict.HLfit(model, re.form = NA)[, 1]
  out <- data.frame(name = name,
                    D_tjur = mean(ifelse(data$win, data$pred, 1- data$pred)) -
                             mean(ifelse(data$win == F, data$pred, 1- data$pred)))
  return(out)
}


################################################################################
#'log likelihood
#'
#'Wrapper arround logLik.
#'
#'@note Returns a tibble, so be aware of the automatic rounding.
#'
#'@name  get_logLik
#'@param model model to be used
#'@param name name of the model
#'@export
#'@examples
#'data(mod_sex_null_diff_PQL)
#'get_logLik(mod_sex_null_diff_PQL, "sex_null_PQL")
#'
get_logLik <- function(model, name) {
  dplyr::bind_cols(name = name,
            loglik = spaMM::logLik.HLfit(model))
}
