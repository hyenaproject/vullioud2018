#' Social support drives female dominance in the spotted hyena.
#'
#' This R package gives the tools to reproduce the main statistical analysis of
#' the article: Social support drives female dominance in the spotted hyena.
#'
#' This package has not been conceived for general use!
#'
#' In the examples below, we provide the workflow leading the main results
#' presented in the paper.
#'
#'
#' @note The statistics (AIC, Tjurs'D and logLik) of the models will looks
#' slightly different than in the article because the examples given
#' in this script used models fitted with method = PQL similar as the models
#' used for the predictions. However, the statistics presented in the manuscript
#' used models fitted with method = PQL/L. The correct models can however be
#' computed if necessary, by setting the option: fit_method = "PQL/L, in the
#' corresponding functions.
#'
#' @name vullioud2018
#' @aliases vullioud2018
#' @docType package
#' @keywords package
#' @examples
#'
#'
#'##########################################
#'###  Basic information about the data  ###
#'##########################################
#'
#'### The main data set contains 4258 one-on-one interactions with clear dominance
#'### display. It contains all the variables needed to run the models and
#'### the different analysis presented in the manuscript:
#'### - the IDs of both interacting parties (focal, other),
#'### - the Date and time of the interaction (DT),
#'### - the difference in social support (predictor_NEW),
#'### - the difference in body-mass (delta_weight),
#'### - the sexes of the interacting parties (sex),
#'### - the outcome for the focal individual (win),
#'### - the birth order between the 2 individuals or their (A_B)
#'### - the residency status for the two interacting individuals (resid_f, resid_o)
#'
#'data(d1)
#'head(d1)
#'
#'
#'#########################################
#'###  Creating the different datasets  ###
#'#########################################
#'
#'### As each set of models requires a different choice of focal individuals,
#'### the datasets have to be reshaped. Moreover, as models are fitted
#'### separately for intra-sex and inter-sex interactions, different datasets
#'### must also be computed.
#'
#'### First, we subset the main dataset between intra and inter-sex interactions
#'
#'data_same_sex <- prepare_same_sex(d1)
#'data_diff_sex <- prepare_different_sex(d1)
#'
#'### Additionally, we create a subset for the models in Supplementary Materials
#'### testing the interactions in which the focal individual is a migrant 
#'### with higher social support in interclan interactions.
#'
#'data_resid <- prepare_resid(d1)
#'
#'### Each model relies on a different dataset: they are created as follow:
#'
#'### SOCIAL SUPPORT MODELS: higest social support as focal
#'
#'same_sex_social <- create_DF_social(data_same_sex)
#'diff_sex_social <- create_DF_social(data_diff_sex)
#'resid_social <- create_DF_social(data_resid)
#'
#'### BODY MASS MODELS: highest body mass as focal
#'
#'diff_sex_weight <- create_DF_weight(data_diff_sex)
#'same_sex_weight <- create_DF_weight(data_same_sex)
#'
#'### SEX MODEL females as focal:
#'
#'diff_sex_sex <- create_DF_sex(data_diff_sex)
#'
#'
#'################
#'###  MODELS  ###
#'################
#'
#'### As the models take a long time to fit we only present here the null
#'### model fits used for the main text and for the plots. All models can be fitted
#'### with the adequate function by changing the argument fit_method and model,
#'### please look at the help file for the specific functions for
#'### more information. The five models presented below are readily available in
#'### as companion data of this package.
#'
#'\dontrun{
#'### SOCIAL SUPPORT MODELS:
#'
#'mod_social_null_diff_PQL <- fit_social(fit_method = "PQL",
#'                                       model = "null",
#'                                       DF1 = diff_sex_social)
#'
#'mod_social_null_same_PQL <- fit_social(fit_method = "PQL",
#'                                       model = "null",
#'                                       DF1 = same_sex_social)
#'
#'
#'### BODY MASS MODELS:
#'
#'mod_mass_null_same_PQL <- fit_body_mass(fit_method = "PQL",
#'                                        model = "null",
#'                                        DF1 = same_sex_weight)
#'
#'mod_mass_null_diff_PQL <- fit_body_mass(fit_method = "PQL",
#'                                        model = "null",
#'                                        DF1 = diff_sex_weight)
#'
#'### SEX MODELS:
#'
#'mod_sex_null_diff_PQL <- fit_sex(fit_method = "PQL",
#'                                 model = "null",
#'                                 DF1 = diff_sex_sex)
#'
#'### To access the models stored in the package, run:
#'
#'data(mod_social_null_diff_PQL)
#'data(mod_social_null_same_PQL)
#'data(mod_mass_null_diff_PQL)
#'data(mod_mass_null_same_PQL)
#'data(mod_sex_null_diff_PQL)
#'
#'}
#'
#'
#'####################
#'###  STATISTICS  ###
#'####################
#'
#'### We created four wrapping functions to access the main statistics used in the
#'### manuscript.
#'
#'### NOTE: Be aware that the statistics presented in the manuscript were
#'### based on models fitted with option fit_method = "PQL/L" and the example
#'### presented here are based on models fitted with fit_method = "PQL". The
#'### results are thus slightly different.
#'
#'
#'### - get_predictions() returns the predictions and the 95%
#'### confidence interval around those predictions:
#'
#'data(mod_social_null_diff_PQL)
#'
#'\dontrun{
#'get_predictions(mod_social_null_diff_PQL)
#'}
#'
#'### - get_AIC() returns the marginal AIC of the model:
#'
#'x <- get_AIC(mod_social_null_diff_PQL, name = "mod_social_null_diff_PQL")
#'as.data.frame(x)
#'
#'### - get_TJUR() returns the Tjur's coefficient of discrimination D:
#'
#'x2 <- get_TJUR(mod_social_null_diff_PQL, name = "mod_social_null_diff_PQL")
#'as.data.frame(x2)
#'
#'###  get_logLik() returns the log likelihood of the model:
#'
#'x3 <- get_logLik(mod_social_null_diff_PQL, name = "mod_social_null_diff_PQL")
#'as.data.frame(x3)
#'
#'
#'###############
#'###  PLOTS  ###
#'###############
#'### we plot here the two plots of the main text.
#'
#'### PLOT1: the first plots display the predictions of the five main models.
#'
#'data(mod_social_null_diff_PQL)
#'data(mod_social_null_same_PQL)
#'data(mod_mass_null_diff_PQL)
#'data(mod_mass_null_same_PQL)
#'data(mod_sex_null_diff_PQL)
#'
#'plot1(mod_social_null_diff_PQL,
#'      mod_social_null_same_PQL,
#'      mod_mass_null_diff_PQL,
#'      mod_mass_null_same_PQL,
#'      mod_sex_null_diff_PQL, 
#'      PDF = TRUE)
#'      
#'### PLOT2: the second plot represents the relatedness at different times.
#'
#'data(females_relat)
#'data(migrants_relat)
#'data(natives_relat)
#'
#'plot2(DF_female = females_relat,
#'      DF_migrant = migrants_relat,
#'      DF_native = natives_relat, PDF = TRUE)
#'
NULL
