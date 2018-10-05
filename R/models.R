#' Create the correlation matrix (used internally)
#'
#Create the correlation matrix used in the models.
#'
#'@name buildcorrsigned
#'@param id1 focal id
#'@param id2 other id
#'@param type type of interaction
#'@export
#'@return a list with pairsID, correlation Matrix, and decomposed matrices

buildcorrsigned <- function(id1, id2, type) {
  if(any(as.character(id1) == as.character(id2)))
    stop("id1 and id2 are sometimes the same")
  id1 <- paste(type, id1, sep = ":")
  id2 <- paste(type, id2, sep = ":")
  pairs <- paste(id1, id2, sep="_")
  u_pairs <- unique(pairs)
  E <- expand.grid(id1 = u_pairs, id2 = u_pairs)
  AB <- strsplit(as.character(E$id1), "_")
  E$A <- unlist(lapply(AB, function(i) i[1]))
  E$B <- unlist(lapply(AB, function(i) i[2]))
  CD <- strsplit(as.character(E$id2), "_")
  E$C <- unlist(lapply(CD, function(i) i[1]))
  E$D <- unlist(lapply(CD, function(i) i[2]))
  E$U <- apply(E, 1, function(i) length(unique(i[c("A", "B", "C", "D")])))
  E$Corr <- NA
  E$Corr[(E$U <= 2) & (E$A == E$C)] <- 1
  E$Corr[(E$U <= 2) & (E$A == E$D)] <- -1
  E$Corr[E$U == 3 & ((E$A == E$C)|(E$B == E$D))] <- 0.5
  E$Corr[E$U == 3 & ((E$A == E$D)|(E$B == E$C))] <- -0.5
  E$Corr[E$U == 4] <- 0
  E$A <- E$B <- E$C <- E$D <- NULL
  M <- matrix(E$Corr, ncol=length(u_pairs), nrow=length(u_pairs))
  rownames(M) <- colnames(M) <- u_pairs

  return(list(corrM = M, pairsID = pairs))
}

################################################################################
################################################################################
#'Fit social support models
#'
#'Fits the models when the social support is focal.
#'
#'@name  fit_social
#'@param fit_method "PQL" or" PQL/L" in quotes
#'@param model "full", "nomass", "nosex", "null" in quotes
#'@param DF1 dataset to use: same_sex_social or diff_sex_social
#'@return a list (fitted model with spaMM)
#'@import spaMM
#'@export
#'@examples
#'\dontrun{
#'data(same_sex_social)
#'data(diff_sex_social)
#'
#'mod_social_full_same_PQL <- fit_social(fit_method = "PQL",
#'                                       model = "full",
#'                                       DF1 = same_sex_social)
#'mod_social_full_diff_PQL <- fit_social(fit_method = "PQL",
#'                                       model = "full",
#'                                       DF1 = diff_sex_social)
#'
#'mod_social_full_same_PQLL <- fit_social(fit_method = "PQL/L",
#'                                        model = "full",
#'                                        DF1 = same_sex_social)
#'mod_social_full_diff_PQLL <- fit_social(fit_method = "PQL/L",
#'                                        model = "full",
#'                                        DF1 = diff_sex_social)
#'                                        
#'mod_social_nomass_same_PQL <- fit_social(fit_method = "PQL",
#'                                         model = "nomass",
#'                                         DF1 = same_sex_social)
#'mod_social_nomass_diff_PQL <- fit_social(fit_method = "PQL",
#'                                         model = "nomass",
#'                                         DF1 = diff_sex_social)
#'
#'mod_social_nomass_same_PQLL <- fit_social(fit_method = "PQL/L",
#'                                          model = "nomass",
#'                                          DF1 = same_sex_social)
#'mod_social_nomass_diff_PQLL <- fit_social(fit_method = "PQL/L",
#'                                          model = "nomass",
#'                                          DF1 = diff_sex_social)
#'
#'mod_social_nosex_same_PQL <- fit_social(fit_method = "PQL",
#'                                        model = "nosex",
#'                                        DF1 = same_sex_social)
#'mod_social_nosex_diff_PQL <- fit_social(fit_method = "PQL",
#'                                        model = "nosex",
#'                                        DF1 = diff_sex_social)
#'
#'mod_social_nosex_same_PQLL <- fit_social(fit_method = "PQL/L",
#'                                         model = "nosex",
#'                                         DF1 = same_sex_social)
#'mod_social_nosex_diff_PQLL <- fit_social(fit_method = "PQL/L",
#'                                         model = "nosex",
#'                                         DF1 = diff_sex_social)
#'
#'mod_social_null_same_PQL <- fit_social(fit_method = "PQL",
#'                                       model = "null",
#'                                       DF1 = same_sex_social)
#'mod_social_null_diff_PQL <- fit_social(fit_method = "PQL",
#'                                       model = "null",
#'                                       DF1 = diff_sex_social)
#'
#'mod_social_null_same_PQLL <- fit_social(fit_method = "PQL/L",
#'                                        model = "null",
#'                                        DF1 = same_sex_social)
#'mod_social_null_diff_PQLL <- fit_social(fit_method = "PQL/L",
#'                                        model = "null",
#'                                        DF1 = diff_sex_social)
#'                              }
fit_social <- function(fit_method, model, DF1){
  corr.obj <- buildcorrsigned(DF1$focal, DF1$other, DF1$type)
  DF1$pairsID <- corr.obj$pairsID
  data_mod <- DF1
  data_mod$win_social <- DF1$win

  if (model == "full") {
    
    spaMM::fitme(win_social ~ type * (sex + body_mass_bin) + corrMatrix(1|pairsID),
                 corrMatrix = corr.obj$corrM,
                 family = "binomial", data = data_mod,
                 method = paste(fit_method),
                 control.HLfit = list(LevenbergM = TRUE),
                 verbose = c(TRACE = 1L))
    
  } else if (model == "nosex") {
    
    spaMM::fitme(win_social ~ type * body_mass_bin + corrMatrix(1|pairsID),
                 corrMatrix = corr.obj$corrM,
                 family = "binomial", data = data_mod,
                 method = paste(fit_method),
                 control.HLfit = list(LevenbergM = TRUE),
                 verbose = c(TRACE = 1L))
    
  } else if (model == "nomass") {
    
    spaMM::fitme(win_social ~ type * sex + corrMatrix(1|pairsID),
                 corrMatrix = corr.obj$corrM,
                 family = "binomial", data = data_mod,
                 method = paste(fit_method),
                 control.HLfit = list(LevenbergM = TRUE),
                 verbose = c(TRACE = 1L))
    
  } else if (model == "null") {
    
    spaMM::fitme(win_social ~ type + corrMatrix(1|pairsID),
                 corrMatrix = corr.obj$corrM,
                 family = "binomial", data = data_mod,
                 method = paste(fit_method),
                 control.HLfit = list(LevenbergM = TRUE),
                 verbose = c(TRACE = 1L))
    
  } else {
    
    stop("not a valid model argument")
    
  }
}

################################################################################
#' Fit body mass models
#'
#'Fits the models when the heaviest individual is focal.
#'
#'@name  fit_body_mass
#'@param fit_method "PQL" or "PQL/L" in quotes
#'@param model "full", "nosocial", "nosex", "null" in quotes
#'@param DF1 dataset to use: same_sex_weight or diff_sex_weight
#'@return a list (fitted model with spaMM)
#'@export
#'@examples
#'\dontrun{
#'data(same_sex_weight)
#'data(diff_sex_weight)
#'
#'mod_mass_full_same_PQL <- fit_body_mass(fit_method = "PQL",
#'                                        model = "full",
#'                                        DF1 = same_sex_weight)
#'mod_mass_full_diff_PQL <- fit_social(fit_method = "PQL",
#'                                     model = "full",
#'                                     DF1 = diff_sex_weight)
#'
#'mod_mass_full_same_PQLL <- fit_social(fit_method = "PQL/L",
#'                                      model = "full",
#'                                      DF1 = same_sex_weight)
#'mod_mass_full_diff_PQLL <- fit_social(fit_method = "PQL/L",
#'                                      model = "full",
#'                                      DF1 = diff_sex_weight)
#'
#'mod_mass_nosocial_same_PQL <- fit_social(fit_method = "PQL",
#'                                         model = "nosocial",
#'                                         DF1 = same_sex_weight)
#'mod_mass_nosocial_diff_PQL <- fit_social(fit_method = "PQL",
#'                                         model = "nosocial",
#'                                         DF1 = diff_sex_weight)
#'
#'mod_mass_nosocial_same_PQLL <- fit_social(fit_method = "PQL/L",
#'                                          model = "nosocial",
#'                                          DF1 = same_sex_weight)
#'mod_mass_nosocial_diff_PQLL <- fit_social(fit_method = "PQL/L",
#'                                          model = "nosocial",
#'                                          DF1 = diff_sex_weight)
#'
#'mod_mass_nosex_same_PQL <- fit_social(fit_method = "PQL",
#'                                      model = "nosex",
#'                                      DF1 = same_sex_weight)
#'mod_mass_nosex_diff_PQL <- fit_social(fit_method = "PQL",
#'                                      model = "nosex",
#'                                      DF1 = diff_sex_weight)
#'
#'mod_mass_nosex_same_PQLL <- fit_social(fit_method = "PQL/L",
#'                                       model = "nosex",
#'                                       DF1 = same_sex_weight)
#'mod_mass_nosex_diff_PQLL <- fit_social(fit_method = "PQL/L",
#'                                       model = "nosex",
#'                                       DF1 = diff_sex_weight)
#'
#'mod_mass_null_same_PQL <- fit_social(fit_method = "PQL",
#'                                     model = "null",
#'                                     DF1 = same_sex_weight)
#'mod_mass_null_diff_PQL <- fit_social(fit_method = "PQL",
#'                                     model = "null",
#'                                     DF1 = diff_sex_weight)
#'
#'mod_mass_null_same_PQLL <- fit_social(fit_method = "PQL/L",
#'                                      model = "null",
#'                                      DF1 = same_sex_weight)
#'mod_mass_null_diff_PQLL <- fit_social(fit_method = "PQL/L",
#'                                      model = "null",
#'                                      DF1 = diff_sex_weight)
#' }

fit_body_mass <- function(fit_method, model, DF1) {
  corr.obj <- buildcorrsigned(DF1$focal, DF1$other, DF1$type)
  DF1$pairsID <- corr.obj$pairsID
  
  data_mod <- DF1
  data_mod$win_heavy <- data_mod$win
  
  if (model == "full") {
  
  spaMM::fitme(win_heavy ~ type * (sex + social_sup_bin) + corrMatrix(1|pairsID),
               corrMatrix = corr.obj$corrM,
               family = "binomial", data = data_mod,
               method = paste(fit_method),
               control.HLfit = list(LevenbergM = TRUE),
               verbose = c(TRACE = 1L))
  
  } else if (model == "nosex") {
  
  spaMM::fitme(win_heavy ~ type * social_sup_bin + corrMatrix(1|pairsID),
               corrMatrix = corr.obj$corrM,
               family = "binomial", data = data_mod,
               method = paste(fit_method),
               control.HLfit = list(LevenbergM = TRUE),
               verbose = c(TRACE = 1L))
  
  } else if (model == "nosocial") {
  
  spaMM::fitme(win_heavy ~ type * sex + corrMatrix(1|pairsID),
               corrMatrix = corr.obj$corrM,
               family = "binomial", data = data_mod,
               method = paste(fit_method),
               control.HLfit = list(LevenbergM = TRUE),
               verbose = c(TRACE = 1L))
    
  } else if (model == "null"){
  
  spaMM::fitme(win_heavy ~ type + corrMatrix(1|pairsID),
               corrMatrix = corr.obj$corrM,
               family = "binomial", data = data_mod,
               method = paste(fit_method),
               control.HLfit = list(LevenbergM = TRUE),
               verbose = c(TRACE = 1L))
    
  } else {
    
  stop("not a valid model argument")

  }
}

################################################################################
#' Fit the models when the female is focal
#'
#'this function fits the models when the female is focal.
#'
#'@name  fit_sex
#'@param fit_method "PQL" or "PQL/L" in quotes
#'@param model "full", "nosocial", "nomass", "null" in quotes
#'@param DF1 dataset to use: diff_sex_sex
#'@return a list (fitted model with spaMM)
#'@export
#'@examples
#'\dontrun{
#'data(diff_sex_sex)
#'mod_sex_full_diff_PQL <- fit_sex(fit_method = "PQL",
#'                                 model = "full",
#'                                 DF1 = diff_sex_sex)
#'mod_sex_full_diff_PQLL <- fit_sex(fit_method = "PQL/L",
#'                                  model = "full",
#'                                  DF1 = diff_sex_sex)
#'
#'mod_sex_nosocial_diff_PQL <-fit_sex(fit_method = "PQL",
#'                                    model = "nosocial",
#'                                    DF1 = diff_sex_sex)
#'mod_sex_nosocial_diff_PQLL <- fit_sex(fit_method = "PQL/L",
#'                                      model = "nosocial",
#'                                      DF1 = diff_sex_sex)
#'
#'mod_sex_nomass_diff_PQL <- fit_sex(fit_method = "PQL",
#'                                   model = "nomass",
#'                                   DF1 = diff_sex_sex)
#'mod_sex_nomass_diff_PQLL <- fit_sex(fit_method = "PQL/L",
#'                                    model = "nomass",
#'                                    DF1 = diff_sex_sex)
#'
#'mod_sex_null_diff_PQL <- fit_sex(fit_method = "PQL",
#'                                 model = "null",
#'                                 DF1 = diff_sex_sex)
#'mod_sex_null_diff_PQLL <- fit_sex(fit_method = "PQL/L",
#'                                  model = "null",
#'                                  DF1 = diff_sex_sex)
#' }
#'
fit_sex <- function(model, fit_method, DF1){
  corr.obj <- buildcorrsigned(DF1$focal, DF1$other, DF1$type)
  DF1$pairsID <- corr.obj$pairsID
  data_mod <- DF1
  data_mod$win_female <- data_mod$win
  
  if (model == "full"){
    
  spaMM::fitme(win_female ~ type * (social_sup_bin + body_mass_bin) + corrMatrix(1|pairsID),
               corrMatrix = corr.obj$corrM,
               family = "binomial", data = data_mod,
               method = paste(fit_method),
               control.HLfit = list(LevenbergM = TRUE),
               verbose = c(TRACE = 1L))
  
  } else if (model == "nosocial"){
  
  spaMM::fitme(win_female ~ type * body_mass_bin + corrMatrix(1|pairsID),
               corrMatrix = corr.obj$corrM,
               family = "binomial", data = data_mod,
               method = paste(fit_method),
               control.HLfit = list(LevenbergM = TRUE),
               verbose = c(TRACE = 1L))
  
  } else if (model == "nomass") {
  
  spaMM::fitme(win_female ~ type * social_sup_bin + corrMatrix(1|pairsID),
               corrMatrix = corr.obj$corrM,
               family = "binomial", data = data_mod,
               method = paste(fit_method),
               control.HLfit = list(LevenbergM = TRUE),
               verbose = c(TRACE = 1L))
  
  } else if (model == "null") {
    
  spaMM::fitme(win_female ~ type + corrMatrix(1|pairsID),
               corrMatrix = corr.obj$corrM,
               family = "binomial", data = data_mod,
               method = paste(fit_method),
               control.HLfit = list(LevenbergM = TRUE),
               verbose = c(TRACE = 1L))
  
  } else {
    
    stop("not a valid model argument")
    
  }
}

################################################################################
#' Fit the residency model
#'
#'this function fits the models when the migrants have higher social support than
#'the native in interclan context.
#'
#'@name  fit_resid
#'@param fit_method "PQL" or "PQL/L"
#'@param DF1 dataset to use: resid_social
#'@return a list (fitted model with spaMM)
#'@export
#'@examples
#'\dontrun{
#'data(resid_social)
#'resid_mod <- fit_resid(fit_method = "PQL", DF1 = resid_social)
#'}

fit_resid <- function(fit_method, DF1){
  corr.obj2 <- buildcorrsigned(DF1$focal, DF1$other, DF1$type)
  DF1$pairsID <- corr.obj2$pairsID

  fitme(win ~ 1 + corrMatrix(1|pairsID),
        corrMatrix = corr.obj2$corrM,
        family = "binomial", data = DF1,
        method = paste(fit_method),
        control.HLfit = list(LevenbergM = TRUE),
        verbose = c(TRACE = 1L))
}
