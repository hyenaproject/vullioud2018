#' Prepare the data for the same sex interactions
#'
#'Keeps the interaction between members of the same sex,
#'selects the variables of interest, renames the type of
#'interaction and creates a binomial version fo the social support and
#'delta weight variables
#'@name prepare_same_sex
#'@param DF1 The interactions database (d1)
#'@return a filtered data frames
#'@import dplyr
#'@export
#'@examples
#'data(d1)
#'data_same_sex <- prepare_same_sex(d1)

prepare_same_sex <- function(DF1) {
  A_B <- '.' <- sex <- focal <- other <- delta_weight <- predictor_NEW <-NULL
  index <- type <- body_mass_bin <- social_sup_bin <- win <- type <- NULL
  DF1 %>%
  dplyr::filter(is.na(A_B) | (A_B != "A0_==_B0")) %>%
  dplyr::filter(sex == "F/F" | sex == "M/M") %>%
  dplyr::filter(!(is.na(sex) | is.na(delta_weight))) %>%
  dplyr::select(focal, other, delta_weight, predictor_NEW, sex, win, type) %>%
  dplyr::mutate(social_sup_bin = predictor_NEW > 0,
         body_mass_bin = delta_weight > 0,
         index = 1:nrow(.),
         type = dplyr::recode(type,
                       intra_migrant = "mig",
                       intra_mix = "mix",
                       intra_native = "nat")) %>%
  droplevels()
}

###############################################################################
#' Prepare the data for the different sexes interactions
#'
#'Keeps the interaction between members of different sexes,
#'selects the variables of interest, renames the type of
#'interaction and creates a binomial version fo the social support and
#'delta weight variables
#'
#'@name prepare_different_sex
#'@param DF1 The interactions database (d1)
#'@return a filtered data frames
#'@import dplyr
#'@export
#'@examples
#'data(d1)
#'data_diff_sex <- prepare_different_sex(d1)

prepare_different_sex <- function(DF1) {
  A_B <- '.' <- sex <- focal <- other <- delta_weight <- predictor_NEW <- NULL
  index <- type <- body_mass_bin <- social_sup_bin <- win <- type <- NULL

  DF1 %>%
    dplyr::filter(is.na(A_B) | (A_B != "A0_==_B0")) %>%
    dplyr::filter(sex != "F/F" & sex != "M/M") %>%
    dplyr::filter(!(is.na(sex) | is.na(delta_weight))) %>%
    dplyr::select(focal, other, delta_weight, predictor_NEW, sex, win, type) %>%
    dplyr::mutate(
                  social_sup_bin = predictor_NEW > 0,
                  body_mass_bin = delta_weight > 0,
                  index = 1:nrow(.),
                  type = dplyr::recode(type,
                                       intra_migrant = "mig",
                                       intra_mix = "mix",
                                       intra_native = "nat")) %>%
    droplevels()
}

###############################################################################
#' Prepare the data for the residency model (SI)
#'
#'Keeps the interclan interactions in which the individuals whith the highest
#'social support is a migrant and its opponent a native.
#'It also selects the variables of interest, renames the type of
#'interaction and creates a binomial version fo the social support and
#'delta weight variables
#'
#'@name prepare_resid
#'@param DF1 The interactions database (d1)
#'@return a filtered data frames
#'@import dplyr
#'@export
#'@examples
#'data(d1)
#'data_resid <- prepare_resid(d1)

prepare_resid <- function(DF1) {
  A_B <- '.' <- sex <- focal <- other <- delta_weight <- predictor_NEW <- NULL
  index <- type <- body_mass_bin <- social_sup_bin <- win <- type <- NULL
  resid_f <- resid_o <- NULL
  DF1 %>%
  dplyr::filter(is.na(A_B) | (A_B != "A0_==_B0")) %>%
  dplyr::filter(type == "inter") %>%
  dplyr::filter(!(is.na(sex) | is.na(delta_weight))) %>%
  dplyr::filter(resid_f == "M" & resid_o == "N") %>%
  dplyr::select(focal, other, delta_weight, predictor_NEW, sex, win, type) %>%
  dplyr::mutate(social_sup_bin = predictor_NEW > 0,
         body_mass_bin = delta_weight > 0,
         index = 1:nrow(.),
         type = dplyr::recode(type,
                       intra_migrant = "mig",
                       intra_mix = "mix",
                       intra_native = "nat")) %>%
  droplevels()
}

###############################################################################
###############################################################################
#' Flip social support (to be used internally)
#'
#'Reverses all interactions in which the focal has a lower social support.
#'@name flip_DF_social
#'@param DF1 data frame
#'@return a df with higest social support as focal
#'@export
#'@import dplyr
flip_DF_social <- function(DF1) {
  '.' <- other <- focal <- win <- social_sup_bin <- body_mass_bin <- sex <- NULL

  d2 <- DF1 %>% split(.$social_sup_bin)

  if (length(d2) == 1) {
    d2 <- dplyr::bind_rows(d2[[1]])
  } else {
    d2 <- d2[[1]] %>%
      dplyr::rename(
        focal = other,
        other = focal) %>%
      dplyr::mutate(
        win = ifelse(win, F,T),
        sex = ifelse(sex == "M/F", "F/M", "M/F"),
        social_sup_bin = ifelse(social_sup_bin, F, T),
        body_mass_bin = ifelse(body_mass_bin, F, T)) %>%
      dplyr::bind_rows(d2[[2]])
  }
  return(d2)
}
###############################################################################

#' Create dataset social support
#'
#'this function create the data use for the model with social support as focal,
#'it reverses all interactions in which the focal has a lower social support.
#'@name create_DF_social
#'@param DF1 The processed data for same sex or different sex
#'@return a DF with higest social support as focal
#'@import purrr
#'@import dplyr
#'@export
#'@examples
#'data(data_same_sex)
#'data(data_diff_sex)
#'data(data_resid)
#'same_sex_social <- create_DF_social(data_same_sex)
#'diff_sex_social <- create_DF_social(data_diff_sex)
#'resid_social <- create_DF_social(data_resid)
#'
create_DF_social <- function(DF1){
  '.' <- type <- index <- NULL

  d1 <- DF1 %>% split(.$type)
  d4 <- purrr::map_dfr(d1, ~ flip_DF_social(.x)) %>%
   dplyr::mutate(type = as.character(type)) %>%
   dplyr::arrange(index)
 return(d4)
}

###############################################################################
#' Flip sex (used internally)
#'
#'Reverses all interactions in which the focal is a male
#'
#'@name flip_DF_sex
#'@param DF1 The processed data for different sex
#'@return a DF with females as focal
#'@export
#'@import dplyr

flip_DF_sex <- function(DF1) {
  '.' <- other <- focal <- win <- social_sup_bin <- body_mass_bin <- sex <- NULL
  d2 <- DF1 %>% split(.$sex)
  d1 <- d2 %>% dplyr::bind_rows()
  if (d1$type[1] == "mix") {
    d2 <- dplyr::bind_rows(d2[["F/M"]])
  } else {
    d2 <- d2[["M/F"]] %>%
      dplyr::rename(focal = other,
             other = focal) %>%
      dplyr::mutate(sex = "F/M",
             win = ifelse(win, F,T),
             social_sup_bin = ifelse(social_sup_bin, F, T),
             body_mass_bin = ifelse(body_mass_bin, F, T)) %>%
      dplyr::bind_rows(d2[["F/M"]])
  }
  return(d2)
}

###############################################################################
#' Create dataset sex
#'
#'Creates the data use for the model with female as focal,
#'it reverses all interactions in which the focal is a male
#'@name create_DF_sex
#'@param DF1 The processed data for different sex
#'@return a DF with females as focal
#'@import dplyr
#'@import purrr
#'@export
#'@examples
#'data(data_diff_sex)
#'diff_sex_sex <- create_DF_sex(data_diff_sex)
create_DF_sex <- function(DF1){
  '.' <- type <- index <- NULL
  d1 <- DF1 %>% split(.$type)
  d4 <- purrr::map_dfr(d1, ~ flip_DF_sex(.x)) %>%
    dplyr::mutate(type = as.character(type)) %>%
    dplyr::arrange(index)
  return(d4)
}

###############################################################################
#' flip body mass (used internally)
#'
#'Reverse all interaction in which the focal has a lower body mass than its
#'opponent
#
#'@name flip_DF_weight
#'@param DF1 The processed data for same sex or different sex
#'@return a DF with heaviest as focal
#'@import dplyr
#'@export

flip_DF_weight <- function(DF1) {
  '.' <- other <- focal <- win <- social_sup_bin <- body_mass_bin <- sex <- NULL
  d2 <- DF1 %>% split(.$body_mass_bin)

  d2 <- d2[[1]] %>%
    dplyr::rename(focal = other,
           other = focal) %>%
    dplyr::mutate(win = ifelse(win, F,T),
           sex = ifelse(sex == "M/F", "F/M", "M/F"),
           social_sup_bin = ifelse(social_sup_bin, F, T),
           body_mass_bin = ifelse(body_mass_bin, F, T)) %>%
    dplyr::bind_rows(d2[[2]])
  return(d2)
}
###############################################################################
#' Create dataset body mass
#'
#'Create the data use for the model with body mass as focal.
#'it reverses all interaction in which the focal has a lower body mass than its
#'opponent
#'@name create_DF_weight
#'@param DF1 The processed data for same sex or different sex
#'@return a DF with heaviest as focal
#'@import purrr
#'@import dplyr
#'@export
#'@examples
#'data(data_diff_sex)
#'data(data_same_sex)
#'diff_sex_weight <- create_DF_weight(data_diff_sex)
#'same_sex_weight <- create_DF_weight(data_same_sex)
create_DF_weight <- function(DF1){
  '.' <- type <- index <- NULL
  d1 <- DF1 %>% split(.$type)
  d4 <- purrr::map_dfr(d1, ~ flip_DF_weight(.x)) %>%
    dplyr::mutate(type = as.character(type)) %>%
    dplyr::arrange(index)
  return(d4)
}
