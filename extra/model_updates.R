#### updates models 
library(vullioud2018)

data(d1)
# head(d1)


#########################################
###  Creating the different datasets  ###
#########################################

### As each set of models require a different focal individual,
### the datasets had to be reframed. Simillarily, as models were fitted
### separately for intra-sex and inter-sex interactions, differents datasets
### were computed.

### first we subsetted the main dataset between intra and inter-sex interactions

data_same_sex <- prepare_same_sex(d1)
data_diff_sex <- prepare_different_sex(d1)

### Aditionnaly we created a subset for the SI models testing the interactions in which
### the focal individual is a migrant with higher social support in interclan
### interaction.

data_resid <- prepare_resid(d1)

### each models has a different dataset: they were created as follow:
### SOCIAL SUPPORT MODELS: higest social support as focal

same_sex_social <- create_DF_social(data_same_sex)
diff_sex_social <- create_DF_social(data_diff_sex)
resid_social <- create_DF_social(data_resid)

### BODY MASS MODELS: higest body mass as focal

diff_sex_weight <- create_DF_weight(data_diff_sex)
same_sex_weight <- create_DF_weight(data_same_sex)

### SEX MODEL females as focal:

diff_sex_sex <- create_DF_sex(data_diff_sex)

###### MODELS #########################

#### mod1 
mod_social_null_diff_PQL <- fit_social(fit_method = "PQL",
                                       model = "null",
                                       DF1 = diff_sex_social)

save(mod_social_null_diff_PQL, file = "mod_social_null_diff_PQL.rda", compress = "xz")

#### mod2
mod_social_null_same_PQL <- fit_social(fit_method = "PQL",
                                       model = "null",
                                       DF1 = same_sex_social)
save(mod_social_null_same_PQL, file = "mod_social_null_same_PQL.rda", compress = "xz")

#### mod3 
### BODY MASS MODELS:

mod_mass_null_same_PQL <- fit_body_mass(fit_method = "PQL",
                                     model = "null",
                                     DF1 = same_sex_weight)

save(mod_mass_null_same_PQL, file = "mod_mass_null_same_PQL.rda", compress = "xz")

###mod4
mod_mass_null_diff_PQL <- fit_body_mass(fit_method = "PQL",
                                     model = "null",
                                     DF1 = diff_sex_weight)

save(mod_mass_null_diff_PQL, file = "mod_mass_null_diff_PQL.rda", compress = "xz")

### SEX MODELS:

mod_sex_null_diff_PQL <- fit_sex(fit_method = "PQL",
                                 model = "null",
                                 DF1 = diff_sex_sex)
save(mod_sex_null_diff_PQL, file = "mod_sex_null_diff_PQL.rda", compress = "xz")




