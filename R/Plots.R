#'plot1 panel (used internally)
#'
#'Builds the panel of plot1.
#'
#'@name plot1_inside
#'@param DF, data frame to be plotted: females_relat, migrants_relat or natives_relat
#'@param legend, include or not the axis legend (logical)
#'@param labels_x_axis, vector of character to specify the axis labels
#'@import ggplot2
#'@import ggthemes
#'@export
#'@examples
#'data(females_relat)
#'plot1_inside(DF = females_relat,
#'            legend = FALSE,
#'            labels_x_axis = c("test", "test"))
plot1_inside <- function(DF,
                      legend,
                      labels_x_axis) {
time <- relat <- NULL

  col1 <- c("#009E73",  "#9ad0f3", "#F0E442", "#D55E00")
tt2 <- ggplot(DF, aes(time, y = relat,fill = time, col = time), alpha = 0.02) +
  geom_boxplot(width = 0.4, outlier.shape = NA) +
  stat_summary(geom = "crossbar",
               width=0.50,
               fatten=0,
               color="black",
               fun.data = function(x){ return(c(y=stats::median(x),
                                                ymin = stats::median(x),
                                                ymax = stats::median(x)))}) +
  scale_y_continuous("Cumulative relatedness",
                     limits = c(0, 6.6)) +
  scale_x_discrete("",
                   labels = labels_x_axis) +
  scale_color_manual(values = col1) +
  scale_fill_manual(values = col1) +
  geom_text(data = DF, aes(x = 1.5, y = 6.6, label = paste(DF$type[1])),
            size = 0.35*7,
            col = "black",
            inherit.aes = F,
            check_overlap = T) +
  geom_hline(yintercept = 6.3)

if(legend == FALSE) {
  tt2 <- tt2 +
    ggthemes::theme_base() +
    guides(color = FALSE, fill = FALSE) +
    theme(text = element_text(size = 7),
          axis.ticks.length = unit(1, "mm"),
          axis.title.x = element_blank(),
          plot.background = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
} else {
  tt2 <- tt2 +
    ggthemes::theme_base() +
    guides(color = FALSE, fill = FALSE) +
    theme(text = element_text(size = 7),
          axis.ticks.length = unit(1, "mm"),
          axis.title.x = element_blank(),
          plot.background = element_blank())
}
return(tt2)
}


################################################################################
#'plot1
#'
#'Creates the plot1 of the paper.
#'
#'@name plot1
#'@param DF_female, females_relat
#'@param DF_migrant, migrants_relat
#'@param DF_native, natives_relat
#'@import ggplot2
#'@import ggthemes
#'@importFrom egg "ggarrange"
#'@export
#'@examples
#'data(females_relat)
#'data(migrants_relat)
#'data(natives_relat)
#'plot1(DF_female = females_relat,
#'      DF_migrant = migrants_relat,
#'      DF_native = natives_relat)

plot1 <- function(DF_female, DF_migrant, DF_native){

Plot_F <- plot1_inside(DF_female,
                    legend = FALSE,
                    labels_x_axis = c("2.5 yrs of age", "4.5 yrs of age")) +
  theme(plot.margin = margin(t = 3, r = 3, b = 3, l = 0, "pt"),
        axis.text.x = element_text(angle = 60, hjust = 1))

Plot_M <- plot1_inside(DF_migrant,
                    legend = TRUE,
                    labels_x_axis = c("1yr before dispersal",
                                      "1yr after dispersal")) +
  theme(plot.margin = margin(t = 3, r = 0, b = 3, l = 3, "pt"),
        axis.text.x = element_text(angle = 60, hjust = 1))

Plot_philo <- plot1_inside(DF_native,
                           legend = FALSE,
                           labels_x_axis = c("1yr before onset of\n reproductive activity",
                                             "1yr after onset of\n reproductive activity")) +
  theme(plot.margin = margin(t = 3, r = -1, b = 3, l = -1, "pt"),
        axis.text.x = element_text(angle = 60, hjust = 1))

x2 <- egg::ggarrange(Plot_M, Plot_philo,Plot_F, ncol = 3)
return(x2)
}

################################################################################
################################################################################
#' Plot2 mass and social panels (used internally)
#'
#' Plots the predictions for the mass and social models. These
#' predictions are computed and formated inside the plot2 function
#'
#'@name plot_intra_inter
#'@param DF, data frame (created inside the plot2 function)
#'@import ggplot2
#'@import ggthemes
#'@export
plot_intra_inter <- function(DF) {
  pred <- pos2 <- type <- Model <- inf <- sup <- NULL
  PPA <- c("#009E73",  "#FB9A99","#9ad0f3", "#D55E00")

  ggplot(DF, aes(y = pred, x = pos2, col = type, shape = Model)) +
    geom_point() +
    geom_hline(yintercept = c(0.25,  0.75), linetype = 3, size = 0.2) +
    geom_hline(yintercept = 0.5, linetype = 3, size = 0.5, col = "black") +
    geom_errorbar(aes(ymin = inf, ymax = sup), width= 0) +
    geom_text(data = DF, aes(x = 2.5, y = 1.10), label = paste(DF$plotname[1]), size = 7/(14/5), col = "black", inherit.aes = F, check_overlap = TRUE) +
    geom_hline(yintercept = 1.05) +
    scale_color_manual(" ",
                       values = PPA) +
    scale_shape_manual(" ",
                       values = c(15, 19))+
    scale_x_continuous(limits = c(0.85,4.10),
                       breaks = c(1, 2, 3, 4),
                       labels =  c("Interclan",  "Intra-Mixed", "Intra-Native", "Intra-Immigrant")) +
    scale_y_continuous("Winning probability (%)",
                       limits = c(0,1.10),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("0", "25", "50", "75", "100")) +
    ggthemes::theme_base()  +
    guides(col = FALSE, shape = FALSE)
}

################################################################################
#' Plot2 sex panel (used internally)
#'
#' Plots the predictions for sex model. These
#' predictions are computed and formated inside the plot2 function
#'
#'@name plot_sex
#'@param DF, a data frame (created inside the plot2 function)
#'@import ggplot2
#'@import ggthemes
#'@export

plot_sex <- function(DF) {
  inf <- sup <- Model <- pred <- pos <- type <- NULL
  PPA_intra <- c("#009E73",  "#9ad0f3", "#D55E00")

  ggplot(DF, aes(y = pred, x = pos, col = type, shape = Model)) +
    geom_point() +
    geom_hline(yintercept = c(0.25,  0.75), linetype = 3, size = 0.2) +
    geom_hline(yintercept = 0.5, linetype = 3, size = 0.5, col = "black") +
    geom_errorbar(aes(ymin = inf, ymax = sup), width= 0) +
    geom_text(data = DF, aes(x = 2, y = 1.10), label = paste(DF$plotname[1]), size = 7/(14/5), col = "black", inherit.aes = FALSE, check_overlap = TRUE) +
    # geom_bar(data = NULL, aes(x =4, y=10), inherit.aes = T) +
    geom_hline(yintercept = 1.05) +
    scale_color_manual(" ",
                       values = PPA_intra) +
    scale_shape_manual(" ",
                       values = c(15,17)) +
    scale_x_continuous(limits = c(0.80,3.20),
                       breaks = c(1, 2, 3),
                       labels =  c("Interclan",  "Intra-Mixed", "Intra-Native")) +
    scale_y_continuous("Winning probability (%)",
                       limits = c(0,1.10),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("0", "25", "50", "75", "100")) +
    ggthemes::theme_base()  +
    guides(col = FALSE, shape = FALSE)
}

################################################################################
#' Plot2
#'
#' Creates the second plot of the paper
#'
#'@name plot2
#'@param Mod_so_diff, social model different sex
#'@param Mod_so_same, social model same sex
#'@param Mod_mass_diff, mass model different sex
#'@param Mod_mass_same, mass model same sex
#'@param Mod_sex, sex model diff sex
#'@import dplyr
#'@import ggplot2
#'@import ggthemes
#'@importFrom egg "ggarrange"
#'@export
#'@examples
#'data(mod_social_null_diff_PQL)
#'data(mod_social_null_same_PQL)
#'data(mod_mass_null_diff_PQL)
#'data(mod_mass_null_same_PQL)
#'data(mod_sex_null_diff_PQL)
#'plot2(
#'mod_social_null_diff_PQL,
#'mod_social_null_same_PQL,
#'mod_mass_null_diff_PQL,
#'mod_mass_null_same_PQL,
#'mod_sex_null_diff_PQL
#')
plot2 <- function(Mod_so_diff,
                  Mod_so_same,
                  Mod_mass_diff,
                  Mod_mass_same,
                  Mod_sex) {

  type <- pred <- inf <- sup <- pos <- Model <- pos2 <- NULL

  PPA <- c("#009E73",  "#FB9A99","#9ad0f3", "#D55E00")
  PPA_legend <-  c("#009E73",  "#9ad0f3", "#D55E00",  "#FB9A99")


  ############### First tweak the data // get the predictions
sex <- get_predictions(Mod_sex) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(pred = pred[10],
            inf = inf[10],
            sup = sup[10]) %>%
  dplyr::mutate(Model = "SEX")

soss3 <- sex %>%
  dplyr::mutate(pos = 1:3) %>%
  dplyr::arrange(pos)
soss3$plotname <- "Sex"
soss3$names <-  c("Intra-Native", "Intra-Mixed", "Interclan")

################ weight
weight_intra <- get_predictions(Mod_mass_same) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(pred = pred[10],
            inf = inf[10],
            sup = sup[10]) %>%
  dplyr::mutate(Model = "WEIGHT_intra")

weight <-get_predictions(Mod_mass_diff) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(pred = pred[10],
            inf = inf[10],
            sup = sup[10]) %>%
  dplyr::mutate(Model = "WEIGHT")

soss2 <- dplyr::bind_rows(weight, weight_intra)
soss2 <- soss2 %>%
  dplyr::mutate(pos = ifelse(type == "inter", 1, ifelse(type == "mix", 2, ifelse(type == "nat", 3, 4))))
soss2 <- soss2 %>%
  dplyr::mutate(pos2 = ifelse(Model == "WEIGHT", pos - 0.1, pos + 0.1))
soss2$plotname <- "Body mass"
soss2 <- soss2 %>%
  dplyr::arrange(pos2)
soss2$pos2[soss2$type == "mig"] <- 4

############# social support

sos_intra <- get_predictions(Mod_so_same) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(pred = pred[10],
            inf = inf[10],
            sup = sup[10]) %>%
  dplyr::mutate(Model = "SO_intra")

sos <- get_predictions(Mod_so_diff) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(pred = pred[10],
            inf = inf[10],
            sup = sup[10]) %>%
  dplyr::mutate(Model = "SO")

soss <- dplyr::bind_rows(sos, sos_intra)
soss <- soss %>%
  dplyr::mutate(pos = ifelse(type == "inter", 1, ifelse(type == "mix", 2, ifelse(type == "nat", 3, 4))))
soss <- soss %>%
  dplyr::mutate(pos2 = ifelse(Model == "SO", pos - 0.1, pos + 0.1))
soss$plotname <- "Social support"
soss <- soss %>%
  dplyr::arrange(pos2)
soss$pos2[soss$type == "mig"] <- 4



############# MAKE THE PLOT
social_plot <- plot_intra_inter(soss) +
  theme(text = element_text(size = 7),
        plot.margin = margin(t = 3, r = 0, b = 3, l = 3, "pt"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = PPA_legend),
        plot.background = element_blank(),
        axis.ticks.length = unit(1, "mm"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = -1)))

body_mass_plot <- plot_intra_inter(soss2) +
  theme(text = element_text(size = 7),
        plot.margin = margin(t = 3, r = 1, b = 3, l = 1, "pt"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = PPA_legend),
        plot.background = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.y = element_blank())

sex_plot <- plot_sex(soss3) +
  theme(text = element_text(size = 7),
        plot.margin = margin(t = 3, r = 3, b = 3, l = 0, "pt"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = PPA_legend),
        plot.background = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.y = element_blank())

ttt <- egg::ggarrange(social_plot, body_mass_plot, sex_plot, ncol = 3, widths = c(4,4,3))
return(ttt)
}
