#'plot relat panel (used internally)
#'
#'Builds the panel of plot2.
#'
#'@name plot_relat_inside
#'@param DF, data frame to be plotted: females_relat, migrants_relat or natives_relat
#'@param legend, include or not the axis legend (logical)
#'@param labels_x_axis, vector of character to specify the axis labels
#'@import ggplot2
#'@import ggthemes
#'@export
#'@examples
#'data(females_relat)
#'plot_relat_inside(DF = females_relat,
#'            legend = FALSE,
#'            labels_x_axis = c("test", "test"))
plot_relat_inside <- function(DF,
                      legend,
                      labels_x_axis) {
time <- relat <- NULL

  col1 <-  c("#E69F00",  "#009E73", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
tt2 <- ggplot2::ggplot(DF, ggplot2::aes(time, y = relat,fill = time, col = time), alpha = 0.02) +
  ggplot2::geom_boxplot(width = 0.4, outlier.shape = NA) +
  ggplot2::stat_summary(geom = "crossbar",
               width=0.50,
               fatten=0,
               color="black",
               fun.data = function(x){ return(c(y=stats::median(x),
                                                ymin = stats::median(x),
                                                ymax = stats::median(x)))}) +
  ggplot2::scale_y_continuous("Cumulative relatedness",
                     limits = c(0, 6.65)) +
  ggplot2::scale_x_discrete("",
                   labels = labels_x_axis) +
  ggplot2::scale_color_manual(values = col1) +
  ggplot2::scale_fill_manual(values = col1) +
  ggplot2::geom_text(data = DF, ggplot2::aes(x = 1.5, y = 6.6, label = paste(DF$type[1])),
            size = 0.35*7,
            col = "black",
            inherit.aes = F,
            check_overlap = T,
            family = "",
            fontface = "plain") +
  ggplot2::geom_hline(yintercept = 6.25, size = 0.195)

if(legend == FALSE) {
  tt2 <- tt2 +
    ggthemes::theme_base() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::theme(text = ggplot2::element_text(size = 7, 
                                                family = "",
                                                face = "plain"),
          axis.title.x = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(), 
          axis.ticks.length = ggplot2::unit(0.75, "mm"),
          axis.ticks = ggplot2::element_line(size = ggplot2::rel(0.4)),
          panel.border = ggplot2::element_rect(size = 0.4), 
          panel.background = element_blank())
} else {
  tt2 <- tt2 +
    ggthemes::theme_base() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::theme(text = ggplot2::element_text(size = 7, 
                                                family = "",
                                                face = "plain"),
          axis.title.x = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(), 
          axis.ticks.length = ggplot2::unit(0.75, "mm"),
          axis.ticks = ggplot2::element_line(size = ggplot2::rel(0.4)),
          panel.border = ggplot2::element_rect(size = 0.4), 
          panel.background = element_blank())
}
return(tt2)
}


################################################################################
#'second plot
#'
#'Creates the second plot of the paper.
#'
#'@name plot2
#'@param DF_female, females_relat
#'@param DF_migrant, migrants_relat
#'@param DF_native, natives_relat
#'@param PDF, a logical to return a PDF version of the plot 
#'@import ggplot2
#'@import ggthemes
#'@importFrom egg "ggarrange"
#'@export
#'@examples
#'data(females_relat)
#'data(migrants_relat)
#'data(natives_relat)
#'plot2(DF_female = females_relat,
#'      DF_migrant = migrants_relat,
#'      DF_native = natives_relat, 
#'      PDF = TRUE)

plot2 <- function(DF_female, DF_migrant, DF_native, PDF){

Plot_F <- plot_relat_inside(DF_female,
                    legend = FALSE,
                    labels_x_axis = c("2.5 yrs of age", "4.5 yrs of age")) +
  ggplot2::theme(plot.margin = ggplot2::margin(t = 4, r = 4, b = 4, l = 0, "pt"),
        axis.text.x = ggplot2::element_text(angle = 55,
                                            hjust = 1, 
                                            size = 7, 
                                            family = "",
                                            face = "plain"))

Plot_M <- plot_relat_inside(DF_migrant,
                    legend = TRUE,
                    labels_x_axis = c("1yr before dispersal",
                                      "1yr after dispersal")) +
  ggplot2::theme(plot.margin = ggplot2::margin(t = 4, r = 0, b = 4, l = 4, "pt"),
        axis.text.x = ggplot2::element_text(angle = 55, 
                                            hjust = 1, 
                                            size = 7, 
                                            family = "",
                                            face = "plain"))

Plot_philo <- plot_relat_inside(DF_native,
                           legend = FALSE,
                           labels_x_axis = c("1yr before onset of\n reproductive activity",
                                             "1yr after onset of\n reproductive activity")) +
  ggplot2::theme(plot.margin = ggplot2::margin(t = 4, r = 0, b = 4, l = 0, "pt"),
        axis.text.x = ggplot2::element_text(angle = 55, 
                                            hjust = 1, 
                                            size = 7, 
                                            family = "",
                                            face = "plain"))

x2 <- egg::ggarrange(Plot_M, Plot_philo,Plot_F, ncol = 3)
if(PDF == TRUE) {
  ggplot2::ggsave(x2, filename = "plot2.pdf", width = 88, height = 78, units= "mm")
} else {
return(x2)
}
}

################################################################################
################################################################################
#' Plot prediction for mass and social panels (used internally)
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
  PPA <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  ggplot2::ggplot(DF, ggplot2::aes(y = pred, x = pos2, col = type, shape = Model)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = c(0.25,  0.75), linetype = 3, size = 0.2) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = 3, size = 0.5, col = "black") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = inf, ymax = sup), width= 0) +
    ggplot2::geom_text(data = DF, ggplot2::aes(x = 2.5, y = 1.08), 
              label = paste(DF$plotname[1]), 
              size = 0.35*7, 
              col = "black", 
              inherit.aes = F, 
              check_overlap = TRUE, 
              hjust = "middle") +
    ggplot2::geom_hline(yintercept = 1.0275,  size = 0.195) +
    ggplot2::scale_color_manual(" ",
                       values = PPA) +
    ggplot2::scale_shape_manual(" ",
                       values = c(15, 19))+
    ggplot2::scale_x_continuous(limits = c(0.85,4.10),
                       breaks = c(1, 2, 3, 4),
                       labels =  c("Interclan",  "Intra-Mixed", "Intra-Native", "Intra-Immigrant")) +
    ggplot2::scale_y_continuous("Winning probability (%)",
                       limits = c(0,1.08),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("0", "25", "50", "75", "100")) +
    ggthemes::theme_base()  +
    ggplot2::guides(col = FALSE, shape = FALSE)
}

################################################################################
#' Plot1 sex panel (used internally)
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
  PPA_intra <- c("#000000", "#56B4E9", "#009E73", "#E69F00")

  ggplot2::ggplot(DF, ggplot2::aes(y = pred, x = pos, col = type, shape = Model)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = c(0.25,  0.75), linetype = 3, size = 0.2) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = 3, size = 0.5, col = "black") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = inf, ymax = sup), width= 0) +
    ggplot2::geom_text(data = DF, ggplot2::aes(x = 2, y = 1.08), 
              label = paste(DF$plotname[1]), 
              size = 0.35*7, 
              col = "black", 
              inherit.aes = FALSE, 
              check_overlap = TRUE, 
              hjust = "middle") +
    # geom_bar(data = NULL, aes(x =4, y=10), inherit.aes = T) +
    ggplot2::geom_hline(yintercept = 1.0275, size = 0.19) +
    ggplot2::scale_color_manual(" ",
                       values = PPA_intra) +
    ggplot2::scale_shape_manual(" ",
                       values = c(15,17)) +
    ggplot2::scale_x_continuous(limits = c(0.80,3.20),
                       breaks = c(1, 2, 3),
                       labels =  c("Interclan",  "Intra-Mixed", "Intra-Native")) +
    ggplot2::scale_y_continuous("Winning probability (%)",
                       limits = c(0,1.08),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("0", "25", "50", "75", "100")) +
    ggthemes::theme_base()  +
    ggplot2::guides(col = FALSE, shape = FALSE)
}

################################################################################
#' Plot1
#'
#' Creates the first plot of the paper
#'
#'@name plot1
#'@param Mod_so_diff, social model different sex
#'@param Mod_so_same, social model same sex
#'@param Mod_mass_diff, mass model different sex
#'@param Mod_mass_same, mass model same sex
#'@param Mod_sex, sex model diff sex
#'@param PDF, logical to return a PDF of the figure
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
#'plot1(
#'mod_social_null_diff_PQL,
#'mod_social_null_same_PQL,
#'mod_mass_null_diff_PQL,
#'mod_mass_null_same_PQL,
#'mod_sex_null_diff_PQL, 
#'PDF = TRUE)
plot1 <- function(Mod_so_diff,
                  Mod_so_same,
                  Mod_mass_diff,
                  Mod_mass_same,
                  Mod_sex, PDF) {

  type <- pred <- inf <- sup <- pos <- Model <- pos2 <- NULL

  PPA <-c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  PPA_legend <-  c("#000000", "#56B4E9", "#009E73", "#E69F00")


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
  ggplot2::theme(
        plot.margin = ggplot2::margin(t = 3, r = 0, b = 3, l = 3, "pt"),
        axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, color = PPA_legend, 
                                            size = 7, 
                                            family = "", 
                                            face = "plain"),
        plot.background = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = -1)), 
        text = ggplot2::element_text(size = 7, 
                                     family = "",
                                     face = "plain"), 
        axis.ticks.length = ggplot2::unit(0.75, "mm"),
        axis.ticks = ggplot2::element_line(size = rel(0.4)),
        panel.border = ggplot2::element_rect(size = 0.4),
        panel.background = element_blank())

body_mass_plot <- plot_intra_inter(soss2) +
  ggplot2::theme(
        plot.margin = ggplot2::margin(t = 3, r = 1, b = 3, l = 1, "pt"),
        axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, color = PPA_legend, 
                                            size = 7, 
                                            family = "", 
                                            face = "plain"),
        plot.background = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(), 
        text = ggplot2::element_text(size = 7, 
                                     family = "",
                                     face = "plain"), 
        axis.ticks.length = ggplot2::unit(0.75, "mm"),
        axis.ticks = ggplot2::element_line(size = rel(0.4)),
        panel.border = ggplot2::element_rect(size = 0.4), 
        panel.background = element_blank())

sex_plot <- plot_sex(soss3) +
  ggplot2::theme(
        plot.margin = ggplot2::margin(t = 3, r = 3, b = 3, l = 0, "pt"),
        axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, color = PPA_legend, 
                                            size = 7, 
                                            family = "", 
                                            face = "plain"),
        plot.background = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(), 
        text = ggplot2::element_text(size = 7, 
                                     family = "",
                                     face = "plain"), 
        axis.ticks.length = ggplot2::unit(0.75, "mm"),
        axis.ticks = ggplot2::element_line(size = rel(0.4)),
        panel.border = ggplot2::element_rect(size = 0.4), 
        panel.background = element_blank())

ttt <- egg::ggarrange(social_plot, body_mass_plot, sex_plot, ncol = 3, widths = c(4,4,3))

  if (PDF == TRUE) {
  ggplot2::ggsave(ttt, filename = "plot1.pdf",  width = 88, height = 78, units= "mm")
} else {
ttt <- egg::ggarrange(social_plot, body_mass_plot, sex_plot, ncol = 3, widths = c(4,4,3))
return(ttt)
}
}

