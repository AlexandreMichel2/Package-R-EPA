#### CONFIGURATION ####

requireNamespace("conflicted", quietly = TRUE)
requireNamespace("showtext", quietly = TRUE)
requireNamespace("WDI", quietly = TRUE)
requireNamespace("lsr", quietly = TRUE)
requireNamespace("Hmisc", quietly = TRUE)
requireNamespace("vcd", quietly = TRUE)
requireNamespace("questionr", quietly = TRUE)
requireNamespace("gmodels", quietly = TRUE)
requireNamespace("car", quietly = TRUE)
requireNamespace("haven", quietly = TRUE)
requireNamespace("DescTools", quietly = TRUE)
requireNamespace("multcomp", quietly = TRUE)
requireNamespace("FactoMineR", quietly = TRUE)
requireNamespace("rworldmap", quietly = TRUE)
requireNamespace("dplyr", quietly = TRUE)
requireNamespace("ggplot2", quietly = TRUE)
requireNamespace("forcats", quietly = TRUE)
requireNamespace("readr", quietly = TRUE)
requireNamespace("stringr", quietly = TRUE)
requireNamespace("purrr", quietly = TRUE)
requireNamespace("tibble", quietly = TRUE)
requireNamespace("tidyr", quietly = TRUE)
requireNamespace("ggthemes", quietly = TRUE)
requireNamespace("ggtext", quietly = TRUE)
requireNamespace("ggpubr", quietly = TRUE)
requireNamespace("ggpmisc", quietly = TRUE)
requireNamespace("ggrepel", quietly = TRUE)
requireNamespace("ggcharts", quietly = TRUE)
requireNamespace("ggforce", quietly = TRUE)
requireNamespace("openxlsx", quietly = TRUE)
requireNamespace("scales", quietly = TRUE)
requireNamespace("lubridate", quietly = TRUE)
requireNamespace("svglite", quietly = TRUE)
requireNamespace("ragg", quietly = TRUE)
requireNamespace("cowplot", quietly = TRUE)
requireNamespace("googleway", quietly = TRUE)
requireNamespace("ggspatial", quietly = TRUE)
requireNamespace("sf", quietly = TRUE)
requireNamespace("rnaturalearth", quietly = TRUE)
requireNamespace("rnaturalearthdata", quietly = TRUE)


.onAttach = function(libname, pkgname)
{
  showtext::showtext_opts(dpi = 96)
  showtext::showtext_auto(enable = TRUE)
  sysfonts::font_add_google("Inter")
  conflicted::conflict_prefer(name = "recode", winner = "car", quiet = TRUE)
  conflicted::conflict_prefer(name = "Recode", winner = "car", quiet = TRUE)
  conflicted::conflict_prefer(name = "filter", winner = "dplyr", quiet = TRUE)
  conflicted::conflict_prefer(name = "select", winner = "dplyr", quiet = TRUE)
  conflicted::conflict_prefer(name = "annotate", winner = "ggpp", quiet = TRUE)
  conflicted::conflict_prefer(name = "col_factor", winner = "scales", quiet = TRUE)
  conflicted::conflict_prefer(name = "discard", winner = "purrr", quiet = TRUE)
  conflicted::conflict_prefer(name = "geyser", winner = "TH.data", quiet = TRUE)
  conflicted::conflict_prefer(name = "lag", winner = "dplyr", quiet = TRUE)
  conflicted::conflict_prefer(name = "some", winner = "car", quiet = TRUE)
  conflicted::conflict_prefer(name = "get_legend", winner = "ggpubr", quiet = TRUE)
  conflicted::conflict_prefer(name = "some", winner = "car", quiet = TRUE)
  conflicted::conflict_prefer(name = "stamp", winner = "lubridate", quiet = TRUE)
  conflicted::conflict_prefer(name = "theme_map", winner = "cowplot", quiet = TRUE)
  conflicted::conflict_prefer(name = "describe", winner = "Hmisc", quiet = TRUE)
  conflicted::conflict_prefer(name = "Label", winner = "Hmisc", quiet = TRUE)
  conflicted::conflict_prefer(name = "Mean", winner = "DescTools", quiet = TRUE)
  conflicted::conflict_prefer(name = "mutate", winner = "dplyr", quiet = TRUE)
  conflicted::conflict_prefer(name = "Quantile", winner = "DescTools", quiet = TRUE)
  conflicted::conflict_prefer(name = "src", winner = "dplyr", quiet = TRUE)
  conflicted::conflict_prefer(name = "summarize", winner = "dplyr", quiet = TRUE)
  conflicted::conflict_prefer(name = "wtd.mean", winner = "Hmisc", quiet = TRUE)
  conflicted::conflict_prefer(name = "wtd.table", winner = "Hmisc", quiet = TRUE)
  conflicted::conflict_prefer(name = "wtd.var", winner = "Hmisc", quiet = TRUE)
  options(scipen = 999)
  print(conflicted::conflict_scout())
}

#### THÈMES GRAPHS GGPLOT2 ####

`:=` <- function(a, b)
{
  stop(":= should not be called directly")
}

#' lm_eq
#'
#' @param x Variable indépendante (VI) sous la forme "base$variable"
#' @param y Variable dépendante (VD) sous la forme "base$variable"
#' @return Équation de la droite en format "character".
#' @description Sert surtout à imprimer l'équation de la droite de régression sur un graphique.
#' @export
lm_eq = function(x, y)
{
  modele = stats::lm(y ~ x)
  coeff = stats::coefficients(modele)
  if (round(coeff[1], 1) >= 0){
    eq = as.character(paste0("y == ", round(coeff[2], 1), "*x + ", round(coeff[1], 1)))
  } else if (round(coeff[1], 1) < 0){
    eq = as.character(paste0("y == ", round(coeff[2], 1), "*x ", round(coeff[1], 1)))
  }
  eq
}

#' theme_pie
#'
#' @param couleur Le vert de l'Université de Sherbrooke est la couleur par défaut, mais elle peut être changée manuelle pour toute autre couleur.
#' @return Retourne le thème qui s'ajoute (+) à un graphique fait à partir ddu package ggplot2.
#' @description Thème personnalisé qui retire les fioritures dans la présentation d'un graphique en pointes de tarte, ou "pie chart".
#' @export
theme_pie = function(couleur = "#018849", legende = TRUE)
{
  if (legende == TRUE) {
    theme = ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
      theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = couleur, hjust = 0.5, vjust = 1, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(face = "bold", size = 12, color = couleur, hjust = 0.5),
        legend.text = ggplot2::element_text(face = "plain", size = 8, color = "black", lineheight = 0.75),
        legend.position = "right",
        legend.margin = ggplot2::margin(0,0,0,0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2.5),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        panel.ontop = TRUE,
        panel.border = ggplot2::element_blank()
      )
  } else if (legende == FALSE) {
    theme = ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
      theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = couleur, hjust = 0.5, vjust = 1, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "none",
        legend.margin = ggplot2::margin(0,0,0,0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2.5),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        panel.ontop = TRUE,
        panel.border = ggplot2::element_blank()
      )
  }
}

#' theme_defaut
#'
#' @param couleur Le vert de l'Université de Sherbrooke est la couleur par défaut, mais elle peut être changée manuelle pour toute autre couleur.
#' @param grille A la valeur "x" par défaut, ce qui signifie aucune grille. La valeur "h" permet une grille horizontale seulement, la valeur "v" une grille verticale et la valeur "hv" pour les deux grilles.
#' @param grille_top A la valeur FALSE par défaut, ce qui signifie que la grille se situe sous les objets du graphique, alors que TRUE appose la grille par-dessus.
#' @return Retourne le thème qui s'ajoute (+) à un graphique fait à partir ddu package ggplot2.
#' @description Thème personnalisé qui est fait pour s'ajuster à une présentation sur une feuille en format lettre.
#' @export
theme_defaut = function(couleur = "#018849", grille = "x", grille_top = FALSE)
{
  if (grille == "h") {
    theme = ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
      theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = couleur, hjust = 0.5, vjust = 1, margin = ggplot2::margin(t = 5, r = 10, b = 10, l = 10)),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.title.x = ggplot2::element_text(face = "bold", size = 12, color = couleur, margin = ggplot2::margin(t = 15, r = 5, b = 5, l = 5)),
        axis.title.y = ggplot2::element_text(face = "bold", size = 12, color = couleur, margin = ggplot2::margin(t = 5, r = 15, b = 5, l = 5)),
        axis.text = ggplot2::element_text(face = "plain", size = 8, color = "black"),
        axis.ticks = ggplot2::element_line(colour = "black", linetype = "solid", lineend = "round"),
        legend.title = ggplot2::element_text(face = "bold", size = 12, color = couleur, hjust = 0.5),
        legend.text = ggplot2::element_text(face = "plain", size = 8, color = "black", lineheight = 0.75),
        panel.grid.major.y = ggplot2::element_line(colour = "black", linetype = 2, lineend = "round"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        panel.ontop = grille_top,
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),
        legend.position = "right",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2)
      )
  } else if (grille == "v"){
    theme = ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
      theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = couleur, hjust = 0.5, vjust = 1, margin = ggplot2::margin(t = 5, r = 10, b = 10, l = 10)),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.title.x = ggplot2::element_text(face = "bold", size = 12, color = couleur, margin = ggplot2::margin(t = 15, r = 5, b = 5, l = 5)),
        axis.title.y = ggplot2::element_text(face = "bold", size = 12, color = couleur, margin = ggplot2::margin(t = 5, r = 15, b = 5, l = 5)),
        axis.text = ggplot2::element_text(face = "plain", size = 8, color = "black"),
        axis.ticks = ggplot2::element_line(colour = "black", linetype = "solid", lineend = "round"),
        legend.title = ggplot2::element_text(face = "bold", size = 12, color = couleur, hjust = 0.5),
        legend.text = ggplot2::element_text(face = "plain", size = 8, color = "black", lineheight = 0.75),
        panel.grid.major.x = ggplot2::element_line(colour = "black", linetype = 2, lineend = "round"),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        panel.ontop = grille_top,
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),
        legend.position = "right",
        legend.margin = ggplot2::margin(0,0,0,0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2)
      )
  } else if (grille == "hv"){
    theme = ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
      theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = couleur, hjust = 0.5, vjust = 1, margin = ggplot2::margin(t = 5, r = 10, b = 10, l = 10)),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.title.x = ggplot2::element_text(face = "bold", size = 12, color = couleur, margin = ggplot2::margin(t = 15, r = 5, b = 5, l = 5)),
        axis.title.y = ggplot2::element_text(face = "bold", size = 12, color = couleur, margin = ggplot2::margin(t = 5, r = 15, b = 5, l = 5)),
        axis.text = ggplot2::element_text(face = "plain", size = 8, color = "black"),
        axis.ticks = ggplot2::element_line(color = "black", linetype = "solid", lineend = "round"),
        legend.title = ggplot2::element_text(face = "bold", size = 12, color = couleur, hjust = 0.5),
        legend.text = ggplot2::element_text(face = "plain", size = 8, color = "black", lineheight = 0.75),
        panel.grid.major = ggplot2::element_line(colour = "black", linetype = 2, lineend = "round"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        panel.ontop = grille_top,
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),
        legend.position = "right",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2)
      )
  } else if (grille == "x"){
    theme = ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
      theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = couleur, hjust = 0.5, vjust = 1, margin = ggplot2::margin(t = 5, r = 10, b = 10, l = 10)),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.title.x = ggplot2::element_text(face = "bold", size = 12, color = couleur, margin = ggplot2::margin(t = 15, r = 5, b = 5, l = 5)),
        axis.title.y = ggplot2::element_text(face = "bold", size = 12, color = couleur, margin = ggplot2::margin(t = 5, r = 15, b = 5, l = 5)),
        axis.text = ggplot2::element_text(face = "plain", size = 8, color = "black"),
        axis.ticks = ggplot2::element_line(colour = "black", linetype = "solid", lineend = "round"),
        legend.title = ggplot2::element_text(face = "bold", size = 12, color = couleur, hjust = 0.5),
        legend.text = ggplot2::element_text(face = "plain", size = 8, color = "black", lineheight = 0.75),
        legend.position = "right",
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        panel.ontop = grille_top,
        panel.border = ggplot2::element_rect(colour = "black", fill = NA)
      )
  }
}

#### MARGE D'ERREUR ####

#' MargeErreur
#'
#' @return Retourne la marge d'erreur en nombre à virgule.
#' @description Fonction interactive qui permet de calculer la marge d'erreur d'un échantillon à partir du Z, du n et du p.
#' @export
MargeErreur = function()
{
  # Cette fonction sert à calculer la marge d'erreur d'un échantillon.
  cat("\n")
  Z = as.numeric(readline(prompt = "Entrez la valeur du Z associée au niveau de confiance désiré (95% = 1.96) : \n"))
  cat("\n")
  pr = as.numeric(readline(prompt = "Entrez la proporition à vérifier en base 1 (0.5 offre la plus grande marge d'erreur) : \n"))
  cat("\n")
  n = as.numeric(readline(prompt = "Entrez la taille de l'échantillon en valeur absolue : \n"))
  me=round(((sqrt(pr*(1-pr)/n))*Z),4)
  cat("\n")
  print(c("Taille de l'échantillon :", n),quote=F)
  print(c("Niveau de confiance :", Z),quote=F)
  print(c("Proportion :", pr),quote=F)
  print(c("Marge d'erreur :", me),quote=F)
  print(c("Intervalle de confiance (borne inférieure) :", pr-me),quote=F)
  print(c("Intervalle de confiance (borne supérieure) :", pr+me),quote=F)
  cat("\n")
  return(me)
}

#### IMPORTATION D'UNE BASE DE DONNÉES ####

#' Importer
#'
#' @param indicateurs Paramètre qui sert uniquement à l'importation de données provenant de la Banque Mondiale (package WDI). S'insère sous forme de vecteur de charactères.
#' @param colonnes A la valeur TRUE par défaut afin de nommer les colonnes de la base à partir de la première rangée de la source (changer pour colonnes = FALSE si la première rangée ne contient pas les noms des colonnes).
#' @param encodage A la valeur "UTF-8" par défaut afin de lire les caractères comme les accents. Il n'est pas recommandé d'en changer la valeur si vous ne comprenez pas ce qu'est l'encodage.
#' @return Retourne un dataframe transformé en "tibble" pour en faciliter les manipulations.
#' @description Fonction interactive qui permet d'importer une base de données à partir d'un fichier, d'un URL ou de la fonction WDI() du package éponyme.
#' @export
Importer = function(data = NULL, indicateurs = NULL, colonnes = TRUE, encodage = "UTF-8")
{
  cat("\n")
  format = as.numeric(readline(prompt = "Choix du format de fichier (inscrire le chiffre correspondant) : \n 1 = .xlsx \n 2 = .csv (anglais ,) \n 3 = .csv (français ;) \n 4 = .sav \n 5 = .dta \n 6 = Package WDI \n"))
  cat("\n")
  if (format == 1) {
    if (is.null(data)) {
      type = as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier = tcltk::tk_choose.files(caption = "Choisir le fichier de la base de données")
      } else if (type == 2) {
        fichier = as.character(readline(prompt = 'Copiez-collez le lien URL de la base de données sans guillemets ici :  \n'))
      }
    } else if (!is.null(data)) {
      fichier = as.character(data)
    }
    cat("\n")
    feuille = as.numeric(readline(prompt = "Entrez le numéro de la feuille Excel désirée (1, 2, 3, etc.) :  \n"))
    base = openxlsx::read.xlsx(fichier, sheet = feuille, colNames = colonnes)
  } else if (format == 2) {
    if (is.null(data)) {
      type = as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier = tcltk::tk_choose.files(caption = "Choisir le fichier de la base de données")
      } else if (type == 2) {
        fichier = as.character(readline(prompt = 'Copiez-collez le lien URL de la base de données sans guillemets ici :  \n'))
      }
    } else if (!is.null(data)) {
      fichier = as.character(data)
    }
    cat("\n")
    base = utils::read.csv(fichier, header = colonnes, encoding = encodage)
  } else if (format == 3) {
    if (is.null(data)) {
      type = as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier = tcltk::tk_choose.files(caption = "Choisir le fichier de la base de données")
      } else if (type == 2) {
        fichier = as.character(readline(prompt = 'Copiez-collez le lien URL de la base de données sans guillemets ici :  \n'))
      }
    } else if (!is.null(data)) {
      fichier = as.character(data)
    }
    cat("\n")
    base = utils::read.csv2(fichier, header = colonnes, encoding = encodage)
  } else if (format == 4) {
    if (is.null(data)) {
      type = as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier = tcltk::tk_choose.files(caption = "Choisir le fichier de la base de données")
      } else if (type == 2) {
        fichier = as.character(readline(prompt = 'Copiez-collez le lien URL de la base de données sans guillemets ici :  \n'))
      }
    } else if (!is.null(data)) {
      fichier = as.character(data)
    }
    cat("\n")
    base = Hmisc::spss.get(fichier, use.value.labels = colonnes, encoding = encodage)
  } else if (format == 5) {
    if (is.null(data)) {
      type = as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier = tcltk::tk_choose.files(caption = "Choisir le fichier de la base de données")
      } else if (type == 2) {
        fichier = as.character(readline(prompt = 'Copiez-collez le lien URL de la base de données sans guillemets ici :  \n'))
      }
    } else if (!is.null(data)) {
      fichier = as.character(data)
    }
    cat("\n")
    base = haven::read_dta(fichier, encoding = encodage)
  } else if (format == 6){
    nbr_pays = as.numeric(readline(prompt = "Entrez le nombre de pays pour lesquels les données seront importées :  \n"))
    cat("\n")
    pays = c()
    for (i in 1:nbr_pays){
      pays[i] = as.character(readline(prompt = paste("Entrez le code (ISO 2 lettres) du pays # ",i," :  \n", sep = "")))
      cat("\n")
    }
    start = as.numeric(readline(prompt = "Entrez la date de début : \n"))
    end = as.numeric(readline(prompt = "Entrez la date de fin : \n"))
    cat("\n")
    base = WDI::WDI(country = pays, indicator = indicateurs, start = start, end = end)
    cat("\n")
  }
  base = tibble::as_tibble(base)
  cat("\n")
  return(base)
}


#### RENOMMER DES VARIABLES ####

#' Renommer
#'
#' @param base Le nom de l'objet qui représente la base de données.
#' @param anciens_noms Les noms des variables à renommer, sous la forme c("nom1", "nom2", "nom3").
#' @param nouv_noms Les nouveaux noms choisis pour les mêmes variables, dans le même ordre, toujours sous la forme c("nom1", "nom2", "nom3").
#' @description Fonction qui utilise rename() mais qui simplifie son utilisation pour l'utilisateur·ice.
#' @return Retourne le dataframe avec les modifications des noms des variables.
#' @export
Renommer = function(base, anciens_noms, nouv_noms)
{
  cat("\n")
  for(i in 1:length(anciens_noms)){
    base = dplyr::rename(base, !! nouv_noms[i] := !! anciens_noms[i])
  }
  cat("\n")
  return(base)
}

#### ANALYSE UNIVARIÉE QUALITATIVE ####

#' SommaireQuali
#'
#' @param variable Variable qualitative (ou catégorielle) sous la forme "base$variable"
#' @param na.rm A la valeur TRUE par défaut afin de retirer les NA du calcul. Peut être changée pour FALSE afin de considérer les NA.
#' @description Fonction qui permet d'effectuer l'analyse univariée d'une variable qualitative en une seule étape.
#' @return Retourne le tableau des fréquences des valeurs possibles que peut prendre la variable analysée.
#' @export
SommaireQuali = function(variable, na.rm = TRUE)
{
  if (na.rm == TRUE){
    x = variable[!is.na(variable)]
  } else if (na.rm == FALSE){
    x = variable
  }
  freqq = freq(x)
  cat("\n")
  cat("Fréquences des valeurs de la variables :\n")
  cat("\n")
  print(freqq)
  cat("\n")
  y = as.factor(x)
  freq = summary(y)
  mode = names(freq)[freq[names(freq)] == max(freq)]
  if (suppressWarnings(!is.na(as.numeric(mode)))){
    mode = as.numeric(mode)
  } else if (mode=="TRUE"|mode=="FALSE"){
    mode = as.logical(mode)
  }
  print(c("Mode :", mode), quote = FALSE)
  return(freqq)
}

#### ANALYSE UNIVARIÉE QUANTITATIVE ####

#' SommaireQuanti
#'
#' @param variable Variable quantitative continue sous la forme "base$variable"
#' @param na.rm A la valeur TRUE par défaut afin de retirer les NA du calcul. Peut être changée pour FALSE afin de considérer les NA.
#' @description Fonction qui permet d'effectuer l'analyse univariée d'une variable quantitative continue en une seule étape.
#' @return Retourne une liste contenant les mesures de tendance centrale et de dispersion suivantes, dans cet ordre : moyenne, médiane et écart-type.
#' @export
SommaireQuanti = function(variable, na.rm = TRUE)
{
  if (na.rm == TRUE){
    moyenne = round(mean(as.numeric(variable),na.rm = TRUE),2)
    mediane = round(stats::median(as.numeric(variable),na.rm = TRUE),2)
    ecart_type = round(stats::sd(as.numeric(variable), na.rm = TRUE),2)
    quantiles = stats::quantile(as.numeric(variable),na.rm = TRUE,names = FALSE)
  } else if (na.rm == FALSE){
    moyenne = round(mean(as.numeric(variable),na.rm = FALSE),2)
    mediane = round(stats::median(as.numeric(variable),na.rm = FALSE),2)
    ecart_type = round(stats::sd(as.numeric(variable), na.rm = FALSE),2)
    quantiles = stats::quantile(as.numeric(variable),na.rm = FALSE,names = FALSE)
  }

  cat("\n")
  print(c("Moyenne :",moyenne), quote = FALSE)
  print(c("Ecart-type :",ecart_type), quote = FALSE)
  cat("\n")
  print(c("Minimum (0%) :",quantiles[1]), quote = FALSE)
  print(c("Premier quartile (25%) :",quantiles[2]), quote = FALSE)
  print(c("Mediane (50%) :",mediane), quote = FALSE)
  print(c("Dernier quartile (75%) :",quantiles[4]), quote = FALSE)
  print(c("Maximum (100%) :",quantiles[5]), quote = FALSE)
  return(list("moyenne" = moyenne, "mediane" = mediane, "ecart_type" = ecart_type))
}

# AJOUTER NUAGE DE POINTS ?

#### ANALYSE BIVARIÉE QUALI+QUALI ####

#' TableLabels
#'
#' @param variable_X Variable indépendante sous la forme "base$variable".
#' @param variable_Y Variable dépendante sous la forme "base$variable".
#' @description Fonction qui ajoute les noms des deux variables au table() afin de mieux identifier les colonnes et les rangées.
#' @return Retourne un tableau qui peut être sauvegardé dans un nouvel objet.
#' @export
TableLabels = function(variable_X, variable_Y)
{
  X_name_long = as.character(deparse(substitute(variable_X)))
  X_name = strsplit(X_name_long,"$", fixed=TRUE)
  X_name = as.character(X_name[[1]][2])

  Y_name_long = as.character(deparse(substitute(variable_Y)))
  Y_name = strsplit(Y_name_long,"$", fixed=TRUE)
  Y_name = as.character(Y_name[[1]][2])

  t1=table(variable_Y,variable_X)
  names(dimnames(t1)) = c(Y_name, X_name)
  return(t1)
}

#' QualiQuali
#'
#' @param variable_X Variable indépendante sous la forme "base$variable".
#' @param variable_Y Variable dépendante sous la forme "base$variable".
#' @param V_corrected A la valeur TRUE par défaut pour utiliser une version corrigée et donc plus précise du V de Cramer. À changer pour FALSE afin d'utiliser la version non corrigée.
#' @param na.rm A la valeur TRUE par défaut pour ne pas considérer les NA dans l'analyse. Changer cette valeur pour FALSE pourrait causer un message d'erreur. Si c'est le cas, rechanger pour TRUE.
#' @description Fonction qui permet d'effectuer automatiquement l'analyse bivariée entre deux variables qualitatives.
#' @return Retourne une liste contenant les mesures de significativité et de taille de l'effet suivantes, dans cet ordre : p-value et V de Cramer.
#' @export
QualiQuali = function(variable_X, variable_Y, V_corrected = TRUE, na.rm = TRUE)
{
  # V_corrected = FALSE --> utilise une version non ajustée du V de Cramer (la version classique)
  X_name_long = as.character(deparse(substitute(variable_X)))
  X_name = strsplit(X_name_long,"$", fixed=TRUE)
  X_name = as.character(X_name[[1]][2])

  Y_name_long = as.character(deparse(substitute(variable_Y)))
  Y_name = strsplit(Y_name_long,"$", fixed=TRUE)
  Y_name = as.character(Y_name[[1]][2])

  base_name_long = as.character(deparse(substitute(variable_X)))
  base_name = strsplit(base_name_long,"$", fixed=TRUE)
  base = get(as.name(base_name[[1]][1]))

  if (na.rm == TRUE){
    x = variable_X[!is.na(variable_X) & !is.na(variable_Y)]
    y = variable_Y[!is.na(variable_X) & !is.na(variable_Y)]
  } else if (na.rm == FALSE){
    x = variable_X
    y = variable_Y
  }

  t1 = TableLabels(x, y)

  cat("\n")
  cat("Tableau de contingence (% de colonnes) : ")
  cat("\n")
  gmodels::CrossTable(y, x, prop.r = FALSE, prop.t=FALSE, prop.chisq=FALSE, format = "SPSS", dnn = c(Y_name, X_name))
  cat("\n")

  chisq = stats::chisq.test(t1)
  p = as.numeric(chisq$p.value)
  if (V_corrected == FALSE){
    V = round(DescTools::CramerV(t1, correct = FALSE), 4)
  } else if (V_corrected == TRUE){
    V = round(DescTools::CramerV(t1, correct = TRUE), 4)
  }

  print(paste("Valeur du p :", p),quote=FALSE)
  cat("\n")
  if (p < 0.05){
    print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» est statistiquement significatif."), quote=FALSE)
    cat("\n")
    print(paste("V de Cramer :", V), quote=FALSE)
    cat("\n")
    if (V < 0.1){
      print(paste("La force de la relation entre la variable «", X_name, "» et la variable «", Y_name, "» est triviale puisque le V de Cramer inférieur à 0.1."), quote=FALSE)
    } else if (V >= 0.1 & V < 0.3){
      print(paste("La force de la relation entre la variable «", X_name, "» et la variable «", Y_name, "» est petite puisque le V de Cramer se situe entre 0.1 et 0.3."), quote=FALSE)
    }
    else if (V >= 0.3 & V < 0.5){
      print(paste("La force de la relation entre la variable «", X_name, "» et la variable «", Y_name, "» est moyenne puisque le V de Cramer se situe entre 0.3 et 0.5."), quote=FALSE)
    }
    else if (V >= 0.5){
      print(paste("La force de la relation entre la variable «", X_name, "» et la variable «", Y_name, "» est grande puisque le V de Cramer est supérieur à 0.5."), quote=FALSE)
    }
  } else if (p >= 0.05){
    print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de relation entre les deux variables."), quote=FALSE)
  }
  return(list("p_value" = p, "V_Cramer" = V))
}

#### ANALYSE BIVARIÉE QUALI+QUANTI ####

#' QualiQuanti
#'
#' @param variable_X Variable indépendante sous la forme "base$variable".
#' @param variable_Y Variable dépendante sous la forme "base$variable".
#' @param Y_ord A la valeur FALSE par défaut pour indiquer que la variable qualitative du croisement n'est pas ordinale (sans hiérarchie, nominale). Si la variable qualitative est ordinale (comme le niveau d'éducation), changer la valeur de l'Argument pour TRUE.
#' @param na.rm A la valeur TRUE par défaut pour ne pas considérer les NA dans l'analyse. Changer cette valeur pour FALSE pourrait causer un message d'erreur. Si c'est le cas, rechanger pour TRUE.
#' @description Fonction qui permet d'effectuer automatiquement l'analyse bivariée entre une variable qualitative et une variable quantitative. Elle détecte automatiquement si la variable qualitative est dichotomique ou polytomique.
#' @return Retourne une liste contenant nécessairement le p-value pour mesurer la significativité, puis la mesure de taille de l'effet correspondant au type de croisement : D de Cohen si VI dichotomique et tau de Kendall si VI polytomique ordinale.
#' @export
QualiQuanti = function(variable_X, variable_Y, Y_ord = FALSE, na.rm = TRUE)
{
  X_name_long = as.character(deparse(substitute(variable_X)))
  X_name = strsplit(X_name_long,"$", fixed=TRUE)
  X_name = as.character(X_name[[1]][2])

  Y_name_long = as.character(deparse(substitute(variable_Y)))
  Y_name = strsplit(Y_name_long,"$", fixed=TRUE)
  Y_name = as.character(Y_name[[1]][2])

  base_name_long = as.character(deparse(substitute(variable_X)))
  base_name = strsplit(base_name_long,"$", fixed=TRUE)
  base = get(as.name(base_name[[1]][1]))

  if (na.rm == TRUE){
    x = variable_X[!is.na(variable_X) & !is.na(variable_Y)]
    y = variable_Y[!is.na(variable_X) & !is.na(variable_Y)]
  } else if (na.rm == FALSE){
    x = variable_X
    y = variable_Y
  }

  t1 = tapply(y, x, FUN = mean)

  cat("\n")
  cat("Tableau des moyennes de chaque groupe : ")
  cat("\n")
  print(t(t1))
  cat("\n")

  if (length(unique(x)) == 2){
    t.test = t.test(y ~ x)
    p = t.test$p.value
    D = round(lsr::cohensD(y ~ x), 4)

    print(paste("Valeur du p :", p),quote=FALSE)
    cat("\n")
    if (p < 0.05){
      print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» est statistiquement significatif."), quote=FALSE)
      cat("\n")
      print(paste("D de Cohen :", D), quote=FALSE)
      cat("\n")
      if (D < 0.2){
        print(paste("La force de la relation entre la variable «", X_name, "» et la variable «", Y_name, "» est triviale puisque le D de Cohen inférieur à 0.2."), quote=FALSE)
      } else if (D >= 0.2 & D < 0.5){
        print(paste("La force de la relation entre la variable «", X_name, "» et la variable «", Y_name, "» est petite puisque le D de Cohen se situe entre 0.2 et 0.5."), quote=FALSE)
      }
      else if (D >= 0.5 & D < 0.8){
        print(paste("La force de la relation entre la variable «", X_name, "» et la variable «", Y_name, "» est moyenne puisque le D de Cohen se situe entre 0.5 et 0.8."), quote=FALSE)
      }
      else if (D >= 0.8){
        print(paste("La force de la relation entre la variable «", X_name, "» et la variable «", Y_name, "» est grande puisque le D de Cohen est supérieur à 0.8."), quote=FALSE)
      }
    } else if (p >= 0.05){
      print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de relation entre les deux variables."), quote=FALSE)
    }
    return(list("p_value" = p, "D_Cohen" = D))

  } else if (length(unique(x)) > 2){
    if (Y_ord == TRUE){
      stats = stats::cor.test(x, y, method = "kendall")
      p = stats$p.value
      tau = round(stats$estimate, 4)

      print(paste("Valeur du p :", p),quote=FALSE)
      cat("\n")
      if (p < 0.05){
        print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» est statistiquement significatif."), quote=FALSE)
        cat("\n")
        print(paste("tau de Kendall :", tau), quote=FALSE)
        cat("\n")
        if (tau >= 0){
          if (tau < 0.1){
            print(paste("La force de la relation proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est triviale puisque le tau de Kendall est inférieur à 0.1."), quote=FALSE)
          } else if (tau >= 0.1 & tau < 0.3){
            print(paste("La force de la relation proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est et petite puisque le tau de Kendall se situe entre 0.1 et 0.3."), quote=FALSE)
          }
          else if (tau >= 0.3 & tau < 0.5){
            print(paste("La force de la relation proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est et moyenne puisque le tau de Kendall se situe entre 0.3 et 0.5."), quote=FALSE)
          }
          else if (tau >= 0.5){
            print(paste("La force de la relation proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est et grande puisque le tau de Kendall est supérieur à 0.5."), quote=FALSE)
          }
        } else if (tau < 0){
          if (tau > -0.1){
            print(paste("La force de la relation inversement proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est triviale puisque le tau de Kendall est supérieur à -0.1."), quote=FALSE)
          } else if (tau <= -0.1 & tau > -0.3){
            print(paste("La force de la relation inversement proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est petite puisque le tau de Kendall se situe entre -0.1 et -0.3."), quote=FALSE)
          }
          else if (tau <= -0.3 & tau > -0.5){
            print(paste("La force de la relation inversement proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est moyenne puisque le tau de Kendall se situe entre -0.3 et -0.5."), quote=FALSE)
          }
          else if (tau <= -0.5){
            print(paste("La force de la relation inversement proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est grande puisque le tau de Kendall est inférieur à -0.5."), quote=FALSE)
          }
        }
      } else if (p >= 0.05){
        print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de relation entre les deux variables."), quote=FALSE)
      }
      return(list("p_value" = p, "tau_Kendall" = tau))

    } else if (Y_ord == FALSE){
      x = as.factor(x)
      anova = stats::aov(y ~ x)
      anova.res = summary(anova) # to print
      p = anova.res[[1]][[5]][1]
      cat("\n")
      cat("\n")
      cat("Modèle d'analyse de la variance à un facteur (one-way ANOVA) : \n")
      cat("\n")
      print(anova.res)
      cat("\n")
      cat("\n")
      print(paste("Valeur du p :", p),quote=FALSE)
      cat("\n")
      if (p < 0.05){
        print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» est statistiquement significatif. L'ANOVA détecte au minimum une différence significative entre les moyennes des groupes."), quote=FALSE)
        cat("\n")
        cat("\n")
        cat("\n")
        test = summary(multcomp::glht(anova, linfct = multcomp::mcp(x = "Tukey")))
        print(test)
        cat("\n")
      } else if (p >= 0.05){
        print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de différence significative entre les moyennes des groupes."), quote=FALSE)
      }
      return(list("p_value" = p))

    }
  }
}


#### ANALYSE BIVARIÉE QUANTI+QUANTI ####

#' QuantiQuanti
#'
#' @param variable_X Variable indépendante sous la forme "base$variable".
#' @param variable_Y Variable dépendante sous la forme "base$variable".
#' @param na.rm A la valeur TRUE par défaut pour ne pas considérer les NA dans l'analyse. Changer cette valeur pour FALSE pourrait causer un message d'erreur. Si c'est le cas, rechanger pour TRUE.
#' @description Fonction qui permet d'effectuer automatiquement l'analyse bivariée entre deux variables quantitatives continues. Si une variable quantitative est recodée en catégories, elle devient une variable qualitative.
#' @return Retourne une liste contenant les mesures de significativité et de taille de l'effet suivantes, dans cet ordre : p-value, r de Pearson (coefficient de corrélation) et R2 (coefficient de détermination).
#' @export
QuantiQuanti = function(variable_X, variable_Y, na.rm = TRUE)
{
  X_name_long = as.character(deparse(substitute(variable_X)))
  X_name = strsplit(X_name_long,"$", fixed=TRUE)
  X_name = as.character(X_name[[1]][2])

  Y_name_long = as.character(deparse(substitute(variable_Y)))
  Y_name = strsplit(Y_name_long,"$", fixed=TRUE)
  Y_name = as.character(Y_name[[1]][2])

  base_name_long = as.character(deparse(substitute(variable_X)))
  base_name = strsplit(base_name_long,"$", fixed=TRUE)
  base = get(as.name(base_name[[1]][1]))

  if (na.rm == TRUE){
    x = variable_X[!is.na(variable_X) & !is.na(variable_Y)]
    y = variable_Y[!is.na(variable_X) & !is.na(variable_Y)]
  } else if (na.rm == FALSE){
    x = variable_X
    y = variable_Y
  }

  modele = stats::lm(y ~ x)
  coeff = stats::coefficients(modele)
  if (round(coeff[1],1) >= 0){
    eq = as.character(paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1)))
  } else if (round(coeff[1],1) < 0){
    eq = as.character(paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1)))
  }
  {plot(x, y,
        main = paste("croisement entre la variable «", X_name, "» et la variable «", Y_name, "»"),
        sub = eq,
        xlab = X_name,
        ylab = Y_name)
    graphics::abline(modele, col = "blue")}
  cat("\n")
  cat("Modèle de régression linéaire simple : \n")
  cat("\n")
  print(summary(modele))
  cat("\n")

  cor = stats::cor.test(x, y, method = "pearson")
  p = as.numeric(cor$p.value)
  r = round(as.numeric(cor$estimate), 4)
  r2 = round((r^2)*100, 4)

  print(paste("Valeur du p :", p),quote=FALSE)
  cat("\n")
  if (p < 0.05){
    print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» est statistiquement significatif."), quote=FALSE)
    cat("\n")
    print(paste("r de Pearson (coefficient de corrélation) :", r), quote=FALSE)
    print(paste("R2 (coefficient de détermination en %) :", r2), quote=FALSE)
    cat("\n")
    if (r >= 0){
      if (r < 0.1){
        print(paste("La force de la relation proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est triviale puisque le r de Pearson est inférieur à 0.1."), quote=FALSE)
      } else if (r >= 0.1 & r < 0.3){
        print(paste("La force de la relation proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est et petite puisque le r de Pearson se situe entre 0.1 et 0.3."), quote=FALSE)
      }
      else if (r >= 0.3 & r < 0.5){
        print(paste("La force de la relation proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est et moyenne puisque le r de Pearson se situe entre 0.3 et 0.5."), quote=FALSE)
      }
      else if (r >= 0.5){
        print(paste("La force de la relation proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est et grande puisque le r de Pearson est supérieur à 0.5."), quote=FALSE)
      }
    } else if (r < 0){
      if (r > -0.1){
        print(paste("La force de la relation inversement proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est triviale puisque le r de Pearson est supérieur à -0.1."), quote=FALSE)
      } else if (r <= -0.1 & r > -0.3){
        print(paste("La force de la relation inversement proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est petite puisque le r de Pearson se situe entre -0.1 et -0.3."), quote=FALSE)
      }
      else if (r <= -0.3 & r > -0.5){
        print(paste("La force de la relation inversement proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est moyenne puisque le r de Pearson se situe entre -0.3 et -0.5."), quote=FALSE)
      }
      else if (r <= -0.5){
        print(paste("La force de la relation inversement proportionnelle entre la variable «", X_name, "» et la variable «", Y_name, "» est grande puisque le r de Pearson est inférieur à -0.5."), quote=FALSE)
      }
    }
  } else if (p >= 0.05){
    print(paste("Le croisement entre la variable «", X_name, "» et la variable «", Y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de relation entre les deux variables.\n"), quote=FALSE)
  }
  return(list("p_value" = p, "r_Pearson" = r, "R2" = r2))
}



#### NOTES SUPPLÉMENTAIRES ####

# Ceci crée un object qui a comme nom la valeur initiale de X (donc y) assigne la valeur "valeur"
# x = "y"
# assign(x, "valeur")
