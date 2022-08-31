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


.onAttach <- function(libname, pkgname) {
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

`:=` <- function(a, b) {
  stop(":= should not be called directly")
}

#' lm_eq
#'
#' @param x Variable indépendante (VI) sous la forme 'base$variable'
#' @param y Variable dépendante (VD) sous la forme 'base$variable'
#' @return Équation de la droite en format 'character'.
#' @description Sert à imprimer une équation de droite de régression sur un graphique.
#' @export
lm_eq <- function(x, y) {
  modele <- stats::lm(y ~ x)
  coeff <- stats::coefficients(modele)
  if (round(coeff[1], 1) >= 0) {
    eq <- as.character(paste0("y == ", round(coeff[2], 1), "*x + ", round(coeff[1], 1)))
  } else if (round(coeff[1], 1) < 0) {
    eq <- as.character(paste0("y == ", round(coeff[2], 1), "*x ", round(coeff[1], 1)))
  }
  eq
}

#' theme_pie
#'
#' @param couleur Le vert 'Université de Sherbrooke' est la couleur par défaut, mais elle peut être changée manuellement pour toute autre couleur.
#' @return Retourne le thème qui est ajouté (+) à un graphique fait à partir du package 'ggplot2'.
#' @description Thème personnalisé qui retire les fioritures dans un graphique en pointes de tarte, ou 'pie chart'.
#' @export
theme_pie <- function(couleur = "#018849", legende = TRUE) {
  if (legende == TRUE) {
    theme <- ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
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
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2.5),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        panel.ontop = TRUE,
        panel.border = ggplot2::element_blank()
      )
  } else if (legende == FALSE) {
    theme <- ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
      theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = couleur, hjust = 0.5, vjust = 1, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "none",
        legend.margin = ggplot2::margin(0, 0, 0, 0),
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
#' @param couleur Le vert 'Université de Sherbrooke' est la couleur par défaut, mais elle peut être changée manuellement pour toute autre couleur.
#' @param grille A la valeur 'x' par défaut, ce qui signifie aucune grille. La valeur 'h' permet une grille horizontale seulement, la valeur 'v' une grille verticale et la valeur 'hv' pour les deux grilles.
#' @param grille_top A la valeur FALSE par défaut, ce qui signifie que la grille se situe sous les objets du graphique, alors que TRUE appose la grille par-dessus.
#' @return Retourne le thème qui est ajouté (+) à un graphique fait à partir du package 'ggplot2'.
#' @description Thème personnalisé qui est fait pour s'ajuster à une présentation sur une document en format lettre (8 1/2 x 11).
#' @export
theme_defaut <- function(couleur = "#018849", grille = "x", grille_top = FALSE) {
  if (grille == "h") {
    theme <- ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
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
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2)
      )
  } else if (grille == "v") {
    theme <- ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
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
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2)
      )
  } else if (grille == "hv") {
    theme <- ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
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
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2)
      )
  } else if (grille == "x") {
    theme <- ggplot2::theme_bw(base_family = "Inter", base_line_size = 0.25) +
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
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin = ggplot2::margin(t = 0, r = 5, b = 0, l = -2),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        panel.ontop = grille_top,
        panel.border = ggplot2::element_rect(colour = "black", fill = NA)
      )
  }
}

#### MARGE D'ERREUR ####

#' marge_erreur
#'
#' @return Retourne la valeur de la marge et
#' @description Fonction interactive qui permet de mesurer la marge derreur dun échantillon à partir du Z, du n et du p. Les valeurs des arguments peuvent être entrées non-interactivement aussi.
#' @export
marge_erreur <- function(z = NULL, pr = NULL, n = NULL) {
  # Cette fonction sert à calculer la marge d'erreur d'un échantillon.
  if (is.null(c(z, pr, n))) {
    cat("\n")
    z <- as.numeric(readline(prompt = "Entrez la valeur du Z associée au niveau de confiance désiré (95% = 1.96) : \n"))
    cat("\n")
    pr <- as.numeric(readline(prompt = "Entrez la proporition à vérifier en base 1 (0.5 offre la plus grande marge d'erreur) : \n"))
    cat("\n")
    n <- as.numeric(readline(prompt = "Entrez la taille de l'échantillon en valeur absolue : \n"))
  }
  me <- round(((sqrt(pr * (1 - pr) / n)) * z), 4)
  cat("\n")
  print(c("Taille de l'échantillon :", n), quote = FALSE)
  print(c("Niveau de confiance :", z), quote = FALSE)
  print(c("Proportion :", pr), quote = FALSE)
  print(c("Marge d'erreur :", me), quote = FALSE)
  print(c("Intervalle de confiance (borne inférieure) :", pr - me), quote = FALSE)
  print(c("Intervalle de confiance (borne supérieure) :", pr + me), quote = FALSE)
  cat("\n")
  return(me)
}

#### IMPORTATION D'UNE BASE DE DONNÉES ####

#' importer
#'
#' @param indicateurs Paramètre qui sert uniquement à importer des données provenant de la Banque Mondiale (package 'WDI'). Est inséré sous forme de vecteur de caractères.
#' @param colonnes A la valeur 'TRUE' par défaut afin de nommer les colonnes de la base à partir de la première rangée de la source (changer pour 'colonnes = FALSE' si la première rangée ne contient pas les noms des colonnes).
#' @param encodage A la valeur 'UTF-8' par défaut afin de lire les caractères comme les accents. Ne changez PAS la valeur de cet argument si vous ne comprenez pas ce dont il est question.
#' @return Retourne un dataframe transformé en 'tibble' pour en faciliter les manipulations.
#' @description Fonction interactive qui importe une base de données qui est sauvegardée dans un fichier, accessible via un URL, ou sinon via la fonction 'WDI()' du package éponyme.
#' @export
importer <- function(data = NULL, indicateurs = NULL, colonnes = TRUE, encodage = "UTF-8") {
  cat("\n")
  format <- as.numeric(readline(prompt = "Choix du format de fichier (inscrire le chiffre correspondant) : \n 1 = .xlsx \n 2 = .csv (anglais ,) \n 3 = .csv (français ;) \n 4 = .sav \n 5 = .dta \n 6 = Package WDI \n"))
  cat("\n")
  if (format == 1) {
    if (is.null(data)) {
      type <- as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier <- file.choose()
      } else if (type == 2) {
        fichier <- as.character(readline(prompt = "Copiez-collez le lien URL de la base de données sans guillemets ici :  \n"))
      }
    } else if (!is.null(data)) {
      fichier <- as.character(data)
    }
    cat("\n")
    feuille <- as.numeric(readline(prompt = "Entrez le numéro de la feuille Excel désirée (1, 2, 3, etc.) :  \n"))
    base <- openxlsx::read.xlsx(fichier, sheet = feuille, colNames = colonnes)
  } else if (format == 2) {
    if (is.null(data)) {
      type <- as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier <- file.choose()
      } else if (type == 2) {
        fichier <- as.character(readline(prompt = "Copiez-collez le lien URL de la base de données sans guillemets ici :  \n"))
      }
    } else if (!is.null(data)) {
      fichier <- as.character(data)
    }
    cat("\n")
    base <- utils::read.csv(fichier, header = colonnes, encoding = encodage)
  } else if (format == 3) {
    if (is.null(data)) {
      type <- as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier <- file.choose()
      } else if (type == 2) {
        fichier <- as.character(readline(prompt = "Copiez-collez le lien URL de la base de données sans guillemets ici :  \n"))
      }
    } else if (!is.null(data)) {
      fichier <- as.character(data)
    }
    cat("\n")
    base <- utils::read.csv2(fichier, header = colonnes, encoding = encodage)
  } else if (format == 4) {
    if (is.null(data)) {
      type <- as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier <- file.choose()
      } else if (type == 2) {
        fichier <- as.character(readline(prompt = "Copiez-collez le lien URL de la base de données sans guillemets ici :  \n"))
      }
    } else if (!is.null(data)) {
      fichier <- as.character(data)
    }
    cat("\n")
    base <- Hmisc::spss.get(fichier, use.value.labels = colonnes, encoding = encodage)
  } else if (format == 5) {
    if (is.null(data)) {
      type <- as.numeric(readline(prompt = "Le fichier est-il téléchargé sur l'ordinateur ou hébergé sur un site web (URL)? \n 1 = fichier téléchargé \n 2 = hébergé sur un site web (URL) \n"))
      cat("\n")
      if (type == 1) {
        fichier <- file.choose()
      } else if (type == 2) {
        fichier <- as.character(readline(prompt = "Copiez-collez le lien URL de la base de données sans guillemets ici :  \n"))
      }
    } else if (!is.null(data)) {
      fichier <- as.character(data)
    }
    cat("\n")
    base <- haven::read_dta(fichier)
  } else if (format == 6) {
    nbr_pays <- as.numeric(readline(prompt = "Entrez le nombre de pays pour lesquels les données seront importées :  \n"))
    cat("\n")
    pays <- c()
    for (i in 1:nbr_pays) {
      pays[i] <- as.character(readline(prompt = paste("Entrez le code (ISO 2 lettres) du pays # ", i, " :  \n", sep = "")))
      cat("\n")
    }
    start <- as.numeric(readline(prompt = "Entrez la date de début : \n"))
    end <- as.numeric(readline(prompt = "Entrez la date de fin : \n"))
    cat("\n")
    base <- WDI::WDI(country = pays, indicator = indicateurs, start = start, end = end)
    cat("\n")
  }
  base <- tibble::as_tibble(base)
  cat("\n")
  return(base)
}


#### RENOMMER DES VARIABLES ####

#' renommer
#'
#' @param base Objet qui représente la base de données.
#' @param anciens_noms Les noms des variables à renommer, sous la forme 'c("nom1", "nom2", "nom3")'.
#' @param nouv_noms Les nouveaux noms choisis pour les mêmes variables, dans le même ordre, toujours sous la forme 'c("nom1", "nom2", "nom3")'.
#' @description Fonction qui utilise 'rename()' mais qui simplifie son utilisation pour son utilisateur·ice.
#' @return Retourne le dataframe avec les modifications des noms des variables.
#' @export
renommer <- function(base, anciens_noms, nouv_noms) {
  cat("\n")
  for (i in seq_along(anciens_noms)) {
    base <- dplyr::rename(base, !!nouv_noms[i] := !!anciens_noms[i])
  }
  cat("\n")
  return(base)
}

#### ANALYSE UNIVARIÉE QUALITATIVE ####

#' univar_quali
#'
#' @param variable Variable qualitative (ou catégorielle) sous la forme 'base$variable'
#' @param na.rm A la valeur 'TRUE' par défaut afin de retirer les NA du calcul. Peut être changée pour 'FALSE' afin de considérer les NA.
#' @description Fonction qui effectue une analyse univariée sur une variable qualitative en une seule étape.
#' @return Retourne le 'tableau des fréquences des valeurs' possibles que peut prendre la variable analysée.
#' @export
univar_quali <- function(variable, na.rm = TRUE) {
  if (na.rm == TRUE) {
    x <- variable[!is.na(variable)]
  } else if (na.rm == FALSE) {
    x <- variable
  }
  freqq <- freq(x)
  cat("\n")
  cat("Fréquences des valeurs de la variables :\n")
  cat("\n")
  print(freqq)
  cat("\n")
  y <- as.factor(x)
  freq <- summary(y)
  mode <- names(freq)[freq[names(freq)] == max(freq)]
  if (suppressWarnings(!is.na(as.numeric(mode)))) {
    mode <- as.numeric(mode)
  } else if (mode == "TRUE" || mode == "FALSE") {
    mode <- as.logical(mode)
  }
  print(c("Mode :", mode), quote = FALSE)
  cat("\n")
  return(list("mode" = mode))
}

#### ANALYSE UNIVARIÉE QUANTITATIVE ####

#' univar_quanti
#'
#' @param variable Variable quantitative continue sous la forme 'base$variable'
#' @param na.rm A la valeur 'TRUE' par défaut afin de retirer les NA du calcul. Peut être changée pour 'FALSE' afin de considérer les NA.
#' @description Fonction qui permet effectue une analyse univariée sur une variable quantitative continue en une seule étape.
#' @return Retourne une liste contenant les mesures de tendance centrale et de dispersion suivantes, dans cet ordre : 'moyenne, médiane et écart-type'.
#' @export
univar_quanti <- function(variable, na.rm = TRUE) {
  if (na.rm == TRUE) {
    moyenne <- round(mean(as.numeric(variable), na.rm = TRUE), 2)
    mediane <- round(stats::median(as.numeric(variable), na.rm = TRUE), 2)
    ecart_type <- round(stats::sd(as.numeric(variable), na.rm = TRUE), 2)
    quantiles <- stats::quantile(as.numeric(variable), na.rm = TRUE, names = FALSE)
  } else if (na.rm == FALSE) {
    moyenne <- round(mean(as.numeric(variable), na.rm = FALSE), 2)
    mediane <- round(stats::median(as.numeric(variable), na.rm = FALSE), 2)
    ecart_type <- round(stats::sd(as.numeric(variable), na.rm = FALSE), 2)
    quantiles <- stats::quantile(as.numeric(variable), na.rm = FALSE, names = FALSE)
  }

  cat("\n")
  print(c("Moyenne :", moyenne), quote = FALSE)
  print(c("Ecart-type :", ecart_type), quote = FALSE)
  cat("\n")
  print(c("Minimum (0%) :", quantiles[1]), quote = FALSE)
  print(c("Premier quartile (25%) :", quantiles[2]), quote = FALSE)
  print(c("Mediane (50%) :", mediane), quote = FALSE)
  print(c("Dernier quartile (75%) :", quantiles[4]), quote = FALSE)
  print(c("Maximum (100%) :", quantiles[5]), quote = FALSE)
  cat("\n")
  return(list("moyenne" = moyenne, "mediane" = mediane, "ecart_type" = ecart_type))
}

# AJOUTER NUAGE DE POINTS ?

#### ANALYSE BIVARIÉE QUALI+QUALI ####

#' table_nomsvar
#'
#' @param variable_x Variable indépendante sous la forme 'base$variable'.
#' @param variable_y Variable dépendante sous la forme 'base$variable'.
#' @description Fonction qui ajoute les noms des deux variables au 'table()'' afin de mieux identifier les colonnes et les rangées.
#' @return Retourne un tableau qui peut être sauvegardé dans un nouvel objet.
#' @export
table_nomsvar <- function(variable_x, variable_y) {
  x_name_long <- as.character(deparse(substitute(variable_x)))
  x_name <- strsplit(x_name_long, "$", fixed = TRUE)
  x_name <- as.character(x_name[[1]][2])

  y_name_long <- as.character(deparse(substitute(variable_y)))
  y_name <- strsplit(y_name_long, "$", fixed = TRUE)
  y_name <- as.character(y_name[[1]][2])

  t1 <- table(variable_y, variable_x)
  names(dimnames(t1)) <- c(y_name, x_name)
  return(t1)
}

#' bivar_quali_quali
#'
#' @param variable_x Variable indépendante sous la forme 'base$variable'.
#' @param variable_y Variable dépendante sous la forme 'base$variable'.
#' @param v_corrige A la valeur 'TRUE' par défaut pour utiliser une version corrigée et donc plus précise du V de Cramer. À changer pour 'FALSE' pour utiliser la version non corrigée.
#' @param na.rm A la valeur 'TRUE' par défaut afin de retirer les NA du calcul. Peut être changée pour 'FALSE' afin de considérer les NA.
#' @description Fonction qui effectue automatiquement une analyse bivariée entre deux variables qualitatives sélectionnées.
#' @return Retourne une liste contenant les mesures de significativité et de taille de leffet suivantes, dans cet ordre : p-value et V de Cramer.
#' @export
bivar_quali_quali <- function(variable_x, variable_y, v_corrige = TRUE, na.rm = TRUE) {
  # v_corrige = FALSE --> utilise une version non ajustée du V de Cramer (la version classique)
  x_name_long <- as.character(deparse(substitute(variable_x)))
  x_name <- strsplit(x_name_long, "$", fixed = TRUE)
  x_name <- as.character(x_name[[1]][2])

  y_name_long <- as.character(deparse(substitute(variable_y)))
  y_name <- strsplit(y_name_long, "$", fixed = TRUE)
  y_name <- as.character(y_name[[1]][2])

  base_name_long <- as.character(deparse(substitute(variable_x)))
  base_name <- strsplit(base_name_long, "$", fixed = TRUE)
  base <- get(as.name(base_name[[1]][1]))

  if (na.rm == TRUE) {
    x <- variable_x[!is.na(variable_x) & !is.na(variable_y)]
    y <- variable_y[!is.na(variable_x) & !is.na(variable_y)]
  } else if (na.rm == FALSE) {
    x <- variable_x
    y <- variable_y
  }

  t1 <- table_nomsvar(x, y)

  cat("\n")
  cat("Tableau de contingence (% de colonnes) : ")
  cat("\n")
  gmodels::CrossTable(y, x, prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE, format = "SPSS", dnn = c(y_name, x_name))
  cat("\n")

  chisq <- stats::chisq.test(t1)
  p <- as.numeric(chisq$p.value)
  if (v_corrige == FALSE) {
    v <- round(DescTools::CramerV(t1, correct = FALSE), 4)
  } else if (v_corrige == TRUE) {
    v <- round(DescTools::CramerV(t1, correct = TRUE), 4)
  }

  print(paste("Valeur du p :", p), quote = FALSE)
  cat("\n")
  if (p < 0.05) {
    print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» est statistiquement significatif."), quote = FALSE)
    cat("\n")
    print(paste("V de Cramer :", v), quote = FALSE)
    cat("\n")
    if (v < 0.1) {
      print(paste("La force de la relation entre la variable «", x_name, "» et la variable «", y_name, "» est triviale puisque le V de Cramer inférieur à 0.1."), quote = FALSE)
      cat("\n")
    } else if (v >= 0.1 && v < 0.3) {
      print(paste("La force de la relation entre la variable «", x_name, "» et la variable «", y_name, "» est petite puisque le V de Cramer se situe entre 0.1 et 0.3."), quote = FALSE)
      cat("\n")
    } else if (v >= 0.3 && v < 0.5) {
      print(paste("La force de la relation entre la variable «", x_name, "» et la variable «", y_name, "» est moyenne puisque le V de Cramer se situe entre 0.3 et 0.5."), quote = FALSE)
      cat("\n")
    } else if (v >= 0.5) {
      print(paste("La force de la relation entre la variable «", x_name, "» et la variable «", y_name, "» est grande puisque le V de Cramer est supérieur à 0.5."), quote = FALSE)
      cat("\n")
    }
  } else if (p >= 0.05) {
    print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de relation entre les deux variables."), quote = FALSE)
    cat("\n")
  }
  return(list("p_value" = p, "V_Cramer" = v))
}

#### ANALYSE BIVARIÉE QUALI+QUANTI ####

#' bivar_quali_quanti
#'
#' @param variable_x Variable indépendante sous la forme 'base$variable'.
#' @param variable_y Variable dépendante sous la forme 'base$variable'.
#' @param y_ord A la valeur 'FALSE' par défaut pour indiquer que la variable qualitative du croisement est polytomique nominale (donc sans hiérarchie, non-ordinale). Si la variable qualitative est ordinale (comme un niveau de satisfaction), changer la valeur de 'y_ord' pour 'TRUE'.
#' @param na.rm A la valeur 'TRUE' par défaut afin de retirer les NA du calcul. Peut être changée pour 'FALSE' afin de considérer les NA.
#' @description Fonction qui effectue automatiquement une analyse bivariée entre une variable qualitative et une variable quantitative. Elle détecte automatiquement si la variable qualitative est dichotomique ou polytomique, mais on doit indiquer manuellement son type si elle est polytomique (ordinale ou nominale).
#' @return Retourne une liste contenant nécessairement le p-value pour mesurer la significativité, puis la mesure de taille de leffet correspondant au type de croisement : D de Cohen si VI dichotomique et tau de Kendall si VI polytomique ordinale.
#' @export
bivar_quali_quanti <- function(variable_x, variable_y, y_ord = FALSE, na.rm = TRUE) {
  x_name_long <- as.character(deparse(substitute(variable_x)))
  x_name <- strsplit(x_name_long, "$", fixed = TRUE)
  x_name <- as.character(x_name[[1]][2])

  y_name_long <- as.character(deparse(substitute(variable_y)))
  y_name <- strsplit(y_name_long, "$", fixed = TRUE)
  y_name <- as.character(y_name[[1]][2])

  base_name_long <- as.character(deparse(substitute(variable_x)))
  base_name <- strsplit(base_name_long, "$", fixed = TRUE)
  base <- get(as.name(base_name[[1]][1]))

  if (na.rm == TRUE) {
    x <- variable_x[!is.na(variable_x) & !is.na(variable_y)]
    y <- variable_y[!is.na(variable_x) & !is.na(variable_y)]
  } else if (na.rm == FALSE) {
    x <- variable_x
    y <- variable_y
  }

  t1 <- tapply(y, x, FUN = mean)

  cat("\n")
  cat("Tableau des moyennes de chaque groupe : ")
  cat("\n")
  print(t(t1))
  cat("\n")

  if (length(unique(x)) == 2) {
    t.test <- t.test(y ~ x)
    p <- t.test$p.value
    d <- round(lsr::cohensD(y ~ x), 4)

    print(paste("Valeur du p :", p), quote = FALSE)
    cat("\n")
    if (p < 0.05) {
      print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» est statistiquement significatif."), quote = FALSE)
      cat("\n")
      print(paste("D de Cohen :", d), quote = FALSE)
      cat("\n")
      if (d < 0.2) {
        print(paste("La force de la relation entre la variable «", x_name, "» et la variable «", y_name, "» est triviale puisque le D de Cohen inférieur à 0.2."), quote = FALSE)
        cat("\n")
      } else if (d >= 0.2 && d < 0.5) {
        print(paste("La force de la relation entre la variable «", x_name, "» et la variable «", y_name, "» est petite puisque le D de Cohen se situe entre 0.2 et 0.5."), quote = FALSE)
        cat("\n")
      } else if (d >= 0.5 && d < 0.8) {
        print(paste("La force de la relation entre la variable «", x_name, "» et la variable «", y_name, "» est moyenne puisque le D de Cohen se situe entre 0.5 et 0.8."), quote = FALSE)
        cat("\n")
      } else if (d >= 0.8) {
        print(paste("La force de la relation entre la variable «", x_name, "» et la variable «", y_name, "» est grande puisque le D de Cohen est supérieur à 0.8."), quote = FALSE)
        cat("\n")
      }
    } else if (p >= 0.05) {
      print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de relation entre les deux variables."), quote = FALSE)
      cat("\n")
    }
    return(list("p_value" = p, "D_Cohen" = d))
  } else if (length(unique(x)) > 2) {
    if (y_ord == TRUE) {
      stats <- stats::cor.test(x, y, method = "kendall")
      p <- stats$p.value
      tau <- round(stats$estimate, 4)

      print(paste("Valeur du p :", p), quote = FALSE)
      cat("\n")
      if (p < 0.05) {
        print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» est statistiquement significatif."), quote = FALSE)
        cat("\n")
        print(paste("tau de Kendall :", tau), quote = FALSE)
        cat("\n")
        if (tau >= 0) {
          if (tau < 0.1) {
            print(paste("La force de la relation proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est triviale puisque le tau de Kendall est inférieur à 0.1."), quote = FALSE)
            cat("\n")
          } else if (tau >= 0.1 && tau < 0.3) {
            print(paste("La force de la relation proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est et petite puisque le tau de Kendall se situe entre 0.1 et 0.3."), quote = FALSE)
            cat("\n")
          } else if (tau >= 0.3 && tau < 0.5) {
            print(paste("La force de la relation proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est et moyenne puisque le tau de Kendall se situe entre 0.3 et 0.5."), quote = FALSE)
            cat("\n")
          } else if (tau >= 0.5) {
            print(paste("La force de la relation proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est et grande puisque le tau de Kendall est supérieur à 0.5."), quote = FALSE)
            cat("\n")
          }
        } else if (tau < 0) {
          if (tau > -0.1) {
            print(paste("La force de la relation inversement proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est triviale puisque le tau de Kendall est supérieur à -0.1."), quote = FALSE)
            cat("\n")
          } else if (tau <= -0.1 && tau > -0.3) {
            print(paste("La force de la relation inversement proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est petite puisque le tau de Kendall se situe entre -0.1 et -0.3."), quote = FALSE)
            cat("\n")
          } else if (tau <= -0.3 && tau > -0.5) {
            print(paste("La force de la relation inversement proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est moyenne puisque le tau de Kendall se situe entre -0.3 et -0.5."), quote = FALSE)
            cat("\n")
          } else if (tau <= -0.5) {
            print(paste("La force de la relation inversement proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est grande puisque le tau de Kendall est inférieur à -0.5."), quote = FALSE)
            cat("\n")
          }
        }
      } else if (p >= 0.05) {
        print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de relation entre les deux variables."), quote = FALSE)
        cat("\n")
      }
      return(list("p_value" = p, "tau_Kendall" = tau))
    } else if (y_ord == FALSE) {
      x <- as.factor(x)
      anova <- stats::aov(y ~ x)
      anova.res <- summary(anova) # to print
      p <- anova.res[[1]][[5]][1]
      cat("\n")
      cat("\n")
      cat("Modèle d'analyse de la variance à un facteur (one-way ANOVA) : \n")
      cat("\n")
      print(anova.res)
      cat("\n")
      cat("\n")
      print(paste("Valeur du p :", p), quote = FALSE)
      cat("\n")
      if (p < 0.05) {
        print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» est statistiquement significatif. L'ANOVA détecte au minimum une différence significative entre les moyennes des groupes."), quote = FALSE)
        cat("\n")
        cat("\n")
        cat("\n")
        test <- summary(multcomp::glht(anova, linfct = multcomp::mcp(x = "Tukey")))
        print(test)
        cat("\n")
      } else if (p >= 0.05) {
        print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de différence significative entre les moyennes des groupes."), quote = FALSE)
        cat("\n")
      }
      return(list("p_value" = p))
    }
  }
}

#### ANALYSE BIVARIÉE QUANTI+QUANTI ####

#' bivar_quanti_quanti
#'
#' @param variable_x Variable indépendante sous la forme 'base$variable'.
#' @param variable_y Variable dépendante sous la forme 'base$variable'.
#' @param na.rm A la valeur 'TRUE' par défaut afin de retirer les NA du calcul. Peut être changée pour 'FALSE' afin de considérer les NA. Changer cette valeur pour 'FALSE' pourrait toutefois causer une erreur. Rechanger pour 'TRUE' si cela se produit.
#' @description Fonction qui permet d'effectuer automatiquement une analyse bivariée entre deux variables quantitatives continues. Si une variable quantitative est recodée en catégories, elle devient généralement une variable qualitative.
#' @return Retourne une liste contenant les mesures de significativité et de taille de leffet suivantes, dans cet ordre : p-value, r de Pearson (coefficient de corrélation) et R2 (coefficient de détermination).
#' @export
bivar_quanti_quanti <- function(variable_x, variable_y, na.rm = TRUE) {
  x_name_long <- as.character(deparse(substitute(variable_x)))
  x_name <- strsplit(x_name_long, "$", fixed = TRUE)
  x_name <- as.character(x_name[[1]][2])

  y_name_long <- as.character(deparse(substitute(variable_y)))
  y_name <- strsplit(y_name_long, "$", fixed = TRUE)
  y_name <- as.character(y_name[[1]][2])

  base_name_long <- as.character(deparse(substitute(variable_x)))
  base_name <- strsplit(base_name_long, "$", fixed = TRUE)
  base <- get(as.name(base_name[[1]][1]))

  if (na.rm == TRUE) {
    x <- variable_x[!is.na(variable_x) & !is.na(variable_y)]
    y <- variable_y[!is.na(variable_x) & !is.na(variable_y)]
  } else if (na.rm == FALSE) {
    x <- variable_x
    y <- variable_y
  }

  modele <- stats::lm(y ~ x)
  coeff <- stats::coefficients(modele)
  if (round(coeff[1], 1) >= 0) {
    eq <- as.character(paste0("y = ", round(coeff[2], 1), "*x + ", round(coeff[1], 1)))
  } else if (round(coeff[1], 1) < 0) {
    eq <- as.character(paste0("y = ", round(coeff[2], 1), "*x ", round(coeff[1], 1)))
  }
  {plot(x, y,
    main = paste("croisement entre la variable «", x_name, "» et la variable «", y_name, "»"),
    sub = eq,
    xlab = x_name,
    ylab = y_name
    )
    graphics::abline(modele, col = "blue")
  }
  cat("\n")
  cat("Modèle de régression linéaire simple : \n")
  cat("\n")
  print(summary(modele))
  cat("\n")

  cor <- stats::cor.test(x, y, method = "pearson")
  p <- as.numeric(cor$p.value)
  r <- round(as.numeric(cor$estimate), 4)
  r2 <- round((r^2) * 100, 4)

  print(paste("Valeur du p :", p), quote = FALSE)
  cat("\n")
  if (p < 0.05) {
    print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» est statistiquement significatif."), quote = FALSE)
    cat("\n")
    print(paste("r de Pearson (coefficient de corrélation) :", r), quote = FALSE)
    print(paste("R2 (coefficient de détermination en %) :", r2), quote = FALSE)
    cat("\n")
    if (r >= 0) {
      if (r < 0.1) {
        print(paste("La force de la relation proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est triviale puisque le r de Pearson est inférieur à 0.1."), quote = FALSE)
        cat("\n")
      } else if (r >= 0.1 && r < 0.3) {
        print(paste("La force de la relation proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est et petite puisque le r de Pearson se situe entre 0.1 et 0.3."), quote = FALSE)
        cat("\n")
      } else if (r >= 0.3 && r < 0.5) {
        print(paste("La force de la relation proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est et moyenne puisque le r de Pearson se situe entre 0.3 et 0.5."), quote = FALSE)
        cat("\n")
      } else if (r >= 0.5) {
        print(paste("La force de la relation proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est et grande puisque le r de Pearson est supérieur à 0.5."), quote = FALSE)
        cat("\n")
      }
    } else if (r < 0) {
      if (r > -0.1) {
        print(paste("La force de la relation inversement proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est triviale puisque le r de Pearson est supérieur à -0.1."), quote = FALSE)
        cat("\n")
      } else if (r <= -0.1 && r > -0.3) {
        print(paste("La force de la relation inversement proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est petite puisque le r de Pearson se situe entre -0.1 et -0.3."), quote = FALSE)
        cat("\n")
      } else if (r <= -0.3 && r > -0.5) {
        print(paste("La force de la relation inversement proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est moyenne puisque le r de Pearson se situe entre -0.3 et -0.5."), quote = FALSE)
        cat("\n")
      } else if (r <= -0.5) {
        print(paste("La force de la relation inversement proportionnelle entre la variable «", x_name, "» et la variable «", y_name, "» est grande puisque le r de Pearson est inférieur à -0.5."), quote = FALSE)
        cat("\n")
      }
    }
  } else if (p >= 0.05) {
    print(paste("Le croisement entre la variable «", x_name, "» et la variable «", y_name, "» n'est pas statistiquement significatif. Il n'y a donc pas de relation entre les deux variables.\n"), quote = FALSE)
    cat("\n")
  }
  return(list("p_value" = p, "r_Pearson" = r, "R2" = r2))
}

#### ANALYSE MULTIVARIÉE REGRESSION LINEAIRE ####

#### ANALYSE MULTIVARIÉE REGRESSION LOGISTIQUE ####

#### ANALYSE MULTIVARIÉE ANALYSES FACTORIELLES ####

#### NOTES SUPPLÉMENTAIRES ####

# Ceci crée un object qui a comme nom la valeur initiale de X (donc y) assigne la valeur "valeur"
# x = "y"
# assign(x, "valeur")
