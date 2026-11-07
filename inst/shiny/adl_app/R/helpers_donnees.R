# Fonctions pures : lecture du fichier, validation des choix, ajustement.
# Chaque erreur est levee avec stop(<message en francais>, call. = FALSE)
# pour que l'interface l'affiche telle quelle, sans jamais planter.

SEPARATEURS <- c("Tabulation" = "\t", "Point-virgule ( ; )" = ";",
                 "Virgule ( , )" = ",", "Espace" = " ")
SEUIL_CLASSES <- 20   # au-dela, la cible n'est pas consideree comme categorielle

# Lit un fichier CSV/TXT ou Excel et renvoie un data.frame propre.
lire_donnees <- function(chemin, nom = basename(chemin), sep = "\t", dec = ".",
                         entete = TRUE, feuille = 1, colonnes_min = 2) {
  if (is.null(chemin) || !file.exists(chemin)) {
    stop("Le fichier est introuvable.", call. = FALSE)
  }
  if (isTRUE(file.size(chemin) == 0)) stop("Le fichier est vide.", call. = FALSE)
  if (length(feuille) != 1 || is.na(feuille) || feuille < 1) feuille <- 1

  if (tolower(tools::file_ext(nom)) %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Pour lire un fichier Excel, installez le package readxl : ",
           "install.packages(\"readxl\").", call. = FALSE)
    }
    d <- tryCatch(
      as.data.frame(readxl::read_excel(chemin, sheet = feuille, col_names = entete)),
      error = function(e) stop("Impossible de lire le fichier Excel (feuille ", feuille,
                               ") : fichier abîmé ou numéro de feuille incorrect.",
                               call. = FALSE))
  } else {
    lignes <- tryCatch(readLines(chemin, warn = FALSE, skipNul = TRUE),
                       error = function(e) stop("Le fichier n'est pas un fichier texte lisible.",
                                                call. = FALSE))
    # UTF-8 si le fichier est valide, sinon Latin-1 (cas courant d'Excel sous Windows)
    enc <- if (all(validUTF8(lignes))) "UTF-8-BOM" else "latin1"
    d <- tryCatch(
      suppressWarnings(utils::read.table(
        chemin, header = entete, sep = sep, dec = dec, quote = "\"",
        comment.char = "", na.strings = c("NA", "", "NaN"), strip.white = TRUE,
        stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = enc)),
      error = function(e) stop(
        "Fichier illisible avec ce séparateur : vérifiez le séparateur de colonnes ",
        "et la case des noms de colonnes. Détail : ", conditionMessage(e), call. = FALSE))
  }

  # nettoyage : noms non vides et uniques, lignes/colonnes entierement vides retirees
  noms <- trimws(names(d))
  vides <- is.na(noms) | noms == ""
  noms[vides] <- paste0("V", which(vides))
  names(d) <- make.unique(noms)
  d <- d[rowSums(!is.na(d)) > 0, colSums(!is.na(d)) > 0, drop = FALSE]
  rownames(d) <- NULL

  if (ncol(d) < colonnes_min) {
    stop("Une seule colonne détectée : le séparateur choisi n'est probablement pas le bon.",
         call. = FALSE)
  }
  if (nrow(d) < 2) stop("Le fichier contient moins de 2 lignes de données.", call. = FALSE)
  d
}

# "numérique" ou "qualitative" (texte, facteur, booleen)
type_variable <- function(x) if (is.numeric(x)) "numérique" else "qualitative"

# Structure d'un jeu de donnees : une ligne par variable
table_structure <- function(d) {
  data.frame(
    Variable = names(d),
    Type = vapply(d, type_variable, character(1)),
    `Valeurs manquantes` = vapply(d, function(x) as.character(sum(is.na(x))), character(1)),
    `Valeurs distinctes` = vapply(d, function(x) as.character(length(unique(x[!is.na(x)]))),
                                  character(1)),
    check.names = FALSE, row.names = NULL
  )
}

# Colonnes (apres recodage) redondantes a l'interieur des classes : celles qui
# rendraient la matrice W non inversible.
detecter_colinearite <- function(X, y) {
  M <- adlSISE::Encodeur$new()$fit_transform(X)
  g <- as.integer(y)
  centre <- M - (rowsum(M, g) / tabulate(g))[g, , drop = FALSE]
  norme <- sqrt(colSums(centre^2))
  nuls <- norme < 1e-10
  redondantes <- colnames(M)[nuls]
  if (any(!nuls)) {
    Z <- sweep(centre[, !nuls, drop = FALSE], 2, norme[!nuls], "/")
    q <- qr(Z)
    if (q$rank < ncol(Z)) {
      redondantes <- c(redondantes, colnames(Z)[q$pivot[(q$rank + 1):ncol(Z)]])
    }
  }
  redondantes
}

# Verifie les choix de l'utilisateur et prepare X (explicatives) et y (cible).
# Renvoie une liste ; leve une erreur francaise au moindre probleme.
preparer_analyse <- function(donnees, cible, explicatives,
                             illustratives = character(0), supprimer_na = FALSE) {
  if (is.null(donnees)) stop("Chargez d'abord un fichier de données.", call. = FALSE)
  if (is.null(cible) || !nzchar(cible)) stop("Choisissez la variable cible.", call. = FALSE)
  if (length(explicatives) == 0) {
    stop("Choisissez au moins une variable explicative.", call. = FALSE)
  }
  if (cible %in% explicatives) {
    stop("La variable cible ne peut pas être aussi une variable explicative.", call. = FALSE)
  }
  absentes <- setdiff(c(cible, explicatives, illustratives), names(donnees))
  if (length(absentes) > 0) {
    stop("Colonnes absentes du fichier : ", paste(absentes, collapse = ", "), call. = FALSE)
  }
  illustratives <- setdiff(illustratives, c(cible, explicatives))

  y <- donnees[[cible]]
  X <- donnees[explicatives]

  # valeurs infinies
  infinies <- names(X)[vapply(X, function(x) is.numeric(x) && any(is.infinite(x)), logical(1))]
  if (length(infinies) > 0) {
    stop("Valeurs infinies dans : ", paste(infinies, collapse = ", "), call. = FALSE)
  }

  # valeurs manquantes (cible ou explicatives)
  ok <- stats::complete.cases(X) & !is.na(y)
  supprimees <- sum(!ok)
  if (supprimees > 0) {
    if (!supprimer_na) {
      comptes <- c(stats::setNames(sum(is.na(y)), cible),
                   vapply(X, function(x) sum(is.na(x)), numeric(1)))
      comptes <- comptes[comptes > 0]
      stop("Valeurs manquantes détectées (", paste0(names(comptes), " : ", comptes, collapse = ", "),
           "). Cochez « Supprimer les lignes incomplètes » ou corrigez le fichier.", call. = FALSE)
    }
    y <- y[ok]
    X <- X[ok, , drop = FALSE]
    if (length(y) < 3) {
      stop("Il reste moins de 3 lignes après suppression des valeurs manquantes.", call. = FALSE)
    }
  }

  # cible categorielle
  if (is.numeric(y) && length(unique(y)) > SEUIL_CLASSES) {
    stop("La cible « ", cible, " » est numérique continue (", length(unique(y)),
         " valeurs distinctes) : choisissez une variable catégorielle.", call. = FALSE)
  }
  y <- droplevels(factor(y))
  K <- nlevels(y)
  if (K < 2) {
    stop("La cible « ", cible, " » ne comporte que ", K, " classe : il en faut au moins 2.",
         call. = FALSE)
  }
  if (K > SEUIL_CLASSES) {
    stop("La cible « ", cible, " » a ", K, " classes (maximum ", SEUIL_CLASSES,
         ") : ce n'est pas une variable catégorielle utilisable.", call. = FALSE)
  }
  petites <- names(which(table(y) < 2))
  if (length(petites) > 0) {
    stop("Classe(s) avec un seul individu : ", paste(petites, collapse = ", "),
         ". Il faut au moins 2 individus par classe.", call. = FALSE)
  }

  # qualitatives -> facteurs ; variables constantes interdites
  X[] <- lapply(X, function(x) if (is.numeric(x)) x else factor(x))
  constantes <- names(X)[vapply(X, function(x) length(unique(x)) < 2, logical(1))]
  if (length(constantes) > 0) {
    stop("Variable(s) constante(s), à retirer des explicatives : ",
         paste(constantes, collapse = ", "), call. = FALSE)
  }

  # taille de l'echantillon et colinearite (apres recodage des qualitatives)
  p <- ncol(adlSISE::Encodeur$new()$fit_transform(X))
  if (nrow(X) - K < p) {
    stop("Pas assez d'individus (", nrow(X), ") pour ", p, " variable(s) après recodage et ",
         K, " classes : il faut au moins ", p + K, " individus.", call. = FALSE)
  }
  redondantes <- detecter_colinearite(X, y)
  if (length(redondantes) > 0) {
    stop("Variables colinéaires (ou redondantes) : ", paste(redondantes, collapse = ", "),
         ". Retirez-en au moins une des explicatives.", call. = FALSE)
  }

  # description des explicatives, utile au formulaire et aux nouveaux fichiers
  infos <- lapply(X, function(x) {
    if (is.numeric(x)) {
      list(type = "numérique", moyenne = mean(x), min = min(x), max = max(x))
    } else {
      list(type = "qualitative", niveaux = levels(x))
    }
  })

  list(X = X, y = y, cible = cible, infos = infos, supprimees = supprimees,
       illustratives = donnees[ok, illustratives, drop = FALSE])
}

# Ajuste le modele ADL ; traduit les erreurs en messages comprehensibles.
ajuster_modele <- function(prep, priors = "proportional", selection = "none", alpha = 0.05) {
  if (length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("Le seuil alpha doit être strictement compris entre 0 et 1.", call. = FALSE)
  }
  tryCatch({
    modele <- adlSISE::ADL$new(priors = priors, selection = selection, alpha = alpha)
    modele$fit(prep$X, prep$y)
    modele
  }, error = function(e) {
    stop("Le modèle n'a pas pu être estimé : ", conditionMessage(e), call. = FALSE)
  })
}
