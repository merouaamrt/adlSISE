# Tests de l'application Shiny (inst/shiny/adl_app) : fonctions pures et
# logique serveur via shiny::testServer. Jeu de test : iris.

skip_if_not_installed("shiny")

dossier_app <- system.file("shiny", "adl_app", package = "adlSISE")

# charge les fonctions pures de l'application dans un environnement a part
env_app <- new.env(parent = globalenv())
for (f in list.files(file.path(dossier_app, "R"), pattern = "^helpers_.*\\.R$", full.names = TRUE)) {
  sys.source(f, envir = env_app, keep.source = FALSE)
}
app_fun <- function(nom) get(nom, envir = env_app)

colonnes <- names(iris)[1:4]

test_that("run_app est exportee et l'application est installee", {
  expect_true(is.function(run_app))
  expect_true(file.exists(file.path(dossier_app, "app.R")))
})

# ---------- lecture des fichiers ----------
test_that("lire_donnees lit les separateurs et signale les fichiers illisibles", {
  lire <- app_fun("lire_donnees")
  f <- tempfile(fileext = ".csv")
  write.table(iris, f, sep = "\t", row.names = FALSE)
  expect_equal(dim(lire(f, "iris.csv")), dim(iris))
  write.table(iris, f, sep = ";", row.names = FALSE, dec = ",")
  d <- lire(f, "iris.csv", sep = ";", dec = ",")
  expect_true(is.numeric(d$Sepal.Length))
  # mauvais separateur : une seule colonne
  expect_error(lire(f, "iris.csv", sep = "\t"), "Une seule colonne")
  # fichier vide, introuvable
  vide <- tempfile(fileext = ".csv"); file.create(vide)
  expect_error(lire(vide, "vide.csv"), "vide")
  expect_error(lire("n_existe_pas.csv", "x.csv"), "introuvable")
})

# ---------- validation des choix ----------
test_that("preparer_analyse refuse les cas problematiques avec un message clair", {
  prep <- app_fun("preparer_analyse")
  ok <- prep(iris, "Species", colonnes)
  expect_equal(nlevels(ok$y), 3)
  expect_equal(ok$supprimees, 0)

  # cible a une seule classe
  d <- iris; d$Species <- "a"
  expect_error(prep(d, "Species", colonnes), "au moins 2")
  # cible continue
  expect_error(prep(iris, "Sepal.Length", colonnes[2:4]), "continue")
  # aucune explicative / cible parmi les explicatives
  expect_error(prep(iris, "Species", character(0)), "au moins une variable")
  expect_error(prep(iris, "Species", c("Species", colonnes)), "cible")
  # colonne absente
  expect_error(prep(iris, "Species", c(colonnes, "fantome")), "absentes")
  # variable constante
  d <- iris; d$cst <- 1
  expect_error(prep(d, "Species", c(colonnes, "cst")), "constante")
  # variables colineaires (nom de la variable redondante donne)
  d <- iris; d$double <- 2 * d$Sepal.Length
  expect_error(prep(d, "Species", c(colonnes, "double")), "colin.aires")
  # valeurs manquantes : erreur, ou lignes retirees si demande
  d <- iris; d$Sepal.Width[c(3, 7)] <- NA
  expect_error(prep(d, "Species", colonnes), "manquantes")
  retire <- prep(d, "Species", colonnes, supprimer_na = TRUE)
  expect_equal(retire$supprimees, 2)
  expect_equal(nrow(retire$X), 148)
  # pas assez d'individus
  expect_error(prep(iris[c(1:2, 51:52, 101:102), ], "Species", colonnes), "Pas assez")
})

test_that("ajuster_modele verifie alpha et renvoie un modele ADL", {
  prep <- app_fun("preparer_analyse")(iris, "Species", colonnes)
  ajuster <- app_fun("ajuster_modele")
  expect_s3_class(ajuster(prep, "uniform", "forward", 0.05), "ADL")
  expect_error(ajuster(prep, alpha = 2), "alpha")
})

# ---------- nouveaux individus ----------
test_that("predire_nouveaux est coherent avec ADL et detecte les erreurs", {
  prep <- app_fun("preparer_analyse")(iris, "Species", colonnes)
  modele <- app_fun("ajuster_modele")(prep)
  nouveau <- iris[c(1, 70, 140), colonnes]
  res <- app_fun("predire_nouveaux")(modele, nouveau, prep$infos)
  expect_equal(res$Classe_predite, as.character(modele$predict(nouveau)))
  expect_equal(unname(rowSums(res[, grep("^P\\(", names(res))])), rep(1, 3), tolerance = 1e-8)

  expect_error(app_fun("predire_nouveaux")(modele, nouveau[, 1:2], prep$infos), "absentes")
  mauvais <- nouveau; mauvais$Sepal.Width <- c("a", "b", "c")
  expect_error(app_fun("predire_nouveaux")(modele, mauvais, prep$infos), "num.rique")
  # ligne incomplete : prediction NA, les autres sont conservees
  incomplet <- nouveau; incomplet$Sepal.Width[2] <- NA
  res2 <- app_fun("predire_nouveaux")(modele, incomplet, prep$infos)
  expect_true(is.na(res2$Classe_predite[2]))
  expect_false(anyNA(res2$Classe_predite[c(1, 3)]))
})

test_that("une variable qualitative inconnue est signalee", {
  d <- iris; d$taille <- ifelse(d$Petal.Length > 4, "grand", "petit")
  prep <- app_fun("preparer_analyse")(d, "Species", c("Sepal.Length", "Sepal.Width", "taille"))
  modele <- app_fun("ajuster_modele")(prep)
  nouveau <- data.frame(Sepal.Length = 5, Sepal.Width = 3, taille = "enorme")
  expect_error(app_fun("predire_nouveaux")(modele, nouveau, prep$infos), "inconnue")
  nouveau$taille <- "grand"
  expect_equal(nrow(app_fun("predire_nouveaux")(modele, nouveau, prep$infos)), 1)
})

# ---------- logique serveur ----------
test_that("le serveur ajuste le modele sur iris et affiche les resultats", {
  app <- shiny::shinyAppDir(dossier_app)
  shiny::testServer(app, {
    session$setInputs(demo = 1)
    expect_equal(dim(donnees()), c(150, 5))
    session$setInputs(cible = "Species", explicatives = colonnes, illustratives = character(0),
                      supprimer_na = FALSE, priors = "proportional", selection = "none",
                      alpha = 0.05, lancer = 1)
    expect_null(resultat()$erreur)
    expect_s3_class(resultat()$modele, "ADL")
    expect_match(output$eval_texte, "significativement")
    expect_match(output$conf_texte$html, "Taux d'erreur")
    expect_false(is.null(output$pert_plot))
    # validation croisee
    session$setInputs(plis = 5, graine = 1, lancer_cv = 1)
    expect_null(cv()$erreur)
    expect_gt(cv()$accuracy, 0.9)
    session$setInputs(plis = 1, lancer_cv = 2)
    expect_match(cv()$erreur, "plis")
  })
})

test_that("le serveur affiche des erreurs claires au lieu de planter", {
  app <- shiny::shinyAppDir(dossier_app)
  shiny::testServer(app, {
    session$setInputs(demo = 1)
    # cible numerique continue
    session$setInputs(cible = "Sepal.Length", explicatives = "Sepal.Width", illustratives = character(0),
                      supprimer_na = FALSE, priors = "proportional", selection = "none",
                      alpha = 0.05, lancer = 1)
    expect_match(resultat()$erreur, "continue")
    expect_match(output$alerte$html, "Analyse impossible")
    # alpha invalide
    session$setInputs(cible = "Species", explicatives = colonnes, alpha = 5, lancer = 2)
    expect_match(resultat()$erreur, "alpha")
  })
})

test_that("le serveur gere les variables illustratives et le deploiement", {
  app <- shiny::shinyAppDir(dossier_app)
  fichier <- tempfile(fileext = ".csv")
  write.table(iris[c(1, 60, 120), colonnes], fichier, sep = "\t", row.names = FALSE)
  shiny::testServer(app, {
    session$setInputs(demo = 1)
    session$setInputs(cible = "Species", explicatives = colonnes[1:3], illustratives = "Petal.Width",
                      supprimer_na = FALSE, priors = "uniform", selection = "forward",
                      alpha = 0.05, lancer = 1)
    expect_null(resultat()$erreur)
    session$setInputs(illus_var = "Petal.Width")
    expect_match(output$illus_test, "ANOVA")
    # un individu
    session$setInputs(ind_1 = 5.1, ind_2 = 3.5, ind_3 = 1.4, predire_ind = 1)
    expect_match(output$ind_carte$html, "setosa")
    session$setInputs(ind_1 = NA, predire_ind = 2)
    expect_match(output$ind_carte$html, "manquante")
    # un fichier
    session$setInputs(sep_n = "\t", dec_n = ".",
                      nouveaux = data.frame(name = "n.csv", size = 1, type = "text/csv",
                                            datapath = fichier))
    expect_null(pred_fichier()$erreur)
    expect_equal(nrow(pred_fichier()$table), 3)
    expect_equal(pred_fichier()$table$Classe_predite[1], "setosa")
  })
})
