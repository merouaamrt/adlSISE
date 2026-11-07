# Logique reactive des onglets de resultats.

# Arrete proprement une sortie tant qu'il n'y a pas de resultat valide :
# affiche un message gris a la place du tableau ou du graphique.
exiger_resultat <- function(r) {
  validate(
    need(!is.null(r), "Choisissez vos variables puis cliquez sur « Lancer l'analyse »."),
    need(is.null(r$erreur), "L'analyse n'a pas abouti : voir le message en rouge ci-dessus.")
  )
  r
}

# Petite carte de chiffre cle
carte <- function(valeur, legende) {
  div(class = "carte", div(class = "valeur", valeur), div(class = "legende", legende))
}

# Onglets : resume, evaluation globale, coefficients, pertinence, selection, confusion
serveur_resultats <- function(output, resultat) {
  res <- reactive(exiger_resultat(resultat()))

  # --- Resume ---
  output$resume_cartes <- renderUI({
    r <- res()
    m <- r$modele
    div(class = "cartes",
        carte(nrow(r$X), "Individus"),
        carte(length(m$classes), "Classes"),
        carte(paste(length(m$selected_variables), "/", ncol(r$modele$means)), "Variables retenues"),
        carte(paste0(fmt_num(100 * taux_erreur(m$confusion), 1), " %"), "Erreur (resubstitution)"))
  })
  output$resume_classes <- renderTable(table_classes(res()$modele, res()$y), striped = TRUE)
  output$resume_texte <- renderText(paste(utils::capture.output(res()$modele$summary()),
                                          collapse = "\n"))

  # --- Evaluation globale ---
  output$eval_table <- renderTable(table_evaluation(res()$modele$wilks), striped = TRUE)
  output$eval_texte <- renderText(phrase_evaluation(res()$modele$wilks, res()$alpha))

  # --- Coefficients ---
  output$coef_table <- renderTable(table_coefficients(res()$modele$coefficients), striped = TRUE)

  # --- Pertinence des variables ---
  output$pert_table <- renderTable(table_pertinence(res()$modele$variable_tests), striped = TRUE)
  output$pert_plot <- renderPlot(graphique_pertinence(res()$modele$variable_tests, res()$alpha))

  # --- Historique de selection ---
  output$select_info <- renderUI({
    r <- res()
    if (r$selection == "none") {
      return(p(class = "indice", "Aucune sélection de variables n'a été demandée."))
    }
    toutes <- colnames(adlSISE::Encodeur$new()$fit_transform(r$X))
    retenues <- r$modele$selected_variables
    retirees <- setdiff(toutes, retenues)
    div(class = "cartes",
        carte(length(retenues), "Retenues"), carte(length(retirees), "Écartées"),
        div(class = "carte", style = "flex-basis: 100%;",
            p(strong("Retenues : "), paste(retenues, collapse = ", ")),
            p(strong("Écartées : "), if (length(retirees)) paste(retirees, collapse = ", ") else "aucune")))
  })
  output$select_table <- renderTable({
    r <- res()
    h <- r$modele$selection_history
    req(r$selection != "none", !is.null(h))
    table_historique(h, r$selection)
  }, striped = TRUE)

  # --- Matrice de confusion ---
  output$conf_texte <- renderUI({
    cm <- res()$modele$confusion
    div(class = "cartes", carte(paste0(fmt_num(100 * taux_erreur(cm), 2), " %"), "Taux d'erreur"),
        carte(paste0(fmt_num(100 * (1 - taux_erreur(cm)), 2), " %"), "Taux de bonne classification"))
  })
  output$conf_table <- renderTable(table_confusion(res()$modele$confusion), striped = TRUE)
  output$conf_plot <- renderPlot(graphique_confusion(res()$modele$confusion))

  res
}

# Onglet validation croisee : calcul a la demande (peut etre long)
serveur_validation_croisee <- function(input, output, resultat) {
  cv <- reactiveVal(NULL)
  observeEvent(resultat(), cv(NULL))   # nouveau modele : on efface l'ancienne validation

  observeEvent(input$lancer_cv, {
    r <- resultat()
    req(r, is.null(r$erreur))
    n <- nrow(r$X)
    plis <- input$plis
    cv(tryCatch({
      if (is.null(plis) || is.na(plis) || plis != round(plis) || plis < 2 || plis > n) {
        stop("Le nombre de plis doit être un entier entre 2 et ", n, " (nombre d'individus).",
             call. = FALSE)
      }
      graine <- if (is.null(input$graine) || is.na(input$graine)) NULL else input$graine
      v <- withProgress(message = "Validation croisée en cours…",
                        r$modele$cross_validate(r$X, r$y, folds = plis, seed = graine))
      if (anyNA(v$scores)) {
        stop("Certains plis sont vides : réduisez le nombre de plis.", call. = FALSE)
      }
      c(list(erreur = NULL), v)
    }, error = function(e) list(erreur = paste("Validation croisée impossible :", conditionMessage(e)))))
  })

  output$cv_resume <- renderUI({
    v <- cv()
    if (is.null(v)) return(p(class = "indice", "Choisissez le nombre de plis puis lancez le calcul."))
    if (!is.null(v$erreur)) return(alerte("danger", "Erreur", v$erreur))
    div(class = "cartes",
        carte(paste0(fmt_num(100 * v$accuracy, 2), " %"), "Bonne classification (moyenne)"),
        carte(paste0(fmt_num(100 * v$error, 2), " %"), "Taux d'erreur estimé"),
        carte(length(v$scores), "Plis"))
  })
  output$cv_table <- renderTable({
    v <- cv()
    req(v, is.null(v$erreur))
    table_plis(v$scores)
  }, striped = TRUE)
  output$cv_plot <- renderPlot({
    v <- cv()
    req(v, is.null(v$erreur))
    graphique_plis(v$scores, v$accuracy)
  })
  cv
}

# Onglet variables illustratives (n'entrent pas dans le modele)
serveur_illustratives <- function(input, output, resultat) {
  output$illus_choix <- renderUI({
    r <- exiger_resultat(resultat())
    if (ncol(r$illustratives) == 0) {
      return(p(class = "indice", "Aucune variable illustrative n'a été choisie à l'étape 2."))
    }
    selectInput("illus_var", "Variable illustrative", names(r$illustratives))
  })

  # valeurs de la variable choisie (facteur si qualitative)
  illus <- reactive({
    r <- exiger_resultat(resultat())
    req(input$illus_var, input$illus_var %in% names(r$illustratives))
    x <- r$illustratives[[input$illus_var]]
    if (!is.numeric(x)) x <- factor(x)
    list(x = x, y = r$y, nom = input$illus_var)
  })

  output$illus_titre1 <- renderText({
    d <- illus()
    if (is.numeric(d$x)) "Moyennes par classe" else "Tableau croisé (effectifs)"
  })
  output$illus_table1 <- renderTable({
    d <- illus()
    if (is.numeric(d$x)) table_illustrative_num(d$x, d$y) else table_illustrative_qual(d$x, d$y)
  }, striped = TRUE)
  output$illus_titre2 <- renderText({
    if (is.numeric(illus()$x)) "" else "Profils lignes (% de chaque classe)"
  })
  output$illus_table2 <- renderTable({
    d <- illus()
    req(!is.numeric(d$x))
    table_illustrative_qual(d$x, d$y, pourcentage = TRUE)
  }, striped = TRUE)
  output$illus_test <- renderText({
    d <- illus()
    if (is.numeric(d$x)) {
      paste("Test de comparaison des moyennes (ANOVA) : p-value =", fmt_p(p_anova(d$x, d$y)))
    } else {
      paste("Test du khi-deux d'indépendance (approximatif) : p-value =", fmt_p(p_khi2(d$x, d$y)))
    }
  })
  output$illus_plot <- renderPlot({
    d <- illus()
    graphique_illustrative(d$x, d$y, d$nom)
  })
}
