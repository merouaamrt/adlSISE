# Logique reactive du deploiement : un individu (formulaire) ou un fichier.

serveur_deploiement <- function(input, output, resultat) {
  # ---------- (a) un individu ----------
  # Un champ par variable explicative : numerique -> nombre, qualitative -> liste
  output$formulaire <- renderUI({
    r <- exiger_resultat(resultat())
    champs <- lapply(seq_along(r$infos), function(i) {
      nom <- names(r$infos)[i]
      info <- r$infos[[i]]
      id <- paste0("ind_", i)
      if (info$type == "numérique") {
        column(6, numericInput(id, nom, value = signif(info$moyenne, 4)),
               tags$small(class = "indice",
                          paste0("observé de ", signif(info$min, 4), " à ", signif(info$max, 4))))
      } else {
        column(6, selectInput(id, nom, choices = info$niveaux))
      }
    })
    do.call(fluidRow, champs)
  })

  # Resultat du bouton "Classer cet individu" : tableau de prediction ou erreur
  pred_ind <- eventReactive(input$predire_ind, {
    r <- resultat()
    req(r, is.null(r$erreur))
    tryCatch({
      valeurs <- lapply(seq_along(r$infos), function(i) input[[paste0("ind_", i)]])
      names(valeurs) <- names(r$infos)
      invalides <- names(valeurs)[vapply(valeurs, function(v) {
        is.null(v) || length(v) != 1 || is.na(v)
      }, logical(1))]
      if (length(invalides) > 0) {
        stop("Valeur manquante ou invalide pour : ", paste(invalides, collapse = ", "), ".",
             call. = FALSE)
      }
      nouveau <- data.frame(valeurs, check.names = FALSE, stringsAsFactors = FALSE)
      list(erreur = NULL, table = predire_nouveaux(r$modele, nouveau, r$infos))
    }, error = function(e) list(erreur = conditionMessage(e)))
  })

  output$ind_carte <- renderUI({
    p <- pred_ind()
    if (!is.null(p$erreur)) return(alerte("danger", "Prédiction impossible", p$erreur))
    div(class = "prediction", div(class = "legende", "Classe prédite"),
        div(class = "valeur", p$table$Classe_predite[1]))
  })
  output$ind_table <- renderTable({
    p <- pred_ind()
    req(is.null(p$erreur))
    table_probabilites(p$table, resultat()$modele$classes)
  }, striped = TRUE)
  output$ind_plot <- renderPlot({
    p <- pred_ind()
    req(is.null(p$erreur))
    graphique_probabilites(p$table, resultat()$modele$classes)
  })

  # ---------- (b) un fichier de nouveaux individus ----------
  nouveaux_lus <- reactive({
    req(input$nouveaux)
    f <- input$nouveaux
    tryCatch(
      list(donnees = lire_donnees(f$datapath, f$name, input$sep_n, input$dec_n, TRUE,
                                  colonnes_min = 1),
           erreur = NULL),
      error = function(e) list(donnees = NULL, erreur = conditionMessage(e)))
  })

  pred_fichier <- reactive({
    r <- exiger_resultat(resultat())
    l <- nouveaux_lus()
    if (!is.null(l$erreur)) return(list(erreur = l$erreur))
    tryCatch(
      list(erreur = NULL, table = predire_nouveaux(r$modele, l$donnees, r$infos)),
      error = function(e) list(erreur = conditionMessage(e)))
  })

  output$fichier_message <- renderUI({
    p <- pred_fichier()
    if (!is.null(p$erreur)) return(alerte("danger", "Fichier inutilisable", p$erreur))
    n <- nrow(p$table)
    incomplets <- sum(is.na(p$table$Classe_predite))
    texte <- paste(n - incomplets, "individu(s) classé(s) sur", n, "lignes.")
    if (incomplets > 0) {
      texte <- paste(texte, incomplets, "ligne(s) incomplète(s) laissée(s) sans prédiction.")
    }
    alerte("succes", "Prédictions calculées", texte)
  })
  output$fichier_table <- renderTable({
    p <- pred_fichier()
    req(is.null(p$erreur))
    utils::head(p$table, 200)
  }, striped = TRUE, na = "")
  output$fichier_telechargement <- renderUI({
    p <- pred_fichier()
    req(is.null(p$erreur))
    downloadButton("telecharger", "Télécharger les prédictions (CSV)", class = "btn-primary")
  })
  output$telecharger <- downloadHandler(
    filename = function() "predictions_adl.csv",
    content = function(file) {
      p <- pred_fichier()
      req(is.null(p$erreur))
      utils::write.csv(p$table, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    })

  pred_fichier
}
