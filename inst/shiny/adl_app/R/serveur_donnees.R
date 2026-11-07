# Logique reactive : chargement des donnees, choix des variables, lancement.
# Chaque fonction recoit input/output/session et renvoie les reactives utiles.

# Fichier choisi (ou jeu iris) -> list(donnees, erreur)
serveur_donnees <- function(input, output, session) {
  source_donnees <- reactiveVal("aucune")
  observeEvent(input$fichier, source_donnees("fichier"))
  observeEvent(input$demo, source_donnees("demo"))

  lecture <- reactive({
    if (source_donnees() == "demo") {
      return(list(donnees = as.data.frame(iris), erreur = NULL, nom = "iris (démonstration)"))
    }
    if (source_donnees() == "aucune" || is.null(input$fichier)) {
      return(list(donnees = NULL, erreur = NULL, nom = NULL))
    }
    f <- input$fichier
    tryCatch(
      list(donnees = lire_donnees(f$datapath, f$name, input$sep, input$dec,
                                  isTRUE(input$entete), input$feuille),
           erreur = NULL, nom = f$name),
      error = function(e) list(donnees = NULL, erreur = conditionMessage(e), nom = f$name))
  })

  # les options de lecture Excel n'apparaissent que pour un fichier .xlsx/.xls
  output$est_excel <- reactive({
    source_donnees() == "fichier" && !is.null(input$fichier) &&
      tolower(tools::file_ext(input$fichier$name)) %in% c("xlsx", "xls")
  })
  outputOptions(output, "est_excel", suspendWhenHidden = FALSE)

  output$apercu_info <- renderUI({
    l <- lecture()
    if (is.null(l$donnees)) {
      return(p(class = "indice", "Chargez un fichier (ou le jeu iris) pour commencer."))
    }
    p(class = "indice", strong(l$nom), " : ", nrow(l$donnees), " lignes, ",
      ncol(l$donnees), " colonnes. Les 10 premières lignes sont affichées.")
  })
  output$apercu <- renderTable({
    req(lecture()$donnees)
    head(lecture()$donnees, 10)
  }, striped = TRUE, na = "")
  output$structure <- renderTable({
    req(lecture()$donnees)
    table_structure(lecture()$donnees)
  }, striped = TRUE)

  lecture
}

# Met a jour les listes de choix : la cible est retiree des explicatives, et
# les illustratives sont celles qui ne sont ni cible ni explicatives.
serveur_variables <- function(input, session, donnees) {
  noms_donnees <- function() {
    d <- donnees()
    if (is.null(d)) character(0) else names(d)
  }

  observeEvent(donnees(), {
    noms <- noms_donnees()
    cible <- utils::tail(noms, 1)
    updateSelectInput(session, "cible", choices = noms, selected = cible)
    updateSelectizeInput(session, "explicatives", choices = setdiff(noms, cible),
                         selected = setdiff(noms, cible))
    updateSelectizeInput(session, "illustratives", choices = character(0), selected = character(0))
  }, ignoreNULL = FALSE)

  observeEvent(input$cible, {
    candidates <- setdiff(noms_donnees(), input$cible)
    updateSelectizeInput(session, "explicatives", choices = candidates,
                         selected = intersect(input$explicatives, candidates))
  }, ignoreInit = TRUE)

  observe({
    candidates <- setdiff(noms_donnees(), c(input$cible, input$explicatives))
    updateSelectizeInput(session, "illustratives", choices = candidates,
                         selected = intersect(isolate(input$illustratives), candidates))
  })
}

# Bouton "Lancer" : valide, ajuste, memorise le resultat (ou le message d'erreur)
serveur_analyse <- function(input, session, donnees) {
  resultat <- reactiveVal(NULL)

  observeEvent(input$lancer, {
    r <- tryCatch({
      prep <- preparer_analyse(donnees(), input$cible, input$explicatives,
                               input$illustratives, isTRUE(input$supprimer_na))
      modele <- ajuster_modele(prep, input$priors, input$selection, input$alpha)
      c(list(erreur = NULL, modele = modele, alpha = input$alpha,
             selection = input$selection, priors = input$priors), prep)
    }, error = function(e) list(erreur = conditionMessage(e)))
    resultat(r)
    if (is.null(r$erreur)) updateTabsetPanel(session, "onglets", selected = "resume")
  })

  resultat
}

# Bandeau en haut de l'ecran : erreur de lecture, erreur d'analyse ou information
serveur_alerte <- function(output, lecture, resultat) {
  output$alerte <- renderUI({
    l <- lecture()
    if (!is.null(l$erreur)) return(alerte("danger", "Fichier illisible", l$erreur))
    r <- resultat()
    if (is.null(r)) return(NULL)
    if (!is.null(r$erreur)) return(alerte("danger", "Analyse impossible", r$erreur))
    if (r$supprimees > 0) {
      return(alerte("info", "Lignes incomplètes retirées",
                    paste(r$supprimees, "ligne(s) avec valeurs manquantes ignorée(s).")))
    }
    NULL
  })
}

# Point d'entree du serveur
serveur_principal <- function(input, output, session) {
  lecture <- serveur_donnees(input, output, session)
  donnees <- reactive(lecture()$donnees)
  serveur_variables(input, session, donnees)
  resultat <- serveur_analyse(input, session, donnees)
  serveur_alerte(output, lecture, resultat)
}
