# Interface utilisateur : une fonction par zone de l'ecran.

# Bandeau d'avertissement ou d'information
alerte <- function(type, titre, texte) {
  div(class = paste("alerte", paste0("alerte-", type)), strong(titre), p(texte))
}

# Colonne de gauche : les trois etapes (donnees, variables, options) + bouton
ui_barre_laterale <- function() {
  sidebarPanel(
    width = 4,
    div(class = "etape", h4(span(class = "num", "1"), "Données"),
        fileInput("fichier", NULL, accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls"),
                  buttonLabel = "Parcourir…", placeholder = "CSV, TXT ou Excel"),
        actionLink("demo", "ou charger le jeu de démonstration iris"),
        conditionalPanel("!output.est_excel",
          selectInput("sep", "Séparateur de colonnes", SEPARATEURS, selected = "\t"),
          selectInput("dec", "Séparateur décimal", c("Point ( . )" = ".", "Virgule ( , )" = ","))),
        conditionalPanel("output.est_excel",
          numericInput("feuille", "Numéro de la feuille Excel", 1, min = 1, step = 1)),
        checkboxInput("entete", "La première ligne contient les noms de colonnes", TRUE)),
    div(class = "etape", h4(span(class = "num", "2"), "Variables"),
        selectInput("cible", "Variable cible (catégorielle)", choices = NULL),
        selectizeInput("explicatives", "Variables explicatives", choices = NULL, multiple = TRUE,
                       options = list(plugins = list("remove_button"),
                                      placeholder = "Numériques ou qualitatives")),
        selectizeInput("illustratives", "Variables illustratives (hors modèle)", choices = NULL,
                       multiple = TRUE,
                       options = list(plugins = list("remove_button"), placeholder = "Facultatif")),
        checkboxInput("supprimer_na", "Supprimer les lignes incomplètes", FALSE)),
    div(class = "etape", h4(span(class = "num", "3"), "Options"),
        radioButtons("priors", "Probabilités a priori",
                     c("Proportionnelles aux effectifs" = "proportional",
                       "Égales (uniformes)" = "uniform")),
        radioButtons("selection", "Sélection de variables",
                     c("Aucune" = "none", "Ascendante (forward)" = "forward",
                       "Descendante (backward)" = "backward")),
        numericInput("alpha", "Seuil alpha", 0.05, min = 0.001, max = 0.999, step = 0.01)),
    actionButton("lancer", "Lancer l'analyse", class = "btn-primary btn-lg btn-block")
  )
}

# Premier onglet : apercu du fichier charge
ui_onglet_apercu <- function() {
  tabPanel("Données", value = "donnees",
           h3("Aperçu des données"),
           uiOutput("apercu_info"),
           div(class = "defilement", tableOutput("apercu")),
           h3("Structure des variables"),
           div(class = "defilement", tableOutput("structure")))
}

# --- Onglets de resultats ---
ui_onglet_resume <- function() {
  tabPanel("Résumé", value = "resume",
           h3("Résumé du modèle"), uiOutput("resume_cartes"),
           h3("Classes et probabilités a priori"), tableOutput("resume_classes"),
           tags$details(tags$summary("Afficher le résumé texte complet"),
                        verbatimTextOutput("resume_texte")))
}

ui_onglet_evaluation <- function() {
  tabPanel("Évaluation globale", value = "evaluation",
           h3("Qualité globale de la discrimination"), tableOutput("eval_table"),
           textOutput("eval_texte"),
           p(class = "indice", "Plus le lambda de Wilks est proche de 0, plus les classes sont séparées."))
}

ui_onglet_coefficients <- function() {
  tabPanel("Coefficients", value = "coefficients",
           h3("Fonctions de classement"),
           p(class = "indice", "Un individu est affecté à la classe dont le score est le plus élevé."),
           div(class = "defilement", tableOutput("coef_table")))
}

ui_onglet_pertinence <- function() {
  tabPanel("Pertinence des variables", value = "pertinence",
           h3("Contribution de chaque variable"), tableOutput("pert_table"),
           plotOutput("pert_plot", height = "340px"))
}

ui_onglet_selection <- function() {
  tabPanel("Sélection", value = "selection",
           h3("Historique de la sélection de variables"), uiOutput("select_info"),
           tableOutput("select_table"))
}

ui_onglet_confusion <- function() {
  tabPanel("Matrice de confusion", value = "confusion",
           h3("Matrice de confusion (données d'apprentissage)"), uiOutput("conf_texte"),
           tableOutput("conf_table"), plotOutput("conf_plot", height = "380px"),
           p(class = "indice", "Estimation optimiste : voir la validation croisée pour une mesure plus honnête."))
}

ui_onglet_validation <- function() {
  tabPanel("Validation croisée", value = "validation",
           h3("Validation croisée stratifiée"),
           fluidRow(column(4, numericInput("plis", "Nombre de plis", 5, min = 2, step = 1)),
                    column(4, numericInput("graine", "Graine aléatoire (facultatif)", 1, step = 1)),
                    column(4, br(), actionButton("lancer_cv", "Calculer", class = "btn-primary"))),
           uiOutput("cv_resume"),
           fluidRow(column(5, tableOutput("cv_table")), column(7, plotOutput("cv_plot", height = "300px"))))
}

ui_onglet_illustratives <- function() {
  tabPanel("Variables illustratives", value = "illustratives",
           h3("Variables illustratives"),
           p(class = "indice", "Ces variables ne participent pas au modèle : on les compare aux classes."),
           uiOutput("illus_choix"),
           h3(textOutput("illus_titre1", inline = TRUE)), div(class = "defilement", tableOutput("illus_table1")),
           h3(textOutput("illus_titre2", inline = TRUE)), div(class = "defilement", tableOutput("illus_table2")),
           textOutput("illus_test"), plotOutput("illus_plot", height = "340px"))
}

ui_onglet_deploiement <- function() {
  tabPanel("Déploiement", value = "deploiement",
    h3("Utiliser le modèle"),
    tabsetPanel(id = "mode_deploiement",
      tabPanel("Un individu",
        p(class = "indice", "Renseignez les variables explicatives du modèle."),
        uiOutput("formulaire"),
        actionButton("predire_ind", "Classer cet individu", class = "btn-primary"),
        br(), br(), uiOutput("ind_carte"),
        fluidRow(column(5, tableOutput("ind_table")), column(7, plotOutput("ind_plot", height = "260px")))),
      tabPanel("Un fichier",
        p(class = "indice", "Le fichier doit contenir les colonnes des variables explicatives (mêmes noms)."),
        fileInput("nouveaux", NULL, accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls"),
                  buttonLabel = "Parcourir…", placeholder = "Nouveaux individus"),
        fluidRow(column(6, selectInput("sep_n", "Séparateur de colonnes (CSV)", SEPARATEURS, selected = "	")),
                 column(6, selectInput("dec_n", "Séparateur décimal", c("Point ( . )" = ".", "Virgule ( , )" = ",")))),
        uiOutput("fichier_message"), uiOutput("fichier_telechargement"), br(),
        div(class = "defilement", tableOutput("fichier_table")))))
}

# Zone principale : alerte + onglets
ui_zone_principale <- function() {
  mainPanel(
    width = 8,
    uiOutput("alerte"),
    do.call(tabsetPanel, c(list(id = "onglets"), ui_liste_onglets()))
  )
}

# Liste des onglets, dans l'ordre d'affichage
ui_liste_onglets <- function() {
  list(ui_onglet_apercu(), ui_onglet_resume(), ui_onglet_evaluation(),
       ui_onglet_coefficients(), ui_onglet_pertinence(), ui_onglet_selection(),
       ui_onglet_confusion(), ui_onglet_validation(), ui_onglet_illustratives(),
       ui_onglet_deploiement())
}

ui_principale <- function() {
  fluidPage(
    title = "ADL – Analyse discriminante linéaire",
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    div(class = "bandeau",
        h1("Analyse discriminante linéaire"),
        p("Charger des données, estimer le modèle, interpréter les résultats et classer de nouveaux individus.")),
    sidebarLayout(ui_barre_laterale(), ui_zone_principale())
  )
}
