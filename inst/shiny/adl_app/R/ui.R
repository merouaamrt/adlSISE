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
  list(ui_onglet_apercu())
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
