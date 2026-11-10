# Application Shiny de l'analyse discriminante lineaire (package adlSISE).
# Le code est decoupe dans le dossier R/ (charge automatiquement par Shiny) :
#   helpers_*.R : fonctions pures (lecture, validation, tableaux, graphiques)
#   ui.R        : construction de l'interface
#   serveur_*.R : logique reactive, une fonction par grande partie
library(shiny)

shinyApp(ui = ui_principale(), server = serveur_principal)
