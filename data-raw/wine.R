# Jeu "wine" : UCI Machine Learning Repository, Wine (id 109), CC BY 4.0
# DOI : 10.24432/C5PC7J
# Execution depuis la racine du package : source("data-raw/wine.R")

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
noms <- c("class", "alcohol", "malic_acid", "ash", "alcalinity", "magnesium",
          "total_phenols", "flavanoids", "nonflavanoid_phenols", "proanthocyanins",
          "color_intensity", "hue", "od280_od315", "proline")

fichier <- tempfile(fileext = ".data")
download.file(url, fichier, mode = "wb", quiet = TRUE)
wine <- read.csv(fichier, header = FALSE, col.names = noms)

# cible en facteur (3 cultivars), puis la cible est placee en derniere colonne
wine$class <- factor(wine$class, levels = 1:3)
wine <- wine[, c(noms[-1], "class")]

stopifnot(nrow(wine) == 178, ncol(wine) == 14, !anyNA(wine))
usethis::use_data(wine, overwrite = TRUE, version = 2, compress = "gzip")
