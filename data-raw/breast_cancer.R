# Jeu "breast_cancer" : UCI, Breast Cancer Wisconsin (Original) (id 15), CC BY 4.0
# DOI : 10.24432/C5HP4Z
# Execution depuis la racine du package : source("data-raw/breast_cancer.R")

url <- paste0("https://archive.ics.uci.edu/ml/machine-learning-databases/",
              "breast-cancer-wisconsin/breast-cancer-wisconsin.data")
noms <- c("id", "clump_thickness", "cell_size_uniformity", "cell_shape_uniformity",
          "marginal_adhesion", "epithelial_cell_size", "bare_nuclei", "bland_chromatin",
          "normal_nucleoli", "mitoses", "class")

fichier <- tempfile(fileext = ".data")
download.file(url, fichier, mode = "wb", quiet = TRUE)
# "?" = valeur manquante (16 valeurs, toutes dans bare_nuclei) : on les garde en NA
breast_cancer <- read.csv(fichier, header = FALSE, col.names = noms, na.strings = "?")

breast_cancer$id <- NULL                                   # identifiant supprime
breast_cancer$class <- factor(breast_cancer$class, levels = c(2, 4),
                              labels = c("benign", "malignant"))

stopifnot(nrow(breast_cancer) == 699, ncol(breast_cancer) == 10,
          sum(is.na(breast_cancer)) == 16)
usethis::use_data(breast_cancer, overwrite = TRUE, version = 2, compress = "gzip")
