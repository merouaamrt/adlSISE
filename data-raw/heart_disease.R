# Jeu "heart_disease" : UCI, Heart Disease, base de Cleveland (id 45), CC BY 4.0
# DOI : 10.24432/C52P4X
# Execution depuis la racine du package : source("data-raw/heart_disease.R")

url <- paste0("https://archive.ics.uci.edu/ml/machine-learning-databases/",
              "heart-disease/processed.cleveland.data")
noms <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach",
          "exang", "oldpeak", "slope", "ca", "thal", "num")

fichier <- tempfile(fileext = ".data")
download.file(url, fichier, mode = "wb", quiet = TRUE)
# "?" = valeur manquante (4 pour ca, 2 pour thal) : on les garde en NA
heart_disease <- read.csv(fichier, header = FALSE, col.names = noms, na.strings = "?")

# variables qualitatives en facteurs avec des libelles clairs
heart_disease$sex <- factor(heart_disease$sex, levels = 0:1, labels = c("female", "male"))
heart_disease$cp <- factor(heart_disease$cp, levels = 1:4,
                           labels = c("typical_angina", "atypical_angina",
                                      "non_anginal", "asymptomatic"))
heart_disease$fbs <- factor(heart_disease$fbs, levels = 0:1, labels = c("no", "yes"))
heart_disease$restecg <- factor(heart_disease$restecg, levels = 0:2,
                                labels = c("normal", "st_t_abnormality", "lv_hypertrophy"))
heart_disease$exang <- factor(heart_disease$exang, levels = 0:1, labels = c("no", "yes"))
heart_disease$slope <- factor(heart_disease$slope, levels = 1:3,
                              labels = c("upsloping", "flat", "downsloping"))
heart_disease$thal <- factor(heart_disease$thal, levels = c(3, 6, 7),
                             labels = c("normal", "fixed_defect", "reversible_defect"))

# cible binaire : num = 0 -> pas de maladie, 1 a 4 -> maladie
heart_disease$disease <- factor(ifelse(heart_disease$num > 0, "yes", "no"), levels = c("no", "yes"))
heart_disease$num <- NULL

stopifnot(nrow(heart_disease) == 303, ncol(heart_disease) == 14,
          sum(is.na(heart_disease)) == 6)
usethis::use_data(heart_disease, overwrite = TRUE, version = 2, compress = "gzip")
