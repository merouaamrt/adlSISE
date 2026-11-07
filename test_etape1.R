devtools::load_all()

# DATA 1 du livre (p. 7)
D <- data.frame(
  Groupe = factor(c(rep("g1", 5), rep("g2", 6))),
  X1 = c(0, 2, 4, 6, 8, 5, 7, 9, 11, 13, 4),
  X2 = c(3, 1, 6, 9, 7, 2, 0, 4, 8, 6, 0)
)

m <- ADL$new()
m$fit(D[c("X1", "X2")], D$Groupe)
m$priors
m$means

# protections
try(m$fit(D[c("X1", "X2")], D$Groupe[1:5]))
try(m$fit(D, D$Groupe))
try(m$priors <- 0)

# ---- Etape 1.3 ----
devtools::load_all()
m <- ADL$new()$fit(D[c("X1", "X2")], D$Groupe)
priv <- m$.__enclos_env__$private
priv$FW
priv$FWinv
round(priv$FW %*% priv$FWinv, 10)

D2 <- D
D2$X3 <- 2 * D2$X1
try(ADL$new()$fit(D2[c("X1", "X2", "X3")], D2$Groupe))




# ---- Etape 1.4 ----
devtools::load_all()
m <- ADL$new()$fit(D[c("X1", "X2")], D$Groupe)
round(m$coefficients, 6)
m$classes




# ---- Etape 1.5 ----
devtools::load_all()
m <- ADL$new()$fit(D[c("X1", "X2")], D$Groupe)

# DATA 1 : predictions et probabilites
m$predict(D[c("X1", "X2")])
round(m$predict_proba(D[c("X1", "X2")]), 4)
m$predict(D[1, c("X1", "X2")])          # un seul individu

# iris : comparaison avec MASS::lda
m2  <- ADL$new()$fit(iris[1:4], iris$Species)
ref <- MASS::lda(Species ~ ., data = iris)
p_adl  <- m2$predict(iris[1:4])
p_mass <- predict(ref, iris)$class
table(p_adl, p_mass)
all(p_adl == p_mass)
max(abs(m2$predict_proba(iris[1:4]) - predict(ref, iris)$posterior))

# protections
try(ADL$new()$predict(iris[1:4]))
try(m2$predict(iris[1:2]))

# ---- Etape 1.6 ----
devtools::load_all()
m <- ADL$new()$fit(D[c("X1", "X2")], D$Groupe)
m
m$score(D[c("X1", "X2")], D$Groupe)

m2 <- ADL$new()$fit(iris[1:4], iris$Species)
print(m2)
m2$score(iris[1:4], iris$Species)
table(observe = iris$Species, predit = m2$predict(iris[1:4]))

ADL$new()



# ---- Etape 1.7a : Encodeur ----
devtools::load_all()
X <- data.frame(age = c(20, 35, 50, 41),
                couleur = c("bleu", "rouge", "vert", "rouge"))

enc <- Encodeur$new()
enc$fit(X)
enc$categories
enc$transform(X)

# comparaison avec model.matrix de R
all(enc$transform(X) == model.matrix(~ age + couleur, X)[, -1])

enc$transform(X[2, ])                                   # un seul individu
enc$transform(data.frame(age = 30, couleur = "jaune"))  # modalite inconnue
try(enc$transform(data.frame(age = 30)))                # colonne manquante




# ---- Etape 1.7b : ADL avec variables qualitatives ----
devtools::load_all()
str(warpbreaks)

m3 <- ADL$new()$fit(warpbreaks[c("breaks", "wool")], warpbreaks$tension)
m3

ref3 <- MASS::lda(tension ~ breaks + wool, data = warpbreaks)
p3 <- m3$predict(warpbreaks[c("breaks", "wool")])
all(p3 == predict(ref3, warpbreaks)$class)
max(abs(m3$predict_proba(warpbreaks[c("breaks", "wool")]) -
          predict(ref3, warpbreaks)$posterior))

m3$predict(data.frame(breaks = 30, wool = "B"))   # un seul individu
m3$predict(data.frame(breaks = 30, wool = "C"))   # modalite inconnue

# on n'a rien casse : DATA 1 et iris
round(ADL$new()$fit(D[c("X1", "X2")], D$Groupe)$coefficients, 6)
ADL$new()$fit(iris[1:4], iris$Species)$score(iris[1:4], iris$Species)





# ---- Etape 1.8a : vitesse ----
devtools::load_all()
set.seed(1)                                   # pour avoir toujours les memes donnees
n <- 1e6                                      # 1 million de lignes
Xg <- as.data.frame(matrix(rnorm(n * 20), n, 20))
yg <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
Xg$V1 <- Xg$V1 + as.integer(yg)               # V1 aide a reconnaitre la classe
Xg$cat <- sample(c("x", "y", "z"), n, replace = TRUE)

# notre classe : 20 numeriques + 1 qualitative
system.time(mg <- ADL$new()$fit(Xg, yg))
system.time(pg <- mg$predict(Xg))
mg$score(Xg, yg)

# comparaison avec MASS::lda (20 numeriques)
num <- paste0("V", 1:20)
system.time(ADL$new()$fit(Xg[num], yg))
system.time(MASS::lda(x = as.matrix(Xg[num]), grouping = yg))






# ---- Etape 2.1a : Lambda de Wilks ----
devtools::load_all()

# DATA 1 : W vient de notre classe, V est la covariance totale
m <- ADL$new()$fit(D[c("X1", "X2")], D$Groupe)
W1 <- m$.__enclos_env__$private$FW
V1 <- cov(D[c("X1", "X2")])
wilks_test(W1, V1, n = 11, K = 2)

# iris (3 classes) : comparaison avec la MANOVA officielle de R
m2 <- ADL$new()$fit(iris[1:4], iris$Species)
W2 <- m2$.__enclos_env__$private$FW
V2 <- cov(iris[1:4])
wilks_test(W2, V2, n = 150, K = 3)
summary(manova(as.matrix(iris[1:4]) ~ iris$Species), test = "Wilks")








# ---- Etape 2.1b : Wilks dans ADL ----
devtools::load_all()
m <- ADL$new()$fit(D[c("X1", "X2")], D$Groupe)
m$wilks

m2 <- ADL$new()$fit(iris[1:4], iris$Species)
m2$wilks$lambda
m2$wilks$p_value

# avec une variable qualitative
m3 <- ADL$new()$fit(warpbreaks[c("breaks", "wool")], warpbreaks$tension)
m3$wilks$lambda










# ---- Etape 2.2 : test F de chaque variable ----
devtools::load_all()
m <- ADL$new()$fit(D[c("X1", "X2")], D$Groupe)
m$variable_tests

m2 <- ADL$new()$fit(iris[1:4], iris$Species)
round(m2$variable_tests, 5)

m3 <- ADL$new()$fit(warpbreaks[c("breaks", "wool")], warpbreaks$tension)
round(m3$variable_tests, 5)








# ---- Etape 2.V : validation croisee ----
devtools::load_all()

# iris, 5 plis
cv <- ADL$new()$cross_validate(iris[1:4], iris$Species, folds = 5, seed = 1)
cv

# leave-one-out : comparaison avec MASS::lda(CV = TRUE)
loo <- ADL$new()$cross_validate(iris[1:4], iris$Species, folds = 150)
loo$accuracy
mean(MASS::lda(Species ~ ., data = iris, CV = TRUE)$class == iris$Species)

# warpbreaks : score optimiste contre validation croisee
X3 <- warpbreaks[c("breaks", "wool")]
ADL$new()$fit(X3, warpbreaks$tension)$score(X3, warpbreaks$tension)
ADL$new()$cross_validate(X3, warpbreaks$tension, folds = 5, seed = 1)$accuracy




# ---- Etape 2.3 : selection forward ----
devtools::load_all()
priv <- function(m) m$.__enclos_env__$private

# iris
m2 <- ADL$new()$fit(iris[1:4], iris$Species)
forward_selection(priv(m2)$FW, priv(m2)$FV, n = 150, K = 3, alpha = 0.05)

# verification : a l'etape 1, F = F de l'ANOVA a un facteur
summary(aov(Petal.Length ~ Species, data = iris))

# DATA 1, avec alpha = 0.05 puis alpha = 0.10
m <- ADL$new()$fit(D[c("X1", "X2")], D$Groupe)
forward_selection(priv(m)$FW, priv(m)$FV, n = 11, K = 2, alpha = 0.05)
forward_selection(priv(m)$FW, priv(m)$FV, n = 11, K = 2, alpha = 0.10)






# ---- Etape 2.4 : selection backward ----
devtools::load_all()
priv <- function(m) m$.__enclos_env__$private

# DATA 1 : backward doit garder X1 et X2 (contrairement a forward)
m <- ADL$new()$fit(D[c("X1", "X2")], D$Groupe)
backward_selection(priv(m)$FW, priv(m)$FV, n = 11, K = 2, alpha = 0.05)

# iris : alpha = 0.05 puis alpha = 0.01
m2 <- ADL$new()$fit(iris[1:4], iris$Species)
backward_selection(priv(m2)$FW, priv(m2)$FV, n = 150, K = 3, alpha = 0.05)
backward_selection(priv(m2)$FW, priv(m2)$FV, n = 150, K = 3, alpha = 0.01)

# warpbreaks : la laine doit etre retiree
m3 <- ADL$new()$fit(warpbreaks[c("breaks", "wool")], warpbreaks$tension)
backward_selection(priv(m3)$FW, priv(m3)$FV, n = 54, K = 3, alpha = 0.05)
