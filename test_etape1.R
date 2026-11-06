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
