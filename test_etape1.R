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
