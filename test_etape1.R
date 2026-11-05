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
