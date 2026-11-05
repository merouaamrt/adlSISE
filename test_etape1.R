devtools::load_all()
m <- ADL$new()
m
ADL$new(priors = "unif")
try(ADL$new(priors = "toto"))
try(ADL$new(priors = c(0.5, 0.6)))
try(ADL$new(alpha = 2))

