# Independent checks for the statistical methods used in Exercise 2.
options(warn = 2)
helper <- "take-home exercise2/R/methods.R"
if (file.exists(helper)) source(helper)
stopifnot(exists("gi_star_z"), exists("mk_test"), exists("ehsa_class"),
          exists("scan_llr"), exists("space_time_nb"))

# Gi* must match the published finite-population formula and spdep.
x <- c(1, 2, 4, 5, 9)
nb <- list(c(1L, 2L), c(1L, 2L, 3L), c(2L, 3L, 4L), c(3L, 4L, 5L), c(4L, 5L))
class(nb) <- "nb"; attr(nb, "region.id") <- as.character(seq_along(x))
attr(nb, "sym") <- TRUE; attr(nb, "type") <- "user"
attr(nb, "self.included") <- TRUE
lw <- spdep::nb2listw(nb, style = "B", zero.policy = TRUE)
z <- gi_star_z(x, nb)
stopifnot(isTRUE(all.equal(z, as.numeric(spdep::localG(x, lw)), tolerance = 1e-10)))
stopifnot(all(gi_star_z(rep(0, 5), nb) == 0))

# Time neighborhoods include no future cells; the first month has no fabricated past.
st <- space_time_nb(list(c(1L, 2L), c(1L, 2L)), 3L, 1L)
stopifnot(identical(st[[1]], c(1L, 2L)),
          identical(st[[3]], c(1L, 2L, 3L, 4L)),
          identical(st[[6]], c(3L, 4L, 5L, 6L)))
stopifnot(mk_test(rep(0, 57))$p == 1, mk_test(1:57)$tau > .99,
          mk_test(57:1)$p < .05)
sig <- rep(.001, 57); nonsig <- rep(1, 57)
stopifnot(ehsa_class(rep(2,57),sig,0,.8) == "Persistent hot spot",
          ehsa_class(rep(-2,57),sig,-.5,.001) == "Intensifying cold spot",
          ehsa_class(c(rep(0,56),2),c(nonsig[-57],.001),.2,.1) == "New hot spot",
          ehsa_class(c(rep(0,50),rep(2,7)),c(rep(1,50),rep(.001,7)),.3,.001) == "Consecutive hot spot",
          ehsa_class(rep(0,57),nonsig,0,1) == "No pattern detected")
stopifnot(scan_llr(20,40,50,100) == 0, scan_llr(30,40,50,100) > 0)
expected <- 30*log(.75/.5)+10*log(.25/.5)+20*log((1/3)/.5)+40*log((2/3)/.5)
stopifnot(abs(scan_llr(30,40,50,100)-expected) < 1e-10)
cat("Statistical helper checks passed.\n")
