# Source handson2A/R/prepare.R before running this file interactively.
## ----second-order-setup
# Simple point-process diagnostics use distinct sites; records remain in 2A's KDE.
patterns <- lapply(area_patterns[c("CHOA CHU KANG", "TAMPINES")], unique)
nsim <- 199L
distance_grid <- seq(0, 500, length.out = 129)
functions <- list(G = Gest, F = Fest, K = Kest, L = Lest)
corrections <- c(G = "km", F = "km", K = "Ripley", L = "Ripley")
set.seed(20260921)
# Conditional CSR: each simulation preserves the area's number of distinct sites.
simulations <- lapply(patterns, function(x)
  lapply(seq_len(nsim), function(i) runifpoint(npoints(x), win = Window(x))))
estimates <- lapply(patterns, function(x) lapply(names(functions), function(fn)
  functions[[fn]](x, r = distance_grid, correction = corrections[[fn]])))
for (nm in names(estimates)) names(estimates[[nm]]) <- names(functions)

## ----area-map
tmap_mode("plot")
comparison_maps <- lapply(names(patterns), function(nm) {
  sites <- st_as_sf(data.frame(x = patterns[[nm]]$x, y = patterns[[nm]]$y),
                    coords = c("x", "y"), crs = 3414)
  tm_shape(areas[[nm]]) + tm_borders(col = "#465d91", lwd = 1.5) +
    tm_shape(sites) + tm_dots(col = "#465d91", size = 0.06) +
    tm_title(tools::toTitleCase(tolower(nm))) +
    tm_scalebar(position = tm_pos_in("left", "bottom"))
})
tmap_arrange(comparison_maps[[1]], comparison_maps[[2]], ncol = 2)

## ----envelope-method
envelopes <- lapply(names(patterns), function(nm) {
  result <- lapply(names(functions), function(fn) {
    envelope(patterns[[nm]], fun = functions[[fn]],
             r = distance_grid, correction = corrections[[fn]],
             simulate = simulations[[nm]], nsim = nsim, nrank = 5,
             global = FALSE, use.theory = TRUE, savefuns = TRUE, verbose = FALSE)
  })
  names(result) <- names(functions)
  result
})
names(envelopes) <- names(patterns)

## ----plot-helper
plot_comparison <- function(fn) {
  old <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  on.exit(par(old))
  for (nm in names(envelopes)) {
    e <- envelopes[[nm]][[fn]]
    if (fn == "L") {
      plot(e, . - r ~ r, main = tools::toTitleCase(tolower(nm)),
           xlab = "Distance r (m)", ylab = "L(r) - r (m)", legend = FALSE)
    } else {
      plot(e, main = tools::toTitleCase(tolower(nm)),
           xlab = "Distance r (m)", ylab = if (fn == "K") "K(r) (m²)" else paste0(fn, "(r)"),
           legend = FALSE)
    }
    legend("topleft", c("Observed", "CSR reference", "95% pointwise envelope"),
           lty = c(1, 2, NA), pch = c(NA, NA, 15),
           col = c("black", "red", "grey80"), cex = 0.65, bty = "n")
  }
}

## ----g-function
G_border <- Gest(patterns[["CHOA CHU KANG"]], r = distance_grid, correction = "border")
G_km <- estimates[["CHOA CHU KANG"]][["G"]]
data.frame(distance_m = c(100, 250, 500),
           border = approx(G_border$r, G_border$rs, xout = c(100, 250, 500))$y,
           Kaplan_Meier = approx(G_km$r, G_km$km, xout = c(100, 250, 500))$y)
plot_comparison("G")

## ----f-function
plot_comparison("F")

## ----k-function
plot_comparison("K")

## ----l-function
plot_comparison("L")

## ----global-tests
# A fixed reference and a preselected distance interval make one statistic per curve.
# Compare the observed maximum deviation against the same statistic for every simulation.
deviation_test <- function(e) {
  simulated <- as.data.frame(attr(e, "simfuns"))
  simulated <- as.matrix(simulated[, startsWith(names(simulated), "sim"), drop = FALSE])
  valid <- is.finite(e$obs) & is.finite(e$theo) &
    apply(simulated, 1, function(v) all(is.finite(v)))
  if (!all(valid)) stop("Undefined estimates on the selected distance grid; inspect the window.")
  observed <- max(abs(e$obs - e$theo))
  null_deviations <- apply(abs(sweep(simulated, 1, e$theo, "-")), 2, max)
  c(deviation = observed, p = (1 + sum(null_deviations >= observed)) /
      (1 + ncol(simulated)))
}
test_results <- do.call(rbind, lapply(names(envelopes), function(nm)
  do.call(rbind, lapply(names(functions), function(fn) {
    test <- deviation_test(envelopes[[nm]][[fn]])
    data.frame(area = nm, function_name = fn,
               maximum_deviation = unname(test["deviation"]), p = unname(test["p"]))
  }))))
test_results$p_holm <- p.adjust(test_results$p, method = "holm")
test_results$decision_5pct <- ifelse(test_results$p_holm <= 0.05,
                                    "Reject conditional CSR", "Do not reject conditional CSR")
test_results

## ----distance-summary
distance_summary <- do.call(rbind, lapply(names(envelopes), function(nm)
  do.call(rbind, lapply(names(functions), function(fn) {
    e <- envelopes[[nm]][[fn]]
    at <- which.min(abs(e$r - 250))
    data.frame(area = nm, function_name = fn, distance_m = e$r[at],
               observed = e$obs[at], CSR = e$theo[at],
               envelope_low = e$lo[at], envelope_high = e$hi[at])
  }))))
distance_summary

## ----duplicate-sensitivity
set.seed(20260922)
sensitivity <- do.call(rbind, lapply(names(patterns), function(nm) {
  record_pattern <- area_patterns[[nm]]
  e <- envelope(record_pattern, Lest, r = distance_grid, correction = "Ripley",
                fix.n = TRUE, nsim = nsim, nrank = 5, use.theory = TRUE,
                savefuns = TRUE, verbose = FALSE)
  idx <- which.min(abs(distance_grid - 250))
  data.frame(area = nm, records = npoints(record_pattern), sites = npoints(patterns[[nm]]),
             L_minus_r_records_250m = e$obs[idx] - e$r[idx],
             L_minus_r_sites_250m = envelopes[[nm]]$L$obs[idx] - e$r[idx],
             records_global_p = unname(deviation_test(e)["p"]))
}))
sensitivity
dir.create("handson2B/data/derived", recursive = TRUE, showWarnings = FALSE)
write.csv(test_results, "handson2B/data/derived/csr-tests.csv", row.names = FALSE)
