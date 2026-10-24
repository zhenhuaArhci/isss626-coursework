# Pure statistical helpers. No files are read or written by this script.

space_time_nb <- function(spatial_nb, n_times, temporal_lag = 1L) {
  n <- length(spatial_nb)
  ans <- vector("list", n * n_times)
  for (tt in seq_len(n_times)) for (ii in seq_len(n)) {
    times <- seq.int(max(1L, tt - temporal_lag), tt)
    ans[[(tt - 1L) * n + ii]] <- as.integer(sort(unique(unlist(
      lapply(times, function(t) spatial_nb[[ii]] + (t - 1L) * n)))))
  }
  ans
}

gi_star_z <- function(x, nb) {
  # Binary Gi*: population SD, finite-population correction and self included.
  n <- length(x); mu <- mean(x); ss <- sqrt(mean(x^2) - mu^2)
  if (!is.finite(ss) || ss == 0) return(rep(0, n))
  sizes <- lengths(nb)
  sums <- vapply(nb, function(j) sum(x[j]), numeric(1))
  den <- ss * sqrt((n * sizes - sizes^2) / (n - 1))
  ifelse(den > 0, (sums - mu * sizes) / den, 0)
}

mk_test <- function(x) {
  if (length(unique(x)) < 2L) return(list(tau = 0, p = 1))
  z <- Kendall::MannKendall(x)
  list(tau = as.numeric(z$tau), p = as.numeric(z$sl))
}

ehsa_class <- function(z, p, tau, mk_p, alpha = .05) {
  # Mutually exclusive, sign-specific rules following Esri's pattern definitions.
  h <- z > 0 & p < alpha; c <- z < 0 & p < alpha
  n <- length(z); last <- n
  for (side in c("hot", "cold")) {
    s <- if (side == "hot") h else c
    opposite <- if (side == "hot") c else h
    direction <- if (side == "hot") 1 else -1
    label <- paste(side, "spot")
    if (s[last] && sum(s) == 1L) return(paste("New", label))
    if (sum(s) / n >= .9) {
      if (!s[last]) return(paste("Historical", label))
      if (mk_p < alpha && tau * direction > 0) return(paste("Intensifying", label))
      if (mk_p < alpha && tau * direction < 0) return(paste("Diminishing", label))
      return(paste("Persistent", label))
    }
    if (s[last]) {
      start <- n
      while (start > 1L && s[start - 1L]) start <- start - 1L
      if (start < n && !any(s[seq_len(start - 1L)])) return(paste("Consecutive", label))
      if (any(opposite)) return(paste("Oscillating", label))
    }
    if (any(s) && !any(opposite)) return(paste("Sporadic", label))
  }
  "No pattern detected"
}

block_mk_p <- function(z, block = 3L, nsim = 499L, seed = 20261023L) {
  # Permute whole contiguous time blocks; preserve within-block dependence.
  observed <- abs(mk_test(z)$tau)
  if (observed == 0) return(1)
  blocks <- split(seq_along(z), ceiling(seq_along(z) / block))
  set.seed(seed)
  sims <- replicate(nsim, {
    ind <- unlist(blocks[sample.int(length(blocks))], use.names = FALSE)
    abs(mk_test(z[ind])$tau)
  })
  (1 + sum(sims >= observed - 1e-12)) / (nsim + 1)
}

scan_llr <- function(late_inside, total_inside, total_late, total_all) {
  # One-sided Bernoulli scan: each event is labelled earlier/later.
  a <- late_inside; b <- total_inside - a
  c <- total_late - a; d <- total_all - total_inside - c
  p <- total_late / total_all
  p1 <- a / total_inside; p0 <- c / (total_all - total_inside)
  term <- function(n, q, r) ifelse(n == 0, 0, n * log(pmax(q, 1e-300) / r))
  ll <- term(a,p1,p) + term(b,1-p1,1-p) + term(c,p0,p) + term(d,1-p0,1-p)
  ifelse(is.finite(p1) & is.finite(p0) & p1 > p0, pmax(0, ll), 0)
}

conditional_labels <- function(totals, n_late) {
  # Sequential multivariate hypergeometric sampling preserves both margins.
  remaining_n <- sum(totals); remaining_k <- n_late
  out <- integer(length(totals))
  for (j in seq_len(length(totals) - 1L)) {
    out[j] <- rhyper(1, totals[j], remaining_n - totals[j], remaining_k)
    remaining_k <- remaining_k - out[j]; remaining_n <- remaining_n - totals[j]
  }
  out[length(totals)] <- remaining_k
  out
}
