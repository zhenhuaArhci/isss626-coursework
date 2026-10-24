# Independent scientific and granularity checks. Run analysis.R first.
stopifnot(nrow(village_all) == 622L, nrow(village) == 617L,
  nrow(stores) == 28L, nrow(distmat_long) == 17276L,
  nrow(excluded_villages) == 5L, !anyDuplicated(village$VILLCODE),
  !anyDuplicated(stores$Store_CD), all(distmat_long$distance > 0))
manual <- interaction_matrix %>%
  group_by(VILLCODE) %>%
  mutate(expected = (Store_size / distance^2) / sum(Store_size / distance^2)) %>%
  ungroup() %>% select(VILLCODE, Store_CD, expected)
check <- left_join(huff_share, manual, by = c("VILLCODE", "Store_CD"))
stopifnot(max(abs(check$p_ij - check$expected)) < 1e-12,
  all(huff_share$p_ij >= 0 & huff_share$p_ij <= 1),
  max(abs(tapply(huff_share$p_ij, huff_share$VILLCODE, sum) - 1)) < 1e-12,
  abs(sum(store_sales_est$sum_E_j) - sum(village$P_CNT)) < 1e-6,
  identical(levels(village_TA$TA), sort(stores$Store_CD)),
  length(unique(as.character(village_TA$TA))) == 28L,
  nrow(village_TA) == 617L, !anyNA(village_TA$TA), length(ta_colours) == 28L,
  length(unique(ta_colours)) == 28L)
# Equal attraction and inverse-distance power imply the nearest store wins.
nearest <- apply(as.matrix(distmat), 1, function(x) colnames(distmat)[which.min(x)])
stopifnot(identical(as.character(village_TA$TA), unname(nearest)),
  sum(store_summary$dominant_villages) == 617L)
cat("PASS: IDs, dimensions, formula, probability sums, population conservation,\n",
    "28 legend levels/colours, all 617 village assignments and nearest-store equivalence.\n")
