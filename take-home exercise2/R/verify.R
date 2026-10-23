# Full-output invariants; run after run.R. Validation fails rather than hiding warnings.
options(warn=2)
suppressPackageStartupMessages({library(sf);library(dplyr);library(readr)})
source("take-home exercise2/R/verify-methods.R")
a <- readRDS("take-home exercise2/data/derived/prepared.rds")
b <- readRDS("take-home exercise2/data/derived/analysis.rds")
cube_check <- a$cube
stopifnot(nrow(a$townships)==330L,nrow(cube_check)==18810L,
  length(unique(cube_check$month))==57L,
  all(table(cube_check$month)==330L),all(table(cube_check$TS_PCODE)==57L),
  !anyDuplicated(cube_check[c("TS_PCODE","month")]),
  sum(cube_check$armed_events)==nrow(a$events),
  sum(cube_check$civilian_events)==sum(a$events$civilian),
  all(cube_check$civilian_events<=cube_check$armed_events),
  all(cube_check$armed_events>=0),!anyNA(cube_check),
  min(cube_check$month)==as.Date("2021-01-01"),max(cube_check$month)==as.Date("2025-09-01"))
stopifnot(nrow(b$lmsa)==660L,nrow(b$ehsa)==660L,nrow(b$stgi)==37620L,
  !anyNA(b$lmsa),!anyNA(b$ehsa),!anyNA(b$stgi),
  all(b$lmsa$gi_p>=0 & b$lmsa$gi_p<=1),all(b$lmsa$local_i_p>=0 & b$lmsa$local_i_p<=1),
  all(b$ehsa$mk_p>=0 & b$ehsa$mk_p<=1),all(b$stgi$gi_p>=0 & b$stgi$gi_p<=1),
  all(b$ehsa$mk_p[b$ehsa$map_class!="Not displayed"]<.05),
  all(b$lmsa$gi_p[b$lmsa$gi_class!="Not significant"]<.05),
  all(b$lmsa$local_i_p[b$lmsa$moran_class!="Not significant"]<.05))
for (v in c("armed_events","civilian_events")) {
  lm <- b$lmsa[b$lmsa$variable==v,]
  stopifnot(identical(lm$TS_PCODE,a$townships$TS_PCODE))
  st <- b$stgi[b$stgi$variable==v,]
  stopifnot(identical(st$TS_PCODE,cube_check$TS_PCODE),identical(st$month,cube_check$month))
  # Verify independent analytical Gi* against the stored spdep score.
  nb <- lapply(seq_len(nrow(a$townships)),function(i)sort(unique(c(i,b$queen[[i]]))))
  stopifnot(max(abs(gi_star_z(a$townships[[v]],nb)-lm$gi_analytic))<1e-8)
  stopifnot(isTRUE(all.equal(lm$gi_q,p.adjust(lm$gi_p,"BH"))))
}
stopifnot(all(b$scan$familywise_p>0 & b$scan$familywise_p<=1),
  all(b$scan_top$familywise_p<.05),!anyDuplicated(b$scan_members$TS_PCODE),
  all(b$scan_top$rr>1))
pair_check <- cube_check |> filter(format(month,"%Y") %in% c("2024","2025"),
    as.integer(format(month,"%m"))<=9) |>
  group_by(TS_PCODE) |> summarise(total=sum(armed_events),
    late=sum(armed_events[format(month,"%Y")=="2025"]),.groups="drop")
for (j in seq_len(nrow(b$scan_top))) {
  ids <- b$scan_members$TS_PCODE[b$scan_members$scan_cluster==paste("Cluster",j)]
  inside <- pair_check$TS_PCODE %in% ids
  independently_computed <- (sum(pair_check$late[inside])/sum(pair_check$total[inside])) /
    (sum(pair_check$late[!inside])/sum(pair_check$total[!inside]))
  stopifnot(abs(b$scan_top$rr[j]-independently_computed)<1e-10)
}
totals <- c(10L,20L,30L,0L,40L)
set.seed(1)
for (i in 1:25) {
  r <- conditional_labels(totals,35L)
  stopifnot(sum(r)==35L,all(r<=totals),all(r>=0))
}
figs <- list.files("take-home exercise2/figures",pattern="[.]png$",full.names=TRUE)
stopifnot(length(figs)==10L,all(file.info(figs)$size>10000))
scripts <- list.files("take-home exercise2/R",pattern="[.]R$",full.names=TRUE)
for (f in scripts) parse(f)
text <- c("PASS: 330 townships x 57 months; all zero cells retained and event totals conserved.",
  "PASS: 660 LMSA rows, 660 EHSA rows and 37,620 space-time Gi* rows; no missing statistics.",
  "PASS: all primary statistical maps respect p < 0.05 masks; p and q are valid.",
  "PASS: matrix/cube PCode order matches; independent Gi* formula matches spdep.",
  "PASS: conditional scan preserves margins and selected windows do not overlap.",
  "PASS: ten nonempty figures and all R scripts parse.",
  "PASS: synthetic Gi*, MK, time-neighbourhood and EHSA classification checks.")
writeLines(text,"take-home exercise2/results/verification.txt")
cat(paste(text,collapse="\n"),"\n")
