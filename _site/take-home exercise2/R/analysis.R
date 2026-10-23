## ---- th2-neighbours
if (!exists("cube")) source("take-home exercise2/R/prepare.R")
set.seed(20261023)
n <- nrow(townships); nt <- length(months)
graph_warnings <- character()
queen_original <- withCallingHandlers(
  poly2nb(townships,queen=TRUE,snap=1,row.names=townships$TS_PCODE),
  warning=function(w) {
    if (grepl("no neighbours|sub-graphs",conditionMessage(w))) {
      graph_warnings <<- c(graph_warnings,conditionMessage(w)); invokeRestart("muffleWarning")
    }
  })
queen <- queen_original
isolated <- which(card(queen)==0L)
centres <- st_coordinates(st_point_on_surface(st_geometry(townships)))
distance_m <- as.matrix(dist(centres))
diag(distance_m) <- Inf
bridges <- tibble(from=character(),to=character(),distance_km=double())
if (length(isolated)) for (i in isolated) {
  j <- which.min(distance_m[i,])
  queen[[i]] <- as.integer(j)
  queen[[j]] <- as.integer(sort(unique(c(queen[[j]],i))))
  bridges <- bind_rows(bridges,tibble(from=townships$TS_PCODE[i],to=townships$TS_PCODE[j],distance_km=distance_m[i,j]/1000))
}
attr(queen,"sym") <- TRUE
write_csv(tibble(item=c("Original connected components","Original isolated townships",
  "Final connected components","Minimum neighbours","Maximum neighbours"),
  value=c(n.comp.nb(queen_original)$nc,length(isolated),n.comp.nb(queen)$nc,min(card(queen)),max(card(queen)))),
  file.path(base_dir,"results/graph-diagnostics.csv"))
lw <- nb2listw(queen,style="W",zero.policy=TRUE)
self_nb <- include.self(queen)
lw_star <- nb2listw(self_nb,style="B",zero.policy=TRUE)
write_csv(bridges,file.path(base_dir,"results/island-bridges.csv"))
edges <- bind_rows(lapply(seq_len(n),function(i) tibble(from=townships$TS_PCODE[i],to=townships$TS_PCODE[queen[[i]]])))
write_csv(edges,file.path(base_dir,"results/neighbour-edges.csv"))
variables <- c("armed_events","civilian_events")
labels <- c(armed_events="Armed events",civilian_events="Violence against civilians")

## ---- th2-lmsa
run_lmsa <- function(x, listw=lw, star_listw=lw_star, seed=20261023L) {
  lm <- localmoran_perm(x,listw,nsim=999,alternative="two.sided",iseed=seed,
                       no_repeat_in_row=TRUE,zero.policy=TRUE)
  gp <- localG_perm(x,star_listw,nsim=999,alternative="two.sided",iseed=seed,
                   no_repeat_in_row=TRUE,zero.policy=TRUE)
  internals <- attr(gp,"internals")
  z <- as.numeric(scale(x)); lag_z <- lag.listw(listw,z,zero.policy=TRUE)
  p <- lm[,"Pr(z != E(Ii)) Sim"]
  group <- ifelse(z>0 & lag_z>0,"High-high",ifelse(z<0 & lag_z<0,"Low-low",
    ifelse(z>0 & lag_z<0,"High-low","Low-high")))
  tibble(TS_PCODE=townships$TS_PCODE,local_i=lm[,"Ii"],local_i_p=p,
    local_i_q=p.adjust(p,"BH"),quadrant=group,
    moran_class=ifelse(p<.05,group,"Not significant"),
    gi_z=as.numeric(gp),gi_analytic=as.numeric(localG(x,star_listw)),
    gi_p=internals[,"Pr(z != E(Gi)) Sim"],
    gi_q=p.adjust(internals[,"Pr(z != E(Gi)) Sim"],"BH")) |>
    mutate(gi_class=ifelse(gi_p>=.05,"Not significant",ifelse(gi_z>0,"Hot spot","Cold spot")))
}
lmsa <- bind_rows(lapply(variables,function(v) run_lmsa(townships[[v]]) |> mutate(variable=v)))
write_csv(lmsa |> left_join(st_drop_geometry(townships) |> select(TS_PCODE,TS,ST),by="TS_PCODE"),
          file.path(base_dir,"results/lmsa.csv"))
global <- bind_rows(lapply(variables,function(v) {
  set.seed(20261023)
  g <- moran.mc(townships[[v]],lw,nsim=999,alternative="greater",zero.policy=TRUE)
  tibble(variable=v,moran_i=as.numeric(g$statistic),permutation_p=g$p.value)
}))
write_csv(global,file.path(base_dir,"results/global-moran.csv"))

## ---- th2-ehsa
# Space-time order is month-major, PCode-minor, identical to the complete cube.
st_list <- space_time_nb(self_nb,nt,1L)
as_nb <- function(x) {
  class(x) <- "nb"; attr(x,"region.id") <- as.character(seq_along(x))
  attr(x,"sym") <- FALSE; attr(x,"type") <- "user"; attr(x,"self.included") <- TRUE
  x
}
st_listw <- nb2listw(as_nb(st_list),style="B",zero.policy=TRUE)
stgi <- list(); ehsa <- list()
for (v in variables) {
  message("Space-time Gi* permutations: ",v)
  gp <- localG_perm(cube[[v]],st_listw,nsim=999,alternative="two.sided",
    iseed=20261023L,no_repeat_in_row=TRUE,zero.policy=TRUE)
  bin <- cube |> select(TS_PCODE,month) |> mutate(variable=v,
    gi_z=as.numeric(gp),gi_p=attr(gp,"internals")[,"Pr(z != E(Gi)) Sim"])
  bin$gi_q <- p.adjust(bin$gi_p,"BH")
  stgi[[v]] <- bin
  eh <- bind_rows(lapply(seq_len(n),function(i) {
    z <- bin$gi_z[seq(i,nrow(bin),by=n)]; p <- bin$gi_p[seq(i,nrow(bin),by=n)]
    mk <- mk_test(z)
    tibble(TS_PCODE=townships$TS_PCODE[i],variable=v,tau=mk$tau,mk_p=mk$p,
      block3_p=block_mk_p(z,3L,499L,20261023L+i),
      block6_p=block_mk_p(z,6L,499L,20261023L+i),
      hot_months=sum(z>0 & p<.05),cold_months=sum(z<0 & p<.05),
      last_z=tail(z,1),last_p=tail(p,1),classification=ehsa_class(z,p,mk$tau,mk$p))
  }))
  eh$mk_q <- p.adjust(eh$mk_p,"BH")
  eh$map_class <- ifelse(eh$mk_p<.05 & eh$classification!="No pattern detected",eh$classification,"Not displayed")
  ehsa[[v]] <- eh
}
stgi <- bind_rows(stgi); ehsa <- bind_rows(ehsa)
write_csv(stgi,file.path(base_dir,"results/space-time-gi.csv"))
write_csv(ehsa |> left_join(st_drop_geometry(townships) |> select(TS_PCODE,TS,ST),by="TS_PCODE"),
          file.path(base_dir,"results/ehsa.csv"))

## ---- th2-sensitivity
knn_nb <- knn2nb(knearneigh(centres,k=6),sym=TRUE,row.names=townships$TS_PCODE)
knn_lw <- nb2listw(knn_nb,style="W")
knn_star <- nb2listw(include.self(knn_nb),style="B")
sensitivity <- bind_rows(lapply(variables,function(v) {
  baseline <- lmsa |> filter(variable==v)
  alt <- run_lmsa(townships[[v]],knn_lw,knn_star)
  log_counts <- run_lmsa(log1p(townships[[v]]))
  precise_count <- if(v=="armed_events") townships$precise_events else
    as.numeric(table(factor(events$TS_PCODE[events$civilian==1 & events$geo_precision==1],levels=townships$TS_PCODE)))
  precise_lmsa <- run_lmsa(precise_count)
  bind_rows(lapply(c("six_nearest","log1p_counts","precision1_only"),function(s) {
    a <- switch(s,six_nearest=alt,log1p_counts=log_counts,precision1_only=precise_lmsa)
    main_hot <- baseline$gi_class=="Hot spot"; alt_hot <- a$gi_class=="Hot spot"
    tibble(variable=v,scenario=s,significant_moran=sum(a$local_i_p<.05),hotspots=sum(alt_hot),
      retained_baseline_hotspots=sum(main_hot & alt_hot),
      hotspot_jaccard=ifelse(sum(main_hot|alt_hot)>0,sum(main_hot & alt_hot)/sum(main_hot|alt_hot),NA_real_))
  }))
}))
write_csv(sensitivity,file.path(base_dir,"results/sensitivity.csv"))

## ---- th2-scan
# Equal-season windows avoid a misleading comparison with the incomplete year 2025.
pair <- cube |> filter(format(month,"%Y") %in% c("2024","2025"),as.integer(format(month,"%m"))<=9) |>
  mutate(period=ifelse(format(month,"%Y")=="2025","late","early")) |>
  group_by(TS_PCODE,period) |> summarise(events=sum(armed_events),.groups="drop") |>
  pivot_wider(names_from=period,values_from=events) |> arrange(TS_PCODE)
stopifnot(identical(pair$TS_PCODE,townships$TS_PCODE))
tot <- pair$early+pair$late; late <- pair$late
national_late <- sum(late); national_all <- sum(tot)
nearest <- lapply(seq_len(n),function(i) c(i,order(distance_m[i,])))
windows <- list(); centres_i <- integer(); sizes_i <- integer()
for (i in seq_len(n)) for (k in c(5L,9L,17L)) {
  w <- nearest[[i]][seq_len(k)]
  if (sum(tot[w])>0 && sum(tot[w])<=.2*sum(tot)) {
    windows[[length(windows)+1L]] <- w
    centres_i <- c(centres_i,i); sizes_i <- c(sizes_i,k)
  }
}
membership <- Matrix::sparseMatrix(i=rep(seq_along(windows),lengths(windows)),j=unlist(windows),
                                  x=1,dims=c(length(windows),n))
inside_tot <- as.numeric(membership %*% tot)
inside_late <- as.numeric(membership %*% late)
ll <- scan_llr(inside_late,inside_tot,sum(late),sum(tot))
set.seed(20261023)
null_max <- replicate(999,{
  simulated <- conditional_labels(tot,sum(late))
  max(scan_llr(as.numeric(membership %*% simulated),inside_tot,sum(late),sum(tot)))
})
scan <- tibble(window=seq_along(windows),centre=townships$TS[centres_i],centre_pcode=townships$TS_PCODE[centres_i],
  townships=sizes_i,early=inside_tot-inside_late,late=inside_late,llr=ll,
  rr=(inside_late/inside_tot)/((national_late-inside_late)/(national_all-inside_tot)),
  familywise_p=vapply(ll,function(x)(1+sum(null_max>=x-1e-12))/1000,numeric(1))) |> arrange(desc(llr))
# Report strongest nonoverlapping windows; inference was corrected over ALL windows.
keep <- integer(); used <- integer()
for (j in seq_len(nrow(scan))) {
  w <- windows[[scan$window[j]]]
  if (scan$familywise_p[j]<.05 && !any(w %in% used)) {
    keep <- c(keep,j); used <- union(used,w)
  }
  if(length(keep)==3L) break
}
scan_top <- scan[keep,]
scan_members <- bind_rows(lapply(seq_len(nrow(scan_top)),function(j)
  tibble(TS_PCODE=townships$TS_PCODE[windows[[scan_top$window[j]]]],scan_cluster=paste("Cluster",j))))
write_csv(scan,file.path(base_dir,"results/spatial-scan-windows.csv"))
write_csv(scan_top,file.path(base_dir,"results/spatial-scan-top.csv"))
write_csv(scan_members,file.path(base_dir,"results/spatial-scan-members.csv"))
saveRDS(list(lmsa=lmsa,global=global,ehsa=ehsa,stgi=stgi,queen=queen,bridges=bridges,
             sensitivity=sensitivity,scan=scan,scan_top=scan_top,scan_members=scan_members),
        file.path(base_dir,"data/derived/analysis.rds"))
message("LMSA, EHSA, robustness checks and conditional spatial scan complete.")
