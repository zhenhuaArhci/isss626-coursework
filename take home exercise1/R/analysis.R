# Spatial and temporal analysis. Source prepare.R before this file.
## ----thx-first-order
# Retain original naive calendar values; no timezone conversion is made.
month_summary <- points |> st_drop_geometry() |>
  mutate(month=as.integer(substr(incident_datetime,6,7))) |>
  group_by(month) |> summarise(events=n(),fatal_events=sum(fatal),.groups='drop')
month_summary$days <- c(31,28,31,30,31,30,31,31,30,31,30,31)[month_summary$month]
month_summary$events_per_day <- month_summary$events/month_summary$days
save_plot('monthly', ggplot(month_summary,aes(x=month,y=events_per_day)) +
  geom_col(fill='#217c91',width=.65) + geom_text(aes(label=events),vjust=-.4,size=3.5) +
  scale_x_continuous(breaks=1:12,labels=month.abb) +
  labs(title='Monthly accident frequency, adjusted for month length',
    subtitle='Bar height: recorded events per calendar day. Labels: event counts.',
    x=NULL,y='Recorded accidents / day',caption='2022 | Six-province window | No traffic-volume adjustment'),height=4.8)
write_csv(month_summary,file.path(out_dir,'monthly.csv'))
# Planar summaries use the actual polygon and projected metre coordinates.
W <- as.owin(st_geometry(study_window))
X <- ppp(xy[,1],xy[,2],window=W,checkdup=FALSE)
stopifnot(npoints(X)==nrow(points))
bws <- c(1000,2000,3000)
kdes <- lapply(bws,function(h) {
  z <- density(X,sigma=h,eps=250,edge=TRUE,at='pixels')
  # FFT convolution can produce tiny negative round-off values far from events.
  stopifnot(min(z$v,na.rm=TRUE) > -1e-10)
  z$v <- pmax(z$v,0); z
})
names(kdes) <- as.character(bws)
kde_data <- bind_rows(lapply(seq_along(kdes),function(i) {
  dd <- as.data.frame(kdes[[i]]); names(dd) <- c('x','y','density')
  dd$density <- dd$density*1e6
  dd$bandwidth <- paste0(bws[i]/1000,' km'); dd
}))
planar_plot <- ggplot(kde_data,aes(x,y,fill=density)) + geom_raster() +
  geom_sf(data=provinces,fill=NA,colour='#7c8795',linewidth=.25,inherit.aes=FALSE) +
  scale_fill_viridis_c(option='magma',trans='sqrt',breaks=c(0,1,5,10,20),
    name='Events / km²',guide=guide_colourbar(barwidth=14)) +
  facet_wrap(~bandwidth,nrow=1) + coord_sf(crs=32647,datum=NA) +
  labs(title='Regional concentration persists across smoothing scales',
    subtitle='Gaussian planar KDE | edge corrected | common colour scale | 250 m grid',x=NULL,y=NULL,
    caption='Bandwidth is Gaussian standard deviation. Land between roads is smoothed; this is not road risk.') +
  theme(axis.text=element_blank(),axis.ticks=element_blank(),legend.position='bottom')
save_plot('planar-kde',planar_plot,12,5.8)
# Bandwidth sensitivity is evaluated on the same non-missing grid cells.
vals <- do.call(cbind,lapply(kdes,function(z)as.vector(z$v)))
vals <- vals[complete.cases(vals),,drop=FALSE]
rank_cor <- cor(vals,method='spearman')
top_overlap <- function(a,b) {
  a <- which(a>=quantile(a,.9)); b <- which(b>=quantile(b,.9))
  length(intersect(a,b))/length(union(a,b))
}
planar_sensitivity <- data.frame(comparison=c('1 vs 2 km','2 vs 3 km'),
  spearman=c(rank_cor[1,2],rank_cor[2,3]),
  top_decile_jaccard=c(top_overlap(vals[,1],vals[,2]),top_overlap(vals[,2],vals[,3])))
write_csv(planar_sensitivity,file.path(out_dir,'planar-sensitivity.csv'))

## ----thx-second-order
# Fixed-n homogeneous binomial process in the observed window: conditional CSR.
# This is a diagnostic benchmark, not a test of interaction after controlling roads.
set.seed(6262022)
r_seq <- seq(250,5000,250)
lminus <- function(pattern) {
  z <- Lest(pattern,r=c(0,r_seq),correction='border')
  as.numeric(z$border[-1])-r_seq
}
l_obs <- lminus(X)
nsim_csr <- 199L
l_sims <- replicate(nsim_csr,lminus(runifpoint(npoints(X),win=W)))
stopifnot(all(is.finite(l_obs)),all(is.finite(l_sims)))
t_obs <- max(abs(l_obs)); t_sims <- apply(abs(l_sims),2,max)
csr_p <- (1+sum(t_sims>=t_obs))/(nsim_csr+1)
# Conservative simultaneous 95% envelope, using the 190th of 199 simulated maxima.
critical <- sort(t_sims)[ceiling(.95*(nsim_csr+1))]
csr_table <- data.frame(r_m=r_seq,L_minus_r=l_obs,lower=-critical,upper=critical)
csr_plot <- ggplot(csr_table,aes(r_m/1000,L_minus_r/1000)) +
  geom_ribbon(aes(ymin=lower/1000,ymax=upper/1000),fill='#b7c9da',alpha=.7) +
  geom_hline(yintercept=0,colour='#64748b',linetype=2) +
  geom_line(colour='#bd4938',linewidth=1) +
  labs(title='The pattern departs strongly from uniform spatial placement',
    subtitle=paste0('Border-corrected L(r) − r | 199 fixed-count simulations | global Monte Carlo p = ',csr_p),
    x='Euclidean distance r (km)',y='L(r) − r (km)',
    caption='Shaded: simultaneous 95% CSR band over 0.25–5 km. Roads and reporting create spatial heterogeneity.')
save_plot('csr-envelope',csr_plot,9,5.5)
write_csv(csr_table,file.path(out_dir,'csr-envelope.csv'))
# Conditional random labelling: preserve positions AND fatal-event totals in every province-agency stratum.
# Unordered pairs; zero-distance pairs are retained in primary analysis.
r_mark <- c(250,500,1000,2000)
pairs <- closepairs(X,rmax=max(r_mark),twice=FALSE,what='ijd')
stopifnot(length(pairs$i)>0)
strata <- interaction(points$spatial_province,points$agency,drop=TRUE)
strata_index <- split(seq_len(nrow(points)),strata)
mark <- as.integer(points$fatal)
pair_counts <- function(m,include_zero=TRUE) {
  dd <- pairs$d[m[pairs$i]==1L & m[pairs$j]==1L & (include_zero | pairs$d>0)]
  vapply(r_mark,function(r)sum(dd<=r),integer(1))
}
nsim_mark <- 999L
set.seed(6262023)
permuted <- replicate(nsim_mark, {
  z <- mark
  for (idx in strata_index) z[idx] <- z[idx][sample.int(length(idx))]
  z
})
mark_obs <- pair_counts(mark)
mark_sims <- apply(permuted,2,pair_counts)
# Symmetric centring/scaling across observed plus simulations preserves exchangeability.
global_mark <- function(observed,simulated) {
  all_counts <- cbind(observed,simulated)
  means <- rowMeans(all_counts); sds <- apply(all_counts,1,sd)
  stopifnot(all(sds>0))
  maxima <- apply(sweep(sweep(all_counts,1,means,'-'),1,sds,'/'),2,max)
  list(p=(1+sum(maxima[-1]>=maxima[1]))/ncol(all_counts),
    statistic=maxima[1],means=means,sds=sds)
}
mark_test <- global_mark(mark_obs,mark_sims)
mark_table <- data.frame(r_m=r_mark,observed_pairs=mark_obs,
  expected_pairs=rowMeans(mark_sims),
  lower=apply(mark_sims,1,quantile,probs=.025),upper=apply(mark_sims,1,quantile,probs=.975))
mark_table$ratio <- mark_table$observed_pairs/mark_table$expected_pairs
save_plot('fatal-pairs',ggplot(mark_table,aes(r_m/1000,observed_pairs)) +
  geom_ribbon(aes(ymin=lower,ymax=upper),fill='#b7c9da',alpha=.75) +
  geom_line(aes(y=expected_pairs),colour='#217c91',linetype=2,linewidth=.8) +
  geom_line(colour='#bd4938',linewidth=1) + geom_point(colour='#bd4938',size=2) +
  scale_x_continuous(breaks=r_mark/1000) +
  labs(title='Do fatal accidents cluster beyond the observed accident pattern?',
    subtitle=paste0('Province + agency stratified random labels | 999 permutations | one-sided global p = ',mark_test$p),
    x='Euclidean distance threshold (km)',y='Unordered fatal–fatal event pairs',
    caption='Red: observed | Dashed: random-label mean | Shading: pointwise 95% intervals (not a global envelope).'),9,5.6)
write_csv(mark_table,file.path(out_dir,'fatal-pairs.csv'))

## ----thx-sensitivity
# Same random permutations, but omit all coincident pairs.
nozero_obs <- pair_counts(mark,FALSE)
nozero_sims <- apply(permuted,2,pair_counts,include_zero=FALSE)
nozero_test <- global_mark(nozero_obs,nozero_sims)
# Unstratified random labels are a deliberately less-adjusted comparison.
set.seed(6262024)
unstrat_sims <- replicate(nsim_mark,pair_counts(sample(mark)))
unstrat_test <- global_mark(mark_obs,unstrat_sims)
# CSR diagnostic after reducing repeated coordinates to one location each.
Xu <- X[!duplicated(loc_key)]
set.seed(6262025)
lu_obs <- lminus(Xu)
lu_sims <- replicate(nsim_csr,lminus(runifpoint(npoints(Xu),win=W)))
unique_csr_p <- (1+sum(apply(abs(lu_sims),2,max)>=max(abs(lu_obs))))/(nsim_csr+1)
stats_summary <- list(csr_p=csr_p,csr_max_deviation_m=t_obs,csr_unique_p=unique_csr_p,
  fatal_pair_p=mark_test$p,fatal_pair_nozero_p=nozero_test$p,
  fatal_pair_unstratified_p=unstrat_test$p,n_close_pairs=length(pairs$i),
  n_zero_pairs=sum(pairs$d==0),month_peak=month.abb[month_summary$month[which.max(month_summary$events_per_day)]],
  month_low=month.abb[month_summary$month[which.min(month_summary$events_per_day)]])
jsonlite::write_json(list(quality=quality,statistics=stats_summary),file.path(out_dir,'summary.json'),pretty=TRUE,auto_unbox=TRUE)
print(stats_summary); print(mark_table); print(planar_sensitivity)
