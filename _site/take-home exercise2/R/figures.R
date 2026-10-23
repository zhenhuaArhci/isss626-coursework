## ---- th2-figure-setup
if (!exists("ehsa")) {
  library(sf); library(dplyr); library(tidyr); library(readr); library(ggplot2)
  a <- readRDS("take-home exercise2/data/derived/prepared.rds")
  b <- readRDS("take-home exercise2/data/derived/analysis.rds")
  list2env(a,envir=environment()); list2env(b,envir=environment())
}
base_dir <- "take-home exercise2"
labels <- c(armed_events="Armed events",civilian_events="Violence against civilians")
theme_set(theme_minimal(base_size=12,base_family="sans"))
fig_theme <- theme(plot.title=element_text(face="bold",size=18,color="#3b261f"),
  plot.subtitle=element_text(color="#5d554f",margin=margin(b=12)),
  plot.caption=element_text(size=9,color="#666666",hjust=0),
  panel.grid.minor=element_blank(),strip.text=element_text(face="bold",size=12),
  plot.background=element_rect(fill="white",color=NA),legend.title=element_text(face="bold"))
map_theme <- fig_theme + theme(axis.title=element_blank(),axis.text=element_blank(),
  axis.ticks=element_blank(),panel.grid=element_blank(),legend.position="bottom")
map_geo <- st_simplify(townships,dTolerance=120,preserveTopology=TRUE)
states <- map_geo |> group_by(ST) |> summarise(.groups="drop")
source_caption <- "Sources: supplied ACLED extract; MIMU township boundaries v9.4. Analysis: Zhenhua Liu."
save_fig <- function(name,p,w=11,h=8.5) {
  ggsave(file.path(base_dir,"figures",paste0(name,".png")),p,width=w,height=h,dpi=180,bg="white",limitsize=FALSE)
}
map_data <- function(tab) map_geo |> select(TS_PCODE) |> left_join(tab,by="TS_PCODE")
map_base <- function(tab,fill,title,subtitle,colors=NULL) {
  p <- ggplot(map_data(tab))+geom_sf(aes(fill=.data[[fill]]),color="#ffffff",linewidth=.06)+
    geom_sf(data=states,fill=NA,color="#625951",linewidth=.2)+
    facet_wrap(~variable,labeller=as_labeller(labels))+coord_sf(datum=NA)+
    labs(title=title,subtitle=subtitle,caption=source_caption,fill=NULL)+map_theme
  if (!is.null(colors)) p <- p+scale_fill_manual(values=colors,drop=TRUE)
  p
}

## ---- th2-count-figures
count_long <- st_drop_geometry(townships) |> select(TS_PCODE,armed_events,civilian_events) |>
  pivot_longer(-TS_PCODE,names_to="variable",values_to="count")
p <- map_base(count_long,"count","Recorded conflict is geographically concentrated",
  "Township totals, January 2021–September 2025; colour uses a log scale")+
  scale_fill_viridis_c(option="magma",direction=-1,trans="log1p",breaks=c(0,10,100,500,2000),
    labels=scales::label_comma(),name="Recorded events")
save_fig("01-township-counts",p)
monthly <- cube |> group_by(month) |> summarise(across(c(armed_events,civilian_events),sum),.groups="drop") |>
  pivot_longer(-month,names_to="variable",values_to="count")
p <- ggplot(monthly,aes(month,count,color=variable))+geom_line(linewidth=.9)+
  scale_color_manual(values=c(armed_events="#9d382c",civilian_events="#29666f"),labels=labels)+
  scale_x_date(date_breaks="1 year",date_labels="%Y")+
  scale_y_continuous(labels=scales::label_comma())+
  labs(title="The observation window contains 57 complete calendar months",
    subtitle="National monthly totals; 2025 ends in September",x=NULL,y="Recorded events",color=NULL,
    caption=paste("Included: three armed event types, coordinate precision 1–2 and polygon containment.",source_caption,sep="\n"))+fig_theme+
  theme(legend.position="bottom")
save_fig("02-monthly-trend",p,11,5)
top12 <- townships |> st_drop_geometry() |> arrange(desc(armed_events)) |> slice_head(n=12)
heat <- cube |> filter(TS_PCODE %in% top12$TS_PCODE) |>
  mutate(TS=factor(TS,levels=rev(top12$TS)))
p <- ggplot(heat,aes(month,TS,fill=armed_events))+geom_tile()+
  scale_fill_viridis_c(option="magma",direction=-1,trans="log1p",name="Events / month")+
  scale_x_date(date_breaks="1 year",date_labels="%Y")+
  labs(title="A space-time cube preserves quiet months as zeros",
    subtitle="Twelve townships with the highest total armed-event counts",x=NULL,y=NULL,caption=source_caption)+
  fig_theme+theme(panel.grid=element_blank())
save_fig("03-cube-heatmap",p,11,6)

## ---- th2-lmsa-figures
moran_colors <- c("High-high"="#b62230","Low-low"="#2166ac","High-low"="#edab53",
  "Low-high"="#83c5d9","Not significant"="#e8e6e3")
gi_colors <- c("Hot spot"="#b62230","Cold spot"="#2166ac","Not significant"="#e8e6e3")
p <- map_base(lmsa,"moran_class","Local Moran's I: clusters and spatial outliers",
  "Only two-sided conditional permutation p < 0.05 is coloured; 999 permutations",moran_colors)
save_fig("04-local-moran",p)
p <- map_base(lmsa,"gi_class","Gi*: neighbours can amplify or temper local concentration",
  "Self included; only two-sided conditional permutation p < 0.05 is coloured",gi_colors)
save_fig("05-local-gi",p)
qmap <- lmsa |> mutate(fdr_class=ifelse(gi_q<.05,ifelse(gi_z>0,"Hot spot","Cold spot"),"Not significant"))
p <- map_base(qmap,"fdr_class","How much hotspot evidence survives multiple testing?",
  "Benjamini–Hochberg q < 0.05 within each outcome; same spatial weights",gi_colors)
save_fig("06-gi-fdr",p)

## ---- th2-ehsa-figures
eh_colors <- c("New hot spot"="#e66101","Consecutive hot spot"="#df815c",
 "Intensifying hot spot"="#8d1020","Persistent hot spot"="#c42b40",
 "Diminishing hot spot"="#bd5a7b","Sporadic hot spot"="#e4a59e","Oscillating hot spot"="#9e67a8",
 "Historical hot spot"="#9d796a","New cold spot"="#087eac","Consecutive cold spot"="#56a8c2",
 "Intensifying cold spot"="#143e7c","Persistent cold spot"="#367aba","Diminishing cold spot"="#7ca8d5",
 "Sporadic cold spot"="#9ecbd8","Oscillating cold spot"="#637bb4","Historical cold spot"="#687f90",
 "Not displayed"="#e8e6e3","No pattern detected"="#e8e6e3")
p <- map_base(ehsa,"map_class","Emerging patterns with a significant Gi* trend",
 "Colour requires Mann–Kendall p < 0.05 and a classified pattern; grey is not evidence of safety",eh_colors)
save_fig("07-ehsa",p,11,9)
mkmap <- ehsa |> mutate(significant_tau=ifelse(mk_p<.05,tau,NA_real_))
p <- map_base(mkmap,"significant_tau","Which townships move up or down in relative concentration?",
 "Mann–Kendall trend in space-time Gi*; only p < 0.05 is displayed")+
 scale_fill_gradient2(low="#2166ac",mid="#fffdfa",high="#b62230",midpoint=0,limits=c(-1,1),
   na.value="#e8e6e3",name="Kendall tau")
save_fig("08-mk-trend",p)
selected <- ehsa |> filter(variable=="armed_events") |> arrange(desc(abs(tau))) |> slice_head(n=4)
trajectory <- stgi |> filter(variable=="armed_events",TS_PCODE %in% selected$TS_PCODE) |>
  left_join(st_drop_geometry(townships) |> select(TS_PCODE,TS,ST),by="TS_PCODE")
p <- ggplot(trajectory,aes(month,gi_z,color=TS))+geom_hline(yintercept=0,color="#999999",linetype=2)+
  geom_line(linewidth=.8)+scale_x_date(date_breaks="1 year",date_labels="%Y")+
  labs(title="Gi* trends describe relative clustering, not local event growth",
    subtitle="Four strongest absolute Kendall trends in armed-event concentration",x=NULL,y="Space-time Gi* z-score",color="Township",
    caption="Neighbourhood: township plus queen neighbours, current and previous month; first month uses current only.")+fig_theme+
  theme(legend.position="bottom")
save_fig("09-gi-trajectories",p,11,5)

## ---- th2-scan-figure
scanmap <- townships |> st_drop_geometry() |> select(TS_PCODE) |>
  left_join(scan_members,by="TS_PCODE") |> mutate(scan_cluster=replace_na(scan_cluster,"Outside selected clusters"))
p <- ggplot(map_data(scanmap))+geom_sf(aes(fill=scan_cluster),color="white",linewidth=.08)+
  geom_sf(data=states,fill=NA,color="#625951",linewidth=.2)+coord_sf(datum=NA)+
  scale_fill_manual(values=c("Cluster 1"="#8c1f33","Cluster 2"="#d56e33","Cluster 3"="#754d89",
    "Outside selected clusters"="#e8e6e3"),drop=TRUE)+
  labs(title="Where did the conflict share shift towards 2025?",
    subtitle="Conditional Bernoulli spatial scan: Jan–Sep 2024 versus Jan–Sep 2025",fill=NULL,
    caption=paste("Top nonoverlapping clusters with familywise Monte Carlo p < 0.05; 999 simulations.",source_caption,sep="\n"))+map_theme
save_fig("10-spatial-scan",p,8.5,8.5)
message("Ten analytical figures written.")
