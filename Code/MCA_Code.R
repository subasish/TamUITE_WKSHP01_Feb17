### Multiple Correspondence Analysis
### REF PAPER: Das, S., and Sun, X. Factor Association with Multiple Correspondence Analysis in 
###            Vehicle–Pedestrian Crashes. TRR. http://dx.doi.org/10.3141/2519-11
setwd("/Users/subasishdas1/Desktop/TAMU_ITE_WKSHP1_Feb17/Data")
am <- read.csv("WWC_2008_13_Fin1.csv")
dim(am)

library(FactoMineR)
library(ggplot2)
library(ggrepel)

cats = apply(am, 2, function(x) nlevels(as.factor(x)))
cats

mca1 = MCA(am, graph = FALSE)
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))
library(RColorBrewer)

ggplot(data=mca1_vars_df)+
  geom_point(aes(x = Dim.1, y = Dim.2, color='red', size=3)) +
  geom_text_repel(aes(Dim.1, y = Dim.2, label = rownames(mca1_vars_df)), size=4, force=4)+
  scale_y_continuous(limits = c(-0.75, 0.75))+
  scale_x_continuous(limits = c(-0.75, 0.75))+theme_bw()+theme(legend.position="none")

