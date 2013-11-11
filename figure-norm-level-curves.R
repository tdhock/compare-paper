##load("simulation.RData")

load("simulation.samples.RData")

source("tikz.R")
source("Nsamp.R")
source("colors.R")

pfactor <- function(x){
  x <- ifelse(x=="training data", x, paste(x, "model"))
  factor(x, c("training data", "rank model", "rank2 model", "compare model"))
}
eq.lab <- "equality pair\n$y_i=0$"
ineq.lab <- "inequality pair\n$y_i\\in\\{-1,1\\}$"
pair.colors <- yi.colors[c("0","1")]
pair.types <- c(eq.lab, ineq.lab)
names(pair.colors) <- pair.types

##norm.list <- simulation$train
Nseed <- 2
norm.list <- simulation.samples$data[[Nsamp]][[Nseed]]
seg.df <- data.frame()
arrow.df <- data.frame()
for(norm in show.norm){
  Pairs <- norm.list[[norm]]$train
  m <- with(Pairs, rbind(Xi, Xip))
  yi <- Pairs$yi
  segs <- with(Pairs, data.frame(Xi, Xip))[yi == 0,]
  seg.df <- rbind(seg.df, data.frame(norm, segs))
  arrow.df <- with(Pairs,{
    rbind(arrow.df,
          data.frame(norm, Xip, Xi)[yi == -1,],
          data.frame(norm, Xi, Xip)[yi == 1,])
  })
}
seg.df$what <- factor(eq.lab, pair.types)
arrow.df$what <- factor(ineq.lab, pair.types)
train.fun <- pfactor("training data")
seg.df$fun <- train.fun
arrow.df$fun <- train.fun
library(grid)
basePlot <- ggplot(,aes(X1, X2))+
  facet_grid(.~norm)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  coord_equal()
segPlot <- basePlot+
  aes(xend=X1.1,yend=X2.1)+
  geom_segment(data=seg.df)+
  geom_segment(data=arrow.df,
               arrow=arrow(type="closed",length=unit(0.075,"in")),
               color="red")
print(segPlot)


##all.ranks <- simulation$rank
all.ranks <- subset(simulation.samples$rank,
                    seed==Nseed & N==Nsamp & norm==show.norm)
labels <- c(l1="||x||_1^2",
            l2="||x||_2^2",
            linf="||x||_\\infty^2")
all.ranks$label <- sprintf("$r(x) = %s$", labels[as.character(all.ranks$norm)])
seg.df$label <- sprintf("$r(x) = %s$", labels[as.character(seg.df$norm)])
arrow.df$label <- sprintf("$r(x) = %s$", labels[as.character(arrow.df$norm)])
toplot <- data.frame()
plot.funs <- c("rank",
               "rank2",
               "compare")
for(fun in plot.funs){
  these <- subset(all.ranks, what %in% c("latent", fun))
  fun <- pfactor(fun)
  toplot <- rbind(toplot, data.frame(these, fun))
}
toplot$fun.type <- factor(ifelse(toplot$what=="latent", "truth", "learned"))
br <- seq(-2,2,by=1)
lwd <- 1
p <- ggplot()+
  geom_segment(aes(X1, X2, xend=X1.1, yend=X2.1, color=what), data=seg.df,lwd=lwd)+
  geom_segment(aes(X1, X2, xend=X1.1, yend=X2.1, color=what), data=arrow.df,
               arrow=arrow(type="open",length=unit(0.05,"in")),
               lwd=lwd)+
  geom_contour(aes(x1, x2, z=rank, alpha=fun.type, group=fun.type),
               ##breaks=1:4,
               data=toplot, size=1, colour="black")+
  scale_alpha_manual("ranking function",
                     values=c(truth=1/3, latent=1/3,learned=1))+
  facet_grid(.~fun)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"),
        panel.grid=element_blank())+
  coord_equal()+
  ## geom_contour(aes(x1, x2, z=rank, colour=what, group=what),
  ##              data=toplot, size=1)+
  ## scale_colour_manual("lines",values=model.colors, breaks=what.levs,
  ##                     labels=c(eq.lab, ineq.lab, "latent $r$",
  ##                       "SVMrank\nignore $y_i=0$",
  ##                       "SVMrank\ndouble $y_i=0$",
  ##                       "SVMcompare\nmodel"))+
  scale_colour_manual("label",values=pair.colors)+
  scale_x_continuous("feature 1",breaks=br)+
  scale_y_continuous("feature 2",breaks=br)+
  guides(colour=guide_legend(keyheight=2, order=1))
print(p)

tikz("figure-norm-level-curves.tex", h=2)
print(p)
dev.off()

