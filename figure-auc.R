works_with_R("3.0.2", plyr="1.8")

load("simulation.roc.RData")
load("mslr.roc.RData")

source("tikz.R")
source("colors.R")

library(grid)

mslr.roc$auc$norm <- "mslr"
auc <- rbind(simulation.roc$auc,
             mslr.roc$auc[,names(simulation.roc$auc)])
auc <- simulation.roc$auc

labels <- c(l1="||x||_1^2",
            l2="||x||_2^2",
            linf="||x||_\\infty^2")
labels[] <- sprintf("$r(x) = %s$", labels)
labels[["mslr"]] <- "MSLR"
makelabel <- function(x)labels[as.character(x)]
leg <- "function"
## ggplot(simulation.roc$roc, aes(FPR, TPR))+
##   geom_path(aes(colour=fit.name, group=interaction(fit.name, seed)))+
##   facet_grid(norm~prop)+
##   theme_bw()+
##   theme(panel.margin=unit(0,"cm"))+
##   scale_colour_manual(leg,values=model.colors)
auc.stats <- ddply(auc, .(fit.name, norm, prop), summarize,
                   mean=mean(auc), sd=sd(auc))
auc.stats$label <- makelabel(auc.stats$norm)
br <- c("latent", "truth", "compare", "rank2", "rank")
boring <- ggplot(auc.stats, aes(prop*100, mean))+
  geom_ribbon(aes(fill=fit.name,ymin=mean-sd,ymax=mean+sd), alpha=1/4)+
  geom_line(aes(colour=fit.name),lwd=2)+
  facet_grid(.~label)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  xlab("proportion of equality pairs")+
  scale_colour_manual(leg,values=model.colors,breaks=br)+
  scale_fill_manual(leg,values=model.colors,breaks=br)+
  ylab("Area under ROC curve")+
  scale_x_continuous("$\\rho =$ proportion of equality $y_i=0$ pairs",
                     breaks=seq(0,100,by=20))

tikz("figure-simulation-proportion.tex",h=2)
print(boring)
dev.off()
