works_with_R("3.0.2", plyr="1.8", rankSVMcompare="2013.10.25")

load("simulation.proportion.RData")

## matrix versions of the norm.
funs <- list(l2=function(x)rowSums(x*x),
             l1=function(x)rowSums(abs(x))^2,
             linf=function(x)apply(abs(x), 1, max)^2)
tr <- simulation.proportion$test.rank
roc <- data.frame()
auc <- data.frame()
for(prop in names(tr)){
  seed.list <- tr[[prop]]
  for(seed in names(seed.list)){
    norm.list <- seed.list[[seed]]
    for(norm in names(norm.list)){
      fit.list <- norm.list[[norm]]
      latent <- funs[[norm]]
      Pairs <- simulation.proportion$data[[prop]][[seed]][[norm]]$test
      fit.list$truth <- with(Pairs, cbind(latent(Xi), latent(Xip)))
      for(fit.name in names(fit.list)){
        rank.mat <- fit.list[[fit.name]]
        info <- data.frame(fit.name, norm,
                           seed=as.integer(seed),
                           prop=as.numeric(prop))
        stats <- calc.roc(rank.mat, Pairs$yi)
        this.roc <- data.frame(stats$roc, info)
        ## ggplot()+
        ##   geom_path(aes(FPR, TPR), data=this.roc)+
        ##   geom_segment(aes(min,TPR,xend=max,yend=TPR),
        ##                data=stats$boxes,colour="red",lwd=2)
        auc <- rbind(auc, this.auc)
        roc <- rbind(roc, this.roc)
      }
    }
  }
}

simulation.roc <- list(auc=auc, roc=roc)

save(simulation.roc, file="simulation.roc.RData")

