works_with_R("3.0.2", rankSVMcompare="2013.10.25", ggplot2="0.9.3.1",
             doMC="1.3.2")
library(kernlab)
registerDoMC()

source("svmlight.R")

funs <- list(l2=function(x)sum(x*x),
             l1=function(x)sum(abs(x))^2,
             linf=function(x)max(abs(x))^2)
deltas <- list(l2=function()runif(2,-1,1/2),
               l1=function()runif(2,-1/2,1/2),
               linf=function()runif(2,-1,1))
set.seed(1)
pair.sets <- list()
for(norm in names(funs)){
  delta.fun <- deltas[[norm]]
  f <- funs[[norm]]
  Xi <- c()
  Xip <- c()
  yi <- c()
  for(i in 1:20000){
    x <- runif(2,-2,2)
    delta <- delta.fun()
    xp <- x+delta
    noise <- 0
    noise <- rnorm(1,sd=1/4)#comment for noiseless simulation.
    fxdiff <- f(xp)-f(x)+noise
    y <- ifelse(fxdiff < -1, -1L,
                ifelse(fxdiff > 1, 1L, 0L))
    Xi <- rbind(Xi, x)
    Xip <- rbind(Xip, xp)
    rownames(Xi) <- rownames(Xip) <- NULL
    yi <- c(yi, y)
  }
  pair.sets[[norm]] <- list(Xi=Xi, Xip=Xip, yi=yi)
}
lapply(pair.sets, with, table(yi))
## pairs per set, so N/2 equality and N/2 inequality pairs per set.

all.ranks <- list()
unused.err <- list()
data.list <- list()
test.rank.list <- list()
props <- seq(0.1, 0.9, by=0.2)
N <- 400
for(prop in props){
  for(seed in 1:4){
    set.seed(seed)
    norm.list <- list()
    for(norm in names(pair.sets)){
      Pairs <- pair.sets[[norm]]
      is.zero <- Pairs$yi == 0
      equal <- which(is.zero)
      not.equal <- which(!is.zero)
      set.list <- list()
      for(set.name in c("train", "validation", "test")){
        i <- if(set.name=="test"){
          ## always use the same test set.
          c(equal[1:(N*prop)], not.equal[N*(1-prop)])
        }else{
          c(sample(equal, N*prop), sample(not.equal, N*(1-prop)))
        }
        equal <- equal[!equal %in% i]
        not.equal <- not.equal[!not.equal %in% i]
        set.list[[set.name]] <- list(
          Xi=Pairs$Xi[i,],
          Xip=Pairs$Xip[i,],
          yi=Pairs$yi[i])
      }
      norm.list[[norm]] <- set.list
    }
    ## Plot the points.
    point.df <- data.frame()
    seg.df <- data.frame()
    arrow.df <- data.frame()
    for(norm in names(norm.list)){
      set.list <- norm.list[[norm]]
      for(set in names(set.list)){
        Pairs <- set.list[[set]]
        m <- with(Pairs, rbind(Xi, Xip))
        point.df <- rbind(point.df, data.frame(m, norm, set))
        yi <- Pairs$yi
        segs <- with(Pairs, data.frame(Xi, Xip))[yi == 0,]
        seg.df <- rbind(seg.df, data.frame(norm, set, segs))
        arrow.df <- with(Pairs,{
          rbind(arrow.df,
                data.frame(norm, set, Xip, Xi)[yi == -1,],
                data.frame(norm, set, Xi, Xip)[yi == 1,])
        })
      }
    }
    library(grid)
basePlot <- ggplot(,aes(X1, X2))+
  facet_grid(set~norm)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  coord_equal()
pointPlot <- basePlot+
  geom_point(data=point.df)
print(pointPlot)
segPlot <- basePlot+
  aes(xend=X1.1,yend=X2.1)+
  geom_segment(data=seg.df)+
  geom_segment(data=arrow.df, arrow=arrow(type="closed",length=unit(0.05,"in")),
               color="red")
print(segPlot)
    ## Looks fine.
    levs <- seq(-3, 3, l=41)
    X.grid <- as.matrix(expand.grid(x1=levs,x2=levs))
    ## fit SVM.
    for(norm in names(norm.list)){
      cat(sprintf("prop=%4f seed=%4d norm=%s\n", prop, seed, norm))
      Pair.sets <- norm.list[[norm]]
      err.df <- data.frame()
      Cvals <- 10^seq(-3,3,l=10)
      kvals <- 2^seq(-7, 4, l=10)
      model.df <- expand.grid(C=Cvals, k.width=kvals)
      models <- foreach(model.i=1:nrow(model.df))%dopar%{
        model <- model.df[model.i,]
        Cval <- model$C
        k.width <- model$k.width
        ker <- rbfdot(k.width)
        ##ker <- laplacedot(k.width)
        ## cat(sprintf("%4d / %4d C=%5.2f k.width=%5.2f\n",
        ##             model.i, nrow(model.df), Cval, k.width))
        list(compare=softCompareQP(Pair.sets$train, ker, C=Cval),
                     rank=svmlight(Pair.sets$train, Cval, k.width),
        rank2=svmlight(Pair.sets$train, Cval, k.width, equality="bothpairs"))
      }
      ## Quantify their error on the train and validation sets.
      err.df <- foreach(model.i=seq_along(models), .combine=rbind)%dopar%{
        print(model.i)
        fits <- models[[model.i]]
        mInfo <- model.df[model.i,]
        Cval <- mInfo$C
        k.width <- mInfo$k.width
        foreach(fit.name=names(fits), .combine=rbind)%do%{
          fit <- fits[[fit.name]]
          foreach(set=c("train","validation","test"), .combine=rbind)%do%{
            s <- Pair.sets[[set]]
            rank.mat <- cbind(fit$rank(s$Xi), fit$rank(s$Xip))
            roc.info <- calc.roc(rank.mat, s$yi)
            pred <- fit$predict(s$Xi, s$Xip)
            true <- s$yi
            data.frame(FpFnInv(true, pred), auc=roc.info$auc,
                       Cval, k.width, set, fit.name)
          }
        }
      }
      ## train/validation error curves.
      f <- funs[[norm]]
      rank.df <- data.frame(X.grid, rank=apply(X.grid, 1, f), what="latent")
      chosen.df <- data.frame()
      for(what in levels(err.df$fit.name)){
        validation.err <- subset(err.df, what==fit.name & set=="validation")
        test.err <- subset(err.df, what==fit.name & set=="test")
        chosen <- which.min(validation.err$error)
        ## Select model with max AUC on validation set:
        chosen <- which.max(validation.err$auc)
        chosen.df <- rbind(chosen.df, validation.err[chosen,])
        fit <- models[[chosen]][[what]]
        ## Evaluate the rank on a grid, for drawing contour lines.
        r <- fit$rank(X.grid)
        rank.df <- rbind(rank.df, {
          data.frame(X.grid, rank=r-min(r), what)
        })
        unused.err.list[[paste(prop, seed, norm)]] <- data.frame(
          prop, seed, norm, test.err[chosen,])
      }
  overfitPlot <- ggplot(err.df, aes(log2(Cval), error, colour=fit.name))+
    geom_line(aes(group=interaction(set, fit.name), linetype=set))+
    facet_wrap("k.width")+
    theme_bw()+
    theme(panel.margin=unit(0,"cm"))+
    geom_point(data=chosen.df)
  print(overfitPlot)
  normContour <- ggplot(rank.df, aes(x1, x2, z=rank))+
    geom_contour(colour="black")+
    coord_equal()+
    theme_bw()+
    theme(panel.margin=unit(0,"cm"))+
    facet_grid(.~what)
  print(normContour)
    data.list[[as.character(prop)]][[as.character(seed)]] <- norm.list
      ## if the optimal model occurs on the min/max of the validationed
      ## hyperparameters, then this is probably sub-optimal and we need to
      ## define a larger grid.
      ##stopifnot(! validation.err[chosen,"Cval"] %in% range(Cvals))
      ##stopifnot(! validation.err[chosen,"k.width"] %in% range(kvals))
      all.ranks.list[[paste(prop, seed, norm)]] <- data.frame(
        rank.df, norm, seed, prop)
    } ## end norm
  }
}

simulation.proportion <-
  list(rank=all.ranks, error=unused.err, data=data.list)

save(simulation.proportion, file="simulation.proportion.RData")
