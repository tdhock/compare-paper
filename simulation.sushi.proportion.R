source("packages.R")

load("sushi.pairs.RData")

options(mc.cores=2)#because we may swap!
plan(multiprocess)

LAPPLY <- lapply

source("svmlight.R")

funs <- list(l2=function(x)sum(x*x),
             l1=function(x)sum(abs(x))^2,
             linf=function(x)max(abs(x))^2)
set.seed(1)
total.sim <- 25000
pair.sets <- list(sushi=sushi.pairs)
for(norm in names(funs)){
  f <- funs[[norm]]
  Xi <- matrix(runif(2*total.sim, -2, 2), total.sim, 2)
  Xip <- Xi + rnorm(2*total.sim, sd=1/2)
  noise <- rnorm(total.sim, sd=1/4)
  fxdiff <- apply(Xip, 1, f)-apply(Xi, 1, f)+noise
  yi <- ifelse(
    fxdiff < -1, -1L, ifelse(
      1 < fxdiff, 1L, 0L))
  colnames(Xi) <- colnames(Xip) <- c("x1", "x2")
  pair.sets[[norm]] <- list(Xi=Xi, Xip=Xip, yi=yi)
}
lapply(pair.sets, with, table(yi))
## pairs per set, so N/2 equality and N/2 inequality pairs per set.

levs <- seq(-2,2,l=41)
X.grid <- as.matrix(expand.grid(x1=levs,x2=levs))

dir.create("cache-prop")


computeROC <- function(rank.diff, label){
  stopifnot(length(rank.diff)==length(label))
  rd <- data.table(rank.diff=as.numeric(rank.diff), label=as.numeric(label))
  rd[, abs.diff := abs(rank.diff)]
  ord <- rd[order(-abs.diff)]
  ord[, FP := cumsum(label==0)]
  ord[, TP := cumsum((label==-1 & rank.diff < 0) | (label == 1 & rank.diff > 0))]
  ord[, FPR := FP / sum(label != 0)]
  ord[, TPR := TP / sum(label != 0)]
  ord
}

all.ranks.list <- list()
err.list <- list()
data.list <- list()
test.N <- 10000
N <- 100
for(data.name in names(pair.sets)){
  Pairs <- pair.sets[[data.name]]
  true.f <- funs[[data.name]]
  is.zero <- Pairs$yi == 0
  set.vec <- rep(NA, length(is.zero))
  set.seed(1)
  set.vec[sample(which(is.zero), test.N/2)] <- "test"
  set.vec[sample(which(!is.zero), test.N/2)] <- "test"
  is.test <- set.vec=="test" & !is.na(set.vec)
  test.set <- with(Pairs, list(
    Xi=Xi[is.test,], Xip=Xip[is.test,], yi=yi[is.test]))
  for(prop in seq(0.1, 0.9, by=0.1)){
    for(seed in 1:4){
      N.set.vec <- set.vec
      set.seed(seed)
      seed.sets <- list()
      for(set.name in c("train", "validation")){
        N.set.vec[sample(which(is.na(N.set.vec) & is.zero), N*prop)] <- set.name
        N.set.vec[sample(which(is.na(N.set.vec) & !is.zero), N*(1-prop))] <- set.name
        is.set <- N.set.vec==set.name & !is.na(N.set.vec)
        seed.sets[[set.name]] <- with(Pairs, list(
          Xi=Xi[is.set,], Xip=Xip[is.set,], yi=yi[is.set]))
      }
      ## fit SVM.
      err.df.list <- list()
      Cvals <- 10^seq(-1,5,l=10)
      kvals <- 2^seq(-8, 4, l=10)
      model.df <- expand.grid(C=Cvals, k.width=kvals)
      cache.RData <- paste0("cache-prop/", data.name, "_", seed, "_", N, ".RData")
      if(file.exists(cache.RData)){
        load(cache.RData)
      }else{
        cat(sprintf("N=%4d seed=%4d data.name=%s\n", N, seed, data.name))
        err.dt.list <- LAPPLY(1:nrow(model.df), function(model.i){
          model <- model.df[model.i,]
          ##cat(sprintf("%4d / %4d models\n", model.i, nrow(model.df)))
          Cval <- model$C
          k.width <- model$k.width
          ker <- rbfdot(k.width)
          ## TODO: balanced weights!
          fits <- list(
            compare=softCompareQP(seed.sets$train, ker, C=Cval),
            rank2=svmlight(
              seed.sets$train, Cval, k.width, equality="bothpairs",
              filebase=sub("RData", paste0("rank2", "_", model.i), cache.RData)),
            rank=svmlight(
              seed.sets$train, Cval, k.width,
              filebase=sub("RData", paste0("rank", "_", model.i), cache.RData)))
          data.table(expand.grid(fit.name=names(fits), set=c("train","validation")))[, {
            fit <- fits[[fit.name]]
            s <- seed.sets[[set]]
            rank.diff <- fit$rank(s$Xip)-fit$rank(s$Xi)
            true <- s$yi
            roc.dt <- computeROC(rank.diff, true)
            auc <- WeightedROC::WeightedAUC(roc.dt[order(abs.diff)])
            data.table(
              FpFnInv(true, pred),
              fit=list(list(fit)),
              Cval, k.width)
          }, by=list(fit.name, set)]
        })
        err.dt <- do.call(rbind, err.dt.list)
        save(err.dt, file=cache.RData)
      }
      chosen.dt <- err.dt[set=="validation", {
        .SD[which.min(error)]
      }, by=list(fit.name)]
            
      ggplot()+
        theme_bw()+
        theme(panel.margin=grid::unit(0, "lines"))+
        facet_grid(set ~ fit.name)+
        scale_fill_gradient(low="white", high="black")+
        geom_tile(aes(
          log10(Cval), log2(k.width), fill=error), data=err.dt)+
        geom_point(aes(
          log10(Cval), log2(k.width)), color="red", data=chosen.dt)

      train.validation <- with(seed.sets, list(
        Xi=rbind(train$Xi, validation$Xi),
        Xip=rbind(train$Xip, validation$Xip),
        yi=c(train$yi, validation$yi)))
      
      for(model.i in 1:nrow(chosen.dt)){
        model <- chosen.dt[model.i,]
        fit <- model$fit[[1]]
        pred.vec <- with(test.set, fit$predict(Xi, Xip))
        table(pred.vec, test.set$yi)
        err.list[[paste(N, seed, data.name, model$fit.name)]] <- data.table(
          N, seed, data.name, fit.name=model$fit.name, FpFnInv(test.set$yi, pred.vec))
        if(data.name != "sushi"){
          all.ranks.list[[paste(data.name, N, seed, model$fit.name)]] <-
            data.table(X.grid, rank=as.numeric(fit$rank(X.grid)), what=model$fit.name)
        }
      }

      seed.sets$test <- test.set
      data.list[[paste(N)]][[paste(seed)]][[data.name]] <- seed.sets

      if(data.name != "sushi"){
        all.ranks.list[[paste(data.name, N, seed, "latent")]] <-
          data.table(X.grid, rank=apply(X.grid, 1, true.f), what="latent")
      }
      
    }#data.name
  }#seed
}#N

simulation.sushi.samples <- list(
  rank=do.call(rbind, all.ranks.list), error=do.call(rbind, err.list), data=data.list)

save(simulation.sushi.samples, file="simulation.sushi.samples.RData")

