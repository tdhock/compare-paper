source("packages.R")

load("sushi.pairs.RData")

plan(multiprocess)

source("svmlight.R")

funs <- list(l2=function(x)sum(x*x),
             l1=function(x)sum(abs(x))^2,
             linf=function(x)max(abs(x))^2)
deltas <- list(l2=function()runif(2,-1,1/2),
               l1=function()runif(2,-1/2,1/2),
               linf=function()runif(2,-1,1))
set.seed(1)
total.sim <- 25000
pair.sets <- list(sushi=sushi.pairs)
for(norm in names(funs)){
  delta.fun <- deltas[[norm]]
  f <- funs[[norm]]
  Xi <- matrix(runif(2*total.sim, -2, 2), total.sim, 2)
  Xip <- Xi + rnorm(2*total.sim, sd=1/2)
  fxdiff <- apply(Xip, 1, f)-apply(Xi, 1, f)
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

all.ranks.list <- list()
err.list <- list()
data.list <- list()
test.N <- 10000
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
  for(N in c(50, 100, 200, 400, 800)){
    for(seed in 1:4){
      N.set.vec <- set.vec
      set.seed(seed)
      seed.sets <- list()
      for(set.name in c("train", "validation")){
        N.set.vec[sample(which(is.na(N.set.vec) & is.zero), N/2)] <- set.name
        N.set.vec[sample(which(is.na(N.set.vec) & !is.zero), N/2)] <- set.name
        is.set <- N.set.vec==set.name & !is.na(N.set.vec)
        seed.sets[[set.name]] <- with(Pairs, list(
          Xi=Xi[is.set,], Xip=Xip[is.set,], yi=yi[is.set]))
      }
      exp.N <- c(test.N, N, N)/2
      stopifnot(sum(table(N.set.vec, is.zero) == cbind(exp.N, exp.N))==6)

      ## fit SVM.
      cat(sprintf("N=%4d seed=%4d data.name=%s\n", N, seed, data.name))
      err.df.list <- list()
      Cvals <- 10^seq(-1,5,l=10)
      models <- list()
      kvals <- 2^seq(-8, 4, l=10)
      model.df <- expand.grid(C=Cvals, k.width=kvals)
      err.dt.list <- future_lapply(1:nrow(model.df), function(model.i){
        model <- model.df[model.i,]
        ##cat(sprintf("%4d / %4d models\n", model.i, nrow(model.df)))
        Cval <- model$C
        k.width <- model$k.width
        ker <- rbfdot(k.width)
        fits <- list(
          compare=softCompareQP(seed.sets$train, ker, C=Cval),
          rank2=svmlight(seed.sets$train, Cval, k.width, equality="bothpairs"),
          rank=svmlight(seed.sets$train, Cval, k.width))
        models[[model.i]] <- fits
        data.table(expand.grid(fit.name=names(fits), set=c("train","validation")))[, {
          fit <- fits[[fit.name]]
          s <- seed.sets[[set]]
          pred <- fit$predict(s$Xi, s$Xip)
          true <- s$yi
          data.table(
            FpFnInv(true, pred),
            Cval, k.width)
          }, by=list(fit.name, set)]
      })
      err.dt <- do.call(rbind, err.dt.list)
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
        fit <- if(model$fit.name=="compare"){
          ker <- rbfdot(model$k.width)
          softCompareQP(seed.sets$train, ker, C=model$Cval)
        }else{
          svmlight(seed.sets$train, model$Cval, model$k.width)
        }
        pred.vec <- with(test.set, fit$predict(Xi, Xip))
        table(pred.vec, test.set$yi)
        err.list[[paste(N, seed, data.name, model$fit.name)]] <- data.table(
          N, seed, data.name, fit.name=model$fit.name, FpFnInv(test.set$yi, pred.vec))
        all.ranks.list[[paste(data.name, N, seed, model$fit.name)]] <-
          data.table(X.grid, rank=as.numeric(fit$rank(X.grid)), what=model$fit.name)
      }

      true.f(

      seed.sets$test <- test.set
      data.list[[paste(N)]][[paste(seed)]][[data.name]] <- seed.sets
      all.ranks.list[[paste(data.name, N, seed, "latent")]] <-
        data.table(X.grid, rank=apply(X.grid, 1, true.f), what="latent")
      
    }#data.name
  }#seed
}#N

simulation.samples <- list(
  rank=do.call(rbind, all.ranks.list), error=do.call(rbind, err.list), data=data.list)

save(simulation.samples, file="simulation.samples.RData")

