works_with_R("3.0.2", plyr="1.8", rankSVMcompare="2013.10.25")

classify <- function(x, thresh=1){
  stopifnot(is.numeric(thresh))
  stopifnot(is.finite(thresh))
  stopifnot(thresh >= 0)
  stopifnot(length(thresh) == 1)
  ifelse(x > thresh, 1L,
         ifelse(x < -thresh, -1L, 0L))
}

calc.roc <- function(rank.mat, yi){
  stopifnot(is.matrix(rank.mat))
  stopifnot(ncol(rank.mat)==2)
  stopifnot(is.numeric(rank.mat))
  stopifnot(nrow(rank.mat) == length(yi))
  stopifnot(yi %in% c(-1,0,1))
  rank.diff <- rank.mat[,2]-rank.mat[,1]
  thresh.vec <- c(0, sort(abs(rank.diff)))
  this.roc <- data.frame()
  for(thresh in thresh.vec){
    yhat <- classify(rank.diff, thresh)
    err <- FpFnInv(yi, yhat)
    this.roc <- rbind(this.roc, data.frame(thresh, err))
  }
  rates <- within(this.roc, {
    FPR <- false.positive/equality
    FNR <- false.negative/inequality
    TPR <- 1-FNR
  })
  boxes <- ddply(rates, .(TPR), summarize, min=min(FPR), max=max(FPR))
  this.auc <- data.frame(auc=sum(with(boxes, (max-min)*TPR)))
  list(roc=rates, auc=this.auc, boxes=boxes)
}
