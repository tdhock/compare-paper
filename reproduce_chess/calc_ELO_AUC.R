library(caTools)
library(PlayerRatings)
library(data.table)
trapz <- function (x, y)
{
    idx = 2:length(x)
    return(as.double((x[idx] - x[idx - 1]) %*% (y[idx] + y[idx -1]))/2)
}

#Change to load datafile you want
load("2_data.RData")

train <- data.frame(Month=1,White=matches$P1,Black=matches$P2,Score=matches$RESULT)
test <- data.frame(Month=1,White=test_matches$P1,Black=test_matches$P2,Score=test_matches$RESULT)

rankdiff_qp <- function(qp, matrix_Xi, matrix_Xip, t) {
  Xirank <- qp$rank(X = matrix_Xi)
  Xiprank <- qp$rank(X = matrix_Xip)
  Xirank <- cbind(Xirank, Xiprank)

  Xirank <- data.frame(Xirank)
  Xirank$diff <- Xirank[, 2] - Xirank[, 1]
  Xirank$label <- NA

  for (i in 1:nrow(Xirank)) {
    diff <- Xirank[i, 2] - Xirank[i, 1]
    if (diff < (-1 * t)) {
      Xirank[i, 4] <- -1
    }
    if (diff > t) {
      Xirank[i, 4] <- 1
    }
    if (abs(diff) <= t) {
      Xirank[i, 4] <- 0
    }
  }
  return(Xirank$diff)
}

rankdiff <- function(rank, rank_p, t) {

  Xirank <- cbind(rank, rank_p)

  Xirank <- data.frame(Xirank)
  Xirank$diff <- Xirank[, 2] - Xirank[, 1]
  Xirank$label <- NA

  for (i in 1:nrow(Xirank)) {
    diff <- Xirank[i, 2] - Xirank[i, 1]
    if (diff < (-1 * t)) {
      Xirank[i, 4] <- -1
    }
    if (diff > t) {
      Xirank[i, 4] <- 1
    }
    if (abs(diff) <= t) {
      Xirank[i, 4] <- 0
    }
  }
  return(Xirank)
}

compute_threshold <- function(rankdiff_vector, labls) {
  rd <- data.table(diff = rankdiff_vector, label = labls)
  rd[, abs.diff := abs(diff)]
  ord <- rd[order(-abs.diff)]
  ord[, FP := cumsum(label == 0)]
  ord[, TP := cumsum((label == -1 &
                        diff <  0) | (label == 1 & diff > 0))]
  ord[, FPR := FP / sum(label == 0)]
  ord[, TPR := TP / sum(label != 0)]
  ord <-
    rbind(data.table(
      diff = 0,
      label = 0,
      abs.diff = 0,
      FP = 0,
      TP = 0,
      FPR = 0,
      TPR = 0
    ),
    ord)
  FINAL_TPR <- ord[nrow(ord),]$TPR
  ord <-
    rbind(ord,data.table(
      diff = 0,
      label = 0,
      abs.diff = 0,
      FP = 0,
      TP = 0,
      FPR = 1,
      TPR = FINAL_TPR
    ))
  return(ord)
}

calc_AUC <- function(rankdiff_vector, labls) {
  ROC <- compute_threshold(rankdiff_vector, labls)
  AUC <- trapz(x = ROC$FPR, y = ROC$TPR)
  return(AUC)
}

get_ROC <- function(rankdiff_vector, labls) {
  ROC <- compute_threshold(rankdiff_vector, labls)
  return(ROC)
}

calc_Baseline <- function(labls) {
  most_common <- 1
  if (length(labls[labls == -1]) > length(labls[labls == 1])) {
    most_common <- -1
  }
  ROC <- data.frame(FPR=c(0,1),TPR=c(0,length(labls[labls==most_common])/length(labls[labls==-1 | labls == 1])))
  AUC <- trapz(x=ROC$FPR,y=ROC$TPR)
  return(AUC)
}


compute_AUC_andELO <- function(train, test) {
  
## Require chess match data format for train and test df colnames(Month, Whote, Black, Score)  
  
  temp <- tempfile()
  download.file(paste(cm, "initial_ratings.csv", sep = ""), temp)
  cSt <- read.csv(temp)
  cSt <-
    data.frame(
      Player = cSt$Player,
      Rating = 1200,
      Deviation = 200,
      Games = 0,
      Win = 0,
      Draw = 0,
      Loss = 0,
      Lag = 0
    )
  
  robjf2 <- fide(train,cSt)
  re <- robjf2$ratings
  cSt <- re[, c(1, 2, 3, 4, 5, 6, 7,8)]
  cSt$Deviation <- 200
  print(head(cSt))
  cSt <- cSt[, c(1, 2, 3, 4, 5, 6, 7,8)]
  print(head(cSt))
  pred_array <- list()  
  array_counter <- 1
  
  for (i in 1:nrow(test)) {
    prf <- predict(robjf2, test[i,], tng = 0)
    robjf2 <- fide(test[i,], cSt)
    re <- robjf2$ratings
    cSt <- re[, c(1, 2, 3, 4, 5, 6, 7,8)]
    cSt$Deviation <- 200
    cSt <- cSt[, c(1, 2, 3, 4, 5, 6, 7,8)]
    
    pred_array[[array_counter]] <- data.frame(pred = prf)
    array_counter <- array_counter + 1
  }
  pred_array <- do.call(rbind, pred_array)    
  pred_array$pred <- (pred_array$pred * 2) - 1
  ##AUC ROC
  test <- cbind(test, pred_array)

  test$Month <- NULL
  test$White <- NULL
  test$Black <- NULL
  test[test$Score == 1,1] <- 1
  test[test$Score == 0,1] <- -1
  test[test$Score == 0.5,1] <- 0
  return(test)
}

elo <- compute_AUC_andELO(train,test)

print(elo)

AUC <- calc_AUC(elo$pred,elo$Score)
ROC <- get_ROC(elo$pred,elo$Score)

#Change to save correct file
save.image("ELO_2.RData")