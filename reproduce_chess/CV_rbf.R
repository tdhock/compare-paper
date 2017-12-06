library(rankSVMcompare)
library(data.table)

args <- commandArgs(trailingOnly = F)

myargument <- args[length(args)]
myargument <- sub("-","",myargument)

dir <- paste(myargument,sep="")

print(dir)

load(dir)

trapz <- function (x, y)
{
  idx = 2:length(x)
  return(as.double((x[idx] - x[idx - 1]) %*% (y[idx] + y[idx - 1])) / 2)
}

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

rankSVM_CV <-
  function(Xi_train,
           Xip_train,
           train_yi,
           parameter.df,
           kern,
           splits_train) {
    
    #Cross validation loop using different kernels
    #The Xi_train,Xip_train and train_yi arguments are ordered by event date,  First element is oldest event, last element is newest
    if(is.character(kern)){
      kern.fun <- get(kern)
    }
    else if(is.function(kern)){
      kern.fun <- kern
    }
    else{
      stop("kern must be either character or function")
    }
    
    if(is.null(parameter.df$C)){
      stop("C must not be null")
    }
    
    store_AUC <- list()
    
    c.col.index <- which(names(parameter.df)=="C")
    
    for (combination.i in 1:nrow(parameter.df)) {
      
      one.combination <- parameter.df[combination.i,]
      one.combination <- data.frame(one.combination)
      
      kern.params <- one.combination[,-c.col.index]
      kern.params <- list(kern.params)
      C.param <- one.combination[,c.col.index]
      svm_kern <- do.call(kern.fun, kern.params)
      
      for (split.i in 1:length(splits_train)) {
        
        rows_train <- 1:(as.integer(nrow(Xi_train) * splits_train[split.i]))
        
        rows_validation <- (as.integer(nrow(Xi_train) * splits_train[split.i]) + 1):as.integer(nrow(Xi_train))
        Xi_train_run <-
          Xi_train[rows_train,]
        Xi_validation_run <-
          Xi_train[rows_validation,]
        
        Xip_train_run <-
          Xip_train[rows_train,]
        Xip_validation_run <-
          Xip_train[rows_validation,]
        train_yi <- data.frame(train_yi)
        train_yi_run <-
          train_yi[rows_train,1]
        validation_yi_run <-
          train_yi[rows_validation,1]
        
        train_pairs <-
          list(
            Xi = Xi_train_run,
            Xip = Xip_train_run,
            yi = train_yi_run
          )
        
        Xi_validation_run[is.na(Xi_validation_run)] <- 0
        Xip_validation_run[is.na(Xip_validation_run)] <- 0
        validation_yi_run[is.na(validation_yi_run)] <- 0
        
        qp <- softCompareQP(train_pairs,svm_kern,C=C.param)
        rankdiff <- rankdiff_qp(qp,Xi_validation_run,Xip_validation_run,1)
        
        AUC <- calc_AUC(rankdiff,validation_yi_run)

       store_AUC[[paste(combination.i, split.i)]] <-
          data.frame(
            TYPE = paste(one.combination, collapse = "_"),
            CV = splits_train[split.i],
            AUC = AUC
          )
        print("Done1")
      }
    }
    
    store_AUC <- do.call(rbind, store_AUC)
    return(store_AUC)
  }

## TESTS calcAUC
test_rankdiff_vector <- c(0.1, 0.2, -0.3, 0.4)
test_labels <- c(1, 0, -1, 0)
AUC_test <- calc_AUC(test_rankdiff_vector, test_labels)

if (AUC_test != 0.25) {
  stop()
}

test_rankdiff_vector <- c(0,1)
test_labels <- c(0,1)
AUC_test <- calc_AUC(test_rankdiff_vector, test_labels)
if (AUC_test != 1) {
  stop()
}

# TESTS Baseline
test_labels <- c(1,0,-1,0,1,1)
AUC_test <- calc_Baseline(test_labels)
if(AUC_test != 0.375){
  stop()
}

matrix_Xi <-
      data.frame(
        x = matches$ELO_1,
        y = matches$BEAT_BETTER_1,
        z = matches$LOST_WORSE_1,
        k = matches$PERCENT_BEAT_BETTER_1,
        h = matches$PERCENT_LOST_WORSE_1,
        v = matches$Games_P1,
        l = matches$Win_P1,
        m = matches$Draw_P1,
        n = matches$Loss_P1,
        o = matches$Lag_P1,
        p = matches$Elite_P1,
        S = matches$MATCH_HISTORY,
        b = matches$AVG_SCORE_DIFF_P1,
        rs = matches$GLIKO_1,
        r = matches$PLAYER_1_MOVE
      )
    matrix_Xi$Percent_Norm <- (matrix_Xi$m + matrix_Xi$l) / matrix_Xi$v
    matrix_Xi <- data.matrix(matrix_Xi)
    colnames(matrix_Xi) <-
      c(
        "ELO",
        "BEAT_BETTER",
        "LOST_WORST",
        "PERCENT_BEAT_BETTER",
        "PERCENT_LOST_WORST",
        "GAMES",
        "WIN",
        "Draw",
        "Loss",
        "Lag",
        "Elite",
        "MATCH_HISTORY",
        "AVG_SCORE_DIFF",
        "GLIKO",
        "PLAYER_MOVE",
        "PERCENT_NORM"
      )
    

matrix_Xip <-
      data.frame(
        x = matches$ELO_2,
        y = matches$BEAT_BETTER_2,
        z = matches$LOST_WORSE_2,
        k = matches$PERCENT_BEAT_BETTER_2,
        h = matches$PERCENT_LOST_WORSE_2,
        v = matches$Games_P2,
        l = matches$Win_P2,
        m = matches$Draw_P2,
        n = matches$Loss_P2,
        o = matches$Lag_P2,
        p = matches$Elite_P2,
        S = matches$MATCH_HISTORY,
        w = matches$AVG_SCORE_DIFF_P2,
        rs = matches$GLIKO_2,
        f = matches$PLAYER_2_MOVE
      )
    matrix_Xip$Percent_Norm <- (matrix_Xip$m + matrix_Xip$l) / matrix_Xip$v
    matrix_Xip <- data.matrix(matrix_Xip)
    colnames(matrix_Xip) <-
      c(
        "ELO",
        "BEAT_BETTER",
        "LOST_WORST",
        "PERCENT_BEAT_BETTER",
        "PERCENT_LOST_WORST",
        "GAMES",
        "WIN",
        "Draw",
        "Loss",
        "Lag",
        "Elite",
        "MATCH_HISTORY",
        "AVG_SCORE_DIFF",
        "GLIKO",
        "PLAYER_MOVE",
        "PERCENT_NORM"
      )
    
    yi = data.frame(matches$RESULT)
    yi[yi == 1] <- -1
    yi[yi == 0] <- 1
    yi[yi == 0.5] <- 0

    yi[is.na(yi)] <- 0
    ####
    matrix_Xi[is.na(matrix_Xi)] <- 0
    matrix_Xip[is.na(matrix_Xip)] <- 0


test_matches_Xi <-
      data.frame(
        x = test_matches$ELO_1,
        y = test_matches$BEAT_BETTER_1,
        z = test_matches$LOST_WORSE_1,
        k = test_matches$PERCENT_BEAT_BETTER_1,
        h = test_matches$PERCENT_LOST_WORSE_1,
        v = test_matches$Games_P1,
        l = test_matches$Win_P1,
        m = test_matches$Draw_P1,
        n = test_matches$Loss_P1,
        o = test_matches$Lag_P1,
        p = test_matches$Elite_P1,
        S = test_matches$MATCH_HISTORY,
        w = test_matches$AVG_SCORE_DIFF_P1,
        dd = test_matches$GLIKO_1,
        j = test_matches$PLAYER_1_MOVE
      )
    test_matches_Xi$Percent_Norm <- (test_matches_Xi$m + test_matches_Xi$l) / test_matches_Xi$v
    test_matches_Xi <- data.matrix(test_matches_Xi)
    colnames(test_matches_Xi) <-
      c(
        "ELO",
        "BEAT_BETTER",
        "LOST_WORST",
        "PERCENT_BEAT_BETTER",
        "PERCENT_LOST_WORST",
        "GAMES",
        "WIN",
        "Draw",
        "Loss",
        "Lag",
        "Elite",
        "MATCH_HISTORY",
        "AVG_SCORE_DIFF",
        "GLIKO",
	"PLAYER_MOVE",
        "PERCENT_NORM"
      )
    
    test_matches_Xip <-
      data.frame(
        x = test_matches$ELO_2,
        y = test_matches$BEAT_BETTER_2,
        z = test_matches$LOST_WORSE_2,
        k = test_matches$PERCENT_BEAT_BETTER_2,
        h = test_matches$PERCENT_LOST_WORSE_2,
        v = test_matches$Games_P2,
        l = test_matches$Win_P2,
        m = test_matches$Draw_P2,
        n = test_matches$Loss_P2,
        o = test_matches$Lag_P2,
        p = test_matches$Elite_P2,
        S = test_matches$MATCH_HISTORY,
        b = test_matches$AVG_SCORE_DIFF_P2,
        dd = test_matches$GLIKO_2,
        g = test_matches$PLAYER_2_MOVE
      )
    test_matches_Xip$Percent_Norm <- (test_matches_Xip$m + test_matches_Xip$l) / test_matches_Xip$v
    
    test_matches_Xip <- data.matrix(test_matches_Xip)
    colnames(test_matches_Xip) <-
      c(
        "ELO",
        "BEAT_BETTER",
        "LOST_WORST",
        "PERCENT_BEAT_BETTER",
        "PERCENT_LOST_WORST",
        "GAMES",
        "WIN",
        "Draw",
        "Loss",
        "Lag",
        "Elite",
        "MATCH_HISTORY",
        "AVG_SCORE_DIFF",
        "GLIKO",
	"PLAYER_MOVE",
        "PERCENT_NORM"
      )
    
    yi_test = data.frame(test_matches$RESULT)
    yi_test[yi_test == 1] <- -1
    yi_test[yi_test == 0] <- 1
    yi_test[yi_test == 0.5] <- 0
    
    yi_test[is.na(yi_test)] <- 0
    
    ####
    test_matches_Xi[is.na(test_matches_Xi)] <- 0
    test_matches_Xip[is.na(test_matches_Xip)] <- 0
    matrix_Xi[is.na(matrix_Xi)] <- 0
    matrix_Xip[is.na(matrix_Xip)] <- 0
    yi$matches.RESULT[is.na(yi$matches.RESULT)] <- 0
    
    #For removal of all but ELO + GLIKO
    
    #test_matches_Xi <- test_matches_Xi[,c(1,14)]
    #test_matches_Xip <- test_matches_Xip[,c(1,14)]
    #matrix_Xi <- matrix_Xi[,c(1,14)]
    #matrix_Xip <- matrix_Xip[,c(1,14)]

parameter_df <- expand.grid(C=10^seq(-20, 5, l=26),sigma=c(0.1,1,10))

AUC_polydot <- rankSVM_CV(Xi_train = matrix_Xi,Xip_train = matrix_Xip,train_yi = yi$matches.RESULT,parameter.df = parameter_df,kern = rbfdot,splits_train = c(0.5,0.75,0.8,0.85))


##Calculate Optimal Parameters for vanilladot

AUC_CV <- AUC_polydot

AUC_polydot <- aggregate(AUC_polydot,list(AUC_polydot$TYPE),FUN = mean)
AUC_polydot$TYPE <- NULL
AUC_polydot$CV <- NULL
AUC_polydot <- AUC_polydot[order(AUC_polydot$AUC,decreasing = TRUE),]
kern.params <- AUC_polydot$Group.1[1]
kern.params <- toString(kern.params)
kern.params <- strsplit(kern.params,"_")
kern.params <- unlist(kern.params)

pairs <- list(Xi=matrix_Xi,Xip=matrix_Xip,yi=yi$matches.RESULT)

qp <- softCompareQP(pairs,kernel = rbfdot,C=kern.params[1],sigma=kern.params[2])

rankdiff_test <- rankdiff_qp(qp,test_matches_Xi,test_matches_Xip,1)

AUC <- calc_AUC(rankdiff_test,yi_test$test_matches.RESULT)

FINAL_AUC <- list(AUC=AUC,ROC=get_ROC(rankdiff_test,yi_test$test_matches.RESULT))

BASELINE_AUC <- calc_Baseline(yi_test$test_matches.RESULT)

out_dir <- paste(myargument,"_rbf_AUC_aftrs",sep="")

save.image(out_dir)