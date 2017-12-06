library(rankSVMcompare)
library(PlayerRatings)
library(plyr)
cm <- "http://www.chessmetrics.com/KaggleComp/"
temp <- tempfile()
download.file(paste(cm, "primary_training_part1.zip", sep = ""), temp)
chess <- read.csv(unz(temp, "primary_training_part1.csv"))[, 2:5]
download.file(paste(cm, "primary_training_part2.zip", sep = ""), temp)
chess <-
  rbind(chess, read.csv(unz(temp, "primary_training_part2.csv"))[, 2:5])
download.file(paste(cm, "primary_training_part3.zip", sep = ""), temp)
chess <-
  rbind(chess, read.csv(unz(temp, "primary_training_part3.csv"))[, 2:5])
names(chess) <- c("Month", "White", "Black", "Score")
unlink(temp)


#
# Modify Date Ranges for your experiments
train <- chess[chess$Month > 48.5 & chess$Month < 51.5,]

trainM <- train$Month
test <- chess[chess$Month > 51.5 & chess$Month < 60.5,]
testS <- test$Score


select_players <- count(train, .(White))
junk <- count(train, .(Black))
colnames(junk)[1] <- "White"
select_players <-
  merge(junk, select_players, all = TRUE, by = "White")
select_players[is.na(select_players)] <- 0
select_players$gp <- select_players$freq.x + select_players$freq.y
select_players <- select_players[select_players$gp >= 10,]
select_players$freq.x <- NULL
select_players$freq.y <- NULL

for (i in 1:nrow(train)) {
  if (!(train[i, 2] %in% select_players$White) |
      !(train[i, 3] %in% select_players$White)) {
    train[i, 1] <- 0
    
  }
}

train <- train[train$Month != 0,]

for (i in 1:nrow(test)) {
  if (!(test[i, 2] %in% select_players$White) |
      !(test[i, 3] %in% select_players$White)) {
    test[i, 1] <- 0
  }
}

test <- test[test$Month != 0,]


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

download.file(paste(cm, "initial_ratings.csv", sep = ""), temp)
cSt_2 <- read.csv(temp)
cSt_2 <-
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

unlink(temp)

for (i in 1:nrow(train)) {
  if (!(train[i, 2] %in% cSt$Player)) {
    train[i, 1] <- 0
    
  }
}


for (i in 1:nrow(train)) {
  if (!(train[i, 3] %in% cSt$Player)) {
    train[i, 1] <- 0
  }
}

train <- train[train$Month != 0,]

for (i in 1:nrow(test)) {
  if (!(test[i, 2] %in% cSt$Player)) {
    test[i, 1] <- 0
    
  }
}


for (i in 1:nrow(test)) {
  if (!(test[i, 3] %in% cSt$Player)) {
    test[i, 1] <- 0
  }
}

test <- test[test$Month != 0,]


temp <- tempfile()
download.file(paste(cm, "players.csv", sep = ""), temp)
chessPlayers <- read.csv(temp, as.is = TRUE)
names(chessPlayers) <- c("Player", "Name")
unlink(temp)

#ELO

library(PlayerRatings)

matches = data.frame(
  P1 = 0,
  P2 = 0,
  ELO_1 = 0,
  ELO_2 = 0,
  BEAT_BETTER_1 = 0,
  BEAT_BETTER_2 = 0,
  LOST_WORSE_1 = 0,
  LOST_WORSE_2 = 0,
  PERCENT_BEAT_BETTER_1 = 0,
  PERCENT_BEAT_BETTER_2 = 0,
  PERCENT_LOST_WORSE_1 = 0,
  PERCENT_LOST_WORSE_2 = 0,
  Games_P1 = 0,
  Win_P1 = 0,
  Draw_P1 = 0,
  Loss_P1 = 0,
  Lag_P1 = 0,
  Elite_P1 = 0,
  Games_P2 = 0,
  Win_P2 = 0,
  Draw_P2 = 0,
  Loss_P2 = 0,
  Lag_P2 = 0,
  Elite_P2 = 0,
  MATCH_HISTORY = 0,
  AVG_SCORE_DIFF_P1 = 0,
  AVG_SCORE_DIFF_P2 = 0,
  PLAYER_1_MOVE = 0,
  PLAYER_2_MOVE = 0,
  ELO_DIFF_WIN_P1 =0,
  ELO_DIFF_WIN_P2 =0,
  ELO_DIFF_LOSS_P1 =0,
  ELO_DIFF_LOSS_P2=0,
  GLIKO_1=0,
  GLIKO_2=0,
  RESULT = 0
)
temp <-
  data.frame(
    P1 = 0,
    P2 = 0,
    ELO_1 = 0,
    ELO_2 = 0,
    BEAT_BETTER_1 = 0,
    BEAT_BETTER_2 = 0,
    LOST_WORSE_1 = 0,
    LOST_WORSE_2 = 0,
    PERCENT_BEAT_BETTER_1 = 0,
    PERCENT_BEAT_BETTER_2 = 0,
    PERCENT_LOST_WORSE_1 = 0,
    PERCENT_LOST_WORSE_2 = 0,
    Games_P1 = 0,
    Win_P1 = 0,
    Draw_P1 = 0,
    Loss_P1 = 0,
    Lag_P1 = 0,
    Elite_P1 = 0,
    Games_P2 = 0,
    Win_P2 = 0,
    Draw_P2 = 0,
    Loss_P2 = 0,
    Lag_P2 = 0,
    Elite_P2 = 0,
    MATCH_HISTORY = 0,
    AVG_SCORE_DIFF_P1 = 0,
    AVG_SCORE_DIFF_P2 = 0,
    PLAYER_1_MOVE = 0,
    PLAYER_2_MOVE = 0,
    ELO_DIFF_WIN_P1 =0,
    ELO_DIFF_WIN_P2 =0,
    ELO_DIFF_LOSS_P1 =0,
    ELO_DIFF_LOSS_P2=0,
    GLIKO_1=0,
    GLIKO_2=0,
    RESULT =0
  )
BEAT_BETTER <- data.frame(PLAYER = chessPlayers$Player,
                          N = 0,
                          NP = 0)
LOST_WORSE <- data.frame(PLAYER = chessPlayers$Player,
                         N = 0,
                         NP = 0)
GAMES_PLAYED <- data.frame(PLAYER = chessPlayers$Player,
                           N = 0,
                           NP = 0)
score_diff <- data.frame(P1 = 0, ELO_DIFF = 0)

ELO_DIFF_WIN <- data.frame(P=0,ELO_DIFF=(0))
ELO_DIFF_LOSS <- data.frame(P=0,ELO_DIFF=(0))


it <- 0
match_history <- data.frame(P1 = 0, P2 = 0, OUTCOME = 0)

for (i in 1:nrow(train)) {
  temp$P1 <- train[i, 2]
  P1 <- train[i, 2]
  temp$P2 <- train[i, 3]
  P2 <- train[i, 3]
  ELO_1 <- cSt$Rating[cSt$Player == temp$P1[1]]
  ELO_2 <- cSt$Rating[cSt$Player == temp$P2[1]]
  
  GLIKO_1 <- cSt_2$Rating[cSt_2$Player == temp$P1[1]]
  GLIKO_2 <- cSt_2$Rating[cSt_2$Player == temp$P2[1]]
  
  if (it == 0) {
    robjf1 <- fide(train[i,])
    re <- robjf1$ratings
  }
  
  if (it == 0) {
    robjf1_g <- glicko(train[i,])
    re_2 <- robjf1_g$ratings
  }
  
  temp$ELO_1 <- ELO_1
  temp$ELO_2 <- ELO_2
  temp$GLIKO_1 <- GLIKO_1
  temp$GLIKO_2 <- GLIKO_2
  temp$RESULT <- train[i, 4]
  
  GAMES_PLAYED[GAMES_PLAYED$PLAYER == P1, 2] <-
    GAMES_PLAYED[GAMES_PLAYED$PLAYER == P1, 2] + 1
  GAMES_PLAYED[GAMES_PLAYED$PLAYER == P2, 2] <-
    GAMES_PLAYED[GAMES_PLAYED$PLAYER == P2, 2] + 1
  
  temp$BEAT_BETTER_1 <-
    BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 2] #/ GAMES_PLAYED[GAMES_PLAYED$PLAYER == P1,2]
  temp$BEAT_BETTER_2 <-
    BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 2] #/ GAMES_PLAYED[GAMES_PLAYED$PLAYER == P2,2]
  temp$LOST_WORSE_1 <-
    LOST_WORSE[LOST_WORSE$PLAYER == P1, 2] #/ GAMES_PLAYED[GAMES_PLAYED$PLAYER == P1,2]
  temp$LOST_WORSE_2 <-
    LOST_WORSE[LOST_WORSE$PLAYER == P2, 2] #/ GAMES_PLAYED[GAMES_PLAYED$PLAYER == P2,2]
  
  temp$PERCENT_BEAT_BETTER_1 <-
    BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 2] / BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 3]
  temp$PERCENT_BEAT_BETTER_2 <-
    BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 2] / BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 3]
  temp$PERCENT_LOST_WORSE_1 <-
    LOST_WORSE[LOST_WORSE$PLAYER == P1, 2] / LOST_WORSE[LOST_WORSE$PLAYER == P1, 3]
  temp$PERCENT_LOST_WORSE_2 <-
    LOST_WORSE[LOST_WORSE$PLAYER == P2, 2] / LOST_WORSE[LOST_WORSE$PLAYER == P2, 3]
  
  ##FIND MUTLIPLIERS
  temp$ELO_DIFF_WIN_P1 <- mean(ELO_DIFF_WIN[ELO_DIFF_WIN$P == P1,2])  
  temp$ELO_DIFF_WIN_P2 <- mean(ELO_DIFF_WIN[ELO_DIFF_WIN$P == P2,2])
  temp$ELO_DIFF_LOSS_P1 <- mean(ELO_DIFF_WIN[ELO_DIFF_LOSS$P == P1,2])
  temp$ELO_DIFF_LOSS_P2 <- mean(ELO_DIFF_WIN[ELO_DIFF_LOSS$P == P2,2])
  
  RESULT <- train[i, 4]
  if ((ELO_1 < ELO_2) & (RESULT == 1 | RESULT == 0.5)) {
    BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 2] <-
      BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 2] + 1
    LOST_WORSE[LOST_WORSE$PLAYER == P2, 2] <-
      LOST_WORSE[LOST_WORSE$PLAYER == P2, 2] + 1
    
  }
  if ((ELO_1 > ELO_2) & (RESULT == 0 | RESULT == 0.5)) {
    BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 2] <-
      BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 2] + 1
    LOST_WORSE[LOST_WORSE$PLAYER == P1, 2] <-
      LOST_WORSE[LOST_WORSE$PLAYER == P1, 2] + 1
    
  }
  
  if((RESULT == 0 | RESULT == 0.5)){
    ELO_DIFF_WIN <- rbind(ELO_DIFF_WIN,data.frame(P=P2,ELO_DIFF=(ELO_1 - ELO_2)))
    ELO_DIFF_LOSS <- rbind(ELO_DIFF_LOSS,data.frame(P=P1,ELO_DIFF=(ELO_2 - ELO_1)))
  }
  if (RESULT == 1 | RESULT == 0.5){
    ELO_DIFF_WIN <- rbind(ELO_DIFF_WIN,data.frame(P=P1,ELO_DIFF=(ELO_2 - ELO_1)))
    ELO_DIFF_LOSS <- rbind(ELO_DIFF_LOSS,data.frame(P=P2,ELO_DIFF=(ELO_1 - ELO_2)))
  }
  if ((ELO_1 < ELO_2)) {
    BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 3] <-
      BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 3] + 1
    
    LOST_WORSE[LOST_WORSE$PLAYER == P2, 3] <-
      LOST_WORSE[LOST_WORSE$PLAYER == P2, 3] + 1
    
  }
  
  if ((ELO_1 > ELO_2)) {
    BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 3] <-
      BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 3] + 1
    
    LOST_WORSE[LOST_WORSE$PLAYER == P1, 3] <-
      LOST_WORSE[LOST_WORSE$PLAYER == P1, 3] + 1
    
  }
  
  temp[1, 13:18] <- re[re$Player == P1, c(3, 4, 5, 6, 7, 8)]
  temp[1, 19:24] <- re[re$Player == P2, c(3, 4, 5, 6, 7, 8)]
  
  temp$RESULT <- train[i, 4]
  
  
  #UPDATE MATCH HISTORY
  if (train[i, 2] > train[i, 3]) {
    P1_save <- train[i, 2]
    P2_save <- train[i, 3]
  }
  if (train[i, 2] < train[i, 3]) {
    P1_save <- train[i, 3]
    P2_save <- train[i, 2]
  }
  
  P1_save <- cbind(P1_save, P2_save)
  colnames(P1_save) <- c("P1", "P2")
  find_matches <- merge(match_history, P1_save, by = c("P1", "P2"))
  
  temp$MATCH_HISTORY <- mean(find_matches$OUTCOME)
  
  temp$AVG_SCORE_DIFF_P1 <- mean(score_diff[score_diff$P1 == P1, 2])
  temp$AVG_SCORE_DIFF_P2 <- mean(score_diff[score_diff$P1 == P2, 2])
  
  score_diff <-
    rbind(score_diff, data.frame(P1 = P1, ELO_DIFF = ELO_2 - ELO_1))
  score_diff <-
    rbind(score_diff, data.frame(P1 = P2, ELO_DIFF = ELO_1 - ELO_2))
  
  
  temp$PLAYER_1_MOVE <- 1
  temp$PLAYER_2_MOVE <- 0
  
  it <- 1
  if (it == 1) {
    robjf1 <- fide(train[i, ], status = cSt)
  }
  
  if (it == 1) {
    robjf1_g <- glicko(train[i, ], status = cSt_2)
  }
  
  re <- robjf1$ratings
  
  cSt <- re[, c()]
  cSt <- re[, c(1, 2, 3, 4, 5, 6, 7)]
  cSt$Deviation <- 200
  cSt <- cSt[, c(1, 2, 8, 3, 4, 5, 6, 7)]
  
  re_2 <- robjf1_g$ratings
  
  cSt_2 <- re_2[]
  cSt_2$Deviation <- 200
  
  temp[is.na(temp)] <- 0.5
  
  matches <- rbind(matches, temp)
  
  #KEEP TRACK OF PREVIOUS MATCHES
  if (train[i, 2] > train[i, 3]) {
    prev_match <-
      data.frame(P1 = train[i, 2],
                 P2 = train[i, 3],
                 OUTCOME = train[i, 4])
  }
  if (train[i, 2] < train[i, 3]) {
    prev_match <-
      data.frame(P1 = train[i, 3],
                 P2 = train[i, 2],
                 OUTCOME = test[i, 4])
  }
  
  match_history <- rbind(match_history, prev_match)
  
}

matches <- matches[-1,]

####TEST MATCHES

#cSt <-
#  data.frame(
#    Player = cSt$Player,
#    Rating = 1200,
#    Deviation = 200,
#    Games = 0
#  )

test_matches <- matches
temp <-
  data.frame(
    P1 = 0,
    P2 = 0,
    ELO_1 = 0,
    ELO_2 = 0,
    BEAT_BETTER_1 = 0,
    BEAT_BETTER_2 = 0,
    LOST_WORSE_1 = 0,
    LOST_WORSE_2 = 0,
    PERCENT_BEAT_BETTER_1 = 0,
    PERCENT_BEAT_BETTER_2 = 0,
    PERCENT_LOST_WORSE_1 = 0,
    PERCENT_LOST_WORSE_2 = 0,
    Games_P1 = 0,
    Win_P1 = 0,
    Draw_P1 = 0,
    Loss_P1 = 0,
    Lag_P1 = 0,
    Elite_P1 = 0,
    Games_P2 = 0,
    Win_P2 = 0,
    Draw_P2 = 0,
    Loss_P2 = 0,
    Lag_P2 = 0,
    Elite_P2 = 0,
    MATCH_HISTORY = 0,
    AVG_SCORE_DIFF_P1 = 0,
    AVG_SCORE_DIFF_P2 = 0,
    PLAYER_1_MOVE = 0,
    PLAYER_2_MOVE = 0,
    ELO_DIFF_WIN_P1 =0,
    ELO_DIFF_WIN_P2 =0,
    ELO_DIFF_LOSS_P1 =0,
    ELO_DIFF_LOSS_P2=0,
    GLIKO_1=0,
    GLIKO_2=0,
    RESULT = 0
  )

it <- 0

for (i in 1:nrow(test)) {
  temp$P1[1] <- test[i, 2]
  P1 <- test[i, 2]
  
  temp$P2[1] <- test[i, 3]
  P2 <- test[i, 3]
  
  ELO_1 <- cSt$Rating[cSt$Player == temp$P1[1]]
  ELO_2 <- cSt$Rating[cSt$Player == temp$P2[1]]
  temp$ELO_1 <- ELO_1
  temp$ELO_2 <- ELO_2
  GLIKO_1 <- cSt_2$Rating[cSt_2$Player == temp$P1[1]]
  GLIKO_2 <- cSt_2$Rating[cSt_2$Player == temp$P2[1]]
  temp$GLIKO_1 <- GLIKO_1
  temp$GLIKO_2 <- GLIKO_2
  temp$RESULT <- test[i, 4]
  
  
  if (it == 0) {
    robjf1 <- fide(rbind(train, test[i,]), status = cSt)
    re <- robjf1$ratings
  }
  
  if (it == 0) {
    robjf1_g <- glicko(rbind(train, test[i,]), status = cSt_2)
    re_2 <- robjf1_g$ratings
  }
  
  temp$BEAT_BETTER_1 <-
    BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 2] #/ GAMES_PLAYED[GAMES_PLAYED$PLAYER == P1,2]
  temp$BEAT_BETTER_2 <-
    BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 2] #/ GAMES_PLAYED[GAMES_PLAYED$PLAYER == P2,2]
  temp$LOST_WORSE_1 <-
    LOST_WORSE[LOST_WORSE$PLAYER == P1, 2] #/ GAMES_PLAYED[GAMES_PLAYED$PLAYER == P1,2]
  temp$LOST_WORSE_2 <-
    LOST_WORSE[LOST_WORSE$PLAYER == P2, 2] #/ GAMES_PLAYED[GAMES_PLAYED$PLAYER == P2,2]
  
  temp$PERCENT_BEAT_BETTER_1 <-
    BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 2] / BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 3]
  temp$PERCENT_BEAT_BETTER_2 <-
    BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 2] / BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 3]
  temp$PERCENT_LOST_WORSE_1 <-
    LOST_WORSE[LOST_WORSE$PLAYER == P1, 2] / LOST_WORSE[LOST_WORSE$PLAYER == P1, 3]
  temp$PERCENT_LOST_WORSE_2 <-
    LOST_WORSE[LOST_WORSE$PLAYER == P2, 2] / LOST_WORSE[LOST_WORSE$PLAYER == P2, 3]
  
  ##FIND MUTLIPLIERS
  temp$ELO_DIFF_WIN_P1 <- mean(ELO_DIFF_WIN[ELO_DIFF_WIN$P == P1,2])  
  temp$ELO_DIFF_WIN_P2 <- mean(ELO_DIFF_WIN[ELO_DIFF_WIN$P == P2,2])
  temp$ELO_DIFF_LOSS_P1 <- mean(ELO_DIFF_WIN[ELO_DIFF_LOSS$P == P1,2])
  temp$ELO_DIFF_LOSS_P2 <- mean(ELO_DIFF_WIN[ELO_DIFF_LOSS$P == P2,2])
  
  GAMES_PLAYED[GAMES_PLAYED$PLAYER == P1, 2] <-
    GAMES_PLAYED[GAMES_PLAYED$PLAYER == P1, 2] + 1
  GAMES_PLAYED[GAMES_PLAYED$PLAYER == P2, 2] <-
    GAMES_PLAYED[GAMES_PLAYED$PLAYER == P2, 2] + 1
  
  if ((ELO_1 < ELO_2) & (RESULT == 1 | RESULT == 0.5)) {
    BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 2] <-
      BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 2] + 1
    
    LOST_WORSE[LOST_WORSE$PLAYER == P2, 2] <-
      LOST_WORSE[LOST_WORSE$PLAYER == P2, 2] + 1
    
  }
  if ((ELO_1 > ELO_2) & (RESULT == 0 | RESULT == 0.5)) {
    BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 2] <-
      BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 2] + 1
    LOST_WORSE[LOST_WORSE$PLAYER == P1, 2] <-
      LOST_WORSE[LOST_WORSE$PLAYER == P1, 2] + 1
    
  }
  
  if((RESULT == 0 | RESULT == 0.5)){
    ELO_DIFF_WIN <- rbind(ELO_DIFF_WIN,data.frame(P=P2,ELO_DIFF=(ELO_1 - ELO_2)))
    ELO_DIFF_LOSS <- rbind(ELO_DIFF_LOSS,data.frame(P=P1,ELO_DIFF=(ELO_2 - ELO_1)))
  }
  if (RESULT == 1 | RESULT == 0.5){
    ELO_DIFF_WIN <- rbind(ELO_DIFF_WIN,data.frame(P=P1,ELO_DIFF=(ELO_2 - ELO_1)))
    ELO_DIFF_LOSS <- rbind(ELO_DIFF_LOSS,data.frame(P=P2,ELO_DIFF=(ELO_1 - ELO_2)))
  }
  
  if ((ELO_1 < ELO_2)) {
    BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 3] <-
      BEAT_BETTER[BEAT_BETTER$PLAYER == P1, 3] + 1
    
    LOST_WORSE[LOST_WORSE$PLAYER == P2, 3] <-
      LOST_WORSE[LOST_WORSE$PLAYER == P2, 3] + 1
    
  }
  
  if ((ELO_1 > ELO_2)) {
    BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 3] <-
      BEAT_BETTER[BEAT_BETTER$PLAYER == P2, 3] + 1
    
    LOST_WORSE[LOST_WORSE$PLAYER == P1, 3] <-
      LOST_WORSE[LOST_WORSE$PLAYER == P1, 3] + 1
    
  }
  
  temp[1, 13:18] <- re[re$Player == P1, c(3, 4, 5, 6, 7, 8)]
  temp[1, 19:24] <- re[re$Player == P2, c(3, 4, 5, 6, 7, 8)]
  
  
  #UPDATE MATCH HISTORY
  if (test[i, 2] > test[i, 3]) {
    P1_save <- test[i, 2]
    P2_save <- test[i, 3]
  }
  
  if (test[i, 2] < test[i, 3]) {
    P1_save <- test[i, 3]
    P2_save <- test[i, 2]
  }
  
  P1_save <- cbind(P1_save, P2_save)
  colnames(P1_save) <- c("P1", "P2")
  find_matches <- merge(match_history, P1_save, by = c("P1", "P2"))
  
  temp$MATCH_HISTORY <- mean(find_matches$OUTCOME)
  
  temp$AVG_SCORE_DIFF_P1 <- mean(score_diff[score_diff$P1 == P1, 2])
  temp$AVG_SCORE_DIFF_P2 <- mean(score_diff[score_diff$P1 == P2, 2])
  
  score_diff <-
    rbind(score_diff, data.frame(P1 = P1, ELO_DIFF = ELO_2 - ELO_1))
  score_diff <-
    rbind(score_diff, data.frame(P1 = P2, ELO_DIFF = ELO_1 - ELO_2))
  
  
  temp$PLAYER_1_MOVE <- 1
  temp$PLAYER_2_MOVE <- 0
  
  temp$RESULT <- test[i, 4]
  
  it <- 1
  if (it == 1) {
    robjf1 <- fide(rbind(test[i,]), status = cSt)
  }
  
  if (it == 1) {
    robjf1_g <- glicko(test[i, ], status = cSt_2)
  }
  
  temp[is.na(temp)] <- 0.5
  
  re <- robjf1$ratings
  
  cSt <- re[, c()]
  cSt <- re[, c(1, 2, 3, 4, 5, 6, 7)]
  cSt$Deviation <- 200
  cSt <- cSt[, c(1, 2, 8, 3, 4, 5, 6, 7)]
  
  re_2 <- robjf1_g$ratings
  
  cSt_2 <- re_2
  cSt_2$Deviation <- 200

  test_matches <- rbind(test_matches, temp)
  
  #KEEP TRACK OF PREVIOUS MATCHES
  if (test[i, 2] > test[i, 3]) {
    prev_match <-
      data.frame(P1 = test[i, 2],
                 P2 = test[i, 3],
                 OUTCOME = test[i, 4])
  }
  if (test[i, 2] < test[i, 3]) {
    prev_match <-
      data.frame(P1 = test[i, 3],
                 P2 = test[i, 2],
                 OUTCOME = test[i, 4])
  }
  
  match_history <- rbind(match_history, prev_match)
}

#test_matches <- test_matches[-1, ]
test_matches <-
  test_matches[c((nrow(matches) + 1):(nrow(test_matches))), ]

#test <- test[c((nrow(train) + 1):(nrow(test))),]
##################

matches <- matches[matches$Games_P1 != 0 & matches$Games_P2 != 0 ,]

save.image("5_data.RData")