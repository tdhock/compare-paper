load("sushi.features.RData")
load("sushi.RData")

## Each user only tried 10 sushis.
apply(!is.na(sushi$scores), 1, sum)

sushi.pairs <- list()

for(user.id in 1:nrow(sushi$scores)){
  scores <- sushi$scores[user.id,]
  item.ids <- which(!is.na(scores))
  user.x <- sushi.features$user[user.id,]
  while(length(item.ids) >= 2){
    pick <- order(scores[item.ids])[1:2] 
    picked.items <- item.ids[pick]
    item.ids <- item.ids[-pick]
    item.X <- sushi.features$item[picked.items,]
    xi <- c(item.X[1,], user.x)
    xip <- c(item.X[2,], user.x)
    ranks <- scores[picked.items]
    rank.diff <- ranks[2]-ranks[1]
    ## to calc!! abs(X) etc
  }
}
