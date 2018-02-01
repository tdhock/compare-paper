### sushi3b.5000.10.score #########################################

## matrix style data separated by <sp>

## each row corresponds to the user in the corresponding line of the
## file sushi3.udata

## each column corresponds to the SUSHI in the item set B

## using five-point-scale, 0:the most disliked, 4:the most preferred,
## -1:not rated

sushi.score <- "sushi3-2016/sushi3b.5000.10.score"
if(!file.exists(sushi.score)){
  download.file("http://www.kamishima.net/asset/sushi3-2016.zip", "sushi3-2016.zip")
  system("unzip sushi3-2016.zip")
}

scores <- read.table(sushi.score, sep=" ")
mat <- as.matrix(scores)
mat[mat==-1] <- NA

ucols <- c("userID", "gender", "age", "time",
           "prefecture.until.15", "region.until.15", "east/west.until.15",
           "prefecture.current", "region.current", "east/west.current",
           "moved")
users <- read.table("sushi3-2016/sushi3.udata", col.names=ucols)

icols <- c("itemID", "japanese", "style", "major", "minor",
           "oily", "frequency.eat", "price", "frequency.sold")
items <- read.table("sushi3-2016/sushi3.idata", col.names=icols)

sushi <- list(scores=mat, users=users, items=items)

save(sushi, file="sushi.RData")
