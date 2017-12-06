library(ggplot2)
library(fontcm)
font_import()

four_AUC <- read.table("AUC_table.txt",stringsAsFactors = FALSE,header = TRUE)

four_AUC$kern <- 0
four_AUC$set <- 0
four_AUC$ftrs <- 0

for (i in 1:nrow(four_AUC)){
  sel <- four_AUC[i,1]
  sel <- strsplit(sel,"_")
  four_AUC$set[i] <- sel[[1]][1]
  four_AUC$kern[i] <- sel[[1]][4]
  four_AUC$ftrs[i] <- sel[[1]][6]
}


base <- four_AUC[,c(3,4,5,6)]
four_AUC$Baseline_AUC <- NULL
four_AUC$file <- NULL

base$kern <- "Baseline"
colnames(base)[1] <- "Final_AUC"

four_AUC <- rbind(four_AUC,base)
four_AUC <- four_AUC[!duplicated(four_AUC),]

ELO_4M <- read.table("ELO_4M.txt",stringsAsFactors = FALSE,header=TRUE)
GLIKO_4M <- read.table("GLICKO_4M.txt",stringsAsFactors = FALSE,header=TRUE)

GLIKO_4M$ftrs <- "2ftrs"
ELO_4M$ftrs <- "2ftrs"

four_AUC <- rbind(four_AUC,ELO_4M)
four_AUC <- rbind(four_AUC,GLIKO_4M)

ELO_4M <- read.table("ELO_4M.txt",stringsAsFactors = FALSE,header=TRUE)
GLIKO_4M <- read.table("GLICKO_4M.txt",stringsAsFactors = FALSE,header=TRUE)

GLIKO_4M$ftrs <- "aftrs"
ELO_4M$ftrs <- "aftrs"

four_AUC <- rbind(four_AUC,ELO_4M)
four_AUC <- rbind(four_AUC,GLIKO_4M)

four_AUC[four_AUC$kern == "Vanilla",2] <- "Linear SVM"
four_AUC[four_AUC$kern == "rbf",2] <- "Gaussian SVM"
four_AUC[four_AUC$kern == "polydot",2] <- "Poynomial SVM"
four_AUC[four_AUC$ftrs == "aftrs",4] <- "All Features"
four_AUC[four_AUC$ftrs == "2ftrs",4] <- "Only Glicko and ELO Score"

med <- aggregate(four_AUC,by=list(four_AUC$kern,four_AUC$ftrs),FUN=median)
med$kern <- NULL
med$set <- NULL
med$ftrs <- NULL

colnames(med)[3] <- "Median"
colnames(med)[1] <- "kern"
colnames(med)[2] <- "ftrs"

four_AUC <- merge(four_AUC,med,by=c("kern","ftrs"))
med_four <- med

colnames(four_AUC)[2] <- "Features"

p <- ggplot(four_AUC,aes(x=reorder(as.factor(kern),Median),y=Final_AUC,fill=Features)) + geom_boxplot(outlier.shape = NA)+ geom_jitter(shape=16,size=5, position=position_jitter(0.2),aes(y = Final_AUC,color=Features))  + coord_flip() + xlab("Model") + ylab("AUC") + theme_bw() 

pdf("AUC_chess.pdf", width=20, height=15)
p + theme(text=element_text(family = "CM Roman",size = 24))
dev.off()

