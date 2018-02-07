source("packages.R")

load("simulation.sushi.samples.RData")

## matrix versions of the data.name.
funs <- list(l2=function(x)rowSums(x*x),
             l1=function(x)rowSums(abs(x))^2,
             linf=function(x)apply(abs(x), 1, max)^2)

size.list <- simulation.sushi.samples$data
err <- simulation.sushi.samples$err
err$percent <- err$error / err$count * 100
## sets of training data and bayes error on test data.
sets <- dcast(err, N + seed + data.name ~ fit.name, value.var="percent")
sets$diff <- sets$compare-sets$rank
sets$set.id <- 1:nrow(sets)
diff.df <- sets[,  list(
  mean=mean(diff), sd=sd(diff)
  ), by=list(N, data.name)]

train.dt.list <- list()
bayes.dt.list <- list()
for(set.id in sets$set.id){
  e <- sets[set.id,]
  N <- as.character(e$N)
  data.name <- as.character(e$data.name)
  seed <- as.character(e$seed)
  err$set.id[err$data.name == data.name & err$N == N & err$seed == seed] <- set.id
  set.list <- size.list[[N]][[seed]][[data.name]]
  info <- data.frame(N=as.integer(as.character(N)), data.name, seed, set.id)
  ## The Bayes error on the test data set.
  test <- set.list$test
  fun <- funs[[data.name]]
  if(is.function(fun)){
    fxdiff <- with(test, fun(Xip)-fun(Xi))
    yhat <- ifelse(fxdiff > 1, 1L,
                   ifelse(fxdiff < -1, -1L, 0))
    table(yhat, test$yi)
    percent <- mean(yhat != test$yi) * 100
    bayes.dt.list[[paste(set.id)]] <- data.table(info, percent)
    ## Train pairs, oriented in the same way:
    pair.df <- with(set.list$train,{
      rbind(data.frame(Xt=Xi[yi==1,],Xtp=Xip[yi==1,],yt=1),
            if(any(yi==-1))data.frame(Xt=Xip[yi==-1,],Xtp=Xi[yi==-1,],yt=1),
            data.frame(Xt=Xi[yi==0,],Xtp=Xip[yi==0,],yt=-1))
    })
    train.dt.list[[paste(set.id)]] <- data.table(pair.df, info)
  }
}
train.dt <- do.call(rbind, train.dt.list)
bayes.dt <- do.call(rbind, bayes.dt.list)

bayes.dt$fit.name <- "truth"
combined <- rbind(
  err[,names(bayes.dt), with=FALSE],
  bayes.dt)
percents <- combined[, list(
  mean=mean(percent),
  sd=sd(percent),
  se=sd(percent)/sqrt(length(percent))
  ), by=list(N, fit.name, data.name)]
percents$fit.name <- factor(percents$fit.name, names(model.colors))
labels <- c(l1="1",
            l2="2",
            linf="\\infty")
makelabel <- function(x){
  ifelse(x=="sushi", "sushi",
  sprintf("$r(\\mathbf x) = ||\\mathbf x||_%s^2$", labels[as.character(x)]))
}
percents$label <- makelabel(percents$data.name)
err$label <- makelabel(err$data.name)
##indicator <- data.frame(N=as.integer(Nsamp), label=makelabel(data.name))
leg <- "function"
boring <- ggplot()+
  ##geom_vline(aes(xintercept=N),size=2,data=indicator)+
  geom_ribbon(aes(N, ymin=mean-sd,ymax=mean+sd,fill=fit.name),alpha=1/2, data=percents)+
  geom_line(aes(N, mean, colour=fit.name),lwd=1.5, data=percents)+
  ## Plot actual data:
  ##geom_point(aes(N, error/count*100, colour=fit.name), data=err)+
  facet_grid(.~label)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"),
        panel.grid=element_blank())+
  scale_colour_manual(leg,values=model.colors)+
  scale_fill_manual(leg,values=model.colors)+
  geom_hline(yintercept=50, color="violet")+
  geom_text(aes(200, 50, label="trivial model error rate"),
            vjust=1.5,
            data=data.frame(label=makelabel("linf")),
            color="violet")+
  ylab("percent incorrectly\npredicted test pairs")+
  xlab("$n=$ number of labeled pairs, half equality and half inequality")

tikz("figure-simulation-sushi-samples.tex",h=2)
print(boring)
dev.off()
