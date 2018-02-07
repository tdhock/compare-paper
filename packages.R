works_with_R(
  "3.3.3",
  future="1.4.0",
  kernlab="0.9.19",
  data.table="1.10.4",
  RCurl="1.96.0",
  RJSONIO="1.3.1",
  "tdhock/rankSVMcompare@e4bcbeb1170970c8678eceb7b8b34290df42f657",
  ggplot2="2.1.0",
  namedCapture="2015.12.1",
  tikzDevice="0.10.1")
options(
  tikzDocumentDeclaration="\\documentclass[11pt]{article}\\usepackage{amsmath,amssymb,amsthm}",
  tikzMetricsDictionary="tikzMetrics")

yi.colors <- c("0"="#f8756e", #orange
               "1"="#00ba38", #green
               "-1"="#619cff")
yi.colors <- RColorBrewer::brewer.pal(5, "Set1")[-(1:2)]
names(yi.colors) <- c("-1", "0", "1")
model.colors <- 
  c(rank="skyblue",
    rank2="blue",
    compare="black",
    truth="grey80")
model.colors[["latent"]] <- model.colors[["truth"]]


