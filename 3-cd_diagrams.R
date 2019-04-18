library(scmamp)


detailed_results_ir <- readRDS("results/detailed_results_ir.rds")
detailed_results_rp <- readRDS("results/detailed_results_rp.rds")

res <- rbind(detailed_results_ir,detailed_results_rp)

data <- res[which(res$algorithm=="xgboost"),]
data$pred <- NULL
data$dataset <- NULL
data$obs <- NULL
data$algorithm <- NULL

data1 <- with(data, data[order(strategy,target),])

final <- data.frame(
  B = data1[which(data1$strategy == "B"),]$Accuracy,
  C = data1[which(data1$strategy == "C"),]$Accuracy,
  D = data1[which(data1$strategy == "D"),]$Accuracy,
  E = data1[which(data1$strategy == "E"),]$Accuracy
)



plotCD (final, alpha=0.05, cex=1.25)