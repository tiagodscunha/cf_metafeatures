library(scmamp)


detailed_results_ir <- readRDS("results/new_detailed_results_ir.rds")
detailed_results_rp <- readRDS("results/new_detailed_results_rp.rds")

detailed_results_ir_old <- readRDS("results/detailed_results_ir.rds")
detailed_results_rp_old <- readRDS("results/detailed_results_rp.rds")

res <- rbind(detailed_results_ir,detailed_results_rp,detailed_results_rp_old,detailed_results_ir_old)

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
  E = data1[which(data1$strategy == "E"),]$Accuracy,
  RM = data1[which(data1$strategy == "RM"),]$Accuracy,
  SL = data1[which(data1$strategy == "SL"),]$Accuracy,
  GR = data1[which(data1$strategy == "GR"),]$Accuracy,
  CM = data1[which(data1$strategy == "CM"),]$Accuracy
)



plotCD (final, alpha=0.05, cex=1.25)