# library(caret)
# library(ggplot2)
# library(reshape2)
# library(e1071)
# 
# library(clValid)
# library(clusterSim)
# library(mlbench)
# library(DMwR)
# 
# 
# normalize_data <- function(old_data){
#   averaged <- sapply(2:dim(old_data)[2], replace_na, df=old_data)
#   old_data[,2:dim(old_data)[2]] <- averaged
#   old_data[,2:dim(old_data)[2]] <- data.Normalization(old_data[,2:(dim(old_data)[2])],type="n12",normalization="column") #normalize data
#   old_data
# }
# 
# 
# replace_na <- function(i, df){
#   ind <- which(is.na(df[,i]))
#   df[ind,i] <- mean(df[,i],na.rm=TRUE)
#   df[,i]
# }
# 
# metadatasets=c("RM.csv","SL_IR.csv","SL_RP.csv","GR.csv","CM_IR.csv","CM_RP.csv")
# metrics=c("ndcg","auc","rmse","nmae")
# i <- 1
# 
# all_results <- NULL
# 
# for (t in metadatasets){
#   print(t)
# 
#   data <- read.csv(paste("metafeatures_new/",t,sep=""),sep=";")
# 
#   inner_results <- data.frame(
#     features = colnames(data)[2:length(colnames(data))]
#   )
# 
#   for (metric in metrics){
# 
#     target <- read.csv(paste(paste("performance/performance_",metric,sep=""),".csv",sep=""),sep=";",header = TRUE)
#     target$ranking <- unlist(lapply(target$ranking, function(ranking){
#       strsplit(toString(ranking), ",")[[1]][1]
#     }))
#     colnames(target) <- c("dataset","Class")
# 
# 
#     data <- normalize_data(data)
#     dataset <- merge(x=data,y=target,by.x = "dataset",by.y="dataset")
#     rownames(dataset) <- dataset$dataset
#     dataset$dataset <- NULL
# 
# 
#     if(metric == "ndcg")
#       levels(dataset$Class) <- c("BPRMF","MP","MBPRMF","WBPRMF","WRMF") #NDCG,MAP,MRR
#     else if(metric == "auc")
#       levels(dataset$Class) <- c("MP","MBPRMF","WRMF") #AUC
#     else if (metric=="nmae")
#       levels(dataset$Class) <- c("BMF","GA","IA","MF","SCAFM","SIAFM","SVD++","UA") #NMAE
#     else
#       levels(dataset$Class) <- c("BMF","GA","LFLLM","MF","SCAFM","SIAFM","SVD++") #RMSE
# 
#     set.seed(825)
#     fitControl <- trainControl(method = "LOOCV", search = "grid")
#     xgboost <- train(Class ~ ., data = dataset, method = "xgbTree", trControl = fitControl)
#     x <- varImp(xgboost, scale = FALSE)
# 
# 
#     inner_results[[metric]] <- unlist(x$importance)
# 
#   }
#   rownames(inner_results) <- inner_results$features
#   inner_results$features <- NULL
#   inner_results <- apply(inner_results,1,mean)
#   all_results[[i]] <- inner_results
#   i<-i+1
# }
# 
# all_results
# 
all_results <- readRDS("results/new_feature_imp.rds")

d1 <- melt(all_results[[1]])
d1$metafeature <- rownames(d1)
d1 <- d1[which(d1$value > 0.1),]

d1$metafeature[which(d1$metafeature == "colCounts_min")] <- "I.count.min"
d1$metafeature[which(d1$metafeature == "colMeans_entropy")] <- "I.mean.entropy"
d1$metafeature[which(d1$metafeature == "colMeans_kurtosis")] <- "I.mean.kurtosis"
d1$metafeature[which(d1$metafeature == "colMeans_mode")] <- "I.mean.mode"

g1 <- ggplot(d1,aes(x=metafeature,y=value)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  ggtitle("RM")+ coord_flip()
g1

d2 <- melt(all_results[[2]])
d2$metafeature <- rownames(d2)

d3 <- melt(all_results[[3]])
d3$metafeature <- rownames(d3)

d <- rbind(d2,d3)
d <- d[which(d$value > 0.15),]

d$metafeature[which(d$metafeature == "BiasedMatrixFactorization.NMAE")] <- "BMF.NMAE"
d$metafeature[which(d$metafeature == "BiasedMatrixFactorization.RMSE")] <- "BMF.RMSE"

d$metafeature[which(d$metafeature == "LatentFeatureLogLinearModel.NMAE")] <- "LFLLM.NMAE"
d$metafeature[which(d$metafeature == "LatentFeatureLogLinearModel.RMSE")] <- "LFLLM.RMSE"

d$metafeature[which(d$metafeature == "WeightedBPRMF.AUC")] <- "WBPRMF.AUC"
d$metafeature[which(d$metafeature == "WeightedBPRMF.MAP")] <- "WBPRMF.NDCG"

d$metafeature[which(d$metafeature == "MostPopular.AUC")] <- "MP.AUC"
d$metafeature[which(d$metafeature == "BPRMF.MAP")] <- "BPRMF.NDCG"

g2 <- ggplot(d,aes(x=metafeature,y=value)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))  +
  ggtitle("SL")+ coord_flip()
g2





d3 <- melt(all_results[[4]])
d3$metafeature <- rownames(d3)
d3 <- d3[which(d3$value > 0.07),]

d3$metafeature[which(d3$metafeature == "all_similarity_variance_skewness")] <- "G.sim.var.var"
d3$metafeature[which(d3$metafeature == "communities_alpha_mean_entropy")] <- "com.alpha.mean.ent"
d3$metafeature[which(d3$metafeature == "communities_alpha_mean_skewness")] <- "com.alpha.mean.skewn"
d3$metafeature[which(d3$metafeature == "communities_alpha_skewness_entropy")] <- "com.alpha.skew.ent"

g3 <- ggplot(d3,aes(x=metafeature,y=value)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))  + 
  ggtitle("GR")+ coord_flip()
g3

d4 <- melt(all_results[[5]])
d4$metafeature <- rownames(d4)

d5 <- melt(all_results[[6]])
d5$metafeature <- rownames(d5)

d <- merge(d4,d5,by.x="metafeature",by.y="metafeature")
d$value <- apply(d,1,function(row){mean(as.numeric(row[[2]]),as.numeric(row[[3]]))})


d <- d[which(d$value > 0.07),]

d$metafeature[which(d$metafeature == "colCounts_min")] <- "I.count.min"
d$metafeature[which(d$metafeature == "colMeans_entropy")] <- "I.mean.entropy"
d$metafeature[which(d$metafeature == "colMeans_kurtosis")] <- "I.mean.kurtosis"
d$metafeature[which(d$metafeature == "colMeans_mode")] <- "I.mean.mode"

g4 <- ggplot(d,aes(x=metafeature,y=value)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))  +
  ggtitle("CM")+ coord_flip()

g4

library(gridExtra)
grid.arrange(arrangeGrob(g1,g2,g3,g4,nrow = 2, ncol=2,left=textGrob("Metafeature importance (AUC)", rot = 90, vjust = 1)) )




