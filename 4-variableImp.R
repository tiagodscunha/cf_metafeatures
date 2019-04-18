library(caret)
library(ggplot2)
library(reshape2)
library(e1071)

library(clValid)
library(clusterSim)
library(mlbench)
library(DMwR)


normalize_data <- function(old_data){
  averaged <- sapply(2:dim(old_data)[2], replace_na, df=old_data)
  old_data[,2:dim(old_data)[2]] <- averaged
  old_data[,2:dim(old_data)[2]] <- data.Normalization(old_data[,2:(dim(old_data)[2])],type="n12",normalization="column") #normalize data
  old_data
}


replace_na <- function(i, df){
  ind <- which(is.na(df[,i]))
  df[ind,i] <- mean(df[,i],na.rm=TRUE)
  df[,i]
}

metadatasets=c("mf_B.csv","mf_C.csv","mf_D.csv","mf_E.csv")
metrics=c("ndcg","auc","rmse","nmae")
i <- 1

all_results <- NULL

for (t in metadatasets){
  print(t)

  data <- read.csv(paste("metafeatures_1000users/",t,sep=""),sep=";")

  inner_results <- data.frame(
    features = colnames(data)[2:length(colnames(data))]
  )

  for (metric in metrics){

    target <- read.csv(paste(paste("performance/performance_",metric,sep=""),".csv",sep=""),sep=";",header = TRUE)
    target$ranking <- unlist(lapply(target$ranking, function(ranking){
      strsplit(toString(ranking), ",")[[1]][1]
    }))
    colnames(target) <- c("dataset","Class")


    data <- normalize_data(data)
    dataset <- merge(x=data,y=target,by.x = "dataset",by.y="dataset")
    rownames(dataset) <- dataset$dataset
    dataset$dataset <- NULL


    if(metric == "ndcg")
      levels(dataset$Class) <- c("BPRMF","MP","MBPRMF","WBPRMF","WRMF") #NDCG,MAP,MRR
    else if(metric == "auc")
      levels(dataset$Class) <- c("MP","MBPRMF","WRMF") #AUC
    else if (metric=="nmae")
      levels(dataset$Class) <- c("BMF","GA","IA","MF","SCAFM","SIAFM","SVD++","UA") #NMAE
    else
      levels(dataset$Class) <- c("BMF","GA","LFLLM","MF","SCAFM","SIAFM","SVD++") #RMSE

    set.seed(825)
    fitControl <- trainControl(method = "LOOCV", search = "grid")
    xgboost <- train(Class ~ ., data = dataset, method = "xgbTree", trControl = fitControl)
    x <- varImp(xgboost, scale = FALSE)


    inner_results[[metric]] <- unlist(x$importance)

  }
  rownames(inner_results) <- inner_results$features
  inner_results$features <- NULL
  inner_results <- apply(inner_results,1,mean)
  all_results[[i]] <- inner_results
  i<-i+1
}

all_results

saveRDS(all_results, "feature_imp.rds")

all_results <- readRDS("feature_imp.rds")


d1 <- melt(all_results[[1]])
d1$metafeature <- rownames(d1)

d1$metafeature[which(d1$metafeature == "entropy")] <- "EC.co-ratings.entropy"
d1$metafeature[which(d1$metafeature == "gini")] <- "EC.co-ratings.gini"
d1$metafeature[which(d1$metafeature == "sparsity")] <- "dataset.sparsity.none"

g1 <- ggplot(d1,aes(x=metafeature,y=value)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_text(angle = 70, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  ggtitle("E")
g1

d2 <- melt(all_results[[2]])
d2$metafeature <- rownames(d2)

d2$metafeature[which(d2$metafeature == "density")] <- "dataset.density.none"
d2$metafeature[which(d2$metafeature == "variance")] <- "dataset.ratings.variance"

g2 <- ggplot(d2,aes(x=metafeature,y=value)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_text(angle = 70, hjust = 1),plot.title = element_text(hjust = 0.5))  +
  ggtitle("B")
g2

d3 <- melt(all_results[[3]])
d3$metafeature <- rownames(d3)
d3$metafeature[which(d3$metafeature == "user.standard_deviation.mean")] <- "user.std.mean"
d3$metafeature[which(d3$metafeature == "user.number_neighbours.mean")] <- "user.neighbours.mean"
d3$metafeature[which(d3$metafeature == "user.average_similarity.mean")] <- "user.similarity.mean"
d3$metafeature[which(d3$metafeature == "user.clustering_coefficient.mean")] <- "user.clustering.mean"
d3$metafeature[which(d3$metafeature == "user_coratings.jaccard.mean")] <- "coratings.jaccard.mean"

g3 <- ggplot(d3,aes(x=metafeature,y=value)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_text(angle = 70, hjust = 1),plot.title = element_text(hjust = 0.5))  + 
  ggtitle("D")
g3

d4 <- melt(all_results[[4]])
d4$metafeature <- rownames(d4)

g4 <- ggplot(d4,aes(x=metafeature,y=value)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_text(angle = 70, hjust = 1),plot.title = element_text(hjust = 0.5))  +
  ggtitle("C")

g4

library(gridExtra)
grid.arrange(arrangeGrob(g2,g4,g3,g1,nrow = 2, ncol=2,left=textGrob("Metafeature importance (AUC)", rot = 90, vjust = 1)) )



    
    