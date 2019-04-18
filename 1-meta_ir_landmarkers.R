library(caret)
library(ggplot2)
library(reshape2)
library(e1071)

library(clValid)
library(clusterSim)
library(mlbench)
library(DMwR)

### CUSTOM METHODS ####

maj_voting <-list(type = "Classification", library = NULL,
                  parameters = data.frame(parameter = "parameter",
                                          Class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid")
                    data.frame(parameter = "none"),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, ClassProbs, ...) {
                    yx <- unique(y)
                    yx[which.max(tabulate(match(y, yx)))]
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    modelFit$obsLevels[modelFit[[1]]]
                  },
                  prob = NULL,
                  predictors = NULL,
                  tags = NULL,
                  levels = NULL,
                  sort = function(x) x)

naive_bayes_e1071 <-list(type = "Classification", library = "e1071",
                         parameters = data.frame(parameter = "parameter",
                                                 Class = "character",
                                                 label = "parameter"),
                         grid = function(x, y, len = NULL, search = "grid")
                           data.frame(parameter = "none"),
                         loop = NULL,
                         fit = function(x, y, wts, param, lev, last, ClassProbs, ...) {
                           e1071::naiveBayes(Class ~ ., x=x, y=y)
                         },
                         predict = function(modelFit, newdata, submodels = NULL) {
                           stats::predict(modelFit, newdata)
                         },
                         prob = NULL,
                         predictors = NULL,
                         tags = NULL,
                         levels = NULL,
                         sort = function(x) x)



########################

replace_na <- function(i, df){
  ind <- which(is.na(df[,i]))
  df[ind,i] <- mean(df[,i],na.rm=TRUE)
  df[,i]
}

normalize_data <- function(old_data){
  averaged <- sapply(2:dim(old_data)[2], replace_na, df=old_data)
  old_data[,2:dim(old_data)[2]] <- averaged
  old_data[,2:dim(old_data)[2]] <- data.Normalization(old_data[,2:(dim(old_data)[2])],type="n12",normalization="column") #normalize data
  old_data
}

CFS <- function(data, threshold = 0.6){
  correlationMatrix <- cor(data[,2:(dim(data)[2])], use = "complete.obs")
  #print(correlationMatrix)
  highlyCorrelated <- findCorrelation(as.matrix(correlationMatrix), cutoff=threshold)
  
  data <- cbind(
    dataset=data[,1],
    data[,2:dim(data)[2]][,-highlyCorrelated])
  data
}

calcAccuracy <- function(x){
  unlist(lapply(1:length(x$pred), function(index){
    if(x$pred[index] == x$obs[index]){
      1
    }
    else{
      0
    }
  }))
}

cleanStrategyName <- function(x){
  tmp <- x
  if(grepl("_",tmp)){
    tmp <- toString(strsplit(toString(tmp),'_')[[1]][1])
  }
  tmp1 = toString(strsplit(toString(tmp),'[.]')[[1]][1])
  tmp1
}

metadatasets=c("original_IR.csv","pairwise_IR.csv","ranking_IR.csv","ratio_IR.csv")
metrics=c("ndcg","auc")
i <- 0
j <- 1
complete_results <- list()
detailed_results <- NULL
for (t in metadatasets){
  print(t)
  total_results <- data.frame()
  for (metric in metrics){
    
    data <- read.csv(paste("metafeatures_landmarkers/",t,sep=""),sep=";")
    target <- read.csv(paste(paste("performance/performance_",metric,sep=""),".csv",sep=""),sep=";",header = TRUE)
    target$ranking <- unlist(lapply(target$ranking, function(ranking){
      strsplit(toString(ranking), ",")[[1]][1]
    }))
    colnames(target) <- c("dataset","Class")
    
    #TODO normalize
    data <- normalize_data(data)
    dataset <- merge(x=data,y=target,by.x = "dataset",by.y="dataset")
    rownames(dataset) <- dataset$dataset
    dataset$dataset <- NULL
    if(metric %in% c("ndcg","map","mrr"))
      levels(dataset$Class) <- c("BPRMF","MP","MBPRMF","WBPRMF","WRMF") #NDCG,MAP,MRR
    else
      levels(dataset$Class) <- c("MP","MBPRMF","WRMF") #AUC
    
    #print(dataset)
    
    set.seed(825)
    
    old_dataset <- dataset
    #dataset <- upSample(x = dataset[, -ncol(dataset)],y = as.factor(dataset$Class))
    #dataset <- SMOTE(Class ~ ., data  = dataset, perc.over = 1, k=1, perc.under = 1)
    
    
    
    fitControl <- trainControl(method = "LOOCV", search = "grid")
    c45 <- train(Class ~ ., data = dataset, method = "J48", trControl = fitControl)
    svm_linear <- train(Class ~ ., data = dataset, method = "svmLinear", trControl = fitControl)
    svm_poly <- train(Class ~ ., data = dataset, method = "svmPoly", trControl = fitControl)
    svm_radial <- train(Class ~ ., data = dataset, method = "svmRadial", trControl = fitControl)
    rf <- train(Class ~ ., data = dataset, method = "ranger", trControl = fitControl)
    knn <- train(Class ~ ., data = dataset, method = "kknn", trControl = fitControl)
    xgboost <- train(Class ~ ., data = dataset, method = "xgbTree", trControl = fitControl)
    baseline <- train(Class ~ ., data = old_dataset, method = maj_voting, trControl = fitControl)
    
    # print(ctree$results)
    # print(c45$results)
    # print(c50$results)
    # print(svm_linear$results)
    # print(svm_poly$results)
    # print(svm_radial$results)
    # print(rf$results)
    # print(lda$results)
    # print(nb$results)
    # print(knn$results)
    # print(baseline$results)
    
    
    c45_detailed <-c45$pred[c45$pred$C == c45$bestTune[[1]] & c45$pred$M == c45$bestTune[[2]],]
    c45_detailed$rowIndex <- NULL
    c45_detailed$C <- NULL
    c45_detailed$M <- NULL
    c45_detailed$dataset <- rownames(dataset)
    c45_detailed$Accuracy <- calcAccuracy(c45_detailed)
    c45_detailed <- c45_detailed[,c(3,1,2,4)]
    c45_detailed$algorithm <- "c45"
    c45_detailed$target <- metric
    c45_detailed$strategy <- cleanStrategyName(t)
    
    
    
    svm_linear_detailed <- svm_linear$pred
    svm_linear_detailed$rowIndex <- NULL
    svm_linear_detailed$C <- NULL
    svm_linear_detailed$dataset <- rownames(dataset)
    svm_linear_detailed$Accuracy <- calcAccuracy(svm_linear_detailed)
    svm_linear_detailed <- svm_linear_detailed[,c(3,1,2,4)]
    svm_linear_detailed$algorithm <- "svm_linear"
    svm_linear_detailed$target <- metric
    svm_linear_detailed$strategy <- cleanStrategyName(t)
    
    svm_poly_detailed <-  svm_poly$pred[svm_poly$pred$degree == svm_poly$bestTune[[1]] & svm_poly$pred$scale == svm_poly$bestTune[[2]] & svm_poly$pred$C == svm_poly$bestTune[[3]],]
    svm_poly_detailed$rowIndex <- NULL
    svm_poly_detailed$C <- NULL
    svm_poly_detailed$degree <- NULL
    svm_poly_detailed$scale <- NULL
    svm_poly_detailed$dataset <- rownames(dataset)
    svm_poly_detailed$Accuracy <- calcAccuracy(svm_poly_detailed)
    svm_poly_detailed <- svm_poly_detailed[,c(3,1,2,4)]
    svm_poly_detailed$algorithm <- "svm_poly"
    svm_poly_detailed$target <- metric
    svm_poly_detailed$strategy <- cleanStrategyName(t)
    
    svm_radial_detailed <- svm_radial$pred[svm_radial$pred$sigma == svm_radial$bestTune[[1]] & svm_radial$pred$C == svm_radial$bestTune[[2]],]
    svm_radial_detailed$rowIndex <- NULL
    svm_radial_detailed$C <- NULL
    svm_radial_detailed$sigma <- NULL
    svm_radial_detailed$dataset <- rownames(dataset)
    svm_radial_detailed$Accuracy <- calcAccuracy(svm_radial_detailed)
    svm_radial_detailed <- svm_radial_detailed[,c(3,1,2,4)]
    svm_radial_detailed$algorithm <- "svm_radial"
    svm_radial_detailed$target <- metric
    svm_radial_detailed$strategy <- cleanStrategyName(t)
    
    rf_detailed <- rf$pred[rf$pred$mtry == rf$bestTune[[1]] & rf$pred$splitrule == rf$bestTune[[2]] & rf$pred$min.node.size == rf$bestTune[[3]],]
    rf_detailed$rowIndex <- NULL
    rf_detailed$mtry <- NULL
    rf_detailed$min.node.size <- NULL
    rf_detailed$splitrule <- NULL
    rf_detailed$dataset <- rownames(dataset)
    rf_detailed$Accuracy <- calcAccuracy(rf_detailed)
    rf_detailed <- rf_detailed[,c(3,1,2,4)]
    rf_detailed$algorithm <- "rf"
    rf_detailed$target <- metric
    rf_detailed$strategy <- cleanStrategyName(t)
    
    
    
    knn_detailed <- knn$pred[knn$pred$kmax == knn$bestTune[[1]],]
    knn_detailed$rowIndex <- NULL
    knn_detailed$kmax <- NULL
    knn_detailed$distance <- NULL
    knn_detailed$kernel <- NULL
    knn_detailed$dataset <- rownames(dataset)
    knn_detailed$Accuracy <- calcAccuracy(knn_detailed)
    knn_detailed <- knn_detailed[,c(3,1,2,4)]
    knn_detailed$algorithm <- "knn"
    knn_detailed$target <- metric
    knn_detailed$strategy <- cleanStrategyName(t)
    
    
    xgboost_detailed <- xgboost$pred[xgboost$pred$nrounds == xgboost$bestTune$nrounds &
                                       xgboost$pred$max_depth == xgboost$bestTune$max_depth &
                                       xgboost$pred$eta == xgboost$bestTune$eta &
                                       xgboost$pred$gamma == xgboost$bestTune$gamma &
                                       xgboost$pred$colsample_bytree == xgboost$bestTune$colsample_bytree &
                                       xgboost$pred$min_child_weight == xgboost$bestTune$min_child_weight &
                                       xgboost$pred$subsample == xgboost$bestTune$subsample,]
    xgboost_detailed$rowIndex <- NULL
    xgboost_detailed$eta <- NULL
    xgboost_detailed$max_depth <- NULL
    xgboost_detailed$colsample_bytree <- NULL
    xgboost_detailed$min_child_weight <- NULL
    xgboost_detailed$subsample <- NULL
    xgboost_detailed$nrounds <- NULL
    xgboost_detailed$gamma <- NULL
    xgboost_detailed$dataset <- rownames(dataset)
    xgboost_detailed$Accuracy <- calcAccuracy(xgboost_detailed)
    xgboost_detailed <- xgboost_detailed[,c(3,1,2,4)]
    xgboost_detailed$algorithm <- "xgboost"
    xgboost_detailed$target <- metric
    xgboost_detailed$strategy <- cleanStrategyName(t)
    
    baseline_detailed <- baseline$pred
    baseline_detailed$rowIndex <- NULL
    baseline_detailed$parameter <- NULL
    baseline_detailed$dataset <- rownames(old_dataset)
    baseline_detailed$Accuracy <- calcAccuracy(baseline_detailed)
    baseline_detailed <- baseline_detailed[,c(3,1,2,4)]
    baseline_detailed$algorithm <- "majority_voting"
    baseline_detailed$target <- metric
    baseline_detailed$strategy <- cleanStrategyName(t)
    
    detailed_results <- rbind(detailed_results,c45_detailed,svm_linear_detailed,svm_poly_detailed,svm_radial_detailed,
                              rf_detailed,knn_detailed,xgboost_detailed,baseline_detailed)
    
    results <- data.frame(
      c("c4.5","svm_linear","svm_poly","svm_radial","r_forest","knn","xgboost","majority vote"),
      c(
        c45$results[c45$results$C == c45$bestTune[[1]] & c45$results$M == c45$bestTune[[2]],]$Accuracy,
        svm_linear$results$Accuracy,
        svm_poly$results[svm_poly$results$degree == svm_poly$bestTune[[1]] & svm_poly$results$scale == svm_poly$bestTune[[2]] & svm_poly$results$C == svm_poly$bestTune[[3]]  ,]$Accuracy,
        svm_radial$results[svm_radial$results$sigma == svm_radial$bestTune[[1]] & svm_radial$results$C == svm_radial$bestTune[[2]],]$Accuracy,
        rf$results[rf$results$mtry == rf$bestTune[[1]] & rf$results$splitrule == rf$bestTune[[2]],]$Accuracy,
        knn$results[knn$results$kmax == knn$bestTune[[1]],]$Accuracy,
        xgboost$results[xgboost$results$nrounds == xgboost$bestTune$nrounds &
                          xgboost$results$max_depth == xgboost$bestTune$max_depth &
                          xgboost$results$eta == xgboost$bestTune$eta &
                          xgboost$results$gamma == xgboost$bestTune$gamma &
                          xgboost$results$colsample_bytree == xgboost$bestTune$colsample_bytree &
                          xgboost$results$min_child_weight == xgboost$bestTune$min_child_weight &
                          xgboost$results$subsample == xgboost$bestTune$subsample,]$Accuracy,
        baseline$results$Accuracy),
      c(
        c45$results[c45$results$C == c45$bestTune[[1]] & c45$results$M == c45$bestTune[[2]],]$Kappa,
        svm_linear$results$Kappa,
        svm_poly$results[svm_poly$results$degree == svm_poly$bestTune[[1]] & svm_poly$results$scale == svm_poly$bestTune[[2]] & svm_poly$results$C == svm_poly$bestTune[[3]]  ,]$Kappa,
        svm_radial$results[svm_radial$results$sigma == svm_radial$bestTune[[1]] & svm_radial$results$C == svm_radial$bestTune[[2]],]$Accuracy,
        rf$results[rf$results$mtry == rf$bestTune[[1]] & rf$results$splitrule == rf$bestTune[[2]],]$Kappa,
        knn$results[knn$results$kmax == knn$bestTune[[1]],]$Kappa,
        xgboost$results[xgboost$results$nrounds == xgboost$bestTune$nrounds &
                          xgboost$results$max_depth == xgboost$bestTune$max_depth &
                          xgboost$results$eta == xgboost$bestTune$eta &
                          xgboost$results$gamma == xgboost$bestTune$gamma &
                          xgboost$results$colsample_bytree == xgboost$bestTune$colsample_bytree &
                          xgboost$results$min_child_weight == xgboost$bestTune$min_child_weight &
                          xgboost$results$subsample == xgboost$bestTune$subsample,]$Kappa,
        baseline$results$Kappa))
    
    #print(results)
    colnames(results) <- c("Algorithm",paste("Accuracy_",metric,sep=""),paste("Kappa_",metric,sep=""))
    
    if(i==0)
      total_results <- results
    else
      total_results <- merge(total_results,results,by.x="Algorithm",by.y="Algorithm")
    
    if(i==length(metrics)-1){
      complete_results[[j]] <- total_results
      j <- j + 1
    }
    i<- i +1
  }
  
  #write.table(results,paste(paste("results_meta_",t,sep=""),".csv"),sep=";",row.names = FALSE)
  i <- 0
}
#rm(data,dataset,total_results,results,target,baseline,c45,c50,ctree,fitControl,i,j,maj_voting,metric,rpart,svm,t)

saveRDS(complete_results,"results/landmarkers_results_meta_ir.rds")
saveRDS(detailed_results, "results/landmarkers_detailed_results_ir.rds")
