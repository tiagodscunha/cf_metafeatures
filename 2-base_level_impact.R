

#load prediction results
detailed_ir <- readRDS("results/detailed_results_ir.rds")
detailed_rp <- readRDS("results/detailed_results_rp.rds")
detailed <- rbind(detailed_ir,detailed_rp)

#load rankings
ndcg_ranking <- read.csv("performance/performance_ndcg.csv", sep=";")
auc_ranking <- read.csv("performance/performance_auc.csv", sep=";")
rmse_ranking <- read.csv("performance/performance_rmse.csv", sep=";")
nmae_ranking <- read.csv("performance/performance_nmae.csv", sep=";")

#load actual performance results
perf_values_ndcg <- read.csv("performance/performance_ndcg_values.csv", sep=";")
perf_values_auc <- read.csv("performance/performance_auc_values.csv", sep=";")
perf_values_rmse <- read.csv("performance/performance_rmse_values.csv", sep=";")
perf_values_nmae <- read.csv("performance/performance_nmae_values.csv", sep=";")

#create performance data.frame
ndcg <- merge(ndcg_ranking,perf_values_ndcg, by.x="dataset",by.y="dataset")
ndcg$target <- "ndcg"
colnames(ndcg) <- c("dataset","ranking","performance","target")

auc <- merge(auc_ranking,perf_values_auc, by.x="dataset",by.y="dataset")
auc$target <- "auc"
colnames(auc) <- c("dataset","ranking","performance","target")

rmse <- merge(rmse_ranking,perf_values_rmse, by.x="dataset",by.y="dataset")
rmse$target <- "rmse"
colnames(rmse) <- c("dataset","ranking","performance","target")

nmae <- merge(nmae_ranking,perf_values_nmae, by.x="dataset",by.y="dataset")
nmae$target <- "nmae"
colnames(nmae) <- c("dataset","ranking","performance","target")

all <- rbind(ndcg,auc,rmse,nmae)

rm(detailed_ir,detailed_rp,ndcg,auc,rmse,nmae,auc_ranking,ndcg_ranking,rmse_ranking,nmae_ranking,perf_values_auc,perf_values_ndcg,perf_values_nmae,perf_values_rmse)


#auxiliary functions

getAlgorithmPositionPredicted <- function(x,target,dataset,algorithm){
  rank <- x[x$dataset == dataset & x$target == target, ]
  tmp <- unlist(strsplit(toString(rank$ranking),","))
  index <- which(tmp==algorithm)
  perf <- unlist(strsplit(toString(rank$performance),","))
  
  # print(rank)
  # print(tmp)
  # print(index)
  # print(perf)
  # print(perf[index])
  
  perf[index]
}


#main

strategies <- c("B","C","D","E")
targets <- c("ndcg","auc","rmse","nmae")
algorithms <- c("majority_voting","c45","svm_linear","svm_poly","svm_radial","rf","xgboost","knn")

results_all <- data.frame(
  strategy = character(),
  target = character(),
  algorithm = character(),
  base_metric = numeric()
)

levels(results_all$strategy) <- strategies
levels(results_all$target) <- targets
levels(results_all$algorithm) <- algorithms

x <- lapply(strategies, function(strategy){
  lapply(targets, function(target){
    lapply(algorithms, function(algorithm){
      filtered <- detailed[which(detailed$target==target & detailed$strategy==strategy & detailed$algorithm==algorithm),]
      
      results <- unlist(lapply(1:length(filtered$dataset), function(row_index){
        row <- filtered[row_index,]
        getAlgorithmPositionPredicted(all,target,row$dataset,row$pred)
      }))
      

      # print(strategy)
      # print(target)
      # print(algorithm)
      # print(mean(as.numeric(results)))
      
      results_all[nrow(results_all)+1,] <<- c(strategy,target,algorithm,mean(as.numeric(results)))
      
    })
  })
})


#assign oracle

strategies <- c("B","C","D","E")
targets <- c("ndcg","auc","rmse","nmae")
algorithms <- c("oracle")

levels(results_all$algorithm) <- c(levels(results_all$algorithm),"oracle")

x <- lapply(strategies, function(strategy){
  lapply(targets, function(target){
    lapply(algorithms, function(algorithm){
      filtered <- detailed[which(detailed$target==target & detailed$strategy==strategy & detailed$algorithm=="svm_radial"),]
      
      results <- unlist(lapply(1:length(filtered$dataset), function(row_index){
        row <- filtered[row_index,]
        getAlgorithmPositionPredicted(all,target,row$dataset,row$obs)
      }))
      
      # print(strategy)
      # print(target)
      # print(algorithm)
      # print(mean(as.numeric(results)))
      
      results_all[nrow(results_all)+1,] <<- c(strategy,target,algorithm,mean(as.numeric(results)))
      
    })
  })
})


results_all$base_metric <- as.numeric(results_all$base_metric)

results_all <- results_all[which(results_all$strategy != "A"),] 


strategies <- c("B","C","D","E")
targets <- c("ndcg","auc","nmae","rmse")

plots <- lapply(strategies, function(strategy){
  tmp <- lapply(targets, function(target){
    
    filtered <- results_all[which(results_all$target==target & results_all$strategy==strategy),]
    filtered$base_metric <- as.numeric(filtered$base_metric)
    #print(filtered)
    
    minimum <- min(filtered[which(filtered$algorithm == "majority_voting"),]$base_metric)
    maximum <- max(filtered[which(filtered$algorithm == "oracle"),]$base_metric)
    
    filtered$base_metric <- ((filtered$base_metric - minimum) / (maximum - minimum) ) * 100
    
    filtered
  })
  
  do.call(rbind,tmp)
  
  })

results_all_normalized <- do.call(rbind,plots)

results_all_normalized <- results_all_normalized[which(results_all_normalized$algorithm != "majority_voting"),] 
results_all_normalized <- results_all_normalized[which(results_all_normalized$algorithm != "oracle"),] 

results_all_normalized$target <- factor(results_all_normalized$target, levels = c("auc","ndcg", "nmae", "rmse"))
results_all_normalized$algorithm <- factor(results_all_normalized$algorithm, 
                                           levels = c("c45","knn", "rf", "svm_linear", "svm_poly", "svm_radial","xgboost"))


levels(results_all_normalized$algorithm)[levels(results_all_normalized$algorithm)=="rf"] <- "r_forest"

levels(results_all_normalized$target)[levels(results_all_normalized$target)=="auc"] <- "AUC"
levels(results_all_normalized$target)[levels(results_all_normalized$target)=="ndcg"] <- "NDCG"
levels(results_all_normalized$target)[levels(results_all_normalized$target)=="rmse"] <- "RMSE"
levels(results_all_normalized$target)[levels(results_all_normalized$target)=="nmae"] <- "NMAE"

library(ggplot2)

cbPalette <- c("#999999", "#E69F00",  "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")

g <- ggplot(results_all_normalized,mapping=aes(x=algorithm, y=base_metric, group=algorithm, fill=algorithm)) + 
  facet_grid(target ~ strategy, scales="free_y") +
  geom_bar(stat="identity") +
  guides(fill = guide_legend(title = "Meta-algorithms"))+
  scale_fill_manual(values=cbPalette) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  ylab("Lift (%)")
g






