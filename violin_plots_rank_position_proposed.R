
#metafeatures - PCA - plot datasets

dt1 <- read.csv("metafeatures_new/RM.csv",sep=";")  
dt2 <- read.csv("metafeatures_new/SL_IR.csv",sep=";")
dt2_rp <- read.csv("metafeatures_new/SL_RP.csv",sep=";")
dt3 <- read.csv("metafeatures_new/GR.csv",sep=";")
dt4 <- read.csv("metafeatures_new/CM_IR.csv",sep=";")
dt4_rp <- read.csv("metafeatures_new/CM_RP.csv",sep=";")

dt3 <-  cbind(dt3$dataset,removeCFS(dt3[,2:dim(dt3)[2]],0.7))
dt4 <-  cbind(dt4$dataset,removeCFS(dt4[,2:dim(dt4)[2]],0.7))
dt4_rp <-  cbind(dt4_rp$dataset,removeCFS(dt4_rp[,2:dim(dt4_rp)[2]],0.7))

colnames(dt3)[1] <- colnames(dt4)[1] <- colnames(dt4_rp)[1] <- "dataset"


dt1[is.na(dt1)] <- 0
dt2[is.na(dt2)] <- 0
dt3[is.na(dt3)] <- 0
dt4[is.na(dt4)] <- 0
dt2_rp[is.na(dt2_rp)] <- 0
dt4_rp[is.na(dt4_rp)] <- 0

dt1_pca <- as.data.frame(princomp(dt1[,2:dim(dt1)[2]])$scores[,1:2])
dt2_pca <- as.data.frame(princomp(dt2[,2:dim(dt2)[2]])$scores[,1:2])
dt3_pca <- as.data.frame(princomp(dt3[,2:dim(dt3)[2]])$scores[,1:2])
dt4_pca <- as.data.frame(princomp(dt4[,2:dim(dt4)[2]])$scores[,1:2])

dt2_rp_pca <- as.data.frame(princomp(dt2_rp[,2:dim(dt2_rp)[2]])$scores[,1:2])
dt4_rp_pca <- as.data.frame(princomp(dt4_rp[,2:dim(dt4_rp)[2]])$scores[,1:2])

library(data.table) ## 1.9.3
library(splitstackshape)
library(plyr)
library(BBmisc)

dt1_pca <- normalize(dt1_pca)
dt2_pca <- normalize(dt2_pca)
dt3_pca <- normalize(dt3_pca)
dt4_pca <- normalize(dt4_pca)
dt2_rp_pca <- normalize(dt2_rp_pca)
dt4_rp_pca <- normalize(dt4_rp_pca)

colnames(dt1_pca) <- colnames(dt2_pca) <- colnames(dt3_pca) <- colnames(dt4_pca) <- colnames(dt2_rp_pca)  <- colnames(dt4_rp_pca)  <- c("V1","V2")
dt1_pca$strategy <- "RM"
dt2_pca$strategy <- "SL"
dt3_pca$strategy <- "GR"
dt4_pca$strategy <- "CM"
dt2_rp_pca$strategy <- "SL"
dt4_rp_pca$strategy <- "CM"

dt1_pca$dataset <- dt1$dataset
dt2_pca$dataset <- dt2$dataset
dt3_pca$dataset <- dt3$dataset
dt4_pca$dataset <- dt4$dataset
dt2_rp_pca$dataset <- dt2_rp$dataset
dt4_rp_pca$dataset <- dt4_rp$dataset

final_ir <- rbind(dt1_pca,dt2_pca,dt3_pca,dt4_pca)
final_rp <- rbind(dt1_pca,dt2_rp_pca,dt3_pca,dt4_rp_pca)

#falta definir coluna "good" para cada algoritmo


createFinalMatrix <- function(data){
  algorithms <- sort(unique(unlist(strsplit(levels(data$ranking), split = ","))))
  orders <- lapply(data$ranking, function(x){unlist(strsplit(as.character(x), split = ','))})
  targets <- lapply(orders, function(rank1,rank2){
    unlist(lapply(rank2, function(x){
      positions <- unlist(which(rank1 == x))
      if(length(positions)>0){
        return (unlist(positions))
      }
      else {
        return (length(algorithms)/2) #(length(algorithms)) #last position!  # (length(algorithms)/2) assigning mean ranking improves results for IR but decreases for RP
      }
    }))
  }, rank2=algorithms)
  targets_matrix <- as.matrix(do.call(rbind, targets))
  
  rownames(targets_matrix) <- data$dataset
  colnames(targets_matrix) <- algorithms
  as.data.frame(targets_matrix)
}

simplifyMatrix <- function(data, threshold, goal){
  
  tmp <- as.matrix(data)
  tmp[which(data > 3)] <- NA
  tmp[which(data == 2.5)] <- 2
  as.data.frame(tmp)
}

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
colnames(ndcg) <- c("dataset","ranking","performance")
ndcg_final <- createFinalMatrix(ndcg)
ndcg_final <- simplifyMatrix(ndcg_final)
ndcg_final$target <- "ndcg"

auc <- merge(auc_ranking,perf_values_auc, by.x="dataset",by.y="dataset")
colnames(auc) <- c("dataset","ranking","performance")
auc_final <- createFinalMatrix(auc)
auc_final <- simplifyMatrix(auc_final)
auc_final$target <- "auc"

rmse <- merge(rmse_ranking,perf_values_rmse, by.x="dataset",by.y="dataset")
colnames(rmse) <- c("dataset","ranking","performance")
rmse_final <- createFinalMatrix(rmse)
rmse_final <- simplifyMatrix(rmse_final)
rmse_final$target <- "rmse"

nmae <- merge(nmae_ranking,perf_values_nmae, by.x="dataset",by.y="dataset")
colnames(nmae) <- c("dataset","ranking","performance")
nmae_final <- createFinalMatrix(nmae)
nmae_final <- simplifyMatrix(nmae_final)
nmae_final$target <- "nmae"

all_ir <- rbind(ndcg_final,auc_final)
all_rp <- rbind(rmse_final,nmae_final)

all_ir$dataset <- ndcg$dataset
all_rp$dataset <- ndcg$dataset

all <- rbind(melt(all_ir),melt(all_rp))
colnames(all) <- c("target","dataset","algorithm","rank")

graph_data_ir <- merge(x = final_ir, y = all, by = "dataset", all = TRUE)
graph_data_rp <- merge(x = final_rp, y = all, by = "dataset", all = TRUE)
graph_data <- rbind(graph_data_ir,graph_data_rp)

graph_data <- graph_data[which(graph_data$algorithm == "MostPopular" | 
                                 graph_data$algorithm == "BPRMF" | 
                                 graph_data$algorithm == "WRMF" |
                                 graph_data$algorithm == "BiasedMatrixFactorization" | 
                                 graph_data$algorithm == "SVDPlusPlus" | 
                                 graph_data$algorithm == "SigmoidUserAsymmetricFactorModel"),]

levels(graph_data$algorithm) <- c(levels(graph_data$algorithm),"MP","BMF","SVD++","SUAFM")
graph_data[which(graph_data$algorithm == "MostPopular"),]$algorithm = "MP"
graph_data[which(graph_data$algorithm == "BiasedMatrixFactorization"),]$algorithm = "BMF"
graph_data[which(graph_data$algorithm == "SVDPlusPlus"),]$algorithm = "SVD++"
graph_data[which(graph_data$algorithm == "SigmoidUserAsymmetricFactorModel"),]$algorithm = "SUAFM"

graph_data$rank <- as.factor(graph_data$rank)
# levels(graph_data$good) <- c(levels(graph_data$good),"bad","good")
# graph_data[which(graph_data$good == 1),]$good = "good"
# graph_data[which(graph_data$good == 0),]$good = "bad"

graph_data <- graph_data[which(!is.na(graph_data$rank)),]

library(ggplot2)
library(ggrepel)

g <- ggplot(graph_data, aes(x=V1,y=V2, colour=rank)) +
  geom_point() +
  #geom_text_repel(aes(label=dataset), show.legend = FALSE) +
  theme_bw() + 
  theme (axis.title.x=element_blank(),axis.title.y=element_blank()) +
  theme(legend.position="bottom") + 
  #theme(legend.title=element_blank()) +
  facet_grid(algorithm ~ strategy) 
g

