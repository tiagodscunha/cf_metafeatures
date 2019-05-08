

detailed_results_ir <- readRDS("results/detailed_results_ir.rds")
detailed_results_rp <- readRDS("results/detailed_results_rp.rds")
related_detailed_results_ir <- readRDS("results/new_detailed_results_ir.rds")
related_detailed_results_rp <- readRDS("results/new_detailed_results_rp.rds")

ir <- rbind(detailed_results_ir,related_detailed_results_ir)
ir$task <- "IR"

rp <- rbind(detailed_results_rp,related_detailed_results_rp)
rp$task <- "RP"

all <- rbind(ir,rp)
rownames(all) <- NULL
rm(detailed_results_ir,detailed_results_rp,related_detailed_results_ir,related_detailed_results_rp,ir,rp)


all$pred <- NULL
all$obs <- NULL


#corrigir datasets

all$dataset <- unlist(lapply(all$dataset,function(x){
  tmp <- unlist(strsplit(as.character(x),"[.]"))
  tmp <- tmp[1]
  tmp <- gsub("amazon", "AMZ", tmp)
  tmp <- gsub("jester", "JT", tmp)
  tmp <- gsub("movielens", "ML", tmp)
  tmp <- gsub("movietweetings", "MT", tmp)
  tmp <- gsub("yahoo", "YH", tmp)
  tmp <- gsub("tripadvisor", "TA", tmp)
  tmp <- gsub("bookcrossing", "BC", tmp)
  tmp <- gsub("yelp", "YE", tmp)
  tmp <- gsub("flixter", "FL", tmp)
  
  tmp <- gsub("recsys2014", "RS14", tmp)
  tmp <- gsub("digital-music", "music", tmp)
  tmp <- gsub("instant-video", "video", tmp)
  tmp <- gsub("_", "-", tmp)
  tmp
}))


#dataset, strategy, score
#score given by aggregation algorithm, target and task
# 
# res <- aggregate(all,
#                  by = list(all$dataset,all$strategy),
#                  FUN = mean)
# 
# res$dataset <- NULL
# res$algorithm <- NULL
# res$target <- NULL
# res$strategy <- NULL
# res$task <- NULL
# colnames(res) <- c("dataset","strategy","score")
# 
library(ggplot2)
# 
# res <- all[which(res$dataset == "yelp.csv"),]

res <- all[which(all$strategy == "B" | all$strategy == "C" | all$strategy == "D" | all$strategy == "E"),]
#res <- all[which(all$strategy == "RM" | all$strategy == "SL" | all$strategy == "GR" | all$strategy == "CM"),]
#res <- all[which(all$strategy == "RM"),]

p <- ggplot(res, aes(x=dataset,y=Accuracy,fill=strategy)) + 
  #geom_boxplot(width = .8) +
  geom_violin() +
  coord_flip() + 
  #facet_wrap( ~ strategy, scales="free", ncol=2) + 
  theme(text = element_text(size=8)) + 
  guides(fill=guide_legend(title="Metafeatures"))
  
p



