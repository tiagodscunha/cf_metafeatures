# Metafeatures for Collaborative Filtering Algorithm Selection

Source code for Classification approaches for Collaborative Filtering algorithm selection. It contains all experimental procedures used to empirically compare multiple Collaborative Filtering algorithm selection meta-appraches. The code can be used to replicate the experiments of the contributions presented in the following research papers:

- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2016). Selecting Collaborative Filtering algorithms using Metalearning. In European Conference on Machine Learning and Knowledge Discovery in Databases (pp. 393–409).
- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2017). Recommending Collaborative Filtering algorithms using subsampling landmarkers. In Discovery Science (pp. 189–203).
- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2018). Metalearning and Recommender Systems: A literature review and empirical study on the algorithm selection problem for Collaborative Filtering. Information Sciences, 423, 128–144. 
- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2018). Algorithm Selection for Collaborative Filtering: the influence of graph metafeatures and multicriteria metatargets. ArXiv E-Prints, 1–25. Retrieved from http://arxiv.org/abs/1807.09097

The current code reflects an empirical comparison of such approaches to the scope of CF algorithm selection. The results reported here will be included in the PhD thesis. The code consists on a series of R scripts, aimed at evaluating the aforementioned frameworks on multiple evaluation scenarios. In order to simplify the experiments, the scripts represent individual experiments, each with a specific scope, aimed to be executed independently. In order to organize the code, a prefix number was used to identify the different steps in the meta-approaches:

- 1: metalevel experiments assessing Kendall's tau performance 
- 2: impact on thee baselevel performance experiments 
- 3: Critical Difference diagrams for statistical validation of results
- 4: Experiments to assess metafeature importance

The remaining items in this repository can be organized as follows:

Folders:
- metafeatures_1000users: metafeatures from competing approaches (more details: https://www.sciencedirect.com/science/article/pii/S0020025517309702)
- metafeatures_landmarkers: subsampling landmarkers metafeatures (proposed in https://link.springer.com/chapter/10.1007/978-3-319-67786-6_14)
- metafeatures_new: more recent metafeatures proposed (all available in: http://arxiv.org/abs/1807.09097)
- results: metalevel evaluation results for all metamodels used
- performance: baselevel performance obtained in multiple evaluation measures: RMSE, NMAE, AUC and NDCG

Other Files:
- auxiliary.R: auxiliary functions to process metadata and metatargets
