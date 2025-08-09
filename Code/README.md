# This folder contains all source code used in the manuscript

- **GridSearch for AI-based Approaches**: 
  - *ANN*: mlp_nnet_perf_GridSearch.R
  - *GMDH*: GMDHreg_perf_GridSearch.R
  - *SVR*: ksvm_kernlab_perf_GridSearch.R
  - *RF*: randomForest_perf_GridSearch.R

- **100 trials-based Performance of AI-based Approaches (RMSE, MAE, MAPE, R-Squared, and CC)**: 
  - *ANN*: mlp_nnet_perf_MoreMetrics.R
  - *GMDH*: GMDHreg_perf_MoreMetrics.R
  - *SVR*: ksvm_kernlab_perf_MoreMetrics.R
  - *RF*: randomForest_perf_MoreMetrics.R

- **4-fold CV-based Performance of AI-based Approaches (RMSE, MAE, MAPE, R-Squared, and CC)**: 
  - *ANN*: mlp_nnet_perf_kfold.R
  - *GMDH*: GMDHreg_perf_kfold.R
  - *SVR*: ksvm_kernlab_perf_kfold.R
  - *RF*: randomForest_perf_kfold.R
    
- **Performance of Empirical Equations (RMSE, CC)**: empericalEquations_perf.R
  - *For Culvert*: Lim1995, Abt1985, Emami2010, Taha2020
  - *For Sluice*: Chatterjee1994, Hopfinger2004, Sarkar2005, Dey2006

- **Parameter Importance Strategies**:
  - *VBSA*: 
    - *ANN*: mlp_nnet_sa_VBSA.R
    - *GMDH*: GMDHreg__sa_VBSA.R
    - *SVR*: ksvm_kernlab_sa_VBSA.R
    - *RF*: randomForest_sa_VBSA.R 
  - *DCI*: 
    - *ANN*: mlp_nnet_sa_DCI.R
    - *GMDH*: GMDHreg_sa_DCI.R
    - *SVR*: ksvm_kernlab_sa_DCI
    - *RF*: randomForest_sa_DCI.R  
        
- **Summary**: To summarize prediction performance and important parameters and visualize those in charts
  - *Performance for all*: Summarize_Performance.R
  - *Performance for GMDH (degree = 1 and 2)*: Summarize_Performance_GMDH.R
  - *VBSA*: Summarize_VBSA_final.R
  - *DCI*: Summarize_DCI_final.R

