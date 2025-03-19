# This folder contains all source code used in the manuscript

- **Performance of AI-based Approaches (RMSE, CC)**: 
  - *ANN*: mlp_nnet_perf.R
  - *GMDH*: GMDHreg_perf_n_sa_VBSA.R
  - *SVR*: ksvm_kernlab_perf.R
  - *RF*: randomForest_perf_n_sa_VBSA.R
 
- **erformance of Empirical Equations (RMSE, CC)**: empericalEquations_perf.R
  - *For Culvert*: Lim1995, Abt1985, Emami2010, Taha2020
  - *For Sluice*: Chatterjee1994, Hopfinger2004, Sarkar2005, Dey2006

- **Parameter Importance Strategies**:
  - *VBSA*: 
    - *ANN*: mlp_nnet_sa_VBSA.R
    - *GMDH*: GMDHreg_perf_n_sa_VBSA.R
    - *SVR*: ksvm_kernlab_sa_VBSA.R
    - *RF*: randomForest_perf_n_sa_VBSA.R 
  - *DCI*: GMDHreg_perf_n_sa_VBSA.R
    - *ANN*: mlp_nnet_sa_DCI.R
    - *GMDH*: GMDHreg_sa_DCI.R
    - *SVR*: ksvm_kernlab_sa_DCI
    - *RF*: randomForest_sa_DCI.R  
        
- **Summary**: To summarize prediction performance and important parameters and visualize those in charts
  - *Performance (RMSE, CC): Summarize_Performance.R
  - *VBSA*: Summarize_VBSA_v2.R
  - *DCI: Summarize_DCI_v2.R

