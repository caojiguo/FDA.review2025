# Functional Classification

This directory contains the R code for implementing the functional neural network (FNN) classifier introduced in the manuscript "[Neural Networks as Functional Classifiers](https://doi.org/10.48550/arXiv.2010.04305)", the multiclass functional deep neural network (mfDNN) classifier proposed in "[Multiclass Classification for Multidimensional Functional Data through Deep Neural Networks](https://projecteuclid.org/journals/electronic-journal-of-statistics/volume-18/issue-1/Multiclass-classification-for-multidimensional-functional-data-through-deep-neural-networks/10.1214/24-EJS2229.full)", and other six methods, including functional linear model (FLM), functional nonparametric regression (FNP), functional principal component regression (FPCR), functional partial least squares regression (FPLS), support vector machine (SVM), conventional neural network (NN) for comparison. 

The full implementation of the FNN classifier across various datasets is available in the repository: "[FNN Classifier Github Repository](https://github.com/caojiguo/FunClassifiers)".
For numerical examples of the mfDNN classifier applied to the multi-dimensional functional datasets, please refer to the complete code repository: "[mfDNN Classifier Github Repository](https://github.com/FDASTATAUBURN/mfdnn))".

### Files
- `Classification_demo.py`: Code for reproducing the experiment on the Phoneme dataset.
- `FNN_FunctionsFile.R`: The helper functions file for FNN, which needs to be sourced.
- `mfDNN_FunctionsFile.R`: The helper functions file for mfDNN, which needs to be sourced.
