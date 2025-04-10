# Neural Networks for Function-on-Scalar Regression

This directory contains the dataset and R code for implementing the neural netowrk-based function-on-scalar regression models (NNBB, NNSS, NNBR & NNSR) proposed in the manuscript "[Neural Networks for Scalar Input and Functional Output](https://link.springer.com/article/10.1007/s11222-023-10287-3)".

### Files

- `asfr.RData`: The age-specific fertility rate (ASFR) data set.
- `FoS_reg_functions.R`: The main functions to impletment NNBB, NNSS, NNBR & NNSR, along with two existing models (FoS linear and FAM). This file needs to be sourced.
- `FoS_reg_demo.R`: Code for analyzing the age-specific fertility rate data set using the NN-based models (NNBB, NNSS, NNBR & NNSR) and the tranditional approaches (FoS linear and FAM).
