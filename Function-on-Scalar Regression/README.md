# Neural Networks for Function-on-Scalar Regression

This directory contains the dataset and R implementation for the neural netowrk-based function-on-scalar (FoS) regression model, termed NNBR, introduced in the manuscript "[Neural Networks for Scalar Input and Functional Output](https://doi.org/10.1007/s11222-023-10287-3)". For a more comprehensive mplementation of all neural network-based FoS models developed, including NNBB, NNSS, NNBR & NNSR, please refer to the complete code repository: [Neural network-based FpS Github Repository](https://github.com/sidiwu/NN_SIFO).

### Files

- `asfr.RData`: The age-specific fertility rate (ASFR) data set.
- `FoS_Functions.R`: The helper functions file, which needs to be sourced.
- `FoS_demo.R`: Code for reproducing the numerical experiment on the age-specific fertility rate (ASFR) data set using the neural network-based FoS and the tranditional approach, FoS linear.
