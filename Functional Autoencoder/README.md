# Functional Autoencoders

This directory contains the dataset and Python code for implementing the functional autoencoder (FAE) methodology presented in the manuscript "[Functional Autoencoder for Smoothing and Representation Learning](https://link.springer.com/article/10.1007/s11222-023-10287-3.)". For a more comprehensive implementation, including additional features and documentation, please refer to the complete code repository: [Functional Autoencoder (FAE) Github Repository](https://github.com/CedricBeaulac/FAE).

### Data
The **Data** folder contains files realted to the *El Nino* data set, including the actual observations (`ElNino_ERSST.csv`), corresponding timestamps for obsercations (`ElNino_ERSST_tpts.csv`) and custom classification labels (`ElNino_ERSST_label.csv`).

### Files
- `FAE_Read_ElNino_Data.py`: Code for importing and pre-processing the *El Nino* data set.
- `FAE_demo.py`: Code for implementing the functional autoencoders with regularly spaced functional data.
- `FAE_Functions.py`: The main functions used for running the existing and proposed methods. This file needs to be sourced.
