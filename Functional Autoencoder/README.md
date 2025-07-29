# Functional Autoencoders

This directory contains the dataset and Python code for implementing the functional autoencoder (FAE) methodology presented in the manuscript "[Functional Autoencoder for Smoothing and Representation Learning](https://doi.org/10.1007/s11222-024-10501-w)" and the conventioanl FPCA. For a more comprehensive implementation of FAE, including additional features and documentation, please refer to the complete code repository: [Functional Autoencoder (FAE) Github Repository](https://github.com/CedricBeaulac/FAE).

### Data
The **Datasets** folder contains files realted to the *El Niño* data set, including the actual observations (`ElNino_ERSST.csv`), corresponding timestamps for obsercations (`ElNino_ERSST_tpts.csv`) and custom classification labels (`ElNino_ERSST_label.csv`).

### Files
- `FAE_Read_ElNino_Data.py`: Code for importing and pre-processing the *El Niño* data set.
- `FAE_demo.py`: Code for reproducing the numerical experiment with the *El Niño* dataset.
- `FAE_Functions.py`: The helper functions file, which needs to be sourced.
