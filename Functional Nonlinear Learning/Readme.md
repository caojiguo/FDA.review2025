The folder contains files to run the functional nonlinear learning method on the starlight data. More details can be found in the paper 

Haixu Wang and Jiguo Cao, 2024, "Functional Nonlinear Learning". Journal of Computational and Graphical Statistics. 33:1, 181-191.

To run the python script: python starlight_compile.py StarLightCurves_TRAIN.tsv StarLightCurves_TEST.txv


Required libraries:

numpy, sklearn, tensorflow


Note that we need to use the version python 3.10.2 and tensorflow 2.8.0


Below are the discription of each file:

* starlight_compile.py: The main python script for running FunNoL. It produces a prediction accuracy on curve labels in test data.
* Starlight_model_compile.h5: a pre-trained FunNoL model.
* StarlightCurves_TRAIN.tsv: Training data
* StarlightCurves_TEST.tsv: Test data