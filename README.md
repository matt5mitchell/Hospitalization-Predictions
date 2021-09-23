Hospitalization Predictions: A Novel Neural Network Architecture
================

The patients we serve by community health centers have complex needs and are at high risk of hospitalization. In order to target care coordination services to the right people at the right time, I've developed neural network models to predict hospitalizations in the next 30 days.

### The Problem

Community health centers often do not have enough data to take advantage of the power of most deep learning models. However, standard regression approaches do not necessarily meet our needs because they require tabular data that meet specific assumptions. I was looking for alternative models that would provide the flexibility of deep learning without the risk of over-fitting.

### The Solution

Although the patient populations can be relatively small, many patients are engaged for years. This means that we have a wealth of longitudinal data despite the relatively small population size. To take advantage of this, we proposed taking multiple snapshots of each patient's history to build a large dataset for training deep learning models.

To address the problem of multiple observations per patient \(i.e., repeated measures\), I needed to include patient-level indicator variables in a way that is conceptually similar to mixed-effects regression models. Based on the recommendations of Maity & Pal 2013, I included included a subject-specific input to the final hidden layer of the model. 

Motivated by the desire to take advantage of as much data as possible (and avoid unnecessary aggregation/feature design), I included more than simple feed-forward layers in the architecture. I used two recurrent hidden layers \(GRUs\) to patterns from time series health care utilization data. Additionally, I included a pre-trained population segmentation model \(see [Populatin Segmentation](https://github.com/matt5mitchell/Population-Segmentation)\) to provide nuance to the model.

This model is still a pilot, but the early results of this architecture are promising.

Reference
> Maity, T. K., & Pal, A. K. (2013). Subject specific treatment to neural networks for repeated measures analysis. In Proceedings of the International MultiConference of Engineers and Computer Scientists (Vol. 1).
