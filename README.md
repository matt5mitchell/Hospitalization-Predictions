Hospitalization Predictions: A Novel Neural Network Architecture
================

Central City Concern is a nonprofit organization in Portland, Oregon that is committed to ending homelessness. The patients we serve in our health care programs have complex needs and are at high risk of hospitalization. In order to target care coordination services to the right people at the right time, we've developed neural network models to predict hospitalizations in the next 30 days.

### The Problem

As a community health center with a relatively small patient population, we do not have enough data to take advantage of the power of most deep learning models. However, standard regression approaches do not necessarily meet our needs because they require tabular data that meet specific assumptions. We were looking for alternative models that would provide the flexibility of deep learning without the risk of over-fitting.

### The Solution

Although our patient populations is relatively small, many of our patients are engaged with us for years. This means that we have a wealth of longitudinal data despite the relatively small population size. To take advantage of this, we proposed taking multiple snapshots of each patient's history to build a large dataset for training deep learning models.

To address the problem of multiple observations per patient \(i.e., repeated measures\), we needed to include patient-level indicator variables in a way that is conceptually similar to mixed-effects regression models. Based on the recommendations of Maity & Pal 2013, we included included a subject-specific input to the final hidden layer of the model. 

Motivated by the desire to take advantage of as much data as possible (and avoid unnecessary aggregation/feature design), we included more than simple feed-forward layers in the architecture. We used two recurrent hidden layers \(GRUs\) to patterns from time series health care utilization data. Additionally, we included a pre-trained population segmentation model \(see [KPOP](https://bitbucket.org/centralcityconcern/kpop)\) to provide nuance to the model.

This model is still in testing, but the early results of this architecture are promising.

Reference
> Maity, T. K., & Pal, A. K. (2013). Subject specific treatment to neural networks for repeated measures analysis. In Proceedings of the International MultiConference of Engineers and Computer Scientists (Vol. 1).