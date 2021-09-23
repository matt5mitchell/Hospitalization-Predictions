##############
## RESEARCH ##
##############
## Evaluate effect of subgroups on risk prediction
## Adaptation of Admit Risk Refresh from fall 2017

###########
## Setup ##
###########

#Libraries
if(!require("tidyverse")){
  install.packages("tidyverse")
}
if(!require("keras")){
  install.packages("keras")
}
if(!require("onehot")){
  install.packages("onehot")
}

library(tidyverse)
library(keras)
library(onehot)

#install_keras() #Requires Anaconda and Python 3.x!!!


###################
## Simulate data ##
###################

## Simulate data for K-POP model training
## Subgroups refined based on LCA models and qualitative analysis

## 19 subgroups (including an uncomplicated "other" subgroup)
## Simulated proportions roughly reflect observed proportions (but more balanced)
## Minimum 1000 rows, maximum 2500 rows

## S1 - schizophrenia, stimulant, high utilizers
S1 <- data.frame(
  kpop = "S1",
  AgeBand = sample(1:5, 1000, replace=TRUE, prob=c(10,35,35,18,2)),
  Income = sample(0:1, 1000, replace=TRUE, prob=c(50,50)),
  EDvisits = sample(1:5, 1000, replace=TRUE, prob=c(0,5,25,30,40)),
  IPmed = sample(1:4, 1000, replace=TRUE, prob=c(60,30,8,2)),
  IPpsych = sample(1:4, 1000, replace=TRUE, prob=c(55,30,10,5)),
  TBI = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1000, replace=TRUE, prob=c(65,35)),
  Schizophrenia = sample(0:1, 1000, replace=TRUE, prob=c(0,1)),
  Bipolar = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Depression = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Alcohol = sample(0:1, 1000, replace=TRUE, prob=c(70,30)),
  Opioid = sample(0:1, 1000, replace=TRUE, prob=c(85,15)),
  StimCocaine = sample(0:1, 1000, replace=TRUE, prob=c(25,75)),
  COPD = sample(0:1, 1000, replace=TRUE, prob=c(90, 10)),
  Diabetes = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  Heart = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  HepC = sample(0:1, 1000, replace=TRUE, prob=c(85,15)),
  Kidney = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Cancer = sample(0:1, 1000, replace=TRUE, prob=c(1,0))
)

## S2 - schizophrenia, older, low utilizers
S2 <- data.frame(
  kpop = "S2",
  AgeBand = sample(1:5, 1500, replace=TRUE, prob=c(1,25,35,35,5)),
  Income = sample(0:1, 1500, replace=TRUE, prob=c(5,95)),
  EDvisits = sample(1:5, 1500, replace=TRUE, prob=c(40,25,15,5,1)),
  IPmed = sample(1:4, 1500, replace=TRUE, prob=c(95,5,0,0)),
  IPpsych = sample(1:4, 1500, replace=TRUE, prob=c(90,10,0,0)),
  TBI = sample(0:1, 1500, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1500, replace=TRUE, prob=c(65,35)),
  Schizophrenia = sample(0:1, 1500, replace=TRUE, prob=c(0,1)),
  Bipolar = sample(0:1, 1500, replace=TRUE, prob=c(1,0)),
  Depression = sample(0:1, 1500, replace=TRUE, prob=c(95,5)),
  Alcohol = sample(0:1, 1500, replace=TRUE, prob=c(55,45)),
  Opioid = sample(0:1, 1500, replace=TRUE, prob=c(90,10)),
  StimCocaine = sample(0:1, 1500, replace=TRUE, prob=c(60,40)),
  COPD = sample(0:1, 1500, replace=TRUE, prob=c(70,30)),
  Diabetes = sample(0:1, 1500, replace=TRUE, prob=c(85,15)),
  Heart = sample(0:1, 1500, replace=TRUE, prob=c(1,0)),
  HepC = sample(0:1, 1500, replace=TRUE, prob=c(80,20)),
  Kidney = sample(0:1, 1500, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 1500, replace=TRUE, prob=c(1,0)),
  Cancer = sample(0:1, 1500, replace=TRUE, prob=c(99,1))
)

## B1a - bipolar, younger, low utilizers
B1a <- data.frame(
  kpop = "B1a",
  AgeBand = sample(1:5, 2500, replace=TRUE, prob=c(15,25,25,15,0)),
  Income = sample(0:1, 2500, replace=TRUE, prob=c(50,50)),
  EDvisits = sample(1:5, 2500, replace=TRUE, prob=c(60,10,5,0,0)),
  IPmed = sample(1:4, 2500, replace=TRUE, prob=c(100,1,0,0)),
  IPpsych = sample(1:4, 2500, replace=TRUE, prob=c(100,1,0,0)),
  TBI = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 2500, replace=TRUE, prob=c(30,70)),
  Schizophrenia = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 2500, replace=TRUE, prob=c(0,1)),
  Depression = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Alcohol = sample(0:1, 2500, replace=TRUE, prob=c(60,40)),
  Opioid = sample(0:1, 2500, replace=TRUE, prob=c(60,40)),
  StimCocaine = sample(0:1, 2500, replace=TRUE, prob=c(60,40)),
  COPD = sample(0:1, 2500, replace=TRUE, prob=c(70,30)),
  Diabetes = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Heart = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  HepC = sample(0:1, 2500, replace=TRUE, prob=c(70,30)),
  Kidney = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Cancer = sample(0:1, 2500, replace=TRUE, prob=c(1,0))
)

## B1b - bipolar, younger, high utilizers
B1b <- data.frame(
  kpop = "B1b",
  AgeBand = sample(1:5, 1000, replace=TRUE, prob=c(15,25,25,15,0)),
  Income = sample(0:1, 1000, replace=TRUE, prob=c(50,50)),
  EDvisits = sample(1:5, 1000, replace=TRUE, prob=c(0,0,5,40,50)),
  IPmed = sample(1:4, 1000, replace=TRUE, prob=c(20,20,30,30)),
  IPpsych = sample(1:4, 1000, replace=TRUE, prob=c(80,15,5,1)),
  TBI = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1000, replace=TRUE, prob=c(30,70)),
  Schizophrenia = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 1000, replace=TRUE, prob=c(0,1)),
  Depression = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Alcohol = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  Opioid = sample(0:1, 1000, replace=TRUE, prob=c(30,70)),
  StimCocaine = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  COPD = sample(0:1, 1000, replace=TRUE, prob=c(70,30)),
  Diabetes = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Heart = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  HepC = sample(0:1, 1000, replace=TRUE, prob=c(70,30)),
  Kidney = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Cancer = sample(0:1, 1000, replace=TRUE, prob=c(1,0))
)

## B2a - bipolar, older, low utilizers
B2a <- data.frame(
  kpop = "B2a",
  AgeBand = sample(1:5, 1000, replace=TRUE, prob=c(0,5,25,50,20)),
  Income = sample(0:1, 1000, replace=TRUE, prob=c(20,80)),
  EDvisits = sample(1:5, 1000, replace=TRUE, prob=c(60,10,5,0,0)),
  IPmed = sample(1:4, 1000, replace=TRUE, prob=c(100,1,0,0)),
  IPpsych = sample(1:4, 1000, replace=TRUE, prob=c(100,1,0,0)),
  TBI = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1000, replace=TRUE, prob=c(30,70)),
  Schizophrenia = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 1000, replace=TRUE, prob=c(0,1)),
  Depression = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Alcohol = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  Opioid = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  StimCocaine = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  COPD = sample(0:1, 1000, replace=TRUE, prob=c(50,50)),
  Diabetes = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  Heart = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  HepC = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  Kidney = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Cancer = sample(0:1, 1000, replace=TRUE, prob=c(99,1))
)

## B2b - bipolar, older, high utilizers
B2b <- data.frame(
  kpop = "B2b",
  AgeBand = sample(1:5, 1000, replace=TRUE, prob=c(0,5,25,50,20)),
  Income = sample(0:1, 1000, replace=TRUE, prob=c(20,80)),
  EDvisits = sample(1:5, 1000, replace=TRUE, prob=c(0,5,50,40,5)),
  IPmed = sample(1:4, 1000, replace=TRUE, prob=c(10,40,30,20)),
  IPpsych = sample(1:4, 1000, replace=TRUE, prob=c(80,15,5,1)),
  TBI = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1000, replace=TRUE, prob=c(30,70)),
  Schizophrenia = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 1000, replace=TRUE, prob=c(0,1)),
  Depression = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Alcohol = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  Opioid = sample(0:1, 1000, replace=TRUE, prob=c(70,30)),
  StimCocaine = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  COPD = sample(0:1, 1000, replace=TRUE, prob=c(50,50)),
  Diabetes = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  Heart = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  HepC = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  Kidney = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Cancer = sample(0:1, 1000, replace=TRUE, prob=c(99,1))
)

## TD1 - trauma, depression, younger
TD1 <- data.frame(
  kpop = "TD1",
  AgeBand = sample(1:5, 1000, replace=TRUE, prob=c(45,45,10,1,0)),
  Income = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  EDvisits = sample(1:5, 1000, replace=TRUE, prob=c(90,5,5,0,0)),
  IPmed = sample(1:4, 1000, replace=TRUE, prob=c(95,5,0,0)),
  IPpsych = sample(1:4, 1000, replace=TRUE, prob=c(100,1,0,0)),
  TBI = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Trauma = sample(0:1, 1000, replace=TRUE, prob=c(10,90)),
  Schizophrenia = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Depression = sample(0:1, 1000, replace=TRUE, prob=c(0,1)),
  Alcohol = sample(0:1, 1000, replace=TRUE, prob=c(50,50)),
  Opioid = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  StimCocaine = sample(0:1, 1000, replace=TRUE, prob=c(30,70)),
  COPD = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  Diabetes = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Heart = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  HepC = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  Kidney = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Cancer = sample(0:1, 1000, replace=TRUE, prob=c(1,0))
)

## TD2 - trauma, depression, older
TD2 <- data.frame(
  kpop = "TD2",
  AgeBand = sample(1:5, 2500, replace=TRUE, prob=c(0,20,30,40,10)),
  Income = sample(0:1, 2500, replace=TRUE, prob=c(60,40)),
  EDvisits = sample(1:5, 2500, replace=TRUE, prob=c(40,25,15,10,1)),
  IPmed = sample(1:4, 2500, replace=TRUE, prob=c(90,10,0,0)),
  IPpsych = sample(1:4, 2500, replace=TRUE, prob=c(97,3,0,0)),
  TBI = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 2500, replace=TRUE, prob=c(20,80)),
  Schizophrenia = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Depression = sample(0:1, 2500, replace=TRUE, prob=c(0,1)),
  Alcohol = sample(0:1, 2500, replace=TRUE, prob=c(60,40)),
  Opioid = sample(0:1, 2500, replace=TRUE, prob=c(60,40)),
  StimCocaine = sample(0:1, 2500, replace=TRUE, prob=c(60,40)),
  COPD = sample(0:1, 2500, replace=TRUE, prob=c(70,30)),
  Diabetes = sample(0:1, 2500, replace=TRUE, prob=c(90,10)),
  Heart = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  HepC = sample(0:1, 2500, replace=TRUE, prob=c(60,40)),
  Kidney = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Cancer = sample(0:1, 2500, replace=TRUE, prob=c(99,1))
)

## O1 - opioid, younger
O1 <- data.frame(
  kpop = "O1",
  AgeBand = sample(1:5, 2500, replace=TRUE, prob=c(40,50,10,0,0)),
  Income = sample(0:1, 2500, replace=TRUE, prob=c(80,20)),
  EDvisits = sample(1:5, 2500, replace=TRUE, prob=c(35,25,15,15,10)),
  IPmed = sample(1:4, 2500, replace=TRUE, prob=c(75,10,10,5)),
  IPpsych = sample(1:4, 2500, replace=TRUE, prob=c(1,0,0,0)),
  TBI = sample(0:1, 2500, replace=TRUE, prob=c(100,1)),
  Trauma = sample(0:1, 2500, replace=TRUE, prob=c(75,25)),
  Schizophrenia = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Depression = sample(0:1, 2500, replace=TRUE, prob=c(55,45)),
  Alcohol = sample(0:1, 2500, replace=TRUE, prob=c(90,10)),
  Opioid = sample(0:1, 2500, replace=TRUE, prob=c(0,1)),
  StimCocaine = sample(0:1, 2500, replace=TRUE, prob=c(70,30)),
  COPD = sample(0:1, 2500, replace=TRUE, prob=c(90,10)),
  Diabetes = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Heart = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  HepC = sample(0:1, 2500, replace=TRUE, prob=c(65,35)),
  Kidney = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Cancer = sample(0:1, 2500, replace=TRUE, prob=c(1,0))
)

## O2 - opioid, older
O2 <- data.frame(
  kpop = "O2",
  AgeBand = sample(1:5, 1500, replace=TRUE, prob=c(0,0,30,50,20)),
  Income = sample(0:1, 1500, replace=TRUE, prob=c(40,60)),
  EDvisits = sample(1:5, 1500, replace=TRUE, prob=c(35,25,15,15,10)),
  IPmed = sample(1:4, 1500, replace=TRUE, prob=c(65,20,10,5)),
  IPpsych = sample(1:4, 1500, replace=TRUE, prob=c(100,1,0,0)),
  TBI = sample(0:1, 1500, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1500, replace=TRUE, prob=c(60,40)),
  Schizophrenia = sample(0:1, 1500, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 1500, replace=TRUE, prob=c(1,0)),
  Depression = sample(0:1, 1500, replace=TRUE, prob=c(50,50)),
  Alcohol = sample(0:1, 1500, replace=TRUE, prob=c(70,30)),
  Opioid = sample(0:1, 1500, replace=TRUE, prob=c(0,1)),
  StimCocaine = sample(0:1, 1500, replace=TRUE, prob=c(70,30)),
  COPD = sample(0:1, 1500, replace=TRUE, prob=c(70,30)),
  Diabetes = sample(0:1, 1500, replace=TRUE, prob=c(85,15)),
  Heart = sample(0:1, 1500, replace=TRUE, prob=c(85,15)),
  HepC = sample(0:1, 1500, replace=TRUE, prob=c(40,60)),
  Kidney = sample(0:1, 1500, replace=TRUE, prob=c(95,5)),
  Liver = sample(0:1, 1500, replace=TRUE, prob=c(90,10)),
  Cancer = sample(0:1, 1500, replace=TRUE, prob=c(99,1))
)

## AD1 - alcohol, depression, all ages (but younger)
AD1 <- data.frame(
  kpop = "AD1",
  AgeBand = sample(1:5, 1500, replace=TRUE, prob=c(30,35,25,10,0)),
  Income = sample(0:1, 1500, replace=TRUE, prob=c(40,60)),
  EDvisits = sample(1:5, 1500, replace=TRUE, prob=c(65,10,10,5,10)),
  IPmed = sample(1:4, 1500, replace=TRUE, prob=c(80,10,10,1)),
  IPpsych = sample(1:4, 1500, replace=TRUE, prob=c(1,0,0,0)),
  TBI = sample(0:1, 1500, replace=TRUE, prob=c(100,1)),
  Trauma = sample(0:1, 1500, replace=TRUE, prob=c(75,25)),
  Schizophrenia = sample(0:1, 1500, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 1500, replace=TRUE, prob=c(90,10)),
  Depression = sample(0:1, 1500, replace=TRUE, prob=c(40,60)),
  Alcohol = sample(0:1, 1500, replace=TRUE, prob=c(0,1)),
  Opioid = sample(0:1, 1500, replace=TRUE, prob=c(85,15)),
  StimCocaine = sample(0:1, 1500, replace=TRUE, prob=c(60,40)),
  COPD = sample(0:1, 1500, replace=TRUE, prob=c(70,30)),
  Diabetes = sample(0:1, 1500, replace=TRUE, prob=c(70,30)),
  Heart = sample(0:1, 1500, replace=TRUE, prob=c(80,20)),
  HepC = sample(0:1, 1500, replace=TRUE, prob=c(80,20)),
  Kidney = sample(0:1, 1500, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 1500, replace=TRUE, prob=c(80,20)),
  Cancer = sample(0:1, 1500, replace=TRUE, prob=c(1,0))
)

## AD2a - alcohol, depression, older, low utilizer
AD2a <- data.frame(
  kpop = "AD2a",
  AgeBand = sample(1:5, 2500, replace=TRUE, prob=c(0,5,25,40,30)),
  Income = sample(0:1, 2500, replace=TRUE, prob=c(40,60)),
  EDvisits = sample(1:5, 2500, replace=TRUE, prob=c(70,20,5,5,1)),
  IPmed = sample(1:4, 2500, replace=TRUE, prob=c(95,5,1,0)),
  IPpsych = sample(1:4, 2500, replace=TRUE, prob=c(1,0,0,0)),
  TBI = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 2500, replace=TRUE, prob=c(65,35)),
  Schizophrenia = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 2500, replace=TRUE, prob=c(100,1)),
  Depression = sample(0:1, 2500, replace=TRUE, prob=c(0,1)),
  Alcohol = sample(0:1, 2500, replace=TRUE, prob=c(30,70)),
  Opioid = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  StimCocaine = sample(0:1, 2500, replace=TRUE, prob=c(85,15)),
  COPD = sample(0:1, 2500, replace=TRUE, prob=c(70,30)),
  Diabetes = sample(0:1, 2500, replace=TRUE, prob=c(85,15)),
  Heart = sample(0:1, 2500, replace=TRUE, prob=c(90,10)),
  HepC = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Kidney = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Cancer = sample(0:1, 2500, replace=TRUE, prob=c(99,1))
)

## AD2b - alcohol, depression, older, high utilizer
AD2b <- data.frame(
  kpop = "AD2b",
  AgeBand = sample(1:5, 1000, replace=TRUE, prob=c(0,5,25,40,30)),
  Income = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  EDvisits = sample(1:5, 1000, replace=TRUE, prob=c(0,0,35,15,50)),
  IPmed = sample(1:4, 1000, replace=TRUE, prob=c(25,35,35,5)),
  IPpsych = sample(1:4, 1000, replace=TRUE, prob=c(70,30,0,0)),
  TBI = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1000, replace=TRUE, prob=c(65,35)),
  Schizophrenia = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 1000, replace=TRUE, prob=c(100,1)),
  Depression = sample(0:1, 1000, replace=TRUE, prob=c(0,1)),
  Alcohol = sample(0:1, 1000, replace=TRUE, prob=c(30,70)),
  Opioid = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  StimCocaine = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  COPD = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  Diabetes = sample(0:1, 1000, replace=TRUE, prob=c(85,15)),
  Heart = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  HepC = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Kidney = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Cancer = sample(0:1, 1000, replace=TRUE, prob=c(99,1))
)

## SD1 - stimulant, depression, younger
SD1 <- data.frame(
  kpop = "SD1",
  AgeBand = sample(1:5, 1000, replace=TRUE, prob=c(25,50,25,0,0)),
  Income = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  EDvisits = sample(1:5, 1000, replace=TRUE, prob=c(50,25,20,5,1)),
  IPmed = sample(1:4, 1000, replace=TRUE, prob=c(90,10,1,0)),
  IPpsych = sample(1:4, 1000, replace=TRUE, prob=c(1,0,0,0)),
  TBI = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1000, replace=TRUE, prob=c(65,35)),
  Schizophrenia = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  Depression = sample(0:1, 1000, replace=TRUE, prob=c(30,70)),
  Alcohol = sample(0:1, 1000, replace=TRUE, prob=c(50,50)),
  Opioid = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  StimCocaine = sample(0:1, 1000, replace=TRUE, prob=c(0,1)),
  COPD = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  Diabetes = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  Heart = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  HepC = sample(0:1, 1000, replace=TRUE, prob=c(75,25)),
  Kidney = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Cancer = sample(0:1, 1000, replace=TRUE, prob=c(1,0))
)

## SD2 - stimulant, depression, older
SD2 <- data.frame(
  kpop = "SD2",
  AgeBand = sample(1:5, 1000, replace=TRUE, prob=c(0,5,25,50,20)),
  Income = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  EDvisits = sample(1:5, 1000, replace=TRUE, prob=c(50,10,35,5,1)),
  IPmed = sample(1:4, 1000, replace=TRUE, prob=c(90,10,1,0)),
  IPpsych = sample(1:4, 1000, replace=TRUE, prob=c(1,0,0,0)),
  TBI = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1000, replace=TRUE, prob=c(65,35)),
  Schizophrenia = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  Depression = sample(0:1, 1000, replace=TRUE, prob=c(30,70)),
  Alcohol = sample(0:1, 1000, replace=TRUE, prob=c(50,50)),
  Opioid = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  StimCocaine = sample(0:1, 1000, replace=TRUE, prob=c(0,1)),
  COPD = sample(0:1, 1000, replace=TRUE, prob=c(80,20)),
  Diabetes = sample(0:1, 1000, replace=TRUE, prob=c(75,25)),
  Heart = sample(0:1, 1000, replace=TRUE, prob=c(75,25)),
  HepC = sample(0:1, 1000, replace=TRUE, prob=c(75,25)),
  Kidney = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Liver = sample(0:1, 1000, replace=TRUE, prob=c(1,0)),
  Cancer = sample(0:1, 1000, replace=TRUE, prob=c(99,1))
)

## M3a - medical disease, older, low utilizer
M3a <- data.frame(
  kpop = "M3a",
  AgeBand = sample(1:5, 2500, replace=TRUE, prob=c(0,1,5,40,55)),
  Income = sample(0:1, 2500, replace=TRUE, prob=c(20,80)),
  EDvisits = sample(1:5, 2500, replace=TRUE, prob=c(80,15,5,0,0)),
  IPmed = sample(1:4, 2500, replace=TRUE, prob=c(1,0,0,0)),
  IPpsych = sample(1:4, 2500, replace=TRUE, prob=c(1,0,0,0)),
  TBI = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Trauma = sample(0:1, 2500, replace=TRUE, prob=c(90,10)),
  Schizophrenia = sample(0:1, 2500, replace=TRUE, prob=c(90,10)),
  Bipolar = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Depression = sample(0:1, 2500, replace=TRUE, prob=c(70,30)),
  Alcohol = sample(0:1, 2500, replace=TRUE, prob=c(80,20)),
  Opioid = sample(0:1, 2500, replace=TRUE, prob=c(100,1)),
  StimCocaine = sample(0:1, 2500, replace=TRUE, prob=c(100,1)),
  COPD = sample(0:1, 2500, replace=TRUE, prob=c(80,20)),
  Diabetes = sample(0:1, 2500, replace=TRUE, prob=c(40,60)),
  Heart = sample(0:1, 2500, replace=TRUE, prob=c(60,40)),
  HepC = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Kidney = sample(0:1, 2500, replace=TRUE, prob=c(80,20)),
  Liver = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Cancer = sample(0:1, 2500, replace=TRUE, prob=c(90,10))
)

## M3b - medical disease, older, moderate utilizer
M3b <- data.frame(
  kpop = "M3b",
  AgeBand = sample(1:5, 1000, replace=TRUE, prob=c(0,1,5,40,55)),
  Income = sample(0:1, 1000, replace=TRUE, prob=c(20,80)),
  EDvisits = sample(1:5, 1000, replace=TRUE, prob=c(10,40,40,10,0)),
  IPmed = sample(1:4, 1000, replace=TRUE, prob=c(60,30,10,0)),
  IPpsych = sample(1:4, 1000, replace=TRUE, prob=c(1,0,0,0)),
  TBI = sample(0:1, 1000, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  Schizophrenia = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  Bipolar = sample(0:1, 1000, replace=TRUE, prob=c(85,15)),
  Depression = sample(0:1, 1000, replace=TRUE, prob=c(30,70)),
  Alcohol = sample(0:1, 1000, replace=TRUE, prob=c(50,50)),
  Opioid = sample(0:1, 1000, replace=TRUE, prob=c(40,60)),
  StimCocaine = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  COPD = sample(0:1, 1000, replace=TRUE, prob=c(60,40)),
  Diabetes = sample(0:1, 1000, replace=TRUE, prob=c(70,30)),
  Heart = sample(0:1, 1000, replace=TRUE, prob=c(70,30)),
  HepC = sample(0:1, 1000, replace=TRUE, prob=c(75,25)),
  Kidney = sample(0:1, 1000, replace=TRUE, prob=c(90,10)),
  Liver = sample(0:1, 1000, replace=TRUE, prob=c(70,30)),
  Cancer = sample(0:1, 1000, replace=TRUE, prob=c(90,10))
)

## M3c - medical disease, older, high utilizer
M3c <- data.frame(
  kpop = "M3c",
  AgeBand = sample(1:5, 2000, replace=TRUE, prob=c(0,1,5,40,55)),
  Income = sample(0:1, 2000, replace=TRUE, prob=c(20,80)),
  EDvisits = sample(1:5, 2000, replace=TRUE, prob=c(0,0,30,40,30)),
  IPmed = sample(1:4, 2000, replace=TRUE, prob=c(0,30,30,40)),
  IPpsych = sample(1:4, 2000, replace=TRUE, prob=c(98,2,0,0)),
  TBI = sample(0:1, 2000, replace=TRUE, prob=c(95,5)),
  Trauma = sample(0:1, 2000, replace=TRUE, prob=c(75,25)),
  Schizophrenia = sample(0:1, 2000, replace=TRUE, prob=c(80,20)),
  Bipolar = sample(0:1, 2000, replace=TRUE, prob=c(95,5)),
  Depression = sample(0:1, 2000, replace=TRUE, prob=c(50,50)),
  Alcohol = sample(0:1, 2000, replace=TRUE, prob=c(65,35)),
  Opioid = sample(0:1, 2000, replace=TRUE, prob=c(80,20)),
  StimCocaine = sample(0:1, 2000, replace=TRUE, prob=c(80,20)),
  COPD = sample(0:1, 2000, replace=TRUE, prob=c(60,40)),
  Diabetes = sample(0:1, 2000, replace=TRUE, prob=c(70,30)),
  Heart = sample(0:1, 2000, replace=TRUE, prob=c(60,40)),
  HepC = sample(0:1, 2000, replace=TRUE, prob=c(75,25)),
  Kidney = sample(0:1, 2000, replace=TRUE, prob=c(85,15)),
  Liver = sample(0:1, 2000, replace=TRUE, prob=c(90,10)),
  Cancer = sample(0:1, 2000, replace=TRUE, prob=c(90,10))
)

## U1 - uncomplicated, all ages
U <- data.frame(
  kpop = "U",
  AgeBand = sample(1:5, 2500, replace=TRUE, prob=c(15,25,25,25,10)),
  Income = sample(0:1, 2500, replace=TRUE, prob=c(50,50)),
  EDvisits = sample(1:5, 2500, replace=TRUE, prob=c(70,20,10,1,0)),
  IPmed = sample(1:4, 2500, replace=TRUE, prob=c(100,0,0,0)),
  IPpsych = sample(1:4, 2500, replace=TRUE, prob=c(100,0,0,0)),
  TBI = sample(0:1, 2500, replace=TRUE, prob=c(100,1)),
  Trauma = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Schizophrenia = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Bipolar = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Depression = sample(0:1, 2500, replace=TRUE, prob=c(90,10)),
  Alcohol = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Opioid = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  StimCocaine = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  COPD = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Diabetes = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Heart = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  HepC = sample(0:1, 2500, replace=TRUE, prob=c(95,5)),
  Kidney = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Liver = sample(0:1, 2500, replace=TRUE, prob=c(1,0)),
  Cancer = sample(0:1, 2500, replace=TRUE, prob=c(1,0))
)

## Create final simulated training dataset
#Bind rows
sim_subgroup <- rbind(S1, S2, B1a, B1b, B2a, B2b, TD1, TD2, O1, O2, AD1, AD2a, AD2b, SD1, SD2, M3a, M3b, M3c, U)

#Create factors
sim_subgroup$kpop <- as.factor(sim_subgroup$kpop)
sim_subgroup$kpop <- relevel(sim_subgroup$kpop, "U")
sim_subgroup$AgeBand <- factor(sim_subgroup$AgeBand, labels=c("18-29", "30-39", "40-49", "50-59", "60+"))
sim_subgroup$EDvisits <- factor(sim_subgroup$EDvisits, labels=c("0", "1", "2-3", "4-6", "7+"))
sim_subgroup$IPmed <- factor(sim_subgroup$IPmed, labels=c("0", "1", "2", "3+"))
sim_subgroup$IPpsych <- factor(sim_subgroup$IPpsych, labels=c("0", "1", "2", "3+"))
sim_subgroup[,c(3,7:21)] <- lapply(sim_subgroup[,c(3,7:21)], factor)

#One-hot encode
encode_subgroup <- onehot(sim_subgroup, max_levels = nlevels(sim_subgroup$kpop))
sim_subgroup_onehot <- predict(encode_subgroup, sim_subgroup)

#Shuffle data
set.seed(193)
shuffle <- sample(nrow(sim_subgroup_onehot))
sim_subgroup_shuffled <- sim_subgroup_onehot[shuffle,]

#Define training and testing datasets
set.seed(44)
inTraining <- sample(1:nrow(sim_subgroup_shuffled), round(nrow(sim_subgroup_shuffled)*.75))

#Data as matrices
x_train <- sim_subgroup_shuffled[inTraining, (nlevels(sim_subgroup$kpop)+1):ncol(sim_subgroup_shuffled)]
x_test <- sim_subgroup_shuffled[-inTraining, (nlevels(sim_subgroup$kpop)+1):ncol(sim_subgroup_shuffled)]

y_train <- sim_subgroup_shuffled[inTraining, 1:nlevels(sim_subgroup$kpop)]
y_test <- sim_subgroup_shuffled[-inTraining, 1:nlevels(sim_subgroup$kpop)]


#Define model
input_shape <- ncol(x_train)
output_units <- ncol(y_train)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 64, activation = "relu", input_shape = input_shape,
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = "relu",
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = output_units, activation="softmax")

#Compile model
model %>% compile(
  loss = 'categorical_crossentropy',
  #optimizer = optimizer_sgd(lr=.01, momentum=.95, decay=0),
  #optimizer = optimizer_rmsprop(),
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

#Train model
history <- model %>% fit(
  x_train, y_train, 
  epochs = 10, batch_size = 128, 
  validation_split = 0.2
)

#Evaluate model
model %>% evaluate(x_test, y_test)

#Save model
#model %>% save_model_hdf5("subgroup_model.h5")
