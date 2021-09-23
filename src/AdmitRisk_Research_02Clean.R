##############
## RESEARCH ##
##############
## Evaluate effect of subgroups on risk prediction
## Adaptation of Admit Risk Refresh from fall 2017

###########
## Setup ##
###########

#Libraries
if(!require("lubridate")){
  install.packages("lubridate")
}
if(!require("tidyverse")){
  install.packages("tidyverse")
}
if(!require("eeptools")){
  install.packages("eeptools")
}
if(!require("onehot")){
  install.packages("onehot")
}

library(lubridate)
library(tidyverse)
library(eeptools)
library(onehot)

n_obs <- 12 #number of monthly observations
n_lookback <- 365 #number of days of utiliation history
n_followup <- 30 #number of days in follow-up period (outcome)

## Dates ##

#Reference date (based on refdatekey from 01Load file)
refdate <- ymd(refdatekey)

#Format hospital AdmitDateKey as date
RHosp$AdmitDateKey <- ymd(RHosp$AdmitDateKey)

#Format StartDateKey as date
RAppts$StartDateKey <- ymd(RAppts$StartDateKey)


########################
## Client Vars Part 1 ##
########################

## Start with client demographics ##

#Remove any duplicate client rows
client <- client %>%
  group_by(PatientProfileId) %>%
  filter(row_number()==1)

#Format DOB as date
client$BirthDate <- ymd(client$BirthDate) 

#Create dichotomous income variables
client$Income <- as.factor(ifelse(client$MonthlyIncome>0, 1, 0))

#Dichotomous HomelessYesNo variable
client$HomelessYesNo <- factor(ifelse(client$HomelessYesNo=="Yes", "Yes", "No"))

clientlist <- list()
#Loop to ensure age is accurate for each snapshot
for (i in 1:n_obs) {
  
  #Set reference date back "mm" months from startdate
  mm <- i - 1
  loopdate <- refdate %m+% months(mm)
  
  #Create client age and agebands
  client_tmp <- client
  client_tmp$Age <- floor(eeptools::age_calc(client_tmp$BirthDate, loopdate, units="years")) #Calculate age with eeptools (accounts for leap years)
  client_tmp$AgeBand <- as.factor(ifelse(client_tmp$Age >= 18 & client_tmp$Age < 30, "18-29",
                                     ifelse(client_tmp$Age >= 30 & client_tmp$Age < 40, "30-39",
                                            ifelse(client_tmp$Age >= 40 & client_tmp$Age < 50, "40-49",
                                                   ifelse(client_tmp$Age >= 50 & client_tmp$Age < 60, "50-59",
                                                          "60+")))))
  #Snapshot (month) number
  client_tmp$Snapshot <- i
  
  #Add to list
  clientlist[[i]] <- client_tmp
}

#Combine into single data frame
client2 <- bind_rows(clientlist) %>% 
  arrange(PatientProfileId, Snapshot) #for consistency, arrange by PPID, then snapshot date



## Next, diagnoses ##

#Length of each data frame in the list of diagnosis snapshots
dxlength <- lapply(diag, nrow)

#Combine diagnosis snapshots into single data frame
diag2 <- bind_rows(diag)

#Add snapshot (month) number
dxsnapshot <- list()
for (i in 1:n_obs) {dxsnapshot[[i]] <- rep(i, times=dxlength[i])} #dxlength is variable, hence this step
diag2$Snapshot <- unlist(dxsnapshot)

#Rename diagnosis variables to simplify and remove spaces
#Note: "rename" can't handle all of the columns at once
diag2 <- rename(diag2, 
                   Abscess = `Abscess, Cellulitis, and Lymphangitis`, 
                   AcuteMI=`Acute Myocardial Infarction`,
                   RenalFailure=`Acute Renal Failure`,
                   Alcohol=`Alcohol Use`, 
                   Ostomy=`Artificial Openings for Feeding or Elimination`, 
                   COPD=`Asthma and COPD`, 
                   Bipolar=`Bipolar Disorders`,
                   BreastProstateCA=`Breast, Prostate, and Other Cancers and Tumors`,
                   Arrest=`Cardio-Respiratory Failure and Shock`,
                   Hemorrhage=`Cerebral Hemorrhage`,
                   CKD4=`Chronic Kidney Disease (Stage 4)`,
                   CKD5=`Chronic Kidney Disease (Stage 5)`,
                   COPC=`Chronic Overlapping Pain Conditions`,
                   Pancreatitis=`Chronic Pancreatitis`,
                   Ulcer=`Chronic Ulcer of Skin, Except Pressure`,
                   Cirrhosis=`Cirrhosis of Liver`,
                   Coagulation=`Coagulation Defects and Other Specified Hematological Disorders`,
                   Cocaine=`Cocaine Use`,
                   ColorectalCA=`Colorectal, Bladder, and Other Cancers`,
                   Coma=`Coma and Brain Compression`,
                   CirrhosisComp=`Complications of Cirrhosis`,
                   CHF=`Congestive Heart Failure`,
                   CVDDeficits=`Deficits from Cerebrovascular Disease`,
                   DegenDisc=`Degenerative Disc Disease and Spondylosis`,
                   Developmental=`Developmental Disorder`,
                   DiabetesCompChronic=`Diabetes with Chronic Complications`,
                   DiabetesUncomp=`Diabetes without Complication`,
                   Immunity=`Disorders of Immunity`,
                   FailureThrive=`Failure to Thrive`,
                   Arrhythmias=`Heart Arrhythmias`,
                   HepC=`Hepatitis C`,
                   HIV=`HIV/AIDS`,
                   IVABX=`Infection Requiring Long-Term IV Antibiotics`,
                   ConnectiveTissue=`Inflammatory Connective Tissue Disease`,
                   IntestinalObstruct=`Intestinal Obstruction or Perforation`,
                   IHD=`Ischemic Heart Disease`,
                   Stroke=`Ischemic or Unspecified Stroke`,
                   LungCA=`Lung and Other Severe Cancers`,
                   Lymphoma=`Lymphoma and Other Cancers`,
                   Metastatic=`Metastatic Cancer and Acute Leukemia`,
                   NeuroDegen=`Neurodegenerative Disease`,
                   Opioid=`Opioid Use`,
                   ChronicPain=`Other Chronic Pain`,
                   Paralysis=`Paralysis or Plegia`,
                   Parkinsons=`Parkinson's and Huntington's Diseases`,
                   Peripheral=`Peripheral Vascular Disease`,
                   PeripheralComp=`Peripheral Vascular Disease with Complications`,
                   PressureUlcer=`Pressure Ulcer`,
                   RespArrest=`Respiratory Arrest`,
                   Schizophrenia=`Schizophrenia and Psychosis`,
                   Sedative=`Sedative, Hypnotic, or Anxiolytic Use`,
                   Seizure=`Seizure Disorders and Convulsions`,
                   SelfHarm=`Self-Harm Behaviors`,
                   Sepsis=`Sepsis and Other Infection`,
                   TBI=`Severe Head Injury`,
                   Blood=`Severe Hematological Disorders`,
                   Burn=`Severe Skin Burn or Condition`,
                   Stimulant=`Stimulant Use`,
                   SubstDementia=`Substance-Induced Dementia`,
                   Tobacco=`Tobacco Use`,
                   Trauma=`Trauma-Related Disorders`)
diag2 <- rename(diag2, 
                   OtherCVD=`Other Cerebrovascular Disease`,
                   Concussion=`Concussion and Other Head Injury`,
                   Dementia=`Other Dementia/Impairment`,
                   InflamBowel=`Inflammatory Bowel Disease`,
                   HipFx=`Hip Fracture/Dislocation`,
                   SpinalInjury=`Spinal Cord Injury`,
                   VertebralFx=`Vertebral Fractures without Spinal Cord Injury`,
                   OtherCKD=`Other Chronic Kidney Disease`,
                   DiabCompAcute=`Diabetes with Acute Complications`,
                   HepatitisChronic=`Chronic Hepatitis`,
                   MacularDegen=`Exudative Macular Degeneration`,
                   VitrHemorrhage=`Vitreous Hemorrhage`,
                   CysticFibrosis=`Cystic Fibrosis`,
                   OtherLungChronic=`Fibrosis of Lung and Other Chronic Lung Disorders`,
                   RespiratorTrach=`Respirator Dependence/Tracheostomy Status`,
                   MorbidObesity=`Morbid Obesity`,
                   OtherEndocrine=`Other Significant Endocrine and Metabolic Disorders`,
                   Malnutrition=`Protein-Calorie Malnutrition`,
                   CerbralPalsy=`Cerebral Palsy`,
                   MS=`Multiple Sclerosis`,
                   OtherNeuro=`Other Neurological Disorders`,
                   Dissociative=`Dissociative Disorders`,
                   Eating=`Eating Disorder`,
                   Homicidal=`Homicidal ideation`,
                   Impulse=`Impulse Disorders`,
                   NeuroDev=`Neurodevelopmental Disorders`,
                   OCD=`Obsessive-Compulsive Disorders`,
                   Personality=`Personality Disorders`,
                   SomaticSx=`Somatic Symptom Disorders`,
                   Poverty=`Other Poverty`,
                   Social=`Other Social Problems`,
                   Cannabis=`Cannabis Use`,
                   Gambling=`Gambling Disorder`,
                   OtherDrug=`Other Drug Use`,
                   Transplant=`Major Organ Transplant or Replacement Status`,
                   Prediabetes=`Pre-diabetes`,
                   Empyema=`Empyema and Lung Abscess`)



## Merge client and diagnosis data ##

cldx <- client2 %>%
  left_join(diag2, by=c("PatientProfileId", "Snapshot"))

#Replace NAs
cldx[is.na(cldx)] <- 0


######################
## Utilization Data ##
######################

## Prepare Hospital Utilization ##

#Clean hospital data
hosp <- RHosp %>%
  select(PatientProfileId, AdmitDateKey, VisitFacility, MajorClass, VisitType) %>%
  mutate(Type=if_else(MajorClass=="Inpatient" & !(VisitType %in% c("Behavioral Health", "Mental Health", "Psychiatry", "Psychiatric Services", "Psychology")), "IPmed",
                      if_else(MajorClass=="Inpatient" & VisitType %in% c("Behavioral Health", "Mental Health", "Psychiatry", "Psychiatric Services", "Psychology"), "IPpsych",
                              "ED"))) %>% #Create types
  group_by(PatientProfileId, AdmitDateKey, VisitFacility, Type) %>%
  summarize(Count=n()) %>%
  mutate(Count=if_else(Type!="ED", 1L, Count)) %>% #Only 1 visit per facility per day if inpatient
  ungroup() %>%
  group_by(PatientProfileId, AdmitDateKey, VisitFacility) %>%
  summarize(ED=sum(Count[Type=="ED"]),
            IPmed=sum(Count[Type=="IPmed"]),
            IPpsych=sum(Count[Type=="IPpsych"])) %>%
  mutate(ED=if_else(IPmed>0, 0L,
                    if_else(IPpsych>0, 0L, 
                            ED))) %>% #No ED visits on same day as admit at same facility
  group_by(PatientProfileId, AdmitDateKey) %>%
  summarize(ED=sum(ED),
            IPmed=sum(IPmed),
            IPpsych=sum(IPpsych))


## Clean Appointment Data ##

#Completed appointments
comp <- RAppts %>%
  filter(CompletedAppointment == 1) %>%
  group_by(PatientProfileId, StartDateKey) %>%
  summarize(comp=sum(CompletedAppointment))

#No show appointments
noshow <- RAppts %>%
  filter(NoShowAppointment == 1) %>%
  group_by(PatientProfileId, StartDateKey) %>%
  summarize(noshow=sum(NoShowAppointment))


## Combine Utilization Data ##

#All dates needed
startdate <- ymd(refdatekey) - days(n_lookback)
enddate <- ymd(refdatekey) + months(n_obs-1) + days(n_followup)

#All dates for each client
utiliz <- merge(client$PatientProfileId, seq(startdate, enddate, by="days")) %>%
  rename(PatientProfileId=x, Date=y)

#Add appts to master utilization data frame
utiliz2 <- utiliz %>%
  left_join(comp, by=c("PatientProfileId", "Date"="StartDateKey")) %>%
  left_join(noshow, by=c("PatientProfileId", "Date"="StartDateKey")) %>%
  left_join(hosp, by=c("PatientProfileId", "Date"="AdmitDateKey")) %>%
  arrange(PatientProfileId, Date) #for consistency, arrange by PPID, then snapshot date

utiliz2[is.na(utiliz2)] <- 0


## Create 3D array ##
# Dim 1: number of clients x number of snapshots (12)
# Dim 2: number of lookback days (365)
# Dim 3: number of utilization columns

#Helper table for creating 3D array
client_snapshots <- client$PatientProfileId %>%
  merge(seq(refdate, refdate + months(n_obs-1), by="months")) %>%
  rename(PatientProfileId=x, Snapshot=y) %>%
  arrange(PatientProfileId, Snapshot) #for consistency, arrange by PPID, then snapshot date

#Dimensions for 3D array
tot_obs <- nrow(cldx) #equals number of clients by n_obs
u_vars <- ncol(utiliz2)-2 #utilization variables, excluding PatientProfileId and Date

#Create 3D array of utilization
utiliz3 <- array(0L, dim = c(tot_obs, n_lookback, u_vars))

for(i in 1:tot_obs){
  #Date interval
  lookback <- interval(client_snapshots$Snapshot[i] - days(n_lookback),
                       client_snapshots$Snapshot[i] - days(1))
  utiliz3[i,,] <- utiliz2 %>%
    filter(PatientProfileId==client_snapshots$PatientProfileId[i], #this is already arranged by PPID, then snapshot date
           Date %within% lookback) %>%
    select(-PatientProfileId, -Date) %>%
    mutate_all(as.integer) %>%
    as.matrix(.)
}


#Outcome: utilization in follow-up period
outcomelist <- list()
for (i in 1:n_obs) {
  
  #List of clients
  client_tmp <- client %>% select(PatientProfileId)
  
  #Set reference date back "mm" months from startdate
  mm <- i - 1
  loopdate <- refdate %m+% months(mm)
  
  #Follow up interval
  post <- interval(loopdate, loopdate + days(n_followup))
  
  outcome_tmp <- hosp %>%
    filter(AdmitDateKey %within% post, IPmed>0) %>%
    group_by(PatientProfileId) %>%
    summarize(IPmed=sum(IPmed),
              IPpsych=sum(IPpsych),
              ED=sum(ED)) %>%
    right_join(client_tmp, by="PatientProfileId")
  
  outcome_tmp[is.na(outcome_tmp)] <- 0
  
  #Snapshot (month) number
  outcome_tmp$Snapshot <- i
  
  #Add to list
  outcomelist[[i]] <- outcome_tmp
}

#Convert to data frame
outcome <- bind_rows(outcomelist) %>%
  arrange(PatientProfileId, Snapshot) #for consistency, arrange by PPID, then snapshot date


########################
## Client Vars Part 2 ##
########################

#Prior 365 days (for KPOP)
kpoplist <- list()
for (i in 1:n_obs) {
  
  #List of clients
  client_tmp <- client %>% select(PatientProfileId)
  
  #Set reference date back "mm" months from startdate
  mm <- i - 1
  loopdate <- refdate %m+% months(mm)
  
  prior365 <- interval(loopdate - days(365), loopdate - days(1))
  
  kpop_tmp <- hosp %>%
    filter(AdmitDateKey %within% prior365) %>%
    select(-AdmitDateKey) %>%
    group_by(PatientProfileId) %>%
    summarize_all(sum) %>%
    mutate(ED365=cut(.$ED, breaks=c(0,1,2,4,7,1000), right=FALSE),
           IPmed365=cut(.$IPmed, breaks=c(0,1,2,3,1000), right=FALSE),
           IPpsych365=cut(.$IPpsych, breaks=c(0,1,2,3,1000), right=FALSE)) %>%
    select(PatientProfileId, ED365, IPmed365, IPpsych365)
  
  levels(kpop_tmp$ED365) <- c(0, 1, "2-3", "4-6", "7+")
  levels(kpop_tmp$IPmed365) <- c(0, 1, 2, "3+")
  levels(kpop_tmp$IPpsych365) <- c(0, 1, 2, "3+")
  
  kpop_tmp <- kpop_tmp %>%
    right_join(client_tmp, by="PatientProfileId")
  
  kpop_tmp[is.na(kpop_tmp)] <- 0
  
  #Snapshot (month) number
  kpop_tmp$Snapshot <- i
  
  #Add to list
  kpoplist[[i]] <- kpop_tmp
}

kpop <- bind_rows(kpoplist) %>%
  inner_join(cldx, by=c("PatientProfileId", "Snapshot")) %>%
  arrange(PatientProfileId, Snapshot) %>% #For conistency, arrange by PPID, then snapshot date
  mutate(EDvisits=ED365,
         IPmed=IPmed365,
         IPpsych=IPpsych365,
         StimCocaine=ifelse(Stimulant==1 | Cocaine==1, 1, 0),
         Diabetes=ifelse(DiabetesUncomp==1 | DiabCompAcute==1 | DiabetesCompChronic==1, 1, 0),
         Heart=ifelse(CHF==1 | IHD==1, 1, 0),
         Kidney=ifelse(CKD4==1 | CKD5==1 | OtherCKD==1, 1, 0),
         Liver=ifelse(Cirrhosis==1 | CirrhosisComp==1 | HepatitisChronic==1, 1, 0),
         Cancer=ifelse(BreastProstateCA==1 | ColorectalCA==1 | LungCA==1 | Lymphoma==1 | Metastatic==1, 1, 0)
  ) %>%
  select(PatientProfileId, Snapshot,
         AgeBand, Income,
         EDvisits, IPmed, IPpsych,
         TBI, Trauma, Schizophrenia, Bipolar, Depression,
         Alcohol, Opioid, StimCocaine,
         COPD, Diabetes, Heart, HepC, Kidney, Liver, Cancer
  ) %>% 
  group_by(PatientProfileId, Snapshot, AgeBand, Income, EDvisits, IPmed, IPpsych) %>% 
  mutate_all(function(x){ifelse(x > 0, 1, 0)}) %>%
  ungroup() %>%
  mutate_all(factor)



###################
## Full Datasets ##
###################

#Utilization 3D array
utiliz_input <- utiliz3

#Patient variables
pt_var_encoder <- onehot(kpop %>% select(-PatientProfileId, -Snapshot), max_levels=10)
pt_var_input <- predict(pt_var_encoder, kpop %>% select(-PatientProfileId, -Snapshot))

#Utilization output
output <- outcome %>% select(IPmed) %>% mutate(IPmed=ifelse(IPmed > 0, 1, 0)) %>% as.matrix()
output_aux <- outcome %>% select(IPmed, IPpsych, ED) %>% as.matrix()

#Dimension checks
if(dim(utiliz_input)[1]!=dim(pt_var_input)[1]) {cat("Utilization and Patient Var lengths don't match!!!")}
if(dim(utiliz_input)[1]!=dim(output)[1]) {cat("Input and Output lengths don't match!!!")}

#Save full datasets
save(utiliz_input, file="utiliz_input.rda")
save(pt_var_input, file="pt_var_input.rda")
save(output, file="output.rda")
save(output_aux, file="output_aux.rda")

#Save client snapshots data frame for randomizing later
save(client_snapshots, file="client_snapshots.rda")

#Save client level data to make life easier later
save(kpop, file="kpop.rda")


#####################################
## Data for final training/testing ##
#####################################

# Training data will be further divided into tuning/validation data to tune model hyperparameters
# and then the full training data will be used to train the final model

#Clients for training data
clients_tmp <- unique(client_snapshots$PatientProfileId)

set.seed(224)
inTraining_clients <- sample(clients_tmp, round(length(clients_tmp)*.8), replace=FALSE) #80% of clients in training
inTraining_client_snapshots <- client_snapshots[client_snapshots$PatientProfileId %in% inTraining_clients,] 
inTraining <- as.integer(row.names(client_snapshots[client_snapshots$PatientProfileId %in% inTraining_clients,]))


## Training data ##
utiliz_input_trn <- utiliz_input[inTraining,,]
pt_var_input_trn <- pt_var_input[inTraining,]
output_trn <- output[inTraining]
output_aux_trn <- output_aux[inTraining,]

#Patient identifier (one hot encoded)
pt_id_encoder_trn <- onehot(data.frame(PatientProfileId=factor(inTraining_clients)), max_levels=length(clients_tmp))
pt_id_input_trn <- predict(pt_id_encoder_trn, 
                           inTraining_client_snapshots %>% select(PatientProfileId) %>% mutate_all(factor))
pt_id_input_trn <- as.tibble(pt_id_input_trn) %>% #can't convert to integer all at once, so tibble first
  mutate_all(as.integer) %>% #convert to integer to save space
  as.matrix() #back to matrix

#Final list of training data
train_data <- list(utiliz_input_trn, pt_var_input_trn, pt_id_input_trn, output_trn, output_aux_trn)


## Testing data ##
utiliz_input_tst <- utiliz_input[-inTraining,,]
pt_var_input_tst <- pt_var_input[-inTraining,]
output_tst <- output[-inTraining]
output_aux_tst <- output_aux[-inTraining,]

pt_id_input_tst <- matrix(0L, nrow=nrow(pt_var_input_tst), ncol=ncol(pt_id_input_trn)) #Matrix of zeros--no patient identifiers...

#Final list of testing data
test_data <- list(utiliz_input_tst, pt_var_input_tst, pt_id_input_tst, output_tst, output_aux_tst)

#Save
save(train_data, file="train_data.rda")
save(test_data, file="test_data.rda")


################################
## Data for tuning/validation ##
################################

#Note: code below separates tuning/validation from training data (not full data)

#Clients for tuning data
set.seed(39)
inTuning_clients <- sample(inTraining_clients, round(length(inTraining_clients)*.8), replace=FALSE) #80% of clients in training
inTuning_client_snapshots <- inTraining_client_snapshots[inTraining_client_snapshots$PatientProfileId %in% inTuning_clients,] 
row.names(inTraining_client_snapshots) <- 1:nrow(inTraining_client_snapshots)
inTuning <- as.integer(row.names(inTraining_client_snapshots[inTraining_client_snapshots$PatientProfileId %in% inTuning_clients,]))


## Tuning data ##
utiliz_input_tun <- utiliz_input_trn[inTuning,,]
pt_var_input_tun <- pt_var_input_trn[inTuning,]
output_tun <- output_trn[inTuning]
output_aux_tun <- output_aux_trn[inTuning,]

#Patient identifier (one hot encoded)
pt_id_encoder_tun <- onehot(data.frame(PatientProfileId=factor(inTuning_clients)), max_levels=length(clients_tmp))
pt_id_input_tun <- predict(pt_id_encoder_tun, 
                           inTuning_client_snapshots %>% select(PatientProfileId) %>% mutate_all(factor))
pt_id_input_tun <- as.tibble(pt_id_input_tun) %>% #can't convert to integer all at once, so tibble first
  mutate_all(as.integer) %>% #convert to integer to save space
  as.matrix() #back to matrix

#Final list of tuning data
tune_data <- list(utiliz_input_tun, pt_var_input_tun, pt_id_input_tun, output_tun, output_aux_tun)


## Validation data ##
utiliz_input_val <- utiliz_input_trn[-inTraining,,]
pt_var_input_val <- pt_var_input_trn[-inTraining,]
output_val <- output_trn[-inTraining]
output_aux_val <- output_aux_trn[-inTraining,]

pt_id_input_val <- matrix(0L, nrow=nrow(pt_var_input_val), ncol=ncol(pt_id_input_tun)) #Matrix of zeros--no patient identifiers...

#Final list of validation data
val_data <- list(utiliz_input_val, pt_var_input_val, pt_id_input_val, output_val, output_aux_val)

#Save
save(tune_data, file="tune_data.rda")
save(val_data, file="val_data.rda")
