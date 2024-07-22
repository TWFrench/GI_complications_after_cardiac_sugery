################################################################################
# Gastrointestinal complications and laparotomy after combined cardiac surgery: A retrospective cohort study 
# T.French, D. Damaskos, D. Clinch, H. Koutsogiannidis 
# 
################################################################################
library(survival)
library(ggplot2)
library(dplyr)
library(ggsurvfit)
library(mice) #for MICE 
library(writexl)
library(arsenal)
library(readxl)
GI <- read_excel("Input_Data.xlsx")


################################################################################
#prepping missing data
sum(GI$Serious_GI_Comp)
GI$"Extent Of Disease"[GI$"Extent Of Disease"=='Not Recorded' | GI$"Extent Of Disease"=='NULL']<- NA
table(GI$LVEF, GI$Serious_GI_Comp)
GI$LVEF[GI$LVEF == 'Moderate (LVEF 31 - 50%)' | GI$LVEF =='Fair (LVEF 31-50%)']<- 'Fair'
GI$LVEF[GI$LVEF == 'Poor (LVEF 21-30%)' | GI$LVEF =='Poor (LVEF 21 - 30%)']<- 'Poor'
GI$LVEF[GI$LVEF == 'Very Poor (LVEF < 21%)' | GI$LVEF =='Very Poor LVEF <21%'] <- 'Very poor'
GI$LVEF[GI$LVEF=='Not recorded' | GI$LVEF=='NULL']<- NA


GI$"Dyspnoea Status"[GI$"Dyspnoea Status"=='Not Recorded' | GI$"Dyspnoea Status"=='NULL']<- NA

GI$'Previous MI'[GI$'Previous MI'=='Not Recorded' | GI$'Previous MI'=='NULL' | GI$'Previous MI'=='Unknown']<- NA

GI$'Active Endocarditis'[GI$'Active Endocarditis'=='Maybe' | GI$'Active Endocarditis'=='NULL']<- NA

GI$Claudication[GI$Claudication=='Not Recorded' | GI$Claudication=='NULL']<- NA

GI$Previous_Intervention_1[GI$Previous_Intervention_1 == 'Asc. Aorta / Aortic Arch' |
                             GI$Previous_Intervention_1 == 'CABG' |
                             GI$Previous_Intervention_1 == 'Congenital cardiac' |
                             GI$Previous_Intervention_1 == 'Valve - other'|
                             GI$Previous_Intervention_1 == 'Valve - MVR'|
                             GI$Previous_Intervention_1 == 'Valve - AVR'|
                             GI$Previous_Intervention_1 == 'Other cardiac'] <- 1
GI$Previous_Intervention_1[GI$Previous_Intervention_1 == 'NULL'] <- 0


GI$Hypertension[GI$Hypertension=='Not Recorded' | GI$Hypertension=='NULL']<- NA

GI$Smoking[GI$Smoking=='Not Recorded' | GI$Smoking=='NULL']<- NA

table(GI$"Renal Disease")
GI$"Renal Disease"[GI$"Renal Disease"=='Not Recorded' | GI$"Renal Disease"=='NULL']<- NA
table(GI$Inotropes)
GI$Inotropes[GI$Inotropes=='Not Recorded' | GI$Inotropes=='NULL']<- NA

table(GI$"Pre-Op_Heart_Rhy")
GI$"Pre-Op_Heart_Rhy"[GI$"Pre-Op_Heart_Rhy"=='Not Recorded' | GI$"Pre-Op_Heart_Rhy"=='NULL']<- NA
GI$"Pre-Op_Heart_Rhy"[GI$"Pre-Op_Heart_Rhy"=='Artrial fibrillation/flutter' | 
                        GI$"Pre-Op_Heart_Rhy"=='Complete heart block/pacing' | 
                        GI$"Pre-Op_Heart_Rhy"=='Other abnormal rhythm'  | 
                        GI$"Pre-Op_Heart_Rhy"=='Ventricular fibrillation or ventricular tachycardia']<- 'Abnormal'



table(GI$'Primary Incision')
GI$'Primary Incision'[GI$'Primary Incision'=='Not Recorded' | GI$'Primary Incision'=='NULL']<- NA
GI$'Primary Incision'[GI$'Primary Incision'=='Sternotomy' | GI$'Primary Incision'=='Median Sternotomy']<- 'Sternotomy'
GI$'Primary Incision'[GI$'Primary Incision'=='Heart Port' | 
                        GI$'Primary Incision'=='LAST' |
                        GI$'Primary Incision'=='Mini Thoracotomy' |
                        GI$'Primary Incision'=='Modified Heart Port' |
                        GI$'Primary Incision'=='Partial Sternotomy' |
                        GI$'Primary Incision'=='Thoracotomy' ]<- 'Other'

table(GI$"Bypass Time")
GI$"Bypass Time"[GI$"Bypass Time"=='Not Recorded' | GI$"Bypass Time"=='NULL']<- NA

table(GI$"Cross Clamp Time")
GI$"Cross Clamp Time"[GI$"Cross Clamp Time"=='Not Recorded' | GI$"Cross Clamp Time"=='NULL']<- NA

table(GI$Neurological_Disease_1)
GI$Neurological_Disease_1[GI$Neurological_Disease_1=='Not Recorded' | GI$Neurological_Disease_1=='NULL']<- NA
GI$Neurological_Disease_1[GI$Neurological_Disease_1=='CVA with full recovery/RIND' | GI$Neurological_Disease_1=='CVA with residual deficit' | 
               GI$Neurological_Disease_1=='Other' | GI$Neurological_Disease_1=='TIA']<- 'Yes'


table(GI$`Previous PCI`)
GI$`Previous PCI`[GI$`Previous PCI`=='Not Recorded' | GI$`Previous PCI`=='NULL']<- NA
GI$`Previous PCI`[GI$`Previous PCI`=='PCI > 24 hours before surgery; previous admission' |
                    GI$`Previous PCI`=='PCI > 24 hours before surgery; same admission' |
                    GI$`Previous PCI`=='PCI < 24 hours before surgery'] <- 'Yes'
GI$`Previous PCI`[GI$`Previous PCI`=='No previous Percutaneous Coronary Intervention']<- 'No'


table(GI$Pulmonary_Disease_1)
GI$Pulmonary_Disease_1[GI$Pulmonary_Disease_1=='Not Recorded' | GI$Pulmonary_Disease_1=='NULL']<- NA
GI$Pulmonary_Disease_1[GI$Pulmonary_Disease_1=='Asthma (if use inhalers also tick below)' | 
                         GI$Pulmonary_Disease_1=='Chronic pulmonary disease requiring use of longterm medication' | 
                         GI$Pulmonary_Disease_1=='FEV1 <75% predicted' | 
                         GI$Pulmonary_Disease_1=='Long term use of bronchodilators or steroids' | 
                         GI$Pulmonary_Disease_1=='COPD/emphysema' | 
                         GI$Pulmonary_Disease_1=='Other']<- 'Yes'
GI$Pulmonary_Disease_1[GI$Pulmonary_Disease_1=='No chronic pulmonary disease' | 
                         GI$Pulmonary_Disease_1=='No pulmonary disease']<- 'No'

table(GI$LMS)
GI$LMS[GI$LMS=='Not Recorded' | GI$LMS=='NULL']<- NA
GI$LMS[GI$LMS=='LMS >50% diameter stenosis']<- 'Yes'
GI$LMS[GI$LMS=='No Left Main Stem disease']<- 'No'

Minimally_invasive <- c("LAST", "Mini Thoracotomy", "Modified Heart Port", 
                        "Partial Sternotomy", "Thoracotomy")
GI$Incision <- ifelse(GI$Incision %in% Minimally_invasive, 1, 0)

table(GI$'Angina Status')
GI$`Angina Status`[GI$`Angina Status` == 'Not recorded'] <- NA
GI$CCS_1 <- ifelse(GI$`Angina Status` == "CCS1 - Only on strenuous exertion", 1, 0)
GI$CCS_2 <- ifelse(GI$`Angina Status` == "CCS2 - On moderate exertion (e.g. climbing stairs rapidly)", 1, 0)
GI$CCS_3 <- ifelse(GI$`Angina Status` == "CCS3 - On mild exertion (e.g. walking1-2 blocks at normal pace)", 1, 0)
GI$CCS_4 <- ifelse(GI$`Angina Status` == "CCS4 - On any activity or at rest", 1, 0)
GI$CCS_1 <- as.factor(GI$CCS_1)
GI$CCS_2 <- as.factor(GI$CCS_2)
GI$CCS_3 <- as.factor(GI$CCS_3)
GI$CCS_4 <- as.factor(GI$CCS_4)
################################################################################
md.pattern(GI) #examine missing data patterns

p_missing <- unlist(lapply(GI, function(x) sum(is.na(x))))/nrow(GI) #look at proportion of data missing in each category
sort(p_missing[p_missing > 0], decreasing = TRUE)
################################################################################
#store variables as factors or as continuous 

colnames(GI)
GI$Age <- as.numeric(GI$Age)
GI$Cross_Clamp_Time <- as.numeric(GI$"Cross Clamp Time")
GI$Bypass_Time <- as.numeric(GI$"Bypass Time")
GI$Angina_status <- as.factor(GI$`Angina Status`)
GI$BMI <- as.numeric(GI$BMI)
GI$TTE <- as.numeric(GI$TTE)
GI$LOS <- as.numeric(GI$"Total LOS")
GI$SEX <- as.factor(GI$SEX)
GI$LVEF <- as.factor(GI$LVEF)
GI$Operative_Priority <- as.factor(GI$"Operative Priority")
GI$Extent_of_disease <- as.factor(GI$"Extent Of Disease")
GI$Intended_Booked_Proc <- as.factor(GI$"Intended Booked Proc")
GI$Dyspnoea_Status <- as.factor(GI$"Dyspnoea Status")
GI$Previous_Intervention_1 <- as.factor(GI$Previous_Intervention_1)
GI$Previous_MI <- as.factor(GI$"Previous MI")
GI$Diabetes <- as.factor(GI$Diabetes)
GI$Hypertension <- as.factor(GI$Hypertension)
GI$Smoking <- as.factor(GI$Smoking)
GI$Renal_Disease <- as.factor(GI$"Renal Disease")
GI$Inotropes <- as.factor(GI$Inotropes)
GI$Pre_Op_Heart_Rhy <- as.factor(GI$"Pre-Op_Heart_Rhy")
GI$Primary_Incision <- as.factor(GI$"Primary Incision")
GI$LMS <- as.factor(GI$LMS)
GI$Pulmonary_Disease_1 <- as.factor(GI$Pulmonary_Disease_1)
GI$Previous_PCI <- as.factor(GI$`Previous PCI`)
GI$Neurological_Disease_1 <- as.factor(GI$Neurological_Disease_1)
GI$op_weighting <- as.factor(GI$op_weighting)
################################################################################
#box plots and IQRs to check for outliers, removing likely incorrect data points 
ggplot(GI, aes(x=Serious_GI_Comp,  y=LOS)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)


testing <- GI[complete.cases(GI[ , c('Age')]), ]
Q1 <- quantile(testing$Age, .25)
Q3 <- quantile(testing$Age, .75)
IQR <- IQR(testing$Age)

#subset data where points value is outside 1.5*IQR of Q1 and Q3
outliers <- subset(testing, testing$Age<(Q1 - 1.5*IQR) | testing$Age>(Q3 + 1.5*IQR))
sort(outliers$Age)

GI$BMI[GI$BMI >= 90] <- NA
GI$BMI[GI$BMI <= 10] <- NA
GI$Cross_Clamp_Time[GI$Cross_Clamp_Time > 1000] <- NA
GI$Bypass_Time[GI$Bypass_Time >= 1000] <- NA
################################################################################
#complete case analysis 
#dummy code for case complete (ignore if using MICE)


colnames(GI)

GI$Additive_euroscore <- as.numeric(GI$`Additive Euroscore`)
GI$Female <- ifelse(GI$SEX=='Female', 1, 0)
GI$OP_Urgent <- ifelse(GI$Operative_Priority=='Urgent', 1, 0)
GI$OP_Emergency <- ifelse(GI$Operative_Priority=='Emergency', 1, 0)
GI$OP_salvage <- ifelse(GI$Operative_Priority=='Salvage', 1, 0)

GI$Known_CAD <- ifelse(GI$Extent_of_disease =='One vessel with >50% diameter stenosis' | 
                         GI$Extent_of_disease =='Two vessels with >50% diameter stenosis' |
                         GI$Extent_of_disease =='Three vessels with >50% diameter stenosis', 1, 0)

GI$Isolated_non_CABG <- ifelse(GI$op_weighting =='Isolated non-cabg', 1, 0)
GI$Two_procedures <- ifelse(GI$op_weighting =='Two procedures', 1, 0)
GI$Three_procedures <- ifelse(GI$op_weighting =='Three or more operations', 1, 0)

GI$NYHA_II <- ifelse(GI$Dyspnoea_Status=='NYHA II - On moderate exertion (e.g. on climbing stairs rapidly)', 1, 0)
GI$NYHA_III <- ifelse(GI$Dyspnoea_Status=='NYHA III - On mild exertion (e.g. walking 1-2 blocks at normal pace)', 1, 0)
GI$NYHA_IV <- ifelse(GI$Dyspnoea_Status=='NYHA IV - On any activity or at rest', 1, 0)

GI$Previous_MI <- ifelse(GI$Previous_MI=='One' |
                           GI$Previous_MI=='Two or more', 1, 0)
GI$Diabetes_not_insulin <- ifelse(GI$Diabetes=='Diet' | 
                                    GI$Diabetes=='Oral therapy', 1, 0)
GI$Diabetes_on_insulin <- ifelse(GI$Diabetes=='Insulin', 1, 0)
GI$Hypertension <- ifelse(GI$Hypertension=='Treated or BP >140/90 on >1 occasion prior to admission', 1, 0) 
GI$Ex_smoker <- ifelse(GI$Smoking=='Ex smoker', 1, 0) 
GI$Current_smoker <- ifelse(GI$Smoking=='Current smoker', 1, 0)
GI$Renal_failure <- ifelse(GI$Renal_Disease=='Dialysis for acute renal failure: onset within 6 weeks of cardiac surgery' |
                             GI$Renal_Disease=='Dialysis for chronic renal failure: onset more than 6 weeks prior to cardiac surgery' |
                             GI$Renal_Disease=='No dialysis but Pre-operative acute renal failure (anuria or oliguria < 10ml/hour)', 1, 0) 

GI$Inotropes <- ifelse(GI$Inotropes=='Yes', 1, 0) 
GI$Abnormal_rhythm <- ifelse(GI$Pre_Op_Heart_Rhy=='Abnormal', 1, 0) 

GI$Bypass_Performed <- ifelse(GI$Bypass_performed=='Yes', 1, 0) 
GI$Pulmonary_Disease_1 <- ifelse(GI$Pulmonary_Disease_1=='Yes', 1, 0) 
GI$Neurological_Disease_1 <- ifelse(GI$Neurological_Disease_1=='Yes', 1, 0) 
GI$Previous_PCI <- ifelse(GI$Previous_PCI=='Yes', 1, 0) 

GI$Bypass_Time_GT_150 <- ifelse(GI$Bypass_Time>150, 1, 0)

GI$LVEF[GI$LVEF=='Not Recorded'] <- NA

GI$Moderate_LVEF <- ifelse(GI$LVEF == 'Fair', 1, 0)
GI$Poor_LVEF <- ifelse(GI$LVEF == 'Poor', 1, 0)
GI$Very_poor_LVEF <- ifelse(GI$LVEF == 'Very poor', 1, 0)

GI$Age_GT_80 <- ifelse(GI$Age > 80, 1, 0)
GI$Age_GT_70 <- ifelse(GI$Age > 80, 1, 0)
GI$NYHAIII_IV <- ifelse(GI$NYHA_III == 1 | GI$NYHA_IV ==1, 1, 0)
table(GI$NYHAIII_IV)
table(GI$NYHA_III)
table(GI$NYHA_IV)
################################################################################
#descriptive statistics 
#viewing distributions 
ggplot(GI, aes(x=Age)) + 
  geom_density() #left-tail 
ggplot(GI, aes(x=Cross_Clamp_Time)) + 
  geom_density() #heavy right-tail 
ggplot(GI, aes(x=Bypass_Time)) + 
  geom_density() #heavy right-tail 
ggplot(GI, aes(x=LOS)) + 
  geom_density() #heavy right tail 
colnames(GI)
ggplot(GI, aes(x=BMI)) + 
  geom_density() #normal distribution  

hist(GI$BMI)
################################################################################
#descriptive statistics 

tab3 <- tableby(Serious_GI_Comp ~ BMI, data=GI)
Continuous_normal2 <- as.data.frame(summary(tab3))
write_xlsx(Continuous_normal2, "Continuous_normal.xlsx")
#continuous variables #comparison test set to 'kwt' 
mycontrols  <- tableby.control(test=TRUE, total=FALSE,
                               numeric.test="kwt", cat.test="chisq",
                               numeric.stats=c("N", "median", "q1q3"),
                               cat.stats=c("N","countpct"),
                               stats.labels=list(N='Count', median='Median', q1q3='Q1,Q3'))
tab2 <- tableby(Serious_GI_Comp ~ Age + Cross_Clamp_Time + Bypass_Time + LOS, data=GI, control=mycontrols)
Continuous2 <- as.data.frame(summary(tab2))
Continuous
write_xlsx(Continuous2, "Continuous2.xlsx")


GI$Renal_failure <- as.factor(GI$Renal_failure)
GI$Known_CAD <- as.factor(GI$Known_CAD)
GI$Previous_PCI <- as.factor(GI$Previous_PCI)
GI$Previous_MI <- as.factor(GI$Previous_MI)
GI$post_op_AF <- as.factor(GI$post_op_AF)
GI$post_op_PPM <- as.factor(GI$post_op_PPM)
GI$post_op_pneumonia <- as.factor(GI$post_op_pneumonia)
GI$Post_op_HF <- as.factor(GI$Post_op_HF)
GI$Post_op_vascular_comp <- as.factor(GI$Post_op_vascular_comp)
GI$Thirty_day_mortality <- as.factor(GI$Thirty_day_mortality)
GI$In_hospital_mortality <- as.factor(GI$In_hospital_mortality)
GI$Hypertension <- as.factor(GI$Hypertension)
GI$Pulmonary_Disease_1 <- as.factor(GI$Pulmonary_Disease_1)
GI$Neurological_Disease_1 <- as.factor(GI$Neurological_Disease_1)
GI$Inotropes <- as.factor(GI$Inotropes)

tab3 <- tableby(Serious_GI_Comp ~ SEX +Smoking +Diabetes +Hypertension +LVEF +
                  Pulmonary_Disease_1+Neurological_Disease_1 + Renal_failure + Known_CAD +
                  Dyspnoea_Status +LMS + Pre_Op_Heart_Rhy+ Previous_PCI+ Previous_MI + 
                  Inotropes +op_weighting + Operative_Priority + 
                  Thirty_day_mortality + In_hospital_mortality +
                  Post_op_vascular_comp + Post_op_HF + post_op_pneumonia + 
                  post_op_PPM + post_op_AF + Angina_status, data=GI, control=mycontrols)

categorical2 <- as.data.frame(summary(tab3))

summary(tab3)
categorical
write_xlsx(categorical2, "categorical2.xlsx")


################################################################################
# imputing missing data for regression analysis 
#mode imputation for categorical variables 
calculate_mode <- 
  function(categorical_vector){
    freq_table <- 
      table(categorical_vector)
    mode_value <- names(freq_table)[which.max(freq_table)]
    
    return(mode_value)
  }

list <- c("SEX" , "Smoking", "Diabetes","Hypertension","LVEF", 
  "Pulmonary_Disease_1","Neurological_Disease_1", "Renal_failure","Known_CAD",
  "Dyspnoea_Status","LMS","Pre_Op_Heart_Rhy","Previous_PCI","Previous_MI",
  "Inotropes","op_weighting", "Operative_Priority", 
  "Thirty_day_mortality","In_hospital_mortality",
  "Post_op_vascular_comp","Post_op_HF","post_op_pneumonia", 
  "post_op_PPM","post_op_AF","Angina_status")

impute_mode <- function(df, columns){
  for (col in columns){
    mode_value <- calculate_mode(df[[col]])
    df[[col]][is.na(df[[col]])] <- mode_value
  }
  return(df)
}

GI <- impute_mode(GI, list)

#median imputation for continuous variables 
list<-  c("Age","Cross_Clamp_Time","Bypass_Time","LOS","BMI")

impute_median <- function(df, columns){
  for (col in columns){
    median <- median(df[[col]], na.rm=TRUE)
    df[[col]][is.na(df[[col]])] <- median
  }
  return(df)
}

GI <- impute_median(GI, list)

################################################################################
#unadjusted odds ratios 


colnames(GI)
options(scipen=999)

library(CIplot)

df <- data.frame("Characteristic", "Odds_ratio", "2.5%", "97.5%", "p_value")


list  <- c('Age', 'Cross_Clamp_Time', 'Bypass_Time',
           'Female', 'OP_Urgent', 'OP_Emergency', 'OP_salvage',  
           'Known_CAD', 'NYHA_II', 'Previous_MI',
           'Diabetes_not_insulin',
           'Hypertension','Ex_smoker','Renal_failure','Inotropes','Abnormal_rhythm',
           'Bypass_Performed','LMS',
           'Pulmonary_Disease_1','Neurological_Disease_1','Previous_PCI', 
           'Isolated_non_CABG', 'Two_procedures','Three_procedures',
           'NYHA_III','NYHA_IV','Diabetes_on_insulin','Current_smoker', 'Moderate_LVEF','Poor_LVEF','Very_poor_LVEF',
           'Previous_Intervention_1', "Incision", 
           "One_or_more_units_blood", 
           "Post_op_vascular_comp", "Post_op_HF",
           "post_op_AF", "NYHAIII_IV" , "CCS_1" , "CCS_2" , "CCS_3" , "CCS_4")

for (var in list) {
  
  formula <- as.formula(paste("Serious_GI_Comp ~", var))
  model1 <- glm(formula, data=GI, family = binomial)
  output = c(var, exp(model1$coefficients[2]), exp(confint.default(model1))[2], exp(confint.default(model1))[4], coef(summary(model1))[,4][2])
  df = rbind(df, output)
  
  
  
}
View(df)

write_xlsx(df, "Unadjusted_odds_ratios.xlsx")
################################################################################
#odds ratios adjusted for GIMS score  
df2 <- data.frame("Characteristic", "Odds_ratio", "2.5%", "97.5%", "p_value")

list2  <- c('Cross_Clamp_Time',
            'Female', 'OP_Urgent', 'OP_Emergency', 'OP_salvage',  
            'Known_CAD', 'Previous_MI',
            'Diabetes_not_insulin', 'Diabetes_on_insulin',
            'Hypertension','Renal_failure','Abnormal_rhythm',
            'Bypass_Performed','LMS',
            'Pulmonary_Disease_1','Neurological_Disease_1','Previous_PCI', 
            'Isolated_non_CABG', 'Two_procedures','Three_procedures',
            'Moderate_LVEF','Poor_LVEF','Very_poor_LVEF',
            'Previous_Intervention_1','CCS_1','CCS_2','CCS_3','CCS_4')

for (var in list2) {
  
  formula <- as.formula(paste("Serious_GI_Comp ~ Age_GT_80 + Current_smoker + 
                              Inotropes + NYHAIII_IV + Bypass_Time_GT_150 +
                              post_op_AF + Post_op_vascular_comp + Post_op_HF +", var))
  model2 <- glm(formula, data=GI, family = binomial)
  output = c(var, exp(model2$coefficients[10]), exp(confint.default(model2))[10], exp(confint.default(model2))[20], coef(summary(model2))[,4][10])
  df2 = rbind(df2, output)
  
  
  
}
View(df2)

model2 <- glm(Serious_GI_Comp ~ Age_GT_80 + Current_smoker + 
                              Inotropes + NYHAIII_IV + Bypass_Time_GT_150 +
                              post_op_AF + Post_op_vascular_comp + Post_op_HF, data=GI, family=binomial)

summary(model2)

Odds_ratio <- c(exp(model2$coefficients[2:9]))

CI_2.5 <- c(exp(confint.default(model2))[2:9])

CI_97.5 <- c(exp(confint.default(model2))[11:18])

p_value <- c(coef(summary(model2))[,4][2:9])
results <- data.frame(Odds_ratio, CI_2.5, CI_97.5, p_value)
View(results)
results
################################################################################

model3 <- glm(Thirty_day_mortality~Serious_GI_Comp + Additive_euroscore + LOS, data=GI, family="binomial")
summary(model3)
exp(model3$coefficients)
exp(confint.default(model3))

################################################################################
# univariate coxph regression 
library(survival)
library(ggplot2)
library(dplyr)
library(ggsurvfit)

covariates <- c('Serious_GI_Comp','Age', 'Cross_Clamp_Time', 'Bypass_Time_GT_90',
                'Female', 'OP_Urgent', 'OP_Emergency', 'OP_salvage',  
                'Known_CAD', 'NYHA_II', 'Previous_MI',
                'Diabetes_not_insulin',
                'Hypertension','Ex_smoker','Renal_failure','Inotropes','Abnormal_rhythm',
                'Bypass_Performed','LMS',
                'Pulmonary_Disease_1','Neurological_Disease_1','Previous_PCI', 
                'Isolated_non_CABG', 'Two_procedures','Three_procedures',
                'NYHA_III','NYHA_IV','Diabetes_on_insulin','Current_smoker', 'Moderate_LVEF','Poor_LVEF','Very_poor_LVEF',
                'Previous_Intervention_1')

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(TTE, Event)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = GI)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)
################################################################################
#backwards stepwise selection of covariates, R 


cox.adjusted <- coxph(Surv(TTE, Event) ~ Additive_euroscore+  LOS +  
                        Serious_GI_Comp , data =  GI)

summary(cox.adjusted)




################################################################################
#survival curve 
GI$TTE_months <- GI$TTE/30
GI$TTE_years <- GI$TTE_months/12
Y = Surv(GI$TTE_years, GI$Event == 1)
kmfit = survfit(Y ~ GI$Serious_GI_Comp)
p1 <- survdiff(formula = Surv(TTE_years, Event) ~ Serious_GI_Comp, data=GI)
p1
km <- ggsurvfit(kmfit, lwd=1.5) + add_confidence_interval(type='ribbon', alpha=0.2, lwd=1) + 
  scale_ggsurvfit() +
  labs(x="Time from operation (Years)",
       y="Survival probability",
       caption="Log-rank p-value < 0.001") + 
  scale_color_manual(values=c("#36648B", "red"), labels=c("No GI complication", "GI complication")) + 
  scale_fill_manual(values=c("#36648B", "red"), labels=c("No GI complication", "GI complication")) 
  

km + theme(
  plot.title = element_text(size=20, hjust=0.5),
  axis.text = element_text(size=12),
  axis.title =element_text(size=15),
  legend.position="bottom",
  legend.key.size = unit(1, "cm"),
  plot.margin=unit(c(1,1,0.5,1), 'cm'), 
  axis.line = element_line(color='black'),
  panel.border = element_blank(),
  plot.caption = element_text(hjust = 0.2, size=15, vjust=90, face="italic")
) 

################################################################################