#### Installing Libraries ####
install.packages("igraph")
install.packages("treemap")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("GGally")
install.packages("plyr")
install.packages("tidyverse")
install.packages("naniar")
install.packages("skimr")
install.packages("caret")
install.packages("MLmetrics")
install.packages("imbalance")
install.packages("gridExtra")
install.packages("patchwork")
install.packages("ggplot2")
library(igraph)
library(treemap)
library(viridis)
library(hrbrthemes)
library(GGally)
library(plyr)
library(tidyverse)   # metapackage of all tidyverse packages
library(naniar)      # handling missing data
library(skimr)       # quick overview over the dataset 
library(caret)       # ML toolkit
library(MLmetrics)   # F1 Score
library(imbalance)   # algorithms to deal with imbalanced dataset
library(gridExtra)   # display plots in grids
library(patchwork)   # arrange plots side by side
library(ggplot2)

#### End of Library ####


stroke_data <- read.csv("stroke.csv")
head(stroke_data)

#What data type is this file?
class(stroke_data)

# Lets look at the structure of the file
str(stroke_data)

# Column Name of the dataset
colnames(stroke_data)

# Lets view the summary of the data
# We can see that this shows relevant summaries of the data
summary(stroke_data)

# view the no of records with NA, Could use the complete cases command
na_records <- stroke_data[!complete.cases(stroke_data),]
summary(na_records)

stroke_data %>% filter(!is.na())
# count the no of NA records
nrow(na_records)

# Visualise the patterns of NA data
# using mice or VIM
library(mice)
md.pattern(stroke_data)

# VIM
library(VIM)
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)
summary(missing_values)

######### Data Visualization Starts ###########

# removing of rejected coloums
stroke_data_df <- subset (stroke_data, select = -c(avg_glucose_level,gender,Residence_type,smoking_status))
print(head(stroke_data_df))

# Find out "N/A" values are in stroke_data per column
miss_scan_count(data = stroke_data , search = list("N/A", "Unknown"))

# replace the "N/A" in bmi
stroke_data  <- replace_with_na(data = stroke_data, replace = list(bmi = c("N/A"))) %>%

# change bmi to numeric 
mutate(bmi = as.numeric(bmi))

no_missing <- stroke_data

stroke_data <- stroke_data %>%
  mutate(ever_married = case_when(ever_married =="Yes" ~ 1,
                                  ever_married =="No" ~ 0)
  )

# check
summary(stroke_data)

# visualization of missing data
vis_miss(stroke_data, cluster = TRUE)


# all visualizations based on validity of importance of variables

yes_stroke <- filter(stroke_data, stroke_data$stroke == 1)
no_stroke <- filter(stroke_data, stroke_data$stroke == 0)
summary(stroke_data)
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

# convert main data into numeric
all_numeric <- no_missing

all_numeric <- all_numeric %>%
  mutate(gender = case_when(gender == "Male" ~1,
                            gender == "Female" ~0,
                            gender == "Other" ~2),
         ever_married = case_when(ever_married == "Yes" ~1,
                                  ever_married == "No" ~0),
         work_type = case_when(work_type == "children" ~0,
                               work_type == "Never_worked" ~1,
                               work_type == "Private" ~2,
                               work_type == "Govt_job" ~3,
                               work_type == "Self-employed" ~4),
         Residence_type = case_when(Residence_type == "Urban" ~1,
                                    Residence_type == "Rural" ~0),
         smoking_status = case_when(smoking_status == "never smoked" ~0,
                                    smoking_status == "formerly smoked" ~1,
                                    smoking_status == "smokes" ~2,
                                    smoking_status == "Unknown" ~3) 
  )

# Correlation Matrix Plot
ggcorr(all_numeric, method = c("everything", "pearson"))

# Age Vs. BMI
install.packages("hexbin")
ggplot(no_missing, aes(x=age, y=bmi) ) + geom_hex(bins = 30) + theme_bw()

# Age Vs. Smoking Status
ggplot(stroke_data, aes(x=smoking_status , y=age, fill=smoking_status )) + 
geom_violin()

# bmi, avg_glucose_level, age Density Plot
ggplot(no_missing,aes(x=bmi)) +
geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

ggplot(stroke_data,aes(x=avg_glucose_level)) +
geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

ggplot(stroke_data,aes(x=age)) +
geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)


# Age Vs Smoking count
ggplot(stroke_data, aes(x=age, fill=as.factor(stroke))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(fill="")

# Age distribution by stroke

ggplot(data = stroke_data, aes(x=as.character(stroke), y=age)) +
  geom_boxplot(fill="steelblue") +
  labs(title="Age distribution by stroke", x="stroke", y="age")

##### Data Visualization ends here #####
############################################################

stroke_df <- read.csv("stroke.csv")

# Data source review
glimpse(stroke_df)

# The variables of hypertension, heart_disease and stroke are binomial.
# we will convert them to factors.

strk_data <- stroke_df  
nrow(strk_data)

c_strk_data <- strk_data[!complete.cases(strk_data),]
nrow(c_strk_data)

# list rows with missing values
strk_data[!complete.cases(strk_data),]

glimpse(c_strk_data)

# show summary of all missing vals in a variable
sum(is.na(strk_data$stroke))
sum(is.na(strk_data$heart_disease))

# Removed NAs
na_strk_data <- na.omit(strk_data) 
nrow(na_strk_data)
strk_data <- na_strk_data

nrow(strk_data)

strk_data$hypertension <- factor(strk_data$hypertension,levels = c(0,1), labels = c("No", "Yes"))  
strk_data$heart_disease <- factor(strk_data$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
strk_data$stroke_n <- stroke_df$stroke
strk_data$stroke <- factor(strk_data$stroke, levels = c(0,1), labels = c("No", "Yes"))
strk_data$gender_hypertension <- paste(strk_data$gender, strk_data$hypertension, sep="-")

glimpse(strk_data)

subset(strk_data, select = c("hypertension", "heart_disease", "stroke"))  

# create an interval for age to analyze.
strk_data$age_interval <- cut_interval(strk_data$age, n=5)
strk_data$glucose_interval <- cut_interval(strk_data$avg_glucose_level, n=5)

# When checking the variable «gender» we detect that there is a line «Others». 
# We will delete it as it is irrelevant.

strk_data <- strk_data[which(strk_data$gender!= "Other"), ]  

strk_data <- strk_data[which(is.na(strk_data$bmi)==FALSE), ] 


# The bmi variable is numeric, but for null values, we will have to handle it.
# using user defined function here called - fn_bmi_score

fn_bmi_score <- function(x) {
  
  if (is.na(x)) {
    r="ND"
  }
  else {
    if (x< 18.5)
      r = "Under w"
    if (between(x, 18.5, 24.9)) 
      r= "Normal w"
    if (between(x, 25, 29.9))
      r= "Over w"
    if (x>=30)
      r="Obese"
  }
  
  return(r)
}
#### function ends

# bmi variable is numeric, but for null values need to handle
strk_data$bmi <- as.numeric(strk_data$bmi)
fn_bmi_score_v <- Vectorize(fn_bmi_score)
strk_data$bmi_score <- fn_bmi_score_v(strk_data$bmi)

glimpse(strk_data)
summary(strk_data)

# Let's check for null values for other attributes

strk_data %>% gg_miss_var()

# BMI has missing values.
# 96% of cases are complete
strk_data %>% naniar::prop_complete_case() 

# Review of variables
# Age, BMI, Glucose Histogram
strk_data %>% boxplot(age~gender, data=. , main="Age Histogram", col= c("pink", "blue"))
strk_data %>% boxplot(bmi~gender, data=. , main="BMI Histogram", col= c("pink", "blue"))
strk_data %>% boxplot(avg_glucose_level~gender, data=. , main="Glucose Histogram", col= c("pink", "blue"))

glimpse(strk_data)

########## Variable and Data analysis ##############

### Drop ID column from data frame

colnames(strk_data)
strk_data = subset(strk_data, select = -c(id))
colnames(strk_data)

# Let us look at who has the highest rate of heart attacks. 
# Number of men and women

nrow(strk_data)
strk_data$gender %>% table() %>% prop.table() %>% round(digits = 2)

# Analysis with respect to sex
strk_data %>%
  select(gender, stroke) %>%
  table() %>%
  prop.table(margin = 1) %>%
  round(digits = 2) 

# analysis in case you are a smoker
strk_data %>%
  select(smoking_status, stroke) %>%
  table() %>%
  prop.table(margin = 1) %>%
  round(digits = 2) 

# Analysis by age range
strk_data %>%
  select(age_interval, stroke) %>%
  table() %>%
  prop.table(margin = 1) %>%
  round(digits = 2) 

# Analysis by Hypertension
strk_data %>%
  select(hypertension, stroke) %>%
  table() %>%
  prop.table(margin = 1) %>%
  round(digits = 2) 

# BMI score analysis and gender
strk_data %>%
  select(gender, bmi_score) %>%
  table() %>%
  prop.table(margin = 2) %>%
  round(digits = 2)  

# The extreme cases of the BMI are striking. To analyze this part I will categorize 
# the age information by groups. First I will check minimums and maximums. What I want 
# to analyze is whether there is a pattern by age for these isolated cases.

# BMI chart
strk_data$age %>% summary()

# BMI analysis by sex, age range and heart attack
strk_data %>% 
  ggplot(aes(x=age_interval, fill=stroke)) +
  geom_bar(aes(y=..count..)) +
  stat_boxplot(geom ='errorbar') +
  scale_fill_manual(values=c("green", "red"), aesthetics = "fill") +
  ggtitle("BMI analysis by sex, age range and heart attack") +
  facet_grid(stroke~gender, scales = "free_y")

####### Data Analysis Ends Here ################

##### Q1 - Hypothesis Test starts #####
stroke_df_q5 <- read.csv("stroke.csv")
strk_data_q5 <- stroke_df_q5  

strk_data_q5$heart_disease
strk_data_q5$stroke
class(strk_data_q5$stroke)
is.numeric(strk_data_q5$stroke)

install.packages("psych")
library(psych)

# I'm examining heart_disease and stroke so I need to prepare both variables first
# change both variables to a factor variable as it seems to be a categorical 
# dichotomous variable

glimpse(strk_data_q5$heart_disease)
glimpse(strk_data_q5$stroke)

hd <- factor(strk_data_q5$heart_disease,levels = c(0,1), labels = c("No", "Yes"))  
strk <- factor(strk_data_q5$stroke,levels = c(0,1), labels = c("No", "Yes"))  

hd_strk = subset(strk_data_q5, select = c(stroke, heart_disease))
hd_strk$heart_disease <- hd
hd_strk$stroke <- strk

# checking variables
hd_strk$heart_disease
hd_strk$stroke

# attach 
attach(hd_strk)

# look at the correlation between both of these variables to evaluate the strength 
# of the relationship and whether it is negative or positive

# Both variables stroke and hypertension are categorical dichotomous.

pairs.panels(hd_strk,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# 
library("lattice")
histogram(~stroke | heart_disease, 
          data = hd_strk, 
          main = "Distribution of stroke heart_disease data", 
          xlab = "Heart Disease", 
          ylab = "Stroke")

# We need to check whether the data is normally distributed or not
# Quantile-quantile plot allows us to check if the
# data is distributed normally

str(hd_strk)
class(stroke)

install.packages("varhandle")
library(varhandle)

# We need to check whether the data is normally distributed or not

# grouped bar plot preserving zero count bars
ggplot(hd_strk, 
       aes(x = stroke, 
           fill = heart_disease)) + 
  geom_bar(position = position_dodge(preserve = "single"))

# Seems that the heart disease with "no" as well as "yes" occurrences may not be normally distributed.
install.packages("pwr")
library(pwr)

# after seeing histogram from a dependent categorical variable (stroke)
# with an independent categorical variable (heart_disease)
# I will use the chisq.test" for statistical Analysis

# Format = chisq.test(var1,var2)
# chisq.test(stroke~heart_disease)

chisq.test(hd_strk$stroke,hd_strk$heart_disease)

# p-value < 2.2e-16 # 0.00000000000000022
#The result of the chisq.test show a p-value < 0.05. Therefore the null 
#hypothesis is rejected. The analysis concludes that heart disease is a cause 
#to get a stroke (p = 2.2e-16)

##### Q1 - Hypothesis Test ends #####


###### Q2 - Hypothesis Test starts#############
stroke_df <- read.csv("stroke.csv")
strk_data <- stroke_df  

strk_data$hypertension
strk_data$stroke
class(strk_data$stroke)
is.numeric(strk_data$stroke)

ht <- factor(strk_data$hypertension,levels = c(0,1), labels = c("No", "Yes"))  
strk <- factor(strk_data$stroke,levels = c(0,1), labels = c("No", "Yes"))  

ht_strk = subset(strk_data, select = c(stroke, hypertension))
ht_strk$hypertension <- ht
ht_strk$stroke <- strk

# checking variables
ht_strk$hypertension
ht_strk$stroke

# attach 
attach(ht_strk)

# Both variables stroke and hypertension are categorical dichotomous.

pairs.panels(ht_strk,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# 
library("lattice")
histogram(~stroke | hypertension, 
          data = ht_strk, 
          main = "Distribution of stroke hypertension data", 
          xlab = "Hypertension", 
          ylab = "Stroke")

# We need to check whether the data is normally distributed or not
# Quantile-quantile plot allows us to check if the
# data is distributed normally

str(ht_strk)
class(stroke)

install.packages("varhandle")
library(varhandle)

# change stroke to numeric 
#x <- unclass(stroke)
#class(x)
#is_numeric(x)

#ht_strk$stroke <- x
#ht_strk$stroke
#class(ht_strk$stroke)
#is_numeric(ht_strk$stroke)
# Formal test of normality through widely used Shapiro-Wilks

#normality_test <- shapiro.test(stroke[0:5000])
#normality_test$p.value

# p-value = 5.824864e-90 
# 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005824864
# p-value tells us the chances that the sample comes from a normal distribution 
# Here p-value is clearly lower than 0.05, so not normally distributed

# We can check the normality in each variable using the tapply() function instead
# We cannot use the test for dicotomous data so we refer to the histogram instead

#with(ht_strk, tapply(stroke, hypertension, shapiro.test))

# p-value < 2.2e-16

ht_strk$hypertension
# after seeing histogram and p-value from a dependent categorical variable (stroke)
# with an independent categorical variable (hypertension)
# I will use the chisq.test" for statistical Analysis

# Format = chisq.test(var1,var2)
# chisq.test(stroke~hypertension)

new_ht_strk <- na.omit(ht_strk)
chisq.test(new_ht_strk$stroke,new_ht_strk$hypertension)

p-value < 2.2e-16 # 0.00000000000000022

#The result of the chisq.test show a p-value < 0.05. Therefore the null 
#hypothesis is rejected. The analysis concludes that hypertension is a cause 
#to get a stroke (p = 2.2e-16)

##### Q2 - Hypothesis Test ends #####

############ Q3 starts ###########

stroke_df_q3 <- read.csv("stroke.csv")
strk_data_q3 <- stroke_df_q3  

strk_data_q3$gender
strk_data_q3$stroke
class(strk_data_q3$stroke)
is.numeric(strk_data_q3$stroke)

strk <- factor(strk_data_q3$stroke,levels = c(0,1), labels = c("No", "Yes"))  
gender_strk = subset(strk_data_q3, select = c(stroke, gender))
gender_strk$stroke <- strk

# checking variables
gender_strk$gender
gender_strk$stroke

# attach 
attach(gender_strk)

# Both variables stroke and hypertension are categorical dichotomous.

pairs.panels(gender_strk,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# 
library("lattice")
histogram(~stroke | gender, 
          data = gender_strk, 
          main = "Distribution of stroke gender data", 
          xlab = "Gender", 
          ylab = "Stroke")

# We need to check whether the data is normally distributed or not
# Quantile-quantile plot allows us to check if the
# data is distributed normally

str(gender_strk)
class(stroke)

install.packages("varhandle")
library(varhandle)

gender_strk$gender
# after seeing histogram from a dependent categorical variable (stroke)
# with an independent categorical variable (gender)
# I will use the chisq.test" for statistical Analysis

# Format = chisq.test(var1,var2)
# chisq.test(stroke~gender)

chisq.test(gender_strk$stroke,gender_strk$gender)

# p-value = 0.7895 

#The result of the chisq.test show a p-value = 0.79. Therefore the null 
#hypothesis is not rejected, which means alternative hypothesis is accepted.
#The analysis concludes that More male did not die because 
#of stroke (p = 0.7895)

##### Q3 - Hypothesis Test ends #####

##### Q4 - Hypothesis Test starts #####

stroke_df_q4 <- read.csv("stroke.csv")
strk_data_q4 <- stroke_df_q4  

strk_data_q4$smoking_status
strk_data_q4$stroke
class(strk_data_q4$stroke)
is.numeric(strk_data_q4$stroke)

strk <- factor(strk_data_q4$stroke,levels = c(0,1), labels = c("No", "Yes"))  
ss_strk = subset(strk_data_q4, select = c(stroke, smoking_status))
ss_strk$stroke <- strk

# checking variables
ss_strk$smoking_status
ss_strk$stroke

# attach 
attach(ss_strk)

# Both variables stroke and smoking_status are categorical dichotomous.

pairs.panels(ss_strk,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# 
library("lattice")
histogram(~stroke | smoking_status, 
          data = ss_strk, 
          main = "Distribution of stroke and smoking status", 
          xlab = "smoking_status", 
          ylab = "Stroke")

# We need to check whether the data is normally distributed or not
# Quantile-quantile plot allows us to check if the data is distributed normally

str(ss_strk)
class(stroke)

ss_strk$smoking_status
# after seeing histogram from a dependent categorical variable (stroke)
# with an independent categorical variable (smoking_status)
# I will use the chisq.test" for statistical Analysis

# Format = chisq.test(var1,var2)
# chisq.test(stroke~smoking_status)

chisq.test(ss_strk$stroke,ss_strk$smoking_status)

# p-value = 2.085e-06 # 0.000002085

#The result of the chisq.test show a p-value < 0.05. Therefore the null 
#hypothesis is rejected, which means alternative hypothesis is accepted.
#The analysis concludes that Person who smoke will get a stroke
# (p = 2.085e-06)

##### Q4 - Hypothesis Test ends #####

##### Q5 - Hypothesis Test starts #####

stroke_df_q5 <- read.csv("stroke.csv")
strk_data_q5 <- stroke_df_q5  

strk_data_q5$ever_married
strk_data_q5$stroke
class(strk_data_q5$stroke)
is.numeric(strk_data_q5$stroke)

strk <- factor(strk_data_q5$stroke,levels = c(0,1), labels = c("No", "Yes"))  
em_strk = subset(strk_data_q5, select = c(stroke, ever_married))
em_strk$stroke <- strk

# checking variables
em_strk$ever_married
em_strk$stroke

# attach 
attach(em_strk)

# Both variables stroke and ever_married are categorical dichotomous.

pairs.panels(em_strk,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# 
library("lattice")
histogram(~stroke | ever_married, 
          data = em_strk, 
          main = "Distribution of stroke and ever married data", 
          xlab = "ever_married", 
          ylab = "Stroke")

# We need to check whether the data is normally distributed or not
# Quantile-quantile plot allows us to check if the
# data is distributed normally

str(em_strk)
class(stroke)

em_strk$ever_married
# after seeing histogram from a dependent categorical variable (stroke)
# with an independent categorical variable (ever_married)
# I will use the chisq.test" for statistical Analysis

# Format = chisq.test(var1,var2)
# chisq.test(stroke~ever_married)

chisq.test(em_strk$stroke,em_strk$ever_married)

# p-value = 1.639e-14 # 0.00000000000001639

#The result of the chisq.test show a p-value < 0.05. Therefore the null 
#hypothesis is rejected, which means alternative hypothesis is accepted.
#The analysis concludes that married people died because 
#of stroke (p = 1.639e-14)

##### Q5 - Hypothesis Test ends #####
