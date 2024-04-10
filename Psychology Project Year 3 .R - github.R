
#This script is exploring the relationship between internet addiction and mental health outcomes 

#install packages 
install.packages('tidyverse')
install.packages ("psyntur")
install.packages("NHANES")
install.packages('mice')

# load library's 
library("tidyverse")
library('psyntur')
library('NHANES')
library('mice')

#import data
Internet_df<- read_csv("IAT_data_imported.csv")
Internet_df
glimpse(Internet_df)
View(Internet_df)

#Structure of data-set
str(Internet_df)
summary(Internet_df)
var(Internet_df)

#Descriptive statistics 
#histograms
histogram(Age_grp, data = Internet_df)
histogram(Discipline, data = Internet_df)

#scatterplot
scatterplot(data = Internet_df,
            x= Internet_usg, 
            y= SQR_Total,
            best_fit_line = TRUE)

#correlation
cor_model <- cor.test(x = Internet_df$Internet_usg,
                      y = Internet_df$Internet_Add)
cor_model

Internet_df <- read_csv("IAT_data_imported.csv")

# User defined functions --------------------------------------------------

# for replacing values with NA
replace_with_na <- function(variable, value){
  plyr::mapvalues(variable, from = value, to = NA)
}

# for normalizing variables, i.e. so they have mean of 0 and sd of 1
normalize_variable <- function(variable){
  scale(variable)[,1]
}

# Read in data ------------------------------------------------------------

IAT_raw_df <- read_csv("IAT_data_imported.csv")

# Replace missing values --------------------------------------------------

# replace the 2 with NA in the `Gender` variable
# replace the value `9` with NA in the `Age_grp` variable
# and replace `7` with NA in `Level_study`, `Yr_study`, `Discipline`
# and replace 99 with NA in any internet usage item
# and replace 7 with NA in any SQR item

IAT_clean_df <- IAT_raw_df %>% 
  mutate(Gender = replace_with_na(Gender, 2),
         Age_grp = replace_with_na(Age_grp, 9),
         across(Level_study:Discipline, ~replace_with_na(., 7)),
         across(stay_online:feel_depressed, ~replace_with_na(., 99)),
         across(headache:easily_tired, ~replace_with_na(., 7)))


# Rescale some variables --------------------------------------------------

IAT_clean_df <- mutate(IAT_clean_df,
                       # iun stands for "internet usage normalized"
                       iun = normalize_variable(Internet_usg),
                       # mhn stands for "mental health normalized"
                       mhn = normalize_variable(SQR_Total)
)


# Recode the data type in some variables ----------------------------------

# Some variables are categorical using the `factor` function. 

IAT_clean_df <- mutate(IAT_clean_df, across(Gender:Discipline, factor))

# Save cleaned data file as Rds -------------------------------------------

saveRDS(IAT_clean_df, file = 'IAT_clean.Rds')

# how many unique values in the `Gender` variable
Internet_df %>% count(Gender)

# how many unique values in the `Age_grp` variable
Internet_df %>% count(Age_grp)

# how many unique values in the `Level_study` variable
Internet_df %>% count(Level_study)

# how many unique values in the `stay_online` variable
Internet_df %>% count(stay_online)

# Replace missing values --------------------------------------------------

replace_with_na <- function(variable, value){
  plyr::mapvalues(variable, from = value, to = NA)
}

# To replace the value `9` with NA in the `Age_grp` variable
# and replace `7` with NA in `Level_study`
# and replace 99 with NA in any internet usage question
# and replace 7 with NA in any SQR question
IAT_clean_df <- Internet_df %>%
  mutate(Age_grp = replace_with_na(Age_grp, 9),
         Level_study = replace_with_na(Level_study, 7),
         across(stay_online:feel_depressed, ~replace_with_na(., 99)),
         across(headache:easily_tired, ~replace_with_na(., 7)))

# linear regression models investigating the relationship between internet use and mental health

# Read in RDS data --------------------------------------------------------

IAT_clean_df <- readRDS('IAT_clean.Rds')

# Model 1: bivariate relationship -----------------------------------------

# internet use predicting mental health
M_1 <- lm(SQR_Total ~ Internet_usg, data = IAT_clean_df)
summary(M_1)

# same as above but using standardized/normalized variables
M_2 <- lm(mhn ~ iun, data = IAT_clean_df)
summary(M_2)

# replace internet usage with individual variables 
M_2a <- lm(mhn~ stay_online + neglect_chores + excitement + relationships + life_complaint + school_work + email_socialmedia + job_performance + defensive_secretive + disturbing_thoughts + online_anticipation + life_no_internet + act_annoyed + late_night_logins + feel_preoccupied + online_glued + time_cutdown + hide_online + more_online_time + feel_depressed , data = IAT_clean_df)
summary(M_2a)

# adding gender, age etc as covariates- accounting for demographic variables 
M_3 <- lm(mhn ~ iun + Gender + Age_grp + Level_study + Discipline, data = IAT_clean_df)
summary(M_3)

M_3a <- lm(mhn ~ stay_online + neglect_chores + excitement + relationships + life_complaint + school_work + email_socialmedia + job_performance + defensive_secretive + disturbing_thoughts + online_anticipation + life_no_internet + act_annoyed + late_night_logins + feel_preoccupied + online_glued + time_cutdown + hide_online + more_online_time + feel_depressed + Gender + Age_grp + Level_study + Discipline, data = IAT_clean_df)
summary(M_3a)

# Type II anova 
car::Anova(M_3)
car::Anova(M_3a)

# Looking at interactions
M_4 <- lm(mhn ~ iun * (Gender + Age_grp + Level_study  + Discipline), data = IAT_clean_df)
summary(M_4)

M_4a <- lm(mhn ~ stay_online + neglect_chores + excitement + relationships + life_complaint + school_work + email_socialmedia + job_performance + defensive_secretive + disturbing_thoughts + online_anticipation + life_no_internet + act_annoyed + late_night_logins + feel_preoccupied + online_glued + time_cutdown + hide_online + more_online_time + feel_depressed * (Gender + Age_grp + Level_study  + Discipline), data = IAT_clean_df)
summary(M_4a)

car::Anova(M_4)
car::Anova(M_4a)


# Individual predictors and how they predict mental health
M_5<- lm(Discipline ~ SQR_Total, data = IAT_clean_df)
summary(M_5)

M_6<- lm(Gender ~ SQR_Total, data = IAT_clean_df)
M_7<- lm(Age_grp ~ SQR_Total, data = IAT_clean_df)

summary(M_6)
summary(M_7)

# Missing data imputation with MICE ---------------------------------------

install.packages('mice')
library(mice)

# select only columns mhn, iun, Gender ... Discipline
IAT_clean_df_subset <-  select(IAT_clean_df, mhn, iun, Gender, Age_grp, Level_study, Discipline)

# Impute missing values 
IAT_clean_df_subset__imputed <- mice(IAT_clean_df_subset, m = 100)


# Manipulate M_1 <- lm(SQR_Total ~ Internet_usg, data = IAT_clean_df) with MICE

M_1__mice <- with(IAT_clean_df_subset__imputed,
                  lm(SQR_Total ~ Internet_usg, data = IAT_clean_df))  

# coefficients of model fit using missing data imputation
summary(pool(M_1__mice))
# R^2 of model (with confidence interval)
pool.r.squared(M_1__mice)
# adj R^2 of model (with confidence interval) 
pool.r.squared(M_1__mice, adjusted = TRUE)


# Manipulate M_2 <- lm(mhn ~ iun, data = IAT_clean_df) with MICE 

M_2__mice <- with(IAT_clean_df_subset__imputed,
                  lm(mhn ~ iun, data = IAT_clean_df))

summary(pool(M_2__mice))
pool.r.squared(M_2__mice)
pool.r.squared(M_2__mice, adjusted = TRUE)



# Re-do model M_3 above using the imputed data set M_3 <- lm(mhn ~ iun + Gender + Age_grp + Level_study + Discipline, data = IAT_clean_df)
M_3__mice <- with(IAT_clean_df_subset__imputed,
                  lm(mhn ~ iun + Gender + Age_grp + Level_study + Discipline, data = IAT_clean_df)
)


summary(pool(M_3__mice))
pool.r.squared(M_3__mice) 
pool.r.squared(M_3__mice, adjusted = TRUE)

# D1 - dropping each demographic variable one at a time and completing a model comparison

M_3__mice__drop_Age <- with(IAT_clean_df_subset__imputed,
                            lm(mhn ~ iun + Gender + Level_study + Discipline, data = IAT_clean_df)
)
anova(M_3__mice__drop_Age, M_3__mice)


M_3__mice__drop_Gender <- with(IAT_clean_df_subset__imputed,
                               lm(mhn ~ iun + Age_grp + Level_study + Discipline, data = IAT_clean_df)
)

M_3__mice__drop_Level_study <- with(IAT_clean_df_subset__imputed,
                                    lm(mhn ~ iun + Age_grp + Gender + Discipline, data = IAT_clean_df)
)

M_3__mice__drop_Discipline <- with(IAT_clean_df_subset__imputed,
                                   lm(mhn ~ iun + Age_grp + Level_study + Gender, data = IAT_clean_df)
)

# Type II Anova 

anova(M_3__mice__drop_Gender, M_3__mice)
anova(M_3__mice__drop_Level_study, M_3__mice)
anova(M_3__mice__drop_Discipline, M_3__mice)



#M_4 <- lm(mhn ~ iun * (Gender + Age_grp + Level_study  + Discipline), data = IAT_clean_df) Manipulated with MICE
M_4__mice <- with(IAT_clean_df_subset__imputed,
                  lm(mhn ~ iun * (Gender + Age_grp + Level_study + Discipline), data = IAT_clean_df))

summary(pool(M_4__mice))
pool.r.squared(M_4__mice)
pool.r.squared(M_4__mice, adjusted = TRUE)

#D1 and model comparison for each demographic variable 
M_4__mice__drop_Age <- with(IAT_clean_df_subset__imputed,
                            lm(mhn ~ iun * (Gender + Level_study + Discipline), data = IAT_clean_df)
)

anova(M_4__mice__drop_Age, M_4__mice)

M_4__mice__drop_Gender <- with(IAT_clean_df_subset__imputed,
                               lm(mhn ~ iun * (Age_grp + Level_study + Discipline), data = IAT_clean_df)
)

anova(M_4__mice__drop_Gender, M_4__mice)

M_4__mice__drop_Level_study <- with(IAT_clean_df_subset__imputed,
                                    lm(mhn ~ iun * (Age_grp + Gender + Discipline), data = IAT_clean_df)
)

anova(M_4__mice__drop_Level_study, M_4__mice)

M_4__mice__drop_Discipline <- with(IAT_clean_df_subset__imputed,
                                   lm(mhn ~ iun * (Age_grp + Level_study + Gender), data = IAT_clean_df)
)

anova(M_4__mice__drop_Discipline, M_4__mice)


#M_5<- lm(Discipline ~ SQR_Total, data = IAT_clean_df)

M_5__mice <- with(IAT_clean_df_subset__imputed,
                  lm(Discipline ~ SQR_Total, data = IAT_clean_df))                

summary(pool(M_5__mice))
pool.r.squared(M_5__mice)
pool.r.squared(M_5__mice, adjusted = TRUE)
warnings()


# Factor analysis

# get internet use items
internet_df <- select(IAT_clean_df, stay_online:feel_depressed)

# find number of factors

fa.parallel(internet_df, fm = 'ml', fa = 'fa', n.iter = 1000)

# fit model with 5 factors

fa_model <- fa(internet_df, nfactors = 5)

# project the 20 internet items onto the five latent variables so can be used in a new analyis

IAT_clean_fa_df <-bind_cols(IAT_clean_df,predict(fa_model, internet_df, missing = TRUE))


# project onto latent factors

cronbach_result <- (IAT_clean_df)

print (cronbach_result)

# Linear regression with 5 latent variables 

M_6 <- lm(mhn ~ MR1 + MR2 + MR3 + MR4 + MR5 + Gender + Age_grp + Level_study + Discipline, data = IAT_clean_fa_df)

summary(M_6)

write_csv(IAT_clean_fa_df,file = 'IAT_clean_fa_df.csv')
