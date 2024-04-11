
# Code for cleaning Dataset

#import data
Internet_df<- read_csv("IAT_data_imported.csv")
Internet_df
glimpse(Internet_df)
View(Internet_df)

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

# Converting demographic variables from continuous into categorical variables

IAT_clean_df <- mutate(IAT_clean_df, across(Gender:Discipline, factor))

# Save cleaned data file as Rds

saveRDS(IAT_clean_df, file = 'IAT_clean.Rds')

# how many unique values in the `Gender` variable
Internet_df %>% count(Gender)

# how many unique values in the `Age_grp` variable
Internet_df %>% count(Age_grp)

# how many unique values in the `Level_study` variable
Internet_df %>% count(Level_study)

# how many unique values in the `stay_online` variable
Internet_df %>% count(stay_online)

