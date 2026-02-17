# Load packages and datasets ####
pacman::p_load(tidyverse, anesrake, openxlsx)
Survey <- openxlsx::read.xlsx("_SharedFolder_carney-nationalism/_data/Survey_Questionnaire_With_Coding.xlsx")
Survey <- Survey[3:nrow(Survey),]
DataForJoin <- openxlsx::read.xlsx("_SharedFolder_carney-nationalism/_data/OppositionPartyPledges.xlsx")
Census21 <- read.csv("_SharedFolder_carney-nationalism/_data/Census2021_ind.csv")

# Clean and rename ####
## Age ####
table(Survey$ses_age, useNA = "always")
Survey$ses_age <- as.integer(Survey$ses_age)
Survey$age <- NA_integer_
Survey$age[Survey$ses_age %in% c(18, 34)] <- 1 # 18-34
Survey$age[Survey$ses_age %in% c(35, 54)] <- 2 # 35-54
Survey$age[Survey$ses_age >= 55] <- 3 # 55+
Survey$age <- factor(
  ifelse(Survey$age == 1, "18–34",
  ifelse(Survey$age == 2, "35–54", "55+")),
  levels = c("18–34", "35–54", "55+")
)
table(Survey$age, useNA = "always")

table(Census21$AGEGRP, useNA = "always")
Census21$age <- NA_integer_
Census21$age[Census21$AGEGRP %in% seq(7, 10)] <- 1 # 18-34
Census21$age[Census21$AGEGRP %in% seq(11, 14)] <- 2 # 35-54
Census21$age[Census21$AGEGRP %in% seq(15, 21)] <- 3 # 55+
Census21$age <- factor(
  ifelse(Census21$age == 1, "18–34",
  ifelse(Census21$age == 2, "35–54", "55+")),
  levels = c("18–34", "35–54", "55+")
)
table(Census21$age, useNA = "always")
Census21 <- Census21[complete.cases(Census21$age),] # remove underaged respondents from dataset for proportions

## Gender ####
table(Survey$ses_sex, useNA = "always")
Survey$ses_sex <- as.integer(Survey$ses_sex)
Survey$gender <- NA_integer_ # other gender
Survey$gender[Survey$ses_sex == 2] <- 1 # man
Survey$gender[Survey$ses_sex == 1] <- 2 # woman
Survey$gender <- factor(
  ifelse(Survey$gender == 1, "Man", "Woman"),
  levels = c("Man", "Woman"))
table(Survey$gender, useNA = "always")

table(Census21$Gender, useNA = "always")
Census21$gender <- 3 - Census21$Gender
Census21$gender <- factor(
  ifelse(Census21$gender == 1, "Man", "Woman"),
  levels = c("Man", "Woman"))
table(Census21$gender, useNA = "always")

## Education ####
table(Survey$ses_educ, useNA = "always")
Survey$ses_educ <- as.integer(Survey$ses_educ)
Survey$education <- NA_integer_
Survey$education[Survey$ses_educ <= 5] <- 1 # high school or less
Survey$education[Survey$ses_educ == 6] <- 2 # college
Survey$education[Survey$ses_educ > 6] <- 3 # university
Survey$education <- factor(
  ifelse(Survey$education == 1, "High school",
  ifelse(Survey$education == 2, "College", "University")),
  levels = c("High school", "College", "University")
)
table(Survey$education, useNA = "always")

table(Census21$HDGREE, useNA = "always")
Census21$education <- NA_integer_
Census21$education[Census21$HDGREE %in% seq(1, 2)] <- 1
Census21$education[Census21$HDGREE %in% seq(3, 7)] <- 2
Census21$education[Census21$HDGREE %in% seq(8, 13)] <- 3
Census21$education <- factor(
  ifelse(Census21$education == 1, "High school",
  ifelse(Census21$education == 2, "College", "University")),
  levels = c("High school", "College", "University")
)
table(Census21$education, useNA = "always")

## Income ####
table(Survey$ses_income, useNA = "always")
Survey$ses_income <- as.integer(Survey$ses_income)
Survey$income <- NA_integer_
Survey$income[Survey$ses_income <= 5]  <- 1
Survey$income[Survey$ses_income > 5 & Survey$ses_income <= 8] <- 2
Survey$income[Survey$ses_income > 8] <- 3
Survey$income <- factor(
  ifelse(Survey$income == 3, "High",
  ifelse(Survey$income == 2, "Mid", "Low")),
  levels = c("Low", "Mid", "High")
)
table(Survey$income, useNA = "always")

table(Census21$CFInc, useNA = "always")
Census21$income <- NA_integer_
Census21$income[Census21$CFInc %in% seq(1, 16)] <- 1 # 0-60000
Census21$income[Census21$CFInc %in% seq(17, 29)] <- 2 # 60000-150000
Census21$income[Census21$CFInc %in% seq(30, 33)] <- 3 # 150000-...
Census21$income <- factor(
  ifelse(Census21$income == 3, "High",
  ifelse(Census21$income == 2, "Mid", "Low")),
  levels = c("Low", "Mid", "High")
)
table(Census21$income, useNA = "always")

## Language ####
table(Survey$ses_language, useNA = "always")
Survey$language <- as.integer(Survey$ses_language)
Survey$language <- factor(
  ifelse(Survey$language == 1, "English",
  ifelse(Survey$language == 2, "French", "Other")),
  levels = c("English", "French", "Other")
)
table(Survey$language, useNA = "always")

table(Census21$HLMOSTEN, useNA = "always")
table(Census21$HLMOSTFR, useNA = "always")
table(Census21$HLMOSTNO, useNA = "always")
Census21$language <- NA_integer_
Census21$language[Census21$HLMOSTEN == 1 & Census21$HLMOSTFR == 0] <- 1
Census21$language[Census21$HLMOSTEN == 0 & Census21$HLMOSTFR == 1] <- 2
Census21$language[Census21$HLMOSTNO > 1 & Census21$HLMOSTNO < 88] <- 3
Census21$language <- factor(
  ifelse(Census21$language == 1, "English",
  ifelse(Census21$language == 2, "French", "Other")),
  levels = c("English", "French", "Other")
)
table(Census21$language, useNA = "always")

## Province ####
table(Survey$ses_province, useNA = "always")
Survey$ses_province <- as.integer(Survey$ses_province)
Survey$province <- NA_integer_
Survey$province[Survey$ses_province >= 12 | Survey$ses_province <= 3 | Survey$ses_province == 7 | Survey$ses_province == 8] <- 1
Survey$province[Survey$ses_province == 9] <- 2
Survey$province[Survey$ses_province == 11]  <- 3
Survey$province[Survey$ses_province == 10 | (Survey$ses_province <= 6 & Survey$ses_province > 3)] <- 4
Survey$province <- factor(
  ifelse(Survey$province == 1, "West",
  ifelse(Survey$province == 2, "Ontario",
  ifelse(Survey$province == 3, "Quebec", "Atlantic"))),
  levels = c("West", "Ontario", "Quebec", "Atlantic")
)
table(Survey$province, useNA = "always")

table(Census21$PR, useNA = "always")
Census21$province <- NA_integer_
Census21$province[Census21$PR >= 46] <- 1 # West and North
Census21$province[Census21$PR == 35] <- 2
Census21$province[Census21$PR == 24]  <- 3
Census21$province[Census21$PR <= 13] <- 4
Census21$province <- factor(
  ifelse(Census21$province == 1, "West",
  ifelse(Census21$province == 2, "Ontario",
  ifelse(Census21$province == 3, "Quebec", "Atlantic"))),
  levels = c("West", "Ontario", "Quebec", "Atlantic")
)
table(Census21$province, useNA = "always")

# Census stats & target list ####
targets <- list(
  age = prop.table(table(Census21$age)), # 27% 18-34, 32.7% 35-54, 40.2% 55+
  gender = prop.table(table(Census21$gender)), # 51.1% women, 48.9% men
  education = prop.table(table(Census21$education)), # 40.6% high school, 28.8% college, 30.7% university
  income = prop.table(table(Census21$income)), # 31.5% low, 45.7% mid, 22.9% high (before tax)
  language = prop.table(table(Census21$lang)), # 64% English, 19.4% French, 16.7% other (first language spoken at home)
  province = prop.table(table(Census21$province)) # 31.6% West, 38.8% Ontario, 22.9% Quebec, 6.7% Atlantic
)

# Raking ####
anesrake::anesrakefinder(targets, Survey, choosemethod = "total")
outsave <- anesrake::anesrake(
  inputter = targets,
  dataframe = Survey,
  caseid = 1:length(Survey$gender))
summary(outsave)
Survey$weights <- outsave$weightvec # add weights to the dataset
summary(Survey$weights)
n <- length(Survey$income) # calculate weighting loss (design effect)
weighting_loss <- ((sum(Survey$weights^2) / (sum(Survey$weights))^2) * n) - 1 # Kish's Effective Sample Size formula
wpct(Survey$pid_liberal)
wpct(Survey$pid_liberal, Survey$weights)
