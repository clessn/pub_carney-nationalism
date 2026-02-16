# Load packages and datasets ####
pacman::p_load(tidyverse, anesrake, openxlsx)
Survey <- openxlsx::read.xlsx("_SharedFolder_carney-nationalism/_data/Survey_Questionnaire_With_Coding.xlsx")
Survey <- Survey[3:nrow(Survey),]
DataForJoin <- openxlsx::read.xlsx("_SharedFolder_carney-nationalism/_data/OppositionPartyPledges.xlsx")

# Clean and rename ####
## Gender ####
table(Survey$ses_sex, useNA = "always")
Survey$ses_sex <- as.integer(Survey$ses_sex)
Survey$female <- NA_integer_ # other gender
Survey$female[Survey$ses_sex == 2] <- 1 # man
Survey$female[Survey$ses_sex == 1] <- 2 # woman
Survey$female <- factor(
  ifelse(Survey$female == 1, "Man", "Woman"),
  levels = c("Man", "Woman"))
table(Survey$female, useNA = "always")

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

## Education ####
table(Survey$ses_educ, useNA = "always")
Survey$education <- NA_integer_
Survey$education[Survey$ses_educ <= 5] <- 1 # high school or less
Survey$education[Survey$ses_educ > 5] <- 2 # college or university
Survey$education <- factor(
  ifelse(Survey$education == 1, "Low", "High"),
  levels = c("Low", "High")
)
table(Survey$education, useNA = "always")
Survey$education <- factor(
  ifelse(Survey$educ_group %in% c("educBHS", "educHS"), "Low", "University"),
  levels = c("Low", "University")
)
table(Survey$education)

## Income ####
table(Survey$ses_income, useNA = "always")
Survey$income <- NA_integer_
Survey$income[Survey$ses_income3Cat %in% c("Low", "Mid")]  <- 1
Survey$income[Survey$ses_income3Cat == "High"] <- 2
Survey$income <- factor(
  ifelse(Survey$ses_income3Cat == "High", "High", "Low/Mid"),
  levels = c("Low/Mid", "High")
)
table(Survey$income) 

# Census info & target list
targets <- list(
  female = c("Woman/Other" = 0.51, "Man" = 0.49),
  age = c("18–34" = 0.24, "35–54" = 0.33, "55+" = 0.43),
  education = c("Low" = 0.71, "University" = 0.29),
  income = c("Low/Mid" = 0.835, "High" = 0.165)
)

str(Survey[, c("female", "age", "education", "income")])


# id variable
Survey$caseid <- 1:length(Survey$female)

anesrake::anesrakefinder(targets, Survey, choosemethod = "total")

outsave <- anesrake::anesrake(targets, Survey, caseid = Survey$caseid,
                    verbose= FALSE, cap = 5, choosemethod = "total",
                    type = "pctlim", pctlim = .05 , nlim = 5,
                    iterate = TRUE , force1 = TRUE)

summary(outsave)

# add weights to the dataset
Survey$weightvec <- outsave$weightvec  # safer and clearer

# calculate weighting loss (design effect)
n <- length(Survey$income)

# Kish's Effective Sample Size formula
weighting_loss <- ((sum(Survey$weightvec^2) / (sum(Survey$weightvec))^2) * n) - 1

# Checking
names(Survey)

unweighted <-  wpct(Survey$tradeoff_childcare_benefits_bin)
weighted  <-  wpct(Survey$tradeoff_childcare_benefits_bin, Survey$weightvec)
tab  <- data.frame(unweighted, weighted)

summary(Survey$weightvec)
class(Survey$weightvec)

sum(Survey$weightvec)   # should be ~ equal to n, if not normalized
mean(Survey$weightvec)
