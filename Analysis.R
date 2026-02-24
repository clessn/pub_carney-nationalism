# Load packages and datasets ####
pacman::p_load(tidyverse, anesrake, openxlsx)
Survey <- openxlsx::read.xlsx("_SharedFolder_carney-nationalism/_data/Survey_Questionnaire_With_Coding.xlsx")
Survey <- Survey[3:nrow(Survey),]
OppositionPartyPledges <- openxlsx::read.xlsx("_SharedFolder_carney-nationalism/_data/OppositionPartyPledges.xlsx")
GovernmentPartyPledges <- openxlsx::read.xlsx("_SharedFolder_carney-nationalism/_data/PolimètreCarney_2026-02-16.xlsx", 2)
Census21 <- read.csv("_SharedFolder_carney-nationalism/_data/Census2021_ind.csv")

# Clean and rename ####
## Experimental treatment ####
table(Survey$promise_party, useNA = "always")
table(!is.na(Survey$promise_mostImport_d), !is.na(Survey$promise_mostImport_s), useNA = "always")
Survey$promise_mostImport_d[!is.na(Survey$promise_mostImport_d) & !is.na(Survey$promise_mostImport_s)]
Survey$promise_mostImport_s[!is.na(Survey$promise_mostImport_d) & !is.na(Survey$promise_mostImport_s)]
# surprisingly, 4 respondents have non-missing values for both the treatment and control condition
# # which should not be possible given the survey design. We will remove these respondents from the dataset.
Survey <- Survey[!( !is.na(Survey$promise_mostImport_d) & !is.na(Survey$promise_mostImport_s) ),]
Survey <- Survey[!( is.na(Survey$promise_mostImport_d) & is.na(Survey$promise_mostImport_s) ),]
Survey <- Survey[!is.na(Survey$promise_party),]
Survey$treatment <- NA_integer_
Survey$treatment[!is.na(Survey$promise_mostImport_d)] <- 1 # description provided
Survey$treatment[!is.na(Survey$promise_mostImport_s)] <- 0 # no description of a pledge provided
Survey$treatment <- factor(
  ifelse(Survey$treatment == 1, "Treatment", "Control"),
  levels = c("Control", "Treatment")
)
table(Survey$treatment, useNA = "always")

### Joining datasets with pledge information ####
table(Survey$promise_number, useNA = "always")
table(Survey$X85, useNA = "always") # 40 respondents identified a second pledge accurately
table(Survey$X86, useNA = "always") # no third promise
# 622 non-responses (25%), 250 hallucinated/not in platform (10%), 643 larger-than-promises (26%), and 978 actual promises (39%)
Survey <- left_join(Survey, OppositionPartyPledges, by = "promise_number")
table(Survey$political_party, useNA = "always")
class(Survey$political_party)
table(is.na(Survey$label_fr), useNA = "always")
table(is.na(Survey$label_en), useNA = "always")
GovernmentPartyPledges <- GovernmentPartyPledges |>
  select(Numéro, Libellé.FR, Libellé.EN, sov_y_n)
GovernmentPartyPledges$political_party <- "Libéral"
Survey <- left_join(Survey, GovernmentPartyPledges, by = c("promise_number" = "Numéro"))
Survey$political_party <- Survey$political_party.x
Survey$political_party[!is.na(Survey$political_party.y)] <- Survey$political_party.y[!is.na(Survey$political_party.y)]
table(Survey$political_party, useNA = "always")
Survey$label_fr[!is.na(Survey$Libellé.FR)] <- Survey$Libellé.FR[!is.na(Survey$Libellé.FR)]
Survey$label_en[!is.na(Survey$Libellé.EN)] <- Survey$Libellé.EN[!is.na(Survey$Libellé.EN)]
table(is.na(Survey$label_fr), useNA = "always")
table(is.na(Survey$label_en), useNA = "always")

### Party that made the recalled pledge ####
table(Survey$promise_party, useNA = "always")
Survey$promise_liberal <- factor(
  ifelse(Survey$promise_party == 1, "Liberal", "Other"),
  levels = c("Liberal", "Other")
)
table(Survey$promise_liberal, useNA = "always")
Survey$promise_party <- factor(as.numeric(Survey$promise_party), levels = seq(1, 7),
  labels = c("Liberal", "Conservative", "NDP", "Bloc", "Green", "People's", "Other")
)
table(Survey$promise_party, useNA = "always")
table(Survey$political_party, Survey$promise_party, useNA = "always")

### Recall of a pledge ####
Survey$actual_pledge_recalled <- "Actual pledge recalled"
Survey$actual_pledge_recalled[Survey$promise_number %in% c("0.0.0", "0.0.0.0", "0.0.0.0.0", "0.0.0.0.0.0")] <- "No actual pledge recalled"
Survey$actual_pledge_recalled[Survey$promise_number == "0.0.0"] <- NA
Survey$actual_pledge_recalled <- as.factor(Survey$actual_pledge_recalled)
table(Survey$actual_pledge_recalled, useNA = "always")

Survey$quality_of_recall <- NA
Survey$quality_of_recall[Survey$promise_number == "0.0.0"] <- "Don't know / Refused to answer"
Survey$quality_of_recall[Survey$promise_number == "0.0.0.0"] <- "Hallucinated / Not in platform"
Survey$quality_of_recall[Survey$promise_number %in% c("0.0.0.0.0", "0.0.0.0.0.0")] <- "Larger than actual pledge"
Survey$quality_of_recall[Survey$actual_pledge_recalled == "Actual pledge recalled"] <- "Actual pledge recalled"
Survey$quality_of_recall <- factor(Survey$quality_of_recall,
  levels = c("Don't know / Refused to answer", "Hallucinated / Not in platform", "Larger than actual pledge", "Actual pledge recalled")
)
table(Survey$quality_of_recall, useNA = "always")

### Mediating variable: Recall of a sovereignty-related pledge ####
table(Survey$sov_y_n.x, useNA = "always")
table(Survey$sov_y_n.y, useNA = "always")
Survey$sov_y_n <- Survey$sov_y_n.x
Survey$sov_y_n[is.na(Survey$sov_y_n.x)] <- Survey$sov_y_n.y[is.na(Survey$sov_y_n.x)]
table(Survey$sov_y_n, useNA = "always")
Survey$sovereigntyrelated <- 0
Survey$sovereigntyrelated[Survey$sov_y_n == 1] <- 1
Survey$sovereigntyrelated[Survey$promise_number == "0.0.0.0.0.0"] <- 1
Survey$sovereigntyrelated <- factor(
  ifelse(Survey$sovereigntyrelated == 1, "Sovereignty-related", "Non-sovereignty-related"),
  levels = c("Non-sovereignty-related", "Sovereignty-related")
)
table(Survey$sovereigntyrelated, useNA = "always")
Survey$sovereigntyrelatedpledge <- NA
Survey$sovereigntyrelatedpledge[Survey$sov_y_n == 1] <- 1
Survey$sovereigntyrelatedpledge[Survey$sov_y_n == 0] <- 0
Survey$sovereigntyrelatedpledge <- factor(
  ifelse(Survey$sovereigntyrelatedpledge == 1, "Sovereignty-related pledge recalled", "Non-sovereignty-related pledge recalled"),
  levels = c("Non-sovereignty-related pledge recalled", "Sovereignty-related pledge recalled")
)
table(Survey$sovereigntyrelatedpledge, useNA = "always")

## Liberal Party identification ####
table(Survey$fed_pid, useNA = "always")
Survey$pid_liberal <- factor(
  ifelse(Survey$fed_pid == 1, "Liberal", "Other"),
  levels = c("Liberal", "Other")
)
table(Survey$pid_liberal, useNA = "always")
Survey$pid_party <- factor(as.numeric(Survey$fed_pid), levels = seq(1, 7),
  labels = c("Liberal", "Conservative", "NDP", "Green", "Bloc", "Other", "None")
)
table(Survey$pid_party, useNA = "always")

## Importance of culture and nationalism ####
table(Survey$issue_importanceSlid_6, useNA = "always")
class(Survey$issue_importanceSlid_6)
Survey$importance_culture <- as.integer(Survey$issue_importanceSlid_6) - 1
table(Survey$importance_culture, useNA = "always")

## Age ####
table(Survey$ses_age, useNA = "always")
Survey$ses_age <- as.integer(Survey$ses_age)
Survey$age <- NA_integer_
Survey$age[Survey$ses_age %in% seq(18, 34)] <- 1 # 18-34
Survey$age[Survey$ses_age %in% seq(35, 54)] <- 2 # 35-54
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
Survey$education[Survey$ses_educ <= 6] <- 1 # 5 or less high school or less; 6 = college
Survey$education[Survey$ses_educ > 6] <- 2 # university
Survey$education <- factor(
  ifelse(Survey$education == 1, "High school or college", "University"),
  levels = c("High school or college", "University")
)
table(Survey$education, useNA = "always")

table(Census21$HDGREE, useNA = "always")
Census21$education <- NA_integer_
Census21$education[Census21$HDGREE %in% seq(1, 7)] <- 1 # 1+2 = high school or less, 3-7 = college
Census21$education[Census21$HDGREE %in% seq(8, 13)] <- 2 # university
Census21$education <- factor(
  ifelse(Census21$education == 1, "High school or college", "University"),
  levels = c("High school or college", "University")
)
table(Census21$education, useNA = "always")

## Income ####
table(Survey$ses_income, useNA = "always")
Survey$ses_income <- as.integer(Survey$ses_income)
Survey$income <- NA_integer_
Survey$income[Survey$ses_income <= 5]  <- 1
Survey$income[Survey$ses_income > 5 & Survey$ses_income <= 7] <- 2
Survey$income[Survey$ses_income > 7] <- 3
Survey$income <- factor(
  ifelse(Survey$income == 3, "High",
  ifelse(Survey$income == 2, "Mid", "Low")),
  levels = c("Low", "Mid", "High")
)
table(Survey$income, useNA = "always")

table(Census21$CFInc, useNA = "always")
Census21$income <- NA_integer_
Census21$income[Census21$CFInc %in% seq(1, 16)] <- 1 # 0-60000
Census21$income[Census21$CFInc %in% seq(17, 25)] <- 2 # 60000-110000
Census21$income[Census21$CFInc %in% seq(26, 33)] <- 3 # 110000+
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
Survey$province[Survey$ses_province >= 12 | Survey$ses_province <= 3 | Survey$ses_province == 7 | Survey$ses_province == 8] <- 1 # West and North
Survey$province[Survey$ses_province == 11]  <- 2 # Quebec
Survey$province[Survey$ses_province == 9 | Survey$ses_province == 10 | (Survey$ses_province <= 6 & Survey$ses_province > 3)] <- 3 # 9 = Ontario, other = Atlantic
Survey$province <- factor(
  ifelse(Survey$province == 1, "West",
  ifelse(Survey$province == 2, "Quebec", "Other")),
  levels = c("Other", "West", "Quebec")
)
table(Survey$province, useNA = "always")

table(Census21$PR, useNA = "always")
Census21$province <- NA_integer_
Census21$province[Census21$PR >= 46] <- 1 # West and North
Census21$province[Census21$PR == 24] <- 2 # Quebec
Census21$province[Census21$PR == 35 | Census21$PR <= 13] <- 3 # 35 = Ontario, others are Atlantic provinces
Census21$province <- factor(
  ifelse(Census21$province == 1, "West",
  ifelse(Census21$province == 2, "Quebec", "Other")),
  levels = c("West", "Quebec", "Other")
)
table(Census21$province, useNA = "always")

# Census stats & target list ####
targets <- list(
  age = prop.table(table(Census21$age)), # 27% 18-34, 32.7% 35-54, 40.2% 55+
  gender = prop.table(table(Census21$gender)), # 51.1% women, 48.9% men
  education = prop.table(table(Census21$education)), # 40.6% high school, 28.8% college, 30.7% university
  #income = prop.table(table(Census21$income)), # 31.5% low, 29.5% mid, 39% high (before tax)
  language = prop.table(table(Census21$language)), # 64% English, 19.4% French, 16.7% other (first language spoken at home)
  province = prop.table(table(Census21$province)) # 31.6% West, 38.8% Ontario, 22.9% Quebec, 6.7% Atlantic
)

# Raking ####
anesrake::anesrakefinder(targets, Survey, choosemethod = "total")
outsave <- anesrake::anesrake(
  inputter = targets,
  dataframe = Survey,
  caseid = 1:nrow(Survey))
summary(outsave)
Survey$weights <- outsave$weightvec # add weights to the dataset
summary(Survey$weights)
#Survey$valid <- Survey[Survey$weights > 0.5]
hist(Survey$weights)
((sum(Survey$weights^2) / (sum(Survey$weights))^2) * length(Survey$income)) - 1 # calculate weighting loss (design effect)
unweighted <-  wpct(Survey$pid_party)
weighted  <-  wpct(Survey$pid_party, Survey$weights)
data.frame(unweighted, weighted)

# Results ####
## H1: Liberal voters more likely to cite sovereignty/defense pledges and less likely to be unable to recall a promise. ####
# Plot party ID by recall of sovereignty-related pledge (weighted and unweighted)
table(Survey$pid_party, useNA = "always")
table(Survey$sovereigntyrelatedpledge, useNA = "always")
PartyRecallSovereigntyPlotWeighted <- Survey |>
  filter(!is.na(pid_party), !is.na(sovereigntyrelatedpledge), !is.na(weights)) |>
  group_by(pid_party, sovereigntyrelatedpledge) |>
  dplyr::summarize(number = sum(weights, na.rm = TRUE), .groups = "drop") |>
  ungroup()
# wrap legend labels into (at most) two lines when > 15 chars, recreate + save weighted plot
wrap_two_lines <- function(x, width = 15) {
  vapply(x, function(lbl) {
    if (nchar(lbl) > width) {
      paste(stringr::str_wrap(lbl, width = width), collapse = "\n")
    } else {
      lbl
    }
  }, FUN.VALUE = character(1))
}

ggplot(PartyRecallSovereigntyPlotWeighted, aes(x = pid_party, y = number, fill = sovereigntyrelatedpledge)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_discrete(labels = wrap_two_lines) +
  scale_y_continuous() +
  labs(
    x = "Party identification",
    y = "Number of respondents (weighted)",
    fill = "Promise type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("_SharedFolder_carney-nationalism/_graph/PartyRecallSovereigntyPlotWeighted.png", width = 5.5, height = 4.25)

PartyRecallSovereigntyPlot <- Survey |>
  filter(!is.na(pid_party), !is.na(sovereigntyrelatedpledge)) |>
  group_by(pid_party, sovereigntyrelatedpledge) |>
  dplyr::summarize(number = n())
ggplot(PartyRecallSovereigntyPlot, aes(x = pid_party, y = number, fill = sovereigntyrelatedpledge)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_discrete(labels = wrap_two_lines) +
  scale_y_continuous() +
  labs(
    x = "Party identification",
    y = "Number of respondents",
    fill = "Promise type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("_SharedFolder_carney-nationalism/_graph/PartyRecallSovereigntyPlot.png", width = 5.5, height = 4.25)

ggplot(Survey, aes(x = quality_of_recall)) +
  geom_bar() +
  labs(
    x = "Quality of recall",
    y = "Number of respondents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("_SharedFolder_carney-nationalism/_graph/QualityOfRecall.png", width = 5.5, height = 4.25)

## H2: Pledge-definition treatment increases pledge recall. ####
# weighted logit recalled ~ treatment
table(Survey$actual_pledge_recalled, useNA = "always")
ModelRecallTreatment <- glm(actual_pledge_recalled ~ treatment, family = binomial(), data = Survey, weights = Survey$weights)
summary(ModelRecallTreatment)

# predictions
newdata <- tibble(treatment = factor(c("Treatment", "Control"), levels = levels(Survey$treatment)))
pred <- predict(ModelRecallTreatment, newdata = newdata, se.fit = TRUE, type = "link")
est <- plogis(pred$fit)
se <- pred$se.fit
lower <- plogis(pred$fit - qnorm(0.975) * se)
upper <- plogis(pred$fit + qnorm(0.975) * se)
H2Plot <- newdata |>
  mutate(
    estimate = est,
    lower = lower,
    upper = upper
  )

# plot for H2
ggplot(H2Plot, aes(x = treatment, y = estimate, fill = treatment)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    x = "Experimental condition",
    y = "Estimated probability of recalling an actual pledge"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("_SharedFolder_carney-nationalism/_graph/H2Plot.png", width = 5.5, height = 4.25)

## H3: Perceived importance of culture and nationalism mediates the relationship between sovereignty-pledge recognition and Liberal support. ####
# Adjust models & prepare data for H3 regressions (keep only complete cases for relevant variables)
H3Data <- Survey |>
  mutate(
    pid_lib_bin = ifelse(pid_liberal == "Liberal", 1L, 0L)
  ) |>
  filter(
    !is.na(pid_lib_bin),
    !is.na(sovereigntyrelatedpledge),
    !is.na(importance_culture),
    !is.na(education),
    !is.na(language),
    !is.na(province),
    !is.na(age),
    !is.na(gender),
    !is.na(weights)
  )

# Model: total effect (pid ~ sovereigntyrelatedpledge + controls)
ModelSovereigntyPID <- glm(pid_lib_bin ~ sovereigntyrelatedpledge + education + language + province + age + gender,
  family = binomial(), data = H3Data, weights = weights)
summary(ModelSovereigntyPID)

# Mediator model: importance_culture ~ sovereigntyrelatedpledge + controls (weighted lm)
ModelMediator <- lm(
  importance_culture ~ sovereigntyrelatedpledge + education + language + province + age + gender,
  data = H3Data, weights = weights)
summary(ModelMediator)

# Model: outcome controlled for mediator (pid ~ sovereigntyrelatedpledge + importance_culture + controls)
ModelH3 <- glm(
  pid_lib_bin ~ sovereigntyrelatedpledge + importance_culture + education + language + province + age + gender,
  family = binomial(), data = H3Data, weights = weights)
summary(ModelH3)

# Display results with modelsummary and create a flextable from the three models and save as a Word document
modelsummary::modelsummary(
  list(
    "Outcome: Importance of Culture and Nationalism (1)" = ModelMediator,
    "Outcome: Liberal Party ID (2)" = ModelSovereigntyPID,
    "Outcome: Liberal Party ID + mediator (3)" = ModelH3
  ),
  output = "flextable",
  statistic = "std.error",
  exponentiate = c(TRUE, FALSE, FALSE),
  stars = T,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Deviance|Num\\.obs\\.",
  notes = c("Standard errors in parentheses.",
   "Model 1 is a linear regression, while Models 2 and 3 are logistic regressions. Coefficients for Models 2 and 3 are odds ratios."),
  coef_rename = c(
    "sovereigntyrelatedpledgeSovereignty-related pledge recalled" = "Sovereignty-related pledge recalled",
    "educationUniversity" = "Education: University",
    "languageFrench" = "Language: French",
    "languageOther" = "Language: Other",
    "provinceWest" = "Region: Western Canada",
    "provinceQuebec" = "Region: Quebec",
    "age35–54" = "Age: 35–54",
    "age55+" = "Age: 55+",
    "genderWoman" = "Gender: Woman",
    "importance_culture" = "Importance of Culture and Nationalism"
)) |>
  flextable::save_as_docx(path = "_SharedFolder_carney-nationalism/_output/H3_regression_results_modelsummary.docx")
