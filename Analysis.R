# Load packages and datasets ####
pacman::p_load(tidyverse, anesrake, openxlsx, survey, modelsummary, flextable, stringi)
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
# 622 non-responses (25%), 250 not in platform (10%), 643 broader-than-promises (26%), and 978 actual promises (39%)
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
Survey$actual_pledge_recalled <- factor(Survey$actual_pledge_recalled,
  levels = c("No actual pledge recalled", "Actual pledge recalled")
)
table(Survey$actual_pledge_recalled, useNA = "always")

Survey$quality_of_recall <- NA
Survey$quality_of_recall[Survey$promise_number == "0.0.0"] <- "Don't know / Refused to answer"
Survey$quality_of_recall[Survey$promise_number == "0.0.0.0"] <- "Pledge not found in party platform"
Survey$quality_of_recall[Survey$promise_number %in% c("0.0.0.0.0", "0.0.0.0.0.0")] <- "Too broad to be a pledge"
Survey$quality_of_recall[Survey$actual_pledge_recalled == "Actual pledge recalled"] <- "Actual pledge recalled"
Survey$quality_of_recall <- factor(Survey$quality_of_recall,
  levels = c("Don't know / Refused to answer", "Pledge not found in party platform", "Too broad to be a pledge", "Actual pledge recalled")
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
Survey$sovereigntyrelatedpledgeoranything <- 0
Survey$sovereigntyrelatedpledgeoranything[Survey$sov_y_n == 1] <- 1
Survey$sovereigntyrelatedpledgeoranything <- factor(
  ifelse(Survey$sovereigntyrelatedpledgeoranything == 1, "Sovereignty-related pledge recalled", "Other answer"),
  levels = c("Other answer", "Sovereignty-related pledge recalled")
)
table(Survey$sovereigntyrelatedpledgeoranything, useNA = "always")

## Liberal Party identification ####
table(Survey$fed_pid, useNA = "always")
Survey$pid_liberal <- factor(
  ifelse(Survey$fed_pid == 1, "Liberal", "Other"),
  levels = c("Other", "Liberal")
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
table(Survey$weights, useNA = "always")
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
# compute weighted shares and SE by party & pledge type
PartyRecallSovereigntyPlotWeightedStats <- Survey |>
  filter(!is.na(pid_party), !is.na(sovereigntyrelatedpledge)) |>
  group_by(pid_party) |>
  dplyr::summarize(
    total_w = sum(weights, na.rm = TRUE),
    sum_w2  = sum(weights^2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  right_join(
    Survey |>
      filter(!is.na(pid_party), !is.na(sovereigntyrelatedpledge)) |>
      group_by(pid_party, sovereigntyrelatedpledge) |>
      dplyr::summarize(sub_w = sum(weights, na.rm = TRUE), .groups = "drop"),
    by = "pid_party"
  ) |>
  mutate(
    p = sub_w / total_w,
    neff = (total_w^2) / sum_w2,
    se = sqrt(p * (1 - p) / neff),
    p100 = p * 100,
    se100 = se * 100,
    lower = pmax(0, p100 - 1.96 * se100),
    upper = pmin(100, p100 + 1.96 * se100)
  ) |>
  dplyr::filter(sovereigntyrelatedpledge == "Sovereignty-related pledge recalled")

ggplot(PartyRecallSovereigntyPlotWeightedStats, aes(x = pid_party, y = p100)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
   position = position_dodge(width = 0.75), width = 0.2, size = 0.8) +
  geom_point(aes(x = pid_party, y = p100),
   position = position_dodge(width = 0.75), size = 3, inherit.aes = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Party identification",
       y = "Weighted share of respondents who\ncited sovereignty-related pledges (%)") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("_SharedFolder_carney-nationalism/_graph/PartyRecallSovereigntyPlotWeighted.png", width = 5.5, height = 4.25)
sum(PartyRecallSovereigntyPlotWeightedStats$total_w)
sum(PartyRecallSovereigntyPlotWeightedStats$sub_w)
sum(PartyRecallSovereigntyPlotWeightedStats$sum_w2)

# same plot but for actual pledge recalled (weighted) instead of sovereignty-related pledge recalled
PartyRecallActualPlotWeightedStats <- Survey |>
  filter(!is.na(pid_party), !is.na(actual_pledge_recalled)) |>
  group_by(pid_party) |>
  dplyr::summarize(
    total_w = sum(weights, na.rm = TRUE),
    sum_w2  = sum(weights^2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  right_join(
    Survey |>
      filter(!is.na(pid_party), !is.na(actual_pledge_recalled)) |>
      group_by(pid_party, actual_pledge_recalled) |>
      dplyr::summarize(sub_w = sum(weights, na.rm = TRUE), .groups = "drop"),
    by = "pid_party"
  ) |>
  mutate(
    p = sub_w / total_w,
    neff = (total_w^2) / sum_w2,
    se = sqrt(p * (1 - p) / neff),
    p100 = p * 100,
    se100 = se * 100,
    lower = pmax(0, p100 - 1.96 * se100),
    upper = pmin(100, p100 + 1.96 * se100)
  ) |>
  dplyr::filter(actual_pledge_recalled == "Actual pledge recalled")
ggplot(PartyRecallActualPlotWeightedStats, aes(x = pid_party, y = p100)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
   position = position_dodge(width = 0.75), width = 0.2, size = 0.8) +
  geom_point(aes(x = pid_party, y = p100),
   position = position_dodge(width = 0.75), size = 3, inherit.aes = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Party identification",
       y = "Weighted share of respondents who\nrecalled an actual pledge (%)") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("_SharedFolder_carney-nationalism/_graph/PartyRecallActualPlotWeighted.png", width = 5.5, height = 4.25)
sum(PartyRecallActualPlotWeightedStats$total_w)
sum(PartyRecallActualPlotWeightedStats$sub_w)
sum(PartyRecallActualPlotWeightedStats$sum_w2)
table(Survey$pid_party, Survey$actual_pledge_recalled, useNA = "always")

# add weights to this plot
# compute weighted shares and 95% CIs, then plot (returns ggplot object)
df <- Survey |>
  filter(!is.na(quality_of_recall), !is.na(weights))

overall_total <- sum(df$weights, na.rm = TRUE)
sum_w2_total  <- sum(df$weights^2, na.rm = TRUE)
neff_total    <- (overall_total^2) / sum_w2_total

QualityOfRecallWeightedStats <- df |>
  group_by(quality_of_recall) |>
  dplyr::summarize(
    sub_w = sum(weights, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    p = sub_w / overall_total,
    se = sqrt(p * (1 - p) / neff_total),
    p100 = p * 100,
    se100 = se * 100,
    lower = pmax(0, p100 - 1.96 * se100),
    upper = pmin(100, p100 + 1.96 * se100)
  )

ggplot(QualityOfRecallWeightedStats, aes(x = quality_of_recall, y = p100)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.8) +
  scale_y_continuous(limits = c(0, 50)) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  labs(
    x = "Quality of recall",
    y = "Weighted share of\nrespondents (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("_SharedFolder_carney-nationalism/_graph/QualityOfRecallWeighted.png", width = 5.5, height = 4.25)
sum(QualityOfRecallWeightedStats$sub_w)

## H2: Pledge-definition treatment increases pledge recall. ####
# weighted t-test recalled ~ treatment
df_h2 <- Survey |>
  filter(!is.na(treatment), !is.na(actual_pledge_recalled), !is.na(weights)) |>
  mutate(
    recalled_num = as.numeric(actual_pledge_recalled == "Actual pledge recalled")
  )

design_h2 <- svydesign(ids = ~1, weights = ~weights, data = df_h2)

group_means_h2 <- svyby(
  ~recalled_num,
  ~treatment,
  design_h2,
  svymean,
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE
) |>
  as_tibble() |>
  rename(mean = recalled_num, se = se, lower = ci_l, upper = ci_u) |>
  mutate(across(c(mean, lower, upper), ~ .x * 100)) # percent scale

ttest_h2 <- svyttest(recalled_num ~ treatment, design = design_h2)

ttest_h2_tidy <- tibble(
  estimate_diff = as.numeric(ttest_h2$estimate), # difference in means (Treatment - Control)
  statistic = as.numeric(ttest_h2$statistic),
  parameter = as.numeric(ttest_h2$parameter),
  p_value = as.numeric(ttest_h2$p.value)
)

list(
  design = design_h2,
  group_means = group_means_h2,
  ttest = ttest_h2_tidy
)
# plot group means with 95% CIs
ggplot(group_means_h2, aes(x = treatment, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.8) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  labs(
    x = "Treatment condition",
    y = "Weighted share of respondents who\nrecalled an actual pledge (%)"
  ) +
  theme_minimal()
ggsave("_SharedFolder_carney-nationalism/_graph/H2_PledgeRecallTreatmentEffect.png", width = 5.5, height = 4.25)

## H3: Perceived importance of culture and nationalism mediates the relationship between sovereignty-pledge recognition and Liberal support. ####
# Model: total effect (sovereigntyrelatedpledgeoranything ~ pid + controls)
ModelSovereigntyPID <- glm(sovereigntyrelatedpledgeoranything ~ pid_liberal, family = binomial(), data = Survey)
summary(ModelSovereigntyPID)

ModelSovereigntyPIDCtrl <- glm(sovereigntyrelatedpledgeoranything ~ pid_liberal + education + language + province + age + gender,
  family = binomial(), data = Survey)
summary(ModelSovereigntyPIDCtrl)

# Nationalism model: importance_culture ~ pid_liberal + controls
ModelNationalismPID <- lm(importance_culture ~ pid_liberal, data = Survey)
summary(ModelNationalismPID)

ModelNationalismPIDCtrl <- lm(importance_culture ~ pid_liberal + education + language + province + age + gender,
  data = Survey)
summary(ModelNationalismPIDCtrl)

# Model: outcome controlled for mediator (sovereigntyrelatedpledgeoranything ~ pid_liberal + importance_culture + controls)
ModelH3 <- glm(sovereigntyrelatedpledgeoranything ~ pid_liberal + importance_culture, family = binomial(), data = Survey)
summary(ModelH3)

ModelH3Ctrl <- glm(sovereigntyrelatedpledgeoranything ~ pid_liberal + importance_culture +
  education + language + province + age + gender, family = binomial(), data = Survey)
summary(ModelH3Ctrl)

# Display results with modelsummary and create a flextable from the three models and save as a Word document
modelsummary::modelsummary(
  models = list("(1)" = ModelNationalismPID, "(2)" = ModelNationalismPIDCtrl),
  output = "flextable",
  statistic = "std.error",
  exponentiate = TRUE,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Deviance|Num\\.obs\\.",
  notes = c("Outcome: Importance of Culture and Nationalism", "Standard errors in parentheses.", "Method: OLS regression."),
  coef_rename = c(
    "pid_liberalLiberal" = "Party ID: Liberal",
    "educationUniversity" = "Education: University",
    "languageFrench" = "Language: French",
    "languageOther" = "Language: Other",
    "provinceWest" = "Region: Western Canada",
    "provinceQuebec" = "Region: Quebec",
    "age35–54" = "Age: 35–54",
    "age55+" = "Age: 55+",
    "genderWoman" = "Gender: Woman"
)) |>
  flextable::save_as_docx(path = "_SharedFolder_carney-nationalism/_output/models_culture.docx")

modelsummary::modelsummary(
  list(
    "(3)" = ModelSovereigntyPID,
    "(4)" = ModelH3,
    "(5)" = ModelSovereigntyPIDCtrl,
    "(6)" = ModelH3Ctrl
  ),
  output = "flextable",
  statistic = "std.error",
  exponentiate = FALSE,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Deviance|Num\\.obs\\.",
  notes = c("Outcome: Sovereignty-Related Pledge Recalled", "Standard errors in parentheses.",
   "Method: Binomial logistic regression. Coefficients for Models 2 and 3 are odds ratios."),
  coef_rename = c(
    "pid_liberalLiberal" = "Party ID: Liberal",
    "importance_culture" = "Importance of Culture and Nationalism",
    "educationUniversity" = "Education: University",
    "languageFrench" = "Language: French",
    "languageOther" = "Language: Other",
    "provinceWest" = "Region: Western Canada",
    "provinceQuebec" = "Region: Quebec",
    "age35–54" = "Age: 35–54",
    "age55+" = "Age: 55+",
    "genderWoman" = "Gender: Woman"
)) |>
  flextable::save_as_docx(path = "_SharedFolder_carney-nationalism/_output/models_sovereigntyrecall.docx")
table(Survey$importance_culture, Survey$sovereigntyrelatedpledgeoranything, useNA = "always")
table(Survey$pid_liberal, Survey$sovereigntyrelatedpledgeoranything, useNA = "always")

ActualPledgeDataset <- Survey |>
  filter(actual_pledge_recalled == "Actual pledge recalled")
prop.table(table(ActualPledgeDataset$pid_liberal, ActualPledgeDataset$promise_liberal, useNA = "always"), 1)

# Save top pledges recalled to XLSX ####
TopPledges <- Survey |>
  filter(actual_pledge_recalled == "Actual pledge recalled") |>
  group_by(promise_number) |>
  dplyr::summarize(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  left_join(
    Survey |>
      select(promise_number, label_en, label_fr, sovereigntyrelatedpledge, political_party) |>
      distinct(),
    by = "promise_number"
  )
openxlsx::write.xlsx(TopPledges, "_SharedFolder_carney-nationalism/_output/TopPledges.xlsx")


#### Nic's code ####
# === Nettoyer texte ===
clean_text <- function(x) {
  x <- str_to_lower(x)
  x <- str_trim(x)
  x <- str_squish(x)
  x <- str_remove_all(x, "[[:punct:]]")
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  return(x)
}
Survey$promise_clean <- clean_text(Survey$promise_mostImport_s)
Survey$promise_d_clean <- clean_text(Survey$promise_mostImport_d)
Survey$issue_clean <- clean_text(Survey$issue_mostImport_ope)

# === Catégoriser ===
categorize <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    # Économie / Commerce / Coût de la vie
    str_detect(x, "econom|aconomie|economy|economic|emploi|job|travail|work|chomage|unemploy|entreprise|business|invest|commerce|trade|tarif|tariff|libre.echange|free.trade|import|export|productivit|cost of living|pouvoir dachat|cout de la vie|couts de la vie|croissance|growth|capital expenditure|pro growth|ressources|inflation|negociation|negocier|production petrol") ~ "Économie / Commerce",
    # Santé
    str_detect(x, "sante|santa|health|hopital|hospital|medic|infirm|nurse|doctor|pharma|soins|care|mefaits") ~ "Santé",
    # Logement
    str_detect(x, "logement|housing|loyer|rent|maison|house|habitation|propriete|property|immobil|domicile") ~ "Logement",
    # Environnement / Énergie
    str_detect(x, "environ|climat|climate|vert|green|carbone|carbon|petrole|oil|pipeline|energie|energy|plastique|plastic|emission|nature") ~ "Environnement / Énergie",
    # Sécurité / Crime
    str_detect(x, "crime|criminalite|securite|security|loi.*ordre|ordre.*loi|reduce crime|police") ~ "Sécurité / Crime",
    # Québec / Bloc — AVANT Souveraineté
    str_detect(x, "quebec|bloc|interets du quebec|chien garde|droits du quebec|nos interets|nos droits|clause derogatoire|laicite|majorite.*provinces|droit.*provinces|ma langue|notre culture|unicite|specificite quebecoise|lois du quebec|interets quebecois") ~ "Québec / Bloc",
    # Défense / Souveraineté / Trump / USA
    str_detect(x, "souverain|sovereign|independ|defense|defence|defendre|militaire|military|armee|army|nato|otan|trump|etats.unis|united.states|usa|americain|american|tarri|frontiere|border|menaces|ferme|firm|crise douaniere|proteger.*canada|canada.*proteger|eunis|atatsunis|foreign affairs|surtaxe.*americain") ~ "Souveraineté / Défense / USA",
    # Éducation
    str_detect(x, "educa|aducation|ecole|school|universi|etudiant|student") ~ "Éducation",
    # Immigration
    str_detect(x, "immigra|migra|refugie|refugee|asylum") ~ "Immigration",
    # Fiscalité / Budget / Transferts
    str_detect(x, "taxe|tax|impot|fiscal|budget|deficit|dette|debt|depense|spending|transfert|financement|cadres financiers|finances.*ordre") ~ "Fiscalité / Budget",
    # Canada fort / Unité / Leadership / Respect
    str_detect(x, "strong canada|canada strong|canada fort|unite|unity|nation|pays|country|canadian|canadien|fiert|pride|together|leader.*fort|leadership|governing for everyone|respect.*canada|canada.*respect|institutions|democratie|qualite de vie") ~ "Canada fort / Leadership",
    # Gouvernance / Réforme / Liberté
    str_detect(x, "efficacit|gestion|gouvernement|reform|dereg|changer le systeme|gros bon sens|competence|serieux|wok|liberte|moins etat|representation|equite|responsab") ~ "Gouvernance / Réforme",
    # Services sociaux / Droits
    str_detect(x, "social|aide|help|pauvre|poverty|inequal|inegal|filet|welfare|assurance|acquis sociaux|protection des acquis|services") ~ "Services sociaux",
    # Infrastructure
    str_detect(x, "infrastruc|transport|route|road|train|transit|pont|bridge") ~ "Infrastructure / Transport",
    TRUE ~ "Autre"
  )
}
Survey$promise_cat <- categorize(Survey$promise_clean)
Survey$issue_cat <- categorize(Survey$issue_clean)

# === Mettre en NA les catégories non pertinentes ===
#drop_cats <- c("Autre", "Souveraineté / Défense / USA", "Canada fort / Leadership")
#Survey$promise_cat[Survey$promise_cat %in% drop_cats] <- NA_character_
#Survey$issue_cat[Survey$issue_cat %in% drop_cats] <- NA_character_

# === Filtrer too broad et aussi seulement ceux qui ont répondu aux deux questions ===
too_broad <- Survey  |>
  filter(promise_number %in% c("0.0.0.0.0", "0.0.0.0.0.0")) |>
  filter(!is.na(promise_cat) & !is.na(issue_cat))
cat("N répondants avec les deux réponses =", nrow(too_broad), "\n")
table(too_broad$promise_cat == too_broad$issue_cat, useNA = "always") # pas convaincu de ce résultat

# === Traduire les catégories ===
translate_cat <- function(x) {
  case_when(
    x == "Autre" ~ "Other",
    x == "Souveraineté / Défense / USA" ~ "Sovereignty / Defense / USA",
    x == "Canada fort / Leadership" ~ "Canada Strong / Leadership",
    x == "Économie / Commerce" ~ "Economy / Trade",
    x == "Santé" ~ "Health",
    x == "Logement" ~ "Housing",
    x == "Environnement / Énergie" ~ "Environment / Energy",
    x == "Sécurité / Crime" ~ "Security / Crime",
    x == "Québec / Bloc" ~ "Quebec / Bloc",
    x == "Éducation" ~ "Education",
    x == "Immigration" ~ "Immigration",
    x == "Fiscalité / Budget" ~ "Taxation / Budget",
    x == "Gouvernance / Réforme" ~ "Governance / Reform",
    x == "Services sociaux" ~ "Social Services",
    x == "Infrastructure / Transport" ~ "Infrastructure / Transport",
    TRUE ~ x
  )
}

# === Distributions ===
promise_both <- too_broad  |>
  count(category = translate_cat(promise_cat))  |>
  mutate(source = "Most important pledge",
         prop = n / sum(n))

issue_both <- too_broad  |>
  count(category = translate_cat(issue_cat))  |>
  mutate(source = "Most important issue",
         prop = n / sum(n))

combined_both <- bind_rows(promise_both, issue_both)

# === Graphique ===
ggplot(combined_both, aes(x = reorder(category, prop), y = prop, fill = source)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Most important pledge" = "#2C3E50",
                                "Most important issue" = "#95A5A6")) +
  labs(
    title = "Do 'Too Broad' Respondents Name the Same Issue Twice?",
    subtitle = paste0("Comparing recalled pledge topic with most important issue (n = ", nrow(both), ")"),
    x = NULL,
    y = "Share of responses",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# 1. Pour les répondants qui ont nommé une promesse trop large pour être une promesse, quel genre d'enjeu mentionnent-ils comme
# most important issue : la même chose que la promesse ou quelque chose de différent? Peut-être juste comparer visuellement le contenu
# de Survey$promise_mostImport_d / Survey$promise_mostImport_s avec le contenu de Survey$issue_mostImport_ope lorsque
# Survey$quality_of_recall == "Too broad to be a pledge" et trouver des catégories de réponses. Tu pourrais juste noter quelques
# grandes tendances qualitativement ici je crois.

# Among the 145 respondents whose recalled pledge was coded as "too broad," Economy/Trade dominates both the recalled pledge and
# the most important issue, accounting for roughly 40% of responses in each case. The close alignment between the two bars across
# most categories, particularly Economy/Trade, Quebec/Bloc, and Environment/Energy, suggests that these respondents are projecting
# their own policy priorities rather than recalling specific campaign commitments.

# 2. Analyser le contenu de Survey$issue_mostImport_ope avec des dictionnaires d'enjeux, entre autres pour y repérer les mots-clés liés
# à la souveraineté canadienne et à l'inflation/le cout de la vie. Pour le cout de la vie, tu peux utiliser le dictionnaire d'enjeux
# qui se trouve sur tube; les étapes 0, 1, 4 et 5 te permettront de l'analyser. tube::ellipse_query(con, "dict-subcategories") puis
# cost_life. Pour la souveraineté, je pense que tu devrais en créer un custom à partir des mots-clés qu'on utilise dans Data and
# Methods dans l'article.

# Coût de la vie / inflation : 421 répondants (13.8%)
# Souveraineté canadienne : 567 répondants (18.6%)