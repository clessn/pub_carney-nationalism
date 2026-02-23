# Load packages and datasets ####
pacman::p_load(tidyverse, anesrake, openxlsx)
Survey <- openxlsx::read.xlsx("_SharedFolder_carney-nationalism/_data/Survey_Questionnaire_With_Coding.xlsx")
Survey <- Survey[3:nrow(Survey),]
DataForJoin <- openxlsx::read.xlsx("_SharedFolder_carney-nationalism/_data/OppositionPartyPledges.xlsx")
Census21 <- read.csv("_SharedFolder_carney-nationalism/_data/Census2021_ind.csv")

# Clean and rename ####
## Experimental treatment: Recall of a pledge ####
table(Survey$promise_party, useNA = "always")
table(!is.na(Survey$promise_mostImport_d), !is.na(Survey$promise_mostImport_s), useNA = "always")
Survey$promise_mostImport_d[!is.na(Survey$promise_mostImport_d) & !is.na(Survey$promise_mostImport_s)]
Survey$promise_mostImport_s[!is.na(Survey$promise_mostImport_d) & !is.na(Survey$promise_mostImport_s)]
# surprisingly, 4 respondents have non-missing values for both the treatment and control condition, which should not be possible given the survey design. We will remove these respondents from the dataset.
Survey <- Survey[!( !is.na(Survey$promise_mostImport_d) & !is.na(Survey$promise_mostImport_s) ),]
Survey <- Survey[!( is.na(Survey$promise_mostImport_d) & is.na(Survey$promise_mostImport_s) ),]
Survey <- Survey[!is.na(Survey$promise_party),]
Survey$treatment <- NA_integer_
Survey$treatment[!is.na(Survey$promise_mostImport_d)] <- 1
Survey$treatment[!is.na(Survey$promise_mostImport_s)] <- 0
Survey$treatment <- factor(
  ifelse(Survey$treatment == 1, "Treatment", "Control"),
  levels = c("Treatment", "Control")
)
table(Survey$treatment, useNA = "always")

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

### Mediating variable: Recall of a sovereignty-related pledge ####
table(Survey$promise_number, useNA = "always")
Survey <- left_join(Survey, DataForJoin, by = "promise_number")
table(Survey$political_party, Survey$promise_party, useNA = "always")
#na.omit(Survey$promise_number[Survey$political_party == "NPD" & Survey$promise_party == "Conservative"])
#na.omit(Survey$promise_number[Survey$political_party == "Conservateur" & Survey$promise_party == "Liberal"])

## Liberal Party identification ####
table(Survey$fed_pid, useNA = "always")
Survey$pid_liberal <- factor(
  ifelse(Survey$fed_pid == 1, "Liberal", "Other"),
  levels = c("Liberal", "Other")
)
table(Survey$pid_liberal, useNA = "always")
Survey$pid_party <- factor(as.numeric(Survey$fed_pid), levels = seq(1, 7),
  labels = c("Liberal", "Conservative", "NDP", "Bloc", "Green", "Other", "None")
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
  levels = c("West", "Quebec", "Other")
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
#wpct(Survey$pid_liberal)
#wpct(Survey$pid_liberal, Survey$weights)

# Results ####
## H1: Liberal voters more likely to cite sovereignty/defense pledges and less likely to be unable to recall a promise. ####
# créer la variable promise_type en détectant une colonne indiquant la souveraineté
sovereignty_col <- names(Survey)[grepl("sovereign|sov|souverain|sovereignty", names(Survey), ignore.case = TRUE)][1]
if (is.na(sovereignty_col)) {
  stop("Aucune colonne indiquant la souveraineté trouvée dans Survey. Joignez ou renommez la colonne correspondante dans DataForJoin.")
}

Survey <- Survey |>
  mutate(
    promise_type = dplyr::case_when(
      is.na(promise_number) ~ "Impossible de se rappeler",
      as.character(.data[[sovereignty_col]]) %in% c("1", "Yes", "yes", "Y", "y", "TRUE", "T") ~ "Souveraineté",
      TRUE ~ "Non-souveraineté"
    ),
    promise_type = factor(promise_type, levels = c("Souveraineté", "Non-souveraineté", "Impossible de se rappeler"))
  )

# calcul des parts pondérées par pid_liberal
df_plot <- Survey |>
  filter(!is.na(pid_liberal)) |>
  group_by(pid_liberal, promise_type) |>
  summarize(w = sum(weights, na.rm = TRUE), .groups = "drop") |>
  group_by(pid_liberal) |>
  mutate(share = w / sum(w)) |>
  ungroup()

# graphique à barres groupées
library(ggplot2)

p <- ggplot(df_plot, aes(x = pid_liberal, y = share, fill = promise_type)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Identification partisane",
    y = "Part (%) (pondérée)",
    fill = "Type de promesse",
    title = "Types de promesses identifiées selon identification libérale vs autres"
  ) +
  theme_minimal()

p

## H2: Pledge-definition treatment increases pledge recall. ####
# créer variable binaire (1 = a nommé une vraie promesse)
df_h2 <- Survey |>
  filter(!is.na(treatment), !is.na(weights)) |>
  mutate(
    recalled = ifelse(is.na(promise_type) | promise_type == "Impossible de se rappeler", 0L, 1L)
  )

# ajuster un logit pondéré recalled ~ treatment
fit_h2 <- glm(recalled ~ treatment, family = binomial(), data = df_h2, weights = weights)

# prédictions sur l'échelle du lien (logit) puis transformation et IC 95 %
newdata <- tibble(treatment = factor(c("Treatment", "Control"), levels = levels(df_h2$treatment)))
pred <- predict(fit_h2, newdata = newdata, se.fit = TRUE, type = "link")
est <- plogis(pred$fit)
se <- pred$se.fit
lower <- plogis(pred$fit - 1.96 * se)
upper <- plogis(pred$fit + 1.96 * se)

df_plot_h2 <- newdata |>
  mutate(
    estimate = est,
    lower = lower,
    upper = upper
  )

# graphique : barres avec IC 95 %
library(ggplot2)

p_h2 <- ggplot(df_plot_h2, aes(x = treatment, y = estimate, fill = treatment)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    x = "Condition expérimentale",
    y = "Part (%) ayant nommé une vraie promesse (pondérée)",
    title = "Effet du traitement sur le rappel d'une vraie promesse",
    subtitle = "Estimations et intervalles de confiance à 95 % (modèle logit pondéré)"
  ) +
  theme_minimal()

p_h2

## H3: Perceived importance of culture and nationalism mediates the relationship between sovereignty-pledge recognition and Liberal support. ####
# Ajustements de modèles pour H3 et tableau de régression
library(broom)

# Préparer les données (retenir observations complètes pour variables d'intérêt)
df_h3 <- Survey |>
  mutate(
    pid_lib_bin = ifelse(pid_liberal == "Liberal", 1L, 0L)
  ) |>
  filter(
    !is.na(pid_lib_bin),
    !is.na(promise_type),
    !is.na(importance_culture),
    !is.na(education),
    !is.na(language),
    !is.na(province),
    !is.na(age),
    !is.na(gender),
    !is.na(weights)
  )

# Modèle 1: effet total (pid ~ promise_type + controles)
model_total <- glm(
  pid_lib_bin ~ promise_type + education + language + province + age + gender,
  family = binomial(),
  data = df_h3,
  weights = weights
)

# Modèle du médiateur: importance_culture ~ promise_type + controles (lm pondéré)
mediator_model <- lm(
  importance_culture ~ promise_type + education + language + province + age + gender,
  data = df_h3,
  weights = weights
)

# Modèle 3: outcome contrôlé pour le médiateur (pid ~ promise_type + importance_culture + controles)
model_with_med <- glm(
  pid_lib_bin ~ promise_type + importance_culture + education + language + province + age + gender,
  family = binomial(),
  data = df_h3,
  weights = weights
)

# Fonction utilitaire pour tidy + CI; ajoute OR pour modèles logit
tidy_with_extras <- function(mod, name) {
  td <- broom::tidy(mod, conf.int = TRUE)
  td <- td |> mutate(model = name, term = term)
  if (inherits(mod, "glm") && family(mod)$family == "binomial") {
    td <- td |> mutate(
      OR = exp(estimate),
      OR_conf.low = exp(conf.low),
      OR_conf.high = exp(conf.high)
    )
  } else {
    td <- td |> mutate(OR = NA_real_, OR_conf.low = NA_real_, OR_conf.high = NA_real_)
  }
  td |> select(model, term, estimate, std.error, conf.low, conf.high, OR, OR_conf.low, OR_conf.high, p.value)
}

# Construire tableau combiné
reg_table <- bind_rows(
  tidy_with_extras(model_total, "Total effect (no mediator)"),
  tidy_with_extras(mediator_model, "Mediator (importance_culture)"),
  tidy_with_extras(model_with_med, "Outcome with mediator")
) |>
  arrange(model, term)

# Renvoyer les modèles et le tableau (objet utile en sortie)
reg_results <- list(
  models = list(
    model_total = model_total,
    mediator_model = mediator_model,
    model_with_med = model_with_med
  ),
  table = reg_table,
  data_used = df_h3
)

reg_results