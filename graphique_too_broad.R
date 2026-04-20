library(tube)
library(tidyverse)
library(quanteda)
library(ellipsetxt)

# === Connexion ===
con <- tube::ellipse_connect(env = "PROD")

# === Aller chercher les sous-catégories ===
df_dict_subcats <- tube::ellipse_query(con, "dict-subcategories") |>
  dplyr::collect()

# === Voir la structure ===
cat("=== Colonnes ===\n")
print(names(df_dict_subcats))

cat("\n=== Sous-catégories disponibles ===\n")
print(unique(df_dict_subcats$subcategory))

# === Filtrer cost_life ===
cost_life <- df_dict_subcats %>%
  filter(subcategory == "cost_life")

cat("\n=== Mots-clés cost_life ===\n")
print(cost_life)


cat("=== Catégories disponibles ===\n")
print(unique(df_dict_subcats$category))

# === Chercher cost_life ===
cost_life <- df_dict_subcats %>%
  filter(str_detect(tolower(category), "cost|life|cout|vie|inflation"))

cat("\n=== Résultats cost_life ===\n")
print(cost_life %>% select(category, language, item))



library(tube)
library(tidyverse)
library(quanteda)
library(ellipsetxt)

# === Connexion ===
con <- tube::ellipse_connect(env = "PROD")

# === Aller chercher le dictionnaire cost_life + inflation ===
df_dict_subcats <- tube::ellipse_query(con, "dict-subcategories") |>
  dplyr::collect()

cost_life_dict <- df_dict_subcats %>%
  filter(category %in% c("cost_life", "inflation")) %>%
  select(category, language, item)

cat("=== Mots-clés cost_life + inflation ===\n")
print(cost_life_dict, n = 81)

# === Construire le dictionnaire quanteda ===
quanteda_cost_life <-
  split(cost_life_dict$item, cost_life_dict$category) |>
  quanteda::dictionary()

print(quanteda_cost_life)

# === Préparer le Survey ===
Survey$issue_text <- fix_encoding(Survey$issue_mostImport_ope)

df_survey <- Survey %>%
  filter(!is.na(issue_text) & issue_text != "")

cat("\nN répondants avec issue_mostImport_ope:", nrow(df_survey), "\n")

# === Rouler le dictionnaire ===
result_cost <- ellipsetxt::run_dictionary(df_survey, issue_text, quanteda_cost_life, verbose = TRUE)

df_survey_cost <- cbind(df_survey, result_cost)

# === Résumé ===
cat("\n=== Hits cost_life ===\n")
cat("cost_life:", sum(df_survey_cost$cost_life, na.rm = TRUE), "\n")
cat("inflation:", sum(df_survey_cost$inflation, na.rm = TRUE), "\n")

# === Créer un flag binaire : au moins 1 hit ===
df_survey_cost$cost_life_flag <- (df_survey_cost$cost_life > 0 | df_survey_cost$inflation > 0)

cat("\n=== Répondants mentionnant coût de la vie / inflation ===\n")
cat("Oui:", sum(df_survey_cost$cost_life_flag), "\n")
cat("Non:", sum(!df_survey_cost$cost_life_flag), "\n")
cat("Proportion:", round(mean(df_survey_cost$cost_life_flag), 3), "\n")

# === Dictionnaire custom souveraineté (FR + EN séparés) ===
sovereignty_dict <- quanteda::dictionary(list(
  sovereignty_fr = c(
    # Souveraineté / autonomie
    "souveraineté", "souverain*", "autonomi*", "indépendan*",
    # Défense nationale
    "défense", "defense", "militaire*", "armée", "armee", "otan", "forces armées",
    # USA / Trump / tarifs
    "trump", "états-unis", "etats-unis", "américain*", "americain*",
    "tarif*", "douane*", "douanier*", "surtaxe*", "guerre commerciale",
    # Intégration interprovinciale / résilience
    "interprovincial*", "libre-échange interprovincial", "commerce interprovincial",
    "dépendance", "dependance", "résilien*", "resilien*",
    "autosuffisan*", "approvisionnement",
    # Nation-building
    "nation*", "protéger le canada", "proteger le canada",
    "défendre le canada", "defendre le canada",
    "frontière*", "frontiere*", "sécurité nationale", "securite nationale"
  ),
  sovereignty_en = c(
    # Sovereignty / autonomy
    "sovereignty", "sovereign*", "autonomy", "independen*",
    # National defense
    "defence", "defense", "military", "army", "nato", "armed forces",
    # USA / Trump / tariffs
    "trump", "united states", "american*",
    "tariff*", "trade war", "customs", "surcharge*", "surtax*",
    # Interprovincial / resilience
    "interprovincial*", "interprovincial trade", "interprovincial commerce",
    "dependence", "resilien*", "self-suffici*", "supply chain",
    # Nation-building
    "nation*", "protect canada", "defend canada",
    "border*", "national security"
  )
))

# === Rouler sur issue_mostImport_ope ===
result_sov <- ellipsetxt::run_dictionary(df_survey, issue_text, sovereignty_dict, verbose = TRUE)

df_survey_sov <- cbind(df_survey, result_sov)

# === Résumé ===
cat("=== Hits souveraineté ===\n")
cat("FR:", sum(df_survey_sov$sovereignty_fr, na.rm = TRUE), "\n")
cat("EN:", sum(df_survey_sov$sovereignty_en, na.rm = TRUE), "\n")
cat("Total:", sum(df_survey_sov$sovereignty_fr, na.rm = TRUE) + sum(df_survey_sov$sovereignty_en, na.rm = TRUE), "\n")

# === Flag binaire ===
df_survey_sov$sov_flag <- (df_survey_sov$sovereignty_fr > 0 | df_survey_sov$sovereignty_en > 0)

cat("\nRépondants mentionnant souveraineté:", sum(df_survey_sov$sov_flag), "\n")
cat("Proportion:", round(mean(df_survey_sov$sov_flag), 3), "\n")

# === Voir les réponses flaggées ===
cat("\n=== Réponses flaggées souveraineté ===\n")
df_survey_sov %>%
  filter(sov_flag) %>%
  count(issue_text, sort = TRUE) %>%
  print(n = 30)



# === Voir les réponses flaggées ===
sov_responses <- df_survey_sov %>%
  filter(sov_flag) %>%
  count(issue_text, sort = TRUE)

cat("\n=== Top 30 réponses flaggées souveraineté ===\n")
head(sov_responses, 30)



# === Exclure souveraineté québécoise ===
df_survey_sov$sov_quebec <- str_detect(
  tolower(df_survey_sov$issue_text),
  "québec|quebec|indépendance du|independance du|souveraineté du q|souverainete du q"
)

# Flag souveraineté canadienne seulement
df_survey_sov$sov_canada_flag <- (df_survey_sov$sov_flag & !df_survey_sov$sov_quebec)

cat("=== Souveraineté canadienne (excluant Québec) ===\n")
cat("Total flag souveraineté:", sum(df_survey_sov$sov_flag), "\n")
cat("Souveraineté québécoise exclue:", sum(df_survey_sov$sov_flag & df_survey_sov$sov_quebec), "\n")
cat("Souveraineté canadienne retenue:", sum(df_survey_sov$sov_canada_flag), "\n")
cat("Proportion:", round(mean(df_survey_sov$sov_canada_flag), 3), "\n")

# === Vérifier le top 30 ===
sov_canada_responses <- df_survey_sov %>%
  filter(sov_canada_flag) %>%
  count(issue_text, sort = TRUE)

cat("\n=== Top 30 souveraineté canadienne ===\n")
head(sov_canada_responses, 30)





# === Déconnexion ===
tube::ellipse_disconnect(con)



























# === Déconnexion ===
tube::ellipse_disconnect(con)