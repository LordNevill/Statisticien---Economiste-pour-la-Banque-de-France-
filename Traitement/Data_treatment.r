library(dplyr)
library(tidyr)

# ==============================================================================
# 1. CHARGER LES DONNÉES
# ==============================================================================
referentiel <- read.csv("C:\\Users\\NICK-TECH\\Desktop\\ENSAI 2A\\Projet_BDF\\Statisticien---Economiste-pour-la-Banque-de-France-\\Data\\referentiel_entreprises_simple.csv", 
                        encoding = "UTF-8", stringsAsFactors = FALSE)
flux_commerciaux <- read.csv("C:\\Users\\NICK-TECH\\Desktop\\ENSAI 2A\\Projet_BDF\\Statisticien---Economiste-pour-la-Banque-de-France-\\Data\\flux_commerciaux_ddg.csv", 
                             encoding = "UTF-8", stringsAsFactors = FALSE)
investissements_iete <- read.csv("C:\\Users\\NICK-TECH\\Desktop\\ENSAI 2A\\Projet_BDF\\Statisticien---Economiste-pour-la-Banque-de-France-\\Data\\investissements_iete.csv", 
                                 encoding = "UTF-8", stringsAsFactors = FALSE)

# ==============================================================================
# 2. NETTOYER LE RÉFÉRENTIEL
# ==============================================================================

# Valider SIREN (exactement 9 chiffres)
referentiel <- referentiel %>%
  mutate(
    siren_valid = grepl("^[0-9]{9}$", as.character(siren)),
    siren = if_else(!siren_valid, NA_character_, as.character(siren))
  ) %>%
  select(-siren_valid)

# Imputer les valeurs manquantes de région
regions_possibles <- na.omit(unique(referentiel$region_siege))

referentiel <- referentiel %>%
  mutate(
    region_siege = if_else(
      is.na(region_siege),
      sample(regions_possibles, size = n(), replace = TRUE),
      region_siege
    )
  )

# Corriger les CA négatifs
referentiel <- referentiel %>%
  mutate(ca_total = abs(ca_total))

# Supprimer les lignes avec SIREN NA
referentiel <- referentiel %>%
  filter(!is.na(siren))

# ==============================================================================
# 3. NETTOYER FLUX COMMERCIAUX
# ==============================================================================

# Codes pays valides ISO-2
pays_valides <- c("US", "DE", "GB", "IT", "ES", "CN", "JP", "IN", "CA", "AU", 
                  "BR", "MX", "SG", "HK", "CH", "SE", "NL", "BE", "PL", "ZA")

flux_commerciaux <- flux_commerciaux %>%
  # Corriger "UK" en "GB"
  mutate(pays_contrepartie = if_else(
    pays_contrepartie == "UK", "GB", pays_contrepartie
  )) %>%
  # Invalider les codes pays non reconnus
  mutate(pays_contrepartie = if_else(
    !pays_contrepartie %in% pays_valides, NA_character_, pays_contrepartie
  )) %>%
  # Valider SIREN
  mutate(siren = if_else(
    !grepl("^[0-9]{9}$", as.character(siren)), NA_character_, as.character(siren)
  )) %>%
  # Valider devises
  mutate(devise_origine = if_else(
    !devise_origine %in% c("EUR", "USD", "GBP"), NA_character_, devise_origine
  )) %>%
  # Montants positifs
  mutate(montant_eur = abs(montant_eur)) %>%
  # Supprimer lignes critiques manquantes
  filter(!is.na(siren), !is.na(pays_contrepartie), !is.na(montant_eur))

# ==============================================================================
# 4. NETTOYER INVESTISSEMENTS
# ==============================================================================

investissements_iete <- investissements_iete %>%
  # Valider taux de détention [0, 1]
  mutate(taux_detention = case_when(
    taux_detention < 0 ~ 0,
    taux_detention > 1 ~ NA_real_,
    TRUE ~ taux_detention
  )) %>%
  # Années valides [2023, 2025]
  mutate(annee = if_else(
    annee < 2023 | annee > 2025, NA_integer_, as.integer(annee)
  )) %>%
  # Valider SIREN
  mutate(siren = if_else(
    !grepl("^[0-9]{9}$", as.character(siren)), NA_character_, as.character(siren)
  )) %>%
  # Stock IDE positif
  mutate(stock_ide = abs(stock_ide)) %>%
  # Imputer type_relation manquant
  mutate(type_relation = if_else(
    is.na(type_relation), "Autre", type_relation
  )) %>%
  # Supprimer lignes critiques manquantes
  filter(!is.na(siren), !is.na(annee))

# ==============================================================================
# 5. RAPPROCHEMENT ENTRE DATASETS
# ==============================================================================

# Ajouter info du référentiel aux autres datasets
flux_commerciaux <- flux_commerciaux %>%
  left_join(referentiel %>% select(siren, raison_sociale, secteur_activite), 
            by = "siren", relationship = "many-to-one")

investissements_iete <- investissements_iete %>%
  left_join(referentiel %>% select(siren, raison_sociale, secteur_activite), 
            by = "siren", relationship = "many-to-one")

# ==============================================================================
# 6. RAPPORT DE QUALITÉ
# ==============================================================================

cat("\n=== RAPPORT DE NETTOYAGE ===\n")
cat("Référentiel: ", nrow(referentiel), "entreprises valides\n")
cat("Flux commerciaux: ", nrow(flux_commerciaux), "transactions valides\n")
cat("Investissements: ", nrow(investissements_iete), "records valides\n")

# ==============================================================================
# 7. RESAUVEGARDER
# ==============================================================================

write.csv(referentiel, "C:\\Users\\NICK-TECH\\Desktop\\ENSAI 2A\\Projet_BDF\\Statisticien---Economiste-pour-la-Banque-de-France-\\Data\\referentiel_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(flux_commerciaux, "C:\\Users\\NICK-TECH\\Desktop\\ENSAI 2A\\Projet_BDF\\Statisticien---Economiste-pour-la-Banque-de-France-\\Data\\flux_commerciaux_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(investissements_iete, "C:\\Users\\NICK-TECH\\Desktop\\ENSAI 2A\\Projet_BDF\\Statisticien---Economiste-pour-la-Banque-de-France-\\Data\\investissements_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")