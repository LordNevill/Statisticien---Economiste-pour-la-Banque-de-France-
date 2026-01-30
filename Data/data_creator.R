# ==============================================================================
# RÉFÉRENTIEL ENTREPRISES SIMPLIFIÉ
# ==============================================================================

# Paramètres
set.seed(123)
n_entreprises <- 150

# 1. LISTE DES ENTREPRISES ET LEURS SECTEURS
# ------------------------------------------------------------------------------
# Définition des entreprises avec leur secteur d'activité directement
entreprises_data <- list(
  # Luxe et cosmétiques
  c("LVMH", "Luxe"),
  c("L'Oréal", "Cosmétiques"),
  c("Kering", "Luxe"),
  c("Hermès International", "Luxe"),
  c("Chanel", "Luxe"),
  c("Dior", "Luxe"),
  c("Cartier", "Luxe"),
  c("Lacoste", "Textile"),
  c("Petit Bateau", "Textile"),
  c("Yves Saint Laurent", "Luxe"),
  
  # Énergie et industrie
  c("TotalEnergies", "Énergie"),
  c("Engie", "Énergie"),
  c("EDF", "Énergie"),
  c("Air Liquide", "Industrie"),
  c("Schneider Electric", "Équipements électriques"),
  c("Legrand", "Équipements électriques"),
  c("Saint-Gobain", "Matériaux de construction"),
  c("Vinci", "BTP"),
  c("Bouygues", "BTP"),
  c("Eiffage", "BTP"),
  
  # Automobile
  c("Renault", "Automobile"),
  c("Stellantis", "Automobile"),
  c("Michelin", "Équipement automobile"),
  c("Valeo", "Équipement automobile"),
  c("Faurecia", "Équipement automobile"),
  
  # Aérospatial et défense
  c("Airbus", "Aérospatial"),
  c("Dassault Aviation", "Aérospatial"),
  c("Safran", "Aérospatial"),
  c("Thales", "Défense"),
  
  # Pharmacie et santé
  c("Sanofi", "Pharmaceutique"),
  c("Servier", "Pharmaceutique"),
  c("Ipsen", "Pharmaceutique"),
  c("Biomérieux", "Santé"),
  c("Eurofins", "Laboratoires"),
  
  # Agroalimentaire
  c("Danone", "Agroalimentaire"),
  c("Lactalis", "Agroalimentaire"),
  c("Savencia", "Agroalimentaire"),
  c("Bonduelle", "Agroalimentaire"),
  c("Terrena", "Agriculture"),
  
  # Distribution
  c("Carrefour", "Grande distribution"),
  c("Auchan", "Grande distribution"),
  c("Leclerc", "Grande distribution"),
  c("Intermarché", "Grande distribution"),
  c("Casino", "Grande distribution"),
  c("Fnac Darty", "Distribution spécialisée"),
  c("Boulanger", "Distribution spécialisée"),
  
  # Hôtellerie et restauration
  c("Accor", "Hôtellerie"),
  c("Sodexo", "Restauration collective"),
  c("Elior", "Restauration collective"),
  
  # Télécommunications et médias
  c("Orange", "Télécommunications"),
  c("SFR", "Télécommunications"),
  c("Bouygues Telecom", "Télécommunications"),
  c("TF1", "Médias"),
  c("M6", "Médias"),
  c("Ubisoft", "Jeux vidéo"),
  c("Gameloft", "Jeux vidéo"),
  
  # Technologies
  c("Capgemini", "Services informatiques"),
  c("Atos", "Services informatiques"),
  c("Dassault Systèmes", "Logiciels"),
  c("Criteo", "Publicité en ligne"),
  c("OVHcloud", "Hébergement web"),
  
  # Transport et logistique
  c("La Poste", "Logistique"),
  c("SNCF", "Transport ferroviaire"),
  c("Geodis", "Logistique"),
  c("CMA CGM", "Transport maritime"),
  c("Air France-KLM", "Transport aérien"),
  
  # Immobilier
  c("Unibail-Rodamco-Westfield", "Immobilier commercial"),
  c("Klépierre", "Immobilier commercial"),
  c("Nexity", "Immobilier"),
  
  # Autres
  c("Veolia", "Services environnementaux"),
  c("Suez", "Services environnementaux"),
  c("Publicis Groupe", "Publicité"),
  c("Lagardère", "Médias"),
  c("Bolloré", "Multisectoriel")
)

# 2. COMPLÉTER POUR ATTEINDRE 150 ENTREPRISES
# ------------------------------------------------------------------------------
# Si nous n'avons pas assez d'entreprises, on en ajoute des génériques
n_existantes <- length(entreprises_data)

if (n_existantes < n_entreprises) {
  # Liste de secteurs pour les entreprises génériques
  secteurs_disponibles <- c("Industrie", "Services", "Commerce", "Construction", 
                            "Transport", "Technologie", "Santé", "Agriculture", 
                            "Hôtellerie", "Culture", "Énergie", "Environnement")
  
  for (i in (n_existantes + 1):n_entreprises) {
    secteur <- sample(secteurs_disponibles, 1)
    entreprises_data[[i]] <- c(paste0("Société_Française_", i), secteur)
  }
}

# Mélanger les entreprises
entreprises_data <- sample(entreprises_data, n_entreprises)

# 3. CRÉER LE RÉFÉRENTIEL
# ------------------------------------------------------------------------------
# Extraire les noms et secteurs
raison_sociale <- sapply(entreprises_data, function(x) x[1])
secteur_activite <- sapply(entreprises_data, function(x) x[2])

# Générer les SIREN
siren <- paste0(
  sample(300:900, n_entreprises, replace = TRUE),
  sprintf("%06d", sample(1:999999, n_entreprises, replace = TRUE))
)

# Tailles d'entreprise (basées sur le secteur)
taille_entreprise <- character(n_entreprises)
ca_total <- numeric(n_entreprises)

# Ajuster CA et taille selon le secteur
for (i in 1:n_entreprises) {
  secteur <- secteur_activite[i]
  
  # Définir le CA moyen selon le secteur
  if (secteur %in% c("Luxe", "Énergie", "Automobile", "Aérospatial", "Pharmaceutique")) {
    ca_moyen <- runif(1, 500, 2000)
  } else if (secteur %in% c("Grande distribution", "Télécommunications", "Services informatiques")) {
    ca_moyen <- runif(1, 200, 1000)
  } else if (secteur %in% c("Agroalimentaire", "BTP", "Équipements électriques")) {
    ca_moyen <- runif(1, 100, 500)
  } else {
    ca_moyen <- runif(1, 10, 200)
  }
  
  ca_total[i] <- round(ca_moyen + rnorm(1, 0, ca_moyen * 0.3), 2)
  if (ca_total[i] < 1) ca_total[i] <- round(runif(1, 1, 10), 2)
  
  # Déterminer la taille
  if (ca_total[i] > 1000) {
    taille_entreprise[i] <- "Très grande"
  } else if (ca_total[i] > 100) {
    taille_entreprise[i] <- "Grande"
  } else if (ca_total[i] > 10) {
    taille_entreprise[i] <- "Moyenne"
  } else {
    taille_entreprise[i] <- "Petite"
  }
}

# Création du référentiel simplifié
referentiel <- data.frame(
  siren = siren,
  raison_sociale = raison_sociale,
  secteur_activite = secteur_activite,
  taille_entreprise = taille_entreprise,
  ca_total = ca_total,
  region_siege = sample(c("Île-de-France", "Auvergne-Rhône-Alpes", 
                          "Nouvelle-Aquitaine", "Occitanie", 
                          "Hauts-de-France", "Provence-Alpes-Côte d'Azur",
                          "Pays de la Loire", "Bretagne", 
                          "Normandie", "Grand Est"), 
                        n_entreprises, replace = TRUE),
  date_creation = sample(seq(as.Date('1970-01-01'), 
                             as.Date('2020-12-31'), by="day"), 
                         n_entreprises),
  statut_entreprise = sample(c("Actif", "Actif", "Actif", "En difficulté"), 
                             n_entreprises, replace = TRUE),
  stringsAsFactors = FALSE
)

# 4. TRI ET PRÉSENTATION
# ------------------------------------------------------------------------------
# Trier par raison sociale
referentiel <- referentiel[order(referentiel$raison_sociale), ]
rownames(referentiel) <- NULL


# 5. SAUVEGARDE
# ------------------------------------------------------------------------------
# Sauvegarde en CSV
write.csv(referentiel, "referentiel_entreprises_simple.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nFichier sauvegardé: 'referentiel_entreprises_simple.csv'\n")

# ==============================================================================
# DATASET A : FLUX COMMERCIAUX (SERVICE DDG)
# ==============================================================================

# Paramètres
n_transactions <- 10000
periodes <- seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by="month")
periodes_str <- format(periodes, "%Y-%m")

# Codes pays ISO-2 et devises
pays_codes <- c("US", "DE", "GB", "IT", "ES", "CN", "JP", "IN", "CA", "AU", 
        "BR", "MX", "SG", "HK", "CH", "SE", "NL", "BE", "PL", "ZA")
devises <- c("EUR", "USD", "GBP")
nature_services <- c("R&D", "Services informatiques", "Fret", "Consulting", 
           "Logistique", "Leasing", "Publicité", "Maintenance", 
           "Support technique", "Courtage")

# Générer les transactions
flux_commerciaux <- data.frame(
  id_transaction = paste0("TRX_", sprintf("%06d", 1:n_transactions)),
  siren = sample(referentiel$siren, n_transactions, replace = TRUE),
  periode = sample(periodes_str, n_transactions, replace = TRUE),
  pays_contrepartie = sample(pays_codes, n_transactions, replace = TRUE),
  type_flux = sample(c("Import", "Export"), n_transactions, replace = TRUE),
  nature_service = sample(nature_services, n_transactions, replace = TRUE),
  montant_eur = round(abs(rnorm(n_transactions, mean = 50000, sd = 100000)), 2),
  devise_origine = sample(devises, n_transactions, replace = TRUE),
  stringsAsFactors = FALSE
)

# S'assurer que montant_eur >= 0
flux_commerciaux$montant_eur <- abs(flux_commerciaux$montant_eur)

# Sauvegarde en CSV
write.csv(flux_commerciaux, "flux_commerciaux_ddg.csv", 
      row.names = FALSE, fileEncoding = "UTF-8")

cat("Fichier sauvegardé: 'flux_commerciaux_ddg.csv'\n")
cat(paste("Nombre de transactions:", nrow(flux_commerciaux), "\n"))



# ==============================================================================
# DATASET B : INVESTISSEMENTS & FINANCIER (SERVICE IETE)
# ==============================================================================

# Paramètres
n_investissements <- 2000
annees <- 2023:2025
pays_partenaires <- c("US", "DE", "GB", "IT", "ES", "CN", "JP", "IN", "CA", "AU", 
            "BR", "MX", "SG", "HK", "CH", "SE", "NL", "BE", "PL", "ZA")
types_relations <- c("Maison Mère", "Filiale", "Succursale")

# Générer les investissements
investissements_iete <- data.frame(
  siren = sample(referentiel$siren, n_investissements, replace = TRUE),
  pays_partenaire = sample(pays_partenaires, n_investissements, replace = TRUE),
  annee = sample(annees, n_investissements, replace = TRUE),
  type_relation = sample(types_relations, n_investissements, replace = TRUE),
  taux_detention = round(runif(n_investissements, 0.05, 1.00), 4),
  flux_equity = round(rnorm(n_investissements, mean = 100000, sd = 150000), 2),
  flux_pret_intragroupe = round(rnorm(n_investissements, mean = 50000, sd = 80000), 2),
  stock_ide = round(abs(rnorm(n_investissements, mean = 500000, sd = 250000)), 2),
  stringsAsFactors = FALSE
)

# Ajuster les valeurs négatives pour les flux (peuvent être positifs ou négatifs)
investissements_iete$flux_equity <- round(rnorm(n_investissements, mean = 0, sd = 150000), 2)
investissements_iete$flux_pret_intragroupe <- round(rnorm(n_investissements, mean = 0, sd = 80000), 2)

# S'assurer que stock_ide >= 0
investissements_iete$stock_ide <- abs(investissements_iete$stock_ide)

# Sauvegarde en CSV
write.csv(investissements_iete, "investissements_iete.csv", 
      row.names = FALSE, fileEncoding = "UTF-8")

cat("Fichier sauvegardé: 'investissements_iete.csv'\n")
cat(paste("Nombre d'investissements:", nrow(investissements_iete), "\n"))