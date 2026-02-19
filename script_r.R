################################################################################
# ANALYSE MORTALITÉ / MÉTÉO - RÉGION AURA
# Ce script analyse la relation entre les décès et les conditions météorologiques
# dans la région Auvergne-Rhône-Alpes pour la période 2010-2019
################################################################################

# ==============================================================================
# 1. SETUP & CONFIGURATION -----------------------------------------------------
# ==============================================================================
# Chargement des bibliothèques nécessaires pour l'analyse
library(tidyverse)    # Manipulation de données et visualisation
library(lubridate)    # Gestion des dates
library(sf)           # Données géospatiales (cartes)
library(scales)       # Formatage des échelles graphiques
library(viridis)      # Palettes de couleurs perceptuellement uniformes

# Paramètres globaux de configuration
CONFIG <- list(
  path_data = "data/",                                    # Dossier des données d'entrée
  path_out  = "resultat/",                                # Dossier des résultats de sortie
  reg_code  = "84",                                       # Code INSEE de la région AURA
  depts     = c("01", "03", "07", "15", "26", "38", "42", # Codes des 12 départements
                "43", "63", "69", "73", "74"),
  crs       = 2154                                        # Système de projection (RGF93/Lambert-93)
)

# Création du dossier de sortie s'il n'existe pas déjà
if (!dir.exists(CONFIG$path_out)) dir.create(CONFIG$path_out)

# Définition d'un thème graphique personnalisé pour tous les graphiques
theme_aura <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      strip.text = element_text(face = "bold", size = 10),
      panel.spacing = unit(1, "lines")
    )
}

# ==============================================================================
# 2. CHARGEMENT & PRÉTRAITEMENT ------------------------------------------------
# ==============================================================================

# --- 2.1 Données de décès (INSEE) ---
# Lecture et nettoyage des données de décès
deces_clean <- read_csv(paste0(CONFIG$path_data, "insee_deces.csv"), 
                        show_col_types = FALSE) %>%
  mutate(
    date_deces = ymd(date_deces),                    # Conversion en objet Date
    annee = year(date_deces),                        # Extraction de l'année
    mois  = month(date_deces),                       # Extraction du mois
    dept  = str_sub(code_lieu_deces, 1, 2)           # Extraction du code département (2 premiers chiffres)
  ) %>%
  # Filtrage : période 2010-2019 et départements de la région AURA uniquement
  filter(between(annee, 2010, 2019), dept %in% CONFIG$depts)

# --- 2.2 Données géographiques (Communes) ---
# Chargement du fond de carte des communes (format GeoPackage)
commune_aura <- st_read(paste0(CONFIG$path_data, 
                               "ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"),
                        layer = "commune", quiet = TRUE) %>%
  filter(code_insee_de_la_region == CONFIG$reg_code) %>%  # Filtre région AURA
  st_transform(CONFIG$crs)                                 # Reprojection en Lambert-93

# --- 2.3 Données de population (INSEE 2015) ---
# Lecture des données de population par commune
pop_2015_dept <- read_delim(paste0(CONFIG$path_data, "dossier_complet.csv"), 
                            delim = ";", show_col_types = FALSE) %>%
  select(CODGEO, P16_POP) %>%                    # Sélection des colonnes utiles
  filter(CODGEO %in% commune_aura$code_insee) %>% # Garde seulement les communes AURA
  mutate(dept = str_sub(CODGEO, 1, 2)) %>%       # Extrait le code département
  group_by(dept) %>%
  summarise(pop_dept = sum(P16_POP, na.rm = TRUE), .groups = "drop") # Agrège par département

# --- 2.4 Données météorologiques (SIM2) ---
# Lecture et traitement des données météo mensuelles
meteo_dept <- read_delim(paste0(CONFIG$path_data, "MENS_SIM2_2010-2019.csv"), 
                         delim = ";", show_col_types = FALSE) %>%
  mutate(across(c(LAMBX, LAMBY), ~ .x * 100)) %>%  # Conversion coordonnées (m -> cm)
  st_as_sf(coords = c("LAMBX", "LAMBY"), crs = 27572) %>%  # Création objet spatial (Lambert-93 ancien)
  st_transform(CONFIG$crs) %>%                     # Reprojection vers Lambert-93 actuel
  # Jointure spatiale pour attribuer chaque point météo à une commune
  st_join(commune_aura %>% select(code_insee), join = st_nearest_feature) %>%
  st_drop_geometry() %>%                           # On garde seulement les attributs
  mutate(
    annee = as.numeric(str_sub(DATE, 1, 4)),       # Extraction année (format YYYYMM)
    mois  = as.numeric(str_sub(DATE, 5, 6)),       # Extraction mois
    dept  = str_sub(code_insee, 1, 2)              # Code département
  ) %>%
  group_by(dept, annee, mois) %>%
  summarise(temp_moy = mean(T_MENS, na.rm = TRUE), .groups = "drop") # Moyenne température par dept/mois

# ==============================================================================
# 3. CALCULS STATISTIQUES ------------------------------------------------------
# ==============================================================================

# Création du dataset principal pour l'année 2015 (année de référence)
df_2015 <- deces_clean %>%
  filter(annee == 2015) %>%                        # Filtre année 2015
  group_by(dept, mois) %>%
  summarise(n_deces = n(), .groups = "drop") %>%   # Compte les décès par dept/mois
  inner_join(pop_2015_dept, by = "dept") %>%       # Ajoute la population
  left_join(meteo_dept %>% filter(annee == 2015),  # Ajoute la température
            by = c("dept", "mois")) %>%
  mutate(
    taux_1000 = (n_deces / pop_dept) * 1000,       # Taux pour 1000 habitants
    nom_mois = month(mois, label = TRUE, abbr = TRUE, locale = "fr_FR.UTF-8"), # Nom du mois
    nom_mois = fct_rev(nom_mois),                  # Inverse l'ordre (décembre en premier)
    tranche_temp = cut(temp_moy, breaks = 3,       # Catégorise la température
                       labels = c("Froid", "Modéré", "Chaud"))
  )

# --- Test du Chi-deux ---
# Vérifie l'indépendance entre température et nombre de décès par département
table_chi <- xtabs(n_deces ~ tranche_temp + dept, data = df_2015)  # Table de contingence
test_chi  <- chisq.test(table_chi)                                  # Test statistique
residus_df <- as.data.frame(test_chi$residuals)                     # Récupère les résidus

# --- Préparation des données pour la carte annuelle (Graphique 3) ---
stat_annuelles_dept <- df_2015 %>%
  group_by(dept) %>%
  summarise(
    total_deces = sum(n_deces),           # Total décès annuel
    pop_dept = first(pop_dept),           # Population du département
    temp_ann = mean(temp_moy, na.rm = TRUE), # Température moyenne annuelle
    .groups = "drop"
  ) %>%
  mutate(taux = (total_deces / pop_dept) * 1000)  # Taux annuel pour 1000 hab.

# Agrégation des communes par département pour la carte
geom_dept <- commune_aura %>%
  mutate(dept = str_sub(code_insee, 1, 2)) %>%
  group_by(dept) %>%
  summarise()  # Fusionne les géométries des communes par département

# Jointure des données statistiques avec la géométrie départementale
carte_data <- geom_dept %>%
  inner_join(stat_annuelles_dept, by = "dept")

# --- Préparation des données pour les cartes mensuelles ---
carte_mensuelle_temp <- geom_dept %>%
  inner_join(df_2015, by = "dept") %>%
  mutate(
    nom_mois = month(mois, label = TRUE, abbr = FALSE, locale = "fr_FR.UTF-8"),
    nom_mois = fct_reorder(nom_mois, mois)  # Ordonne les mois chronologiquement
  )

# ==============================================================================
# 4. VISUALISATIONS ------------------------------------------------------------
# ==============================================================================

# --- Graphique 1 : Mortalité régionale globale (barres empilées) ---
df_2015_reg <- df_2015 %>%
  group_by(nom_mois) %>%
  summarise(n_deces = sum(n_deces), temp = mean(temp_moy, na.rm = TRUE))

ggplot(df_2015_reg, aes(x = nom_mois, y = n_deces, fill = temp)) +
  geom_col() + coord_flip() +  # Barres horizontales
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 15) +
  labs(title = "Mortalité mensuelle AURA - 2015", 
       y = "Décès totaux", fill = "Temp °C") +
  theme_aura()

# --- Graphique 2 : Détail par Département (Faceting) ---
# Affiche le taux de mortalité pour chaque département séparément
ggplot(df_2015, aes(x = nom_mois, y = taux_1000, fill = temp_moy)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~dept, scales = "free_x") +  # Un panneau par département
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 15) +
  labs(title = "Taux de mortalité par département (2015)",
       x = "Mois", y = "Décès pour 1 000 hab.", fill = "Temp °C") +
  theme_aura()

# --- Graphique 3 : Carte choroplèthe annuelle ---
ggplot(carte_data) +
  geom_sf(aes(fill = taux), color = "white", linewidth = 0.2) +  # Fond coloré par taux
  geom_sf_text(aes(label = paste0(round(temp_ann, 1), "°C")),
               color = "white", fontface = "bold", size = 3.5, show.legend = FALSE) +
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       name = "Taux de mortalité\n(pour 1 000 hab.)") +
  labs(
    title = "Répartition géographique de la mortalité en Auvergne-Rhône-Alpes (2015)",
    subtitle = "La couleur indique le taux de décès annuel pour 1 000 habitants\nLes étiquettes affichent la température moyenne annuelle enregistrée",
    caption = "Sources : Données décès INSEE & Historique météo SIM2"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5,
                              margin = margin(t = 20, b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", hjust = 0.5,
                                 lineheight = 1.2, margin = margin(b = 15)),
    plot.caption = element_text(size = 8, hjust = 0.9, face = "italic"),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# --- Graphique 4 : Heatmap des résidus du Chi-deux ---
# Visualise les écarts entre valeurs observées et attendues
ggplot(residus_df, aes(x = dept, y = tranche_temp, fill = Freq)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#313695", mid = "#f7f7f7", high = "#a50026", midpoint = 0) +
  labs(title = "Analyse Statistique des Résidus", 
       x = "Département", y = "Tranche thermique", fill = "Écart") +
  theme_aura()

# --- Graphique 5 : Cartes mensuelles (Taux + Température) ---
plot_cartes_mensuelles_temp <- ggplot(carte_mensuelle_temp) +
  geom_sf(aes(fill = taux_1000), color = "white", linewidth = 0.1) +
  geom_sf_text(
    aes(label = paste0(round(temp_moy, 0), "°")),
    color = "white", size = 2.5, fontface = "bold", check_overlap = TRUE
  ) +
  facet_wrap(~nom_mois, ncol = 4) +  # Grille 4 colonnes, un mois par carte
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       name = "Taux de décès\n(pour 1 000 hab.)") +
  labs(
    title = "Mortalité et Températures en Auvergne-Rhône-Alpes (2015)",
    subtitle = "Le fond coloré indique le taux de mortalité ; le texte indique la température moyenne",
    caption = "Données : INSEE & Météo France (SIM2)"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(20, 10, 10, 10),
    strip.text = element_text(face = "bold", size = 11, margin = margin(t = 10, b = 5)),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

# Affichage du graphique final
print(plot_cartes_mensuelles_temp)