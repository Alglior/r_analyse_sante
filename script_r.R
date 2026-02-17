################################################################################
# ANALYSE MORTALITÉ / MÉTÉO - RÉGION AURA
################################################################################

# 1. SETUP & CONFIGURATION -----------------------------------------------------
library(tidyverse)
library(lubridate)
library(sf)
library(scales)
library(viridis)

# Paramètres globaux
CONFIG <- list(
  path_data = "data/",
  path_out  = "resultat/",
  reg_code  = "84",
  depts     = c("01", "03", "07", "15", "26", "38", "42", "43", "63", "69", "73", "74"),
  crs       = 2154
)

if (!dir.exists(CONFIG$path_out)) dir.create(CONFIG$path_out)

theme_aura <- function() {
  theme_minimal() + 
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      strip.text = element_text(face = "bold", size = 10),
      panel.spacing = unit(1, "lines")
    )
}

# 2. CHARGEMENT & PRÉTRAITEMENT ------------------------------------------------

deces_clean <- read_csv(paste0(CONFIG$path_data, "insee_deces.csv"), show_col_types = FALSE) %>%
  mutate(
    date_deces = ymd(date_deces),
    annee = year(date_deces),
    mois  = month(date_deces),
    dept  = str_sub(code_lieu_deces, 1, 2)
  ) %>%
  filter(between(annee, 2010, 2019), dept %in% CONFIG$depts)

commune_aura <- st_read(paste0(CONFIG$path_data, "ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"), 
                        layer = "commune", quiet = TRUE) %>%
  filter(code_insee_de_la_region == CONFIG$reg_code) %>%
  st_transform(CONFIG$crs)

pop_2015_dept <- read_delim(paste0(CONFIG$path_data, "dossier_complet.csv"), delim = ";", show_col_types = FALSE) %>%
  select(CODGEO, P16_POP) %>%
  filter(CODGEO %in% commune_aura$code_insee) %>%
  mutate(dept = str_sub(CODGEO, 1, 2)) %>%
  group_by(dept) %>%
  summarise(pop_dept = sum(P16_POP, na.rm = TRUE), .groups = "drop")

meteo_dept <- read_delim(paste0(CONFIG$path_data, "MENS_SIM2_2010-2019.csv"), delim = ";", show_col_types = FALSE) %>%
  mutate(across(c(LAMBX, LAMBY), ~ .x * 100)) %>%
  st_as_sf(coords = c("LAMBX", "LAMBY"), crs = 27572) %>%
  st_transform(CONFIG$crs) %>%
  st_join(commune_aura %>% select(code_insee), join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  mutate(
    annee = as.numeric(str_sub(DATE, 1, 4)),
    mois  = as.numeric(str_sub(DATE, 5, 6)),
    dept  = str_sub(code_insee, 1, 2)
  ) %>%
  group_by(dept, annee, mois) %>%
  summarise(temp_moy = mean(T_MENS, na.rm = TRUE), .groups = "drop")

# 3. CALCULS STATISTIQUES ------------------------------------------------------

df_2015 <- deces_clean %>%
  filter(annee == 2015) %>%
  group_by(dept, mois) %>%
  summarise(n_deces = n(), .groups = "drop") %>%
  inner_join(pop_2015_dept, by = "dept") %>%
  left_join(meteo_dept %>% filter(annee == 2015), by = c("dept", "mois")) %>%
  mutate(
    taux_1000 = (n_deces / pop_dept) * 1000,
    nom_mois = month(mois, label = TRUE, abbr = TRUE, locale = "fr_FR.UTF-8"),
    nom_mois = fct_rev(nom_mois),
    tranche_temp = cut(temp_moy, breaks = 3, labels = c("Froid", "Modéré", "Chaud"))
  )

# Chi-deux
table_chi <- xtabs(n_deces ~ tranche_temp + dept, data = df_2015)
test_chi  <- chisq.test(table_chi)
residus_df <- as.data.frame(test_chi$residuals)

# 4. VISUALISATIONS ------------------------------------------------------------

# --- Graphique 1 : Régional Global ---
df_2015_reg <- df_2015 %>%
  group_by(nom_mois) %>%
  summarise(n_deces = sum(n_deces), temp = mean(temp_moy, na.rm = TRUE))

ggplot(df_2015_reg, aes(x = nom_mois, y = n_deces, fill = temp)) +
  geom_col() + coord_flip() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 15) +
  labs(title = "Mortalité mensuelle AURA - 2015", y = "Décès totaux", fill = "Temp °C") +
  theme_aura()

# --- Graphique 2 : Détail par Département (Faceting) ---

ggplot(df_2015, aes(x = nom_mois, y = taux_1000, fill = temp_moy)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~dept, scales = "free_x") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 15) +
  labs(title = "Taux de mortalité par département (2015)",
       x = "Mois", y = "Décès pour 1 000 hab.", fill = "Temp °C") +
  theme_aura()

# --- Graphique 3
ggplot(carte_data) +
  geom_sf(aes(fill = taux), color = "white", linewidth = 0.2) +
  geom_sf_text(aes(label = paste0(round(temp_ann, 1), "°C")), 
               color = "white", fontface = "bold", size = 3.5) +
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1,
    name = "Taux de mortalité\n(pour 1 000 hab.)"
  ) +
  labs(
    title = "Répartition géographique de la mortalité en Auvergne-Rhône-Alpes (2015)",
    subtitle = paste("La couleur indique le taux de décès annuel pour 1 000 habitants\n",
                     "Les étiquettes affichent la température moyenne annuelle enregistrée"),
    caption = "Sources : Données décès INSEE & Historique météo SIM2"
  ) +
  theme_void() + 
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "grey30", lineheight = 1.2),
    # CORRECTION ICI : face = "italic" au lieu de italic = TRUE
    plot.caption = element_text(size = 8, hjust = 0.9, face = "italic"), 
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold")
  )

# --- Graphique 4 : Chi-deux (Résidus) ---
ggplot(residus_df, aes(x = dept, y = tranche_temp, fill = Freq)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#313695", mid = "#f7f7f7", high = "#a50026", midpoint = 0) +
  labs(title = "Analyse Statistique des Résidus", x = "Département", y = "Tranche thermique", fill = "Écart") +
  theme_aura()

# 5. EXPORT AUTOMATIQUE DES GRAPHIQUES -----------------------------------------

# Création d'une liste de graphiques pour faciliter l'export
plots_list <- list(
  "01_mortalite_regionale_2015" = ggplot(df_2015_reg, aes(x = nom_mois, y = n_deces, fill = temp)) +
    geom_col() + coord_flip() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 15) +
    labs(title = "Mortalité mensuelle AURA - 2015", y = "Décès totaux", fill = "Temp °C") +
    theme_aura(),
  
  "02_mortalite_par_departement" = ggplot(df_2015, aes(x = nom_mois, y = taux_1000, fill = temp_moy)) +
    geom_col() + coord_flip() +
    facet_wrap(~dept, scales = "free_x") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 15) +
    labs(title = "Taux de mortalité par département (2015)",
         x = "Mois", y = "Décès pour 1 000 hab.", fill = "Temp °C") +
    theme_aura(),
  
  "03_carte_taux_mortalite" = ggplot(carte_data) +
    geom_sf(aes(fill = taux), color = "white", linewidth = 0.2) +
    geom_sf_text(aes(label = paste0(round(temp_ann, 1), "°C")), color = "white", size = 3) +
    scale_fill_viridis_c(option = "magma", direction = -1) +
    theme_void() + labs(title = "Carte du taux de mortalité 2015"),
  
  "04_analyse_statistique_chi2" = ggplot(residus_df, aes(x = dept, y = tranche_temp, fill = Freq)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient2(low = "#313695", mid = "#f7f7f7", high = "#a50026", midpoint = 0) +
    labs(title = "Analyse Statistique des Résidus", x = "Département", y = "Tranche thermique", fill = "Écart") +
    theme_aura()
)

# Boucle d'exportation
# On utilise iwalk pour parcourir le nom et l'objet graphique simultanément
iwalk(plots_list, function(p, name) {
  file_path <- paste0(CONFIG$path_out, name, ".png")
  
  ggsave(
    filename = file_path,
    plot = p,
    width = 10,     # Largeur en pouces
    height = 7,     # Hauteur en pouces
    dpi = 300,      # Qualité impression
    bg = "white"    # Fond blanc forcé pour éviter la transparence
  )
  
  message(paste("Graphique sauvegardé :", file_path))
})


