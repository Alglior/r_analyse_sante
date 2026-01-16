setwd("/home/arthur/Documents/nextcloud_sync/Documents/analyse spatiale et santé/Devoir/")

################################# Import des librairies ########################

library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(scales)
library(readr)
library(sf)
library(tidyr) # Ajouté pour replace_na()

################################ Ouverture des données #########################

deces <- read.csv("data/insee_deces.csv")

################################Prétraitement des données ######################

### conversion des dates de chr vers date
# class(deces$date_naissance) # Vérification
deces <- deces %>%
  mutate(date_deces = ymd(date_deces)) %>%
  mutate(date_naissance = ymd(date_naissance))

### Sélection seulement des données de 2010 à 2019
deces_2010_2019 <- deces %>%
  filter(!is.na(date_deces)) %>%
  filter(between(year(date_deces), 2000, 2019))

# remove(deces) # suppression des tables de traitement intermédiaires pour économiser de la RAM

### Sélection des données qui se situent dans la région AURA
deces_2010_2019 <- deces_2010_2019 %>%
  filter(!is.na(code_lieu_deces)) %>%
  mutate(
    dept = str_sub(code_lieu_deces, 1, 2)  # Extrait les 2 premiers chiffres (département)
  ) %>%
  filter(dept %in% c("01", "03", "07", "15", "26", "38", "42", "43", "63", "69", "73", "74"))

### Calcul de l'âge de décès
deces_2010_2019 <- deces_2010_2019 %>%
  mutate(
    age_revolu = as.period(interval(date_naissance, date_deces))$year
  )

################################# Regroupement du nombre de morts et visualisation ##############

### Morts regroupés par année
deces_par_annee <- deces_2010_2019 %>%
  mutate(annee_deces = year(date_deces)) %>%
  group_by(annee_deces) %>%
  summarise(n_deces = n(), .groups = "drop")

### Plot du nombre de décès par année
deces_par_annee %>%
  ggplot(aes(x=annee_deces, y=n_deces)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Évolution annuelle des décès en AURA")

### morts regroupés par année, mois et commune
deces_par_commune_mois <- deces_2010_2019 %>%
  filter(!is.na(date_deces)) %>%                          
  mutate(
    annee_deces  = year(date_deces),
    mois_deces   = month(date_deces)
  ) %>%
  group_by(code_lieu_deces, annee_deces, mois_deces) %>%   
  summarise(n_deces = n(), .groups = "drop")


### Graphique du nombre de morts par mois
deces_par_mois <- deces_2010_2019 %>%
  filter(!is.na(date_deces)) %>%
  mutate(
    periode = make_date(year = year(date_deces), 
                        month = month(date_deces), 
                        day = 1)
  )%>%
  group_by(periode) %>%            
  summarise(n_deces = n(), .groups = "drop")

ggplot(deces_par_mois, aes(x = periode, y = n_deces)) +
  geom_line(color = "darkred", size = 0.8) +
  scale_x_date(date_breaks = "12 months") +
  labs(
    title = "Nombre de décès par mois (2010-2019)",
    x = "Date", 
    y = "Nombre de décès",
    caption = "Source: données décès filtrées 2010-2020"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#################################### Ajout des données géométriques #############

# st_layers("data/ADE_4-0_GPKG_LAMB93_FXX-ED2025-12-05.gpkg")

region_france <- st_read("data/ADE_4-0_GPKG_LAMB93_FXX-ED2025-12-05.gpkg", 
                         layer = "region", quiet = TRUE)
aura <- region_france %>% 
  filter(code_insee=="84") %>% # extraction de la région AURA
  st_transform(crs = 2154)

commune_france <- st_read("data/ADE_4-0_GPKG_LAMB93_FXX-ED2025-12-05.gpkg", layer = "commune", quiet = TRUE)
commune_aura <- commune_france %>% 
  filter(code_insee_de_la_region == "84") %>%
  st_transform(crs = 2154)

st_write(commune_aura, "resultat/resultat.gpkg", 
         layer = "commune_aura", 
         delete_dsn = TRUE, 
         append = FALSE)


#################################### Ajout des données météorologiques ###############

SIM <- read.csv("data/MENS_SIM2_2010-2019.csv", sep = ";", header = TRUE)

SIM_clean <- SIM %>% 
  mutate(
    LAMBX_M = LAMBX * 100,
    LAMBY_M = LAMBY * 100
  )

SIM_2154 <- st_as_sf(SIM_clean, 
                     coords = c("LAMBX_M", "LAMBY_M"), 
                     crs = 27572) %>%
  st_transform(crs = 2154)

# Filtre spatial
SIM_AURA_2154 <- st_filter(SIM_2154, aura) %>% 
  mutate(id_station = row_number())

st_write(SIM_AURA_2154, "resultat/resultat.gpkg", 
         layer = "SIM_AURA_2154", 
         delete_dsn = FALSE, 
         append = FALSE)

# Jointure spatiale
lien_commune_point <- st_join(SIM_AURA_2154, commune_aura, join = st_nearest_feature)

# Séparation Dates
SIM_AURA_clean <- SIM_AURA_2154 %>%
  mutate(
    annee = as.numeric(substr(as.character(DATE), 1, 4)),
    mois  = as.numeric(substr(as.character(DATE), 5, 6))
  )

# Exemple Pic chaleur
pic_chaleur_juin_2015 <- SIM_AURA_clean %>% filter(annee == 2015, mois == 6) # Correction: 2015 est numerique ici

st_write(pic_chaleur_juin_2015, "resultat/resultat.gpkg", 
         layer = "pic_chaleur_juin_2015", 
         delete_dsn = FALSE, 
         append = FALSE)


#################################### Ajout des données sociales ######################

dossier_complet <- read_delim(
  "data/dossier_complet.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

# 1. Liste des variables de population
# Correction ici : CODGEO doit être présent pour la jointure
vars_pop <- c("CODGEO","P15_POP","P15_POP0014", "P15_POP1529", "P15_POP3044", "P15_POP4559", "P15_POP6074", "P15_POP7589", "P15_POP90P")

# 2. Vérification présence colonnes
vars_selectionnees_insee <- vars_pop[vars_pop %in% colnames(dossier_complet)]

# 3. Création dataframe nettoyé
# Correction ici : on utilise la bonne variable 'vars_selectionnees_insee'
insee_pop_2015 <- dossier_complet[, vars_selectionnees_insee]

head(insee_pop_2015)

#################################### ANALYSE MÉTÉO / DÉCÈS ########################

# Agrégation de la météo moyenne
meteo_mensuelle <- SIM_AURA_clean %>%
  st_drop_geometry() %>%
  group_by(annee, mois) %>%
  summarise(temp_moyenne = mean(T_MENS, na.rm = TRUE), .groups = "drop")

# Préparation des décès
deces_mensuels <- deces_par_mois %>%
  mutate(annee = year(periode), 
         mois = month(periode))

# Jointure finale
df_final <- deces_mensuels %>%
  inner_join(meteo_mensuelle, by = c("annee", "mois"))

# Visualisation 2015
df_2015 <- df_final %>%
  filter(annee == 2015) %>%
  mutate(nom_mois = month(mois, label = TRUE, abbr = FALSE, locale = "fr_FR.UTF-8"))

# Ordre chronologique inverse pour le graph
df_2015$nom_mois <- factor(df_2015$nom_mois, levels = rev(levels(df_2015$nom_mois)))

ggplot(df_2015, aes(x = nom_mois, y = n_deces, fill = temp_moyenne)) +
  geom_col() + 
  coord_flip() + 
  scale_fill_gradient(low = "blue", high = "red", name = "Temp. Moyenne (°C)") +
  labs(
    title = "Mortalité mensuelle en AURA - Année 2015",
    x = "Mois",
    y = "Nombre total de décès"
  ) +
  theme_minimal()

#################################### CALCUL DU TAUX DE DÉCÈS ########################

# 1. Calcul de la population totale 2015 en AURA
# On filtre les communes qui sont dans votre objet 'commune_aura' 
# pour être sûr de ne garder que la région cible.
pop_aura_2015 <- insee_pop_2015 %>%
  filter(CODGEO %in% commune_aura$code_insee) %>%
  summarise(total_pop = sum(P15_POP, na.rm = TRUE)) %>%
  pull(total_pop)

# 2. Intégration du calcul dans le dataframe final
df_final_taux <- df_final %>%
  mutate(
    # Calcul : (Nombre de décès / Population totale) * 1 000
    taux_deces_10k = (n_deces / pop_aura_2015) * 1000
  )

# 3. Visualisation du taux de décès pour l'année 2015
df_2015_taux <- df_final_taux %>%
  filter(annee == 2015) %>%
  mutate(nom_mois = month(mois, label = TRUE, abbr = FALSE, locale = "fr_FR.UTF-8"))

# Réorganisation des mois pour le graphique
df_2015_taux$nom_mois <- factor(df_2015_taux$nom_mois, levels = rev(levels(df_2015_taux$nom_mois)))

ggplot(df_2015_taux, aes(x = nom_mois, y = taux_deces_10k, fill = temp_moyenne)) +
  geom_col() + 
  coord_flip() + 
  scale_fill_gradient(low = "blue", high = "red", name = "Temp. Moyenne (°C)") +
  labs(
    title = "Taux de mortalité mensuel en AURA - Année 2015",
    subtitle = paste("Population totale estimée (2015) :", format(pop_aura_2015, big.mark = " ")),
    x = "Mois",
    y = "Nombre de décès pour 1 000 habitants"
  ) +
  theme_minimal()
