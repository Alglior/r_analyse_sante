setwd("/home/arthur/Documents/nextcloud_sync/Documents/analyse spatiale et santé/Devoir/")

################################# Import des librairies ########################

library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(scales)
library(readr)
library(sf)

################################ Ouverture des données #########################

deces <- read.csv("data/insee_deces.csv")

################################ Création des couches de données ######################

# Créer un dossier s'il n'existe pas
if (!dir.exists("resultat")) dir.create("resultat")

# Créer une table vide (par exemple avec une colonne géométrique)
empty_sf <- st_sf(geometry = st_sfc(crs = 2154))

# Écrire cette table pour initialiser le fichier
st_write(empty_sf, "resultat/resultat.gpkg", layer = "init", delete_dsn = TRUE)

################################Prétraitement des données ######################

### conversion des dates de chr vers date
class(deces$date_naissance)
deces <- deces %>%
  mutate(date_deces = ymd(date_deces))
deces <- deces %>%
  mutate(date_naissance = ymd(date_naissance))

### Sélection seulement des données de 2010 à 2020

deces_2010_2019 <- deces %>%
  filter(!is.na(date_deces)) %>%
  filter(between(year(date_deces), 2000, 2019))

# remove(deces) # suppression des tables de traitement intermédiaires pour économiser de la RAM

### Sélection des données qui se situent dans la région AURA

deces_2010_2019 <- deces_2010_2019 %>%
  filter(!is.na(code_lieu_deces)) %>%
  mutate(
    dept = str_sub(code_lieu_deces, 1, 2)  # Extrait les 2 premiers chiffres (département)
  ) %>%
  filter(dept %in% c("01", "03", "07", "15", "26", "38", "42", "43", "63", "69", "73", "74"))

### Calcul de l'âge de décès

deces_2010_2019 <- deces_2010_2019 %>%
  mutate(
    age_revolu = as.period(interval(date_naissance, date_deces))$year
  )

################################# Regroupement du nombre de morts et visualisation des résultats ##############
### Morts regroupés par année

deces_par_annee <- deces_2010_2019 %>%
  mutate(annee_deces = year(date_deces)) %>%
  group_by(annee_deces) %>%
  summarise(n_deces = n(), .groups = "drop")

### Plot du nombre de décès par année
deces_par_annee %>%
  ggplot( aes(x=annee_deces, y=n_deces)) +
  geom_line() +
  geom_point()

### morts regroupés par année, mois et commune

# V1
deces_par_commune_mois <- deces_2010_2019 %>%
  filter(!is.na(date_deces)) %>%                          # Seulement dates valides
  mutate(
    annee_deces  = year(date_deces),
    mois_deces   = month(date_deces),                     # 1=janvier, 2=février...
    #code_lieu_deces = code_lieu_deces                    # Garde la commune
  ) %>%
  group_by(code_lieu_deces, annee_deces, mois_deces) %>%   # Groupe par 3 variables
  summarise(n_deces = n(), .groups = "drop")


### Graphique du nombre de morts par jour

deces_par_jour <- deces_2010_2019 %>%
  filter(!is.na(date_deces)) %>%
  count(date_deces, name = "n_deces") %>%  # Agrège par jour complet
  arrange(date_deces)

ggplot(deces_par_jour, aes(x = date_deces, y = n_deces)) +
  geom_line(color = "darkred", size = 0.8) +
  geom_smooth(method = "loess", color = "blue", se = TRUE, span = 0.2) + #moyenne glissante pour une meilleure visualisation
  scale_x_date(date_breaks = "3 months") +
  labs(
    title = "Nombre de décès par jour (2010-2019)",
    x = "Date", 
    y = "Nombre de décès",
    caption = "Source: données décès filtrées 2010-2020"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Graphique du nombre de morts par mois

deces_par_mois <- deces_2010_2019 %>%
  filter(!is.na(date_deces)) %>%
  mutate(
    periode = make_date(year = year(date_deces), 
                        month = month(date_deces), 
                        day = 1)
  )%>%
  group_by(periode) %>%           # Groupe par commune + période
  summarise(n_deces = n(), .groups = "drop")

ggplot(deces_par_mois, aes(x = periode, y = n_deces)) +
  geom_line(color = "darkred", size = 0.8) +
  #geom_smooth(method = "loess", color = "blue", se = TRUE, span = 0.2) + #moyenne glissante pour une meilleure visualisation
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

st_layers("data/ADE_4-0_GPKG_LAMB93_FXX-ED2025-12-05.gpkg") # permet de visualiser quelles couches sont présentes dans la donnée

region_france <- st_read("data/ADE_4-0_GPKG_LAMB93_FXX-ED2025-12-05.gpkg", 
                         layer = "region")
aura <- region_france %>% 
  filter(code_insee=="84") # extraction de la région AURA

st_crs(aura) # Vérification du CRS qui est en 4326
aura <- st_transform(aura, crs = 2154) # Transformation du CRS

commune_france <- st_read("data/ADE_4-0_GPKG_LAMB93_FXX-ED2025-12-05.gpkg", layer = "commune")
commune_aura <- commune_france %>% 
  filter(code_insee_de_la_region == "84")
commune_aura <- st_transform(commune_aura, crs = 2154)

st_write(commune_aura, "resultat/resultat.gpkg", 
         layer = "commune_aura", 
         delete_dsn = TRUE, 
         append = FALSE)


#################################### Ajout des données météorologiques ###############

SIM <- read.csv("data/MENS_SIM2_2010-2019.csv", sep = ";", header = TRUE)

SIM_clean <- SIM %>% # Sans cette étape nous nous retrouvons en Algérie avec une France toute petite
  mutate(
    LAMBX_M = LAMBX * 100,
    LAMBY_M = LAMBY * 100
  )

SIM_2154 <- st_as_sf(SIM_clean, 
                     coords = c("LAMBX_M", "LAMBY_M"), 
                     crs = 27572) %>%
  st_transform(crs = 2154)

st_write(SIM_2154, "resultat/resultat.gpkg", 
         layer = "SIM_2154", 
         delete_dsn = FALSE, 
         append = FALSE)

SIM_AURA_2154 <- st_filter(SIM_2154, aura) %>% # on sélectionne uniquement les points situés dans aura
  mutate(id_station = row_number())


st_write(SIM_AURA_2154, "resultat/resultat.gpkg", 
         layer = "SIM_AURA_2154", 
         delete_dsn = FALSE, 
         append = FALSE)

#Supprimer la couche SIM_2154 du geopackage
st_delete("resultat/resultat.gpkg", layer = "SIM_2154")

# Jointure des points météorologiques aux communes de la région aura

lien_commune_point <- st_join(SIM_AURA_2154, commune_aura, # on inverse le sens de sélection = on dupplique les communes sur chaque point pour analyser la dimension temporelle
                              join = st_nearest_feature)

#Séparation des mois / années dans deux autres colonnes séparé
SIM_AURA_clean <- SIM_AURA_2154 %>%
  mutate(
    annee = as.numeric(substr(as.character(DATE), 1, 4)),
    mois  = as.numeric(substr(as.character(DATE), 5, 6))
  )

#Quelle est le mois le plus chaud dans l'années 2015 ou toutes les communes ont eu un pic de chaleur ?
pic_chaleur_juin_2019 <- SIM_AURA_clean %>% filter(annee== "2015", mois == "6")

st_write (pic_chaleur_juin_2019, "resultat/resultat.gpkg", 
         layer = "pic_chaleur_juin_2019", 
         delete_dsn = FALSE, 
         append = FALSE)


#################################### Ajout des données sociales ######################

dossier_complet <- read_delim(
  "data/dossier_complet.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

# 1. Liste des variables de population uniquement
vars_pop <- c("P15_POP0014", "P15_POP1529", "P15_POP3044", "P15_POP4559", "P15_POP6074", "P15_POP7589", "P15_POP90P")

# 2. Vérification de la présence de ces colonnes dans votre tableau
vars_selectionnees_insee <- vars_pop[vars_pop %in% colnames(dossier_complet)]

# 3. Création du nouveau dataframe avec uniquement ces colonnes
insee_pop_2015 <- dossier_complet[, vars_selectionnees]

head(insee_pop_2015)

#################################### ANALYSE des résultats définifs ###############

#graphique de corrélation entre les mois avec météo assez élevé ?°C + et le nombre de décès le même mois.
#regrouper les personnes en trois grandes classes max d'âge (0-10, 60+)

# une autre représentation avec le nombre totale habitant par département + le nombre de mort + météo température

# Agrégation de la météo moyenne par mois pour l'ensemble de la région
meteo_mensuelle <- SIM_AURA_clean %>%
  st_drop_geometry() %>%
  group_by(annee, mois) %>%
  summarise(temp_moyenne = mean(T_MENS, na.rm = TRUE), .groups = "drop")

# Préparation des décès (extraction année/mois depuis la colonne 'periode')
deces_mensuels <- deces_par_mois %>%
  mutate(annee = year(periode), 
         mois = month(periode))

# Jointure finale
df_final <- deces_mensuels %>%
  inner_join(meteo_mensuelle, by = c("annee", "mois"))

# Agrégation globale par mois civil (Moyenne 2010-2019)
analyse_saisonniere <- df_final %>%
  group_by(mois) %>%
  summarise(
    deces_moyens = mean(n_deces, na.rm = TRUE),
    temp_moyenne = mean(temp_moyenne, na.rm = TRUE)
  ) %>%
  mutate(nom_mois = month(mois, label = TRUE, abbr = FALSE, locale = "fr_FR.UTF-8"))


# 1. Filtrer les données pour l'année 2019 uniquement
df_2019 <- df_final %>%
  filter(annee == 2015) %>%
  mutate(nom_mois = month(mois, label = TRUE, abbr = FALSE, locale = "fr_FR.UTF-8"))

# 2. S'assurer que les mois sont dans le bon ordre chronologique
df_2019$nom_mois <- factor(df_2019$nom_mois, levels = rev(levels(df_2019$nom_mois)))

# 3. Création du graphique pour 2015
ggplot(df_2019, aes(x = reorder(nom_mois, mois), y = n_deces, fill = temp_moyenne)) +
  geom_col() + 
  coord_flip() + 
  scale_fill_gradient(low = "blue", high = "red", name = "Temp. Moyenne (°C)") +
  labs(
    title = "Mortalité mensuelle en AURA - Année 2015",
    subtitle = "Relation entre chaleur et pics de décès",
    x = "Mois",
    y = "Nombre total de décès",
    caption = "Données : INSEE & Météo-France"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "italic")
  )

# comparer communes entre elle avec un indicateur ? Prendre le premier graphique et regarder si c’est la meme tendance ?

#Le taux de mortalité Taux = (nombre de décès / la population total ) * 1 000



