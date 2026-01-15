library(sf)
library(dplyr)
library(lubridate)

# 1. Charger les données
var_2010_2019 <- read.csv("SIM/MENS_SIM2_2010-2019.csv", sep = ";", dec = ",", header = TRUE)

# 2. Préparation des points (Echelle hectomètres -> mètres)
df_clean <- var_2010_2019 %>%
  mutate(
    LAMBX_M = LAMBX * 100,
    LAMBY_M = LAMBY * 100
  )

points_sf <- st_as_sf(df_clean, coords = c("LAMBX_M", "LAMBY_M"), crs = 27572)

# 3. Charger le shapefile et FILTRER sur la région 84
# Remarque : Vérifiez si le nom de la colonne est 'INSEE_REG' ou 'code_insee' dans votre shapefile
zone_all <- st_read("REGIONS/REGIONS_FRANCE.shp")

zone_84 <- zone_all %>% 
  filter(code_insee == "84") # On ne garde que Auvergne-Rhône-Alpes

# 4. Harmonisation du CRS
points_sf <- st_transform(points_sf, st_crs(zone_84))

# 5. Intersection (uniquement avec la région 84)
points_coupes <- st_intersection(points_sf, zone_84)

# 6. Extraction des coordonnées pour le CSV
points_csv <- points_coupes %>%
  mutate(
    X_METRES = st_coordinates(.)[,1],
    Y_METRES = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

# 7. Exportation
write.csv2(points_csv, "points_aura_2010_2019.csv", row.names = FALSE)

cat("Nombre de points retenus pour la région 84 :", nrow(points_csv), "\n")


library(sf)
library(dplyr)

# 1. Charger les couches
# Remplacez les chemins vers vos fichiers .shp
communes_all <- st_read("COMMUNES/COMMUNES_FRANCE.shp")
zone_all <- st_read("REGIONS/REGIONS_FRANCE.shp")

# 2. Filtrer la région cible (84 - AURA)
zone_84 <- zone_all %>% 
  filter(code_ins_4 == "84")

# 3. Harmonisation du CRS (Très important pour l'intersection)
# On s'assure que les communes utilisent le même système que la région
if(st_crs(communes_all) != st_crs(zone_84)){
  communes_all <- st_transform(communes_all, st_crs(zone_84))
}

# 4. Découpage (Intersection)
# Cette opération ne garde que les parties des communes situées DANS la région 84
communes_aura <- st_intersection(communes_all, zone_84)

# 5. Nettoyage (Optionnel)
# L'intersection peut créer des géométries invalides ou des "petits morceaux" 
# si une commune est à cheval sur deux régions.
communes_aura <- st_make_valid(communes_aura)

# 6. Sauvegarde
st_write(communes_aura, "communes_aura_84.shp", delete_layer = TRUE)

cat("Nombre de communes après découpage :", nrow(communes_aura), "\n")





