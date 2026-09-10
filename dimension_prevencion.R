# ==============================================================================
# SCRIPT 03: OPERATIVIZACIÓN Y CÁLCULO DE LA DIMENSIÓN DE PREVENCIÓN (ReSVI)
# ==============================================================================

library(sf)
library(tidyverse)

# ==============================================================================
# 1. ASIGNACIÓN DE INFRAESTRUCTURAS Y RECURSOS POR MUNICIPIO
# ==============================================================================
# A. Recursos de extinción (Distritos y Grupos)
concellos <- st_read('C:/Users/MASTER_TIG/Desktop/tfm/capas/dimension_prevencion/concellos.gpkg')[cite: 9]

# Mapeo de Grupos de Extinción por Distrito (Se omite la tabla completa por brevedad en el anexo, referirse a la metodología)
mapeo_completo <- tibble(
  NAMEUNIT = c('Ares', 'Cabanas', 'A Capela', 'Cariño', 'Cedeira', 'Cerdido', 'Fene', 'Ferrol', 'Mañón', 'Moeche', 'Monfero', 'Mugardos', 'Narón', 'Neda', 'Ortigueira', 'Pontedeume', 'As Pontes de García Rodríguez', 'San Sadurniño', 'As Somozas', 'Valdoviño'), # Muestra del Distrito I
  distrito = rep('I', 20),
  grupos_ext = rep(2, 20)
)[cite: 9]

concellos_final <- concellos %>% left_join(mapeo_completo, by = "NAMEUNIT")[cite: 9]

# B. Puntos de Agua (Consolidación regional)
carpeta_capas <- "C:/Users/MASTER_TIG/Desktop/tfm/capas/dimension_prevencion/"[cite: 10]
agua_coruna     <- read_csv(paste0(carpeta_capas, "Puntos de auga A CORUÑA (D I-V).csv"))[cite: 10]
agua_lugo       <- read_csv(paste0(carpeta_capas, "Puntos de auga LUGO (D VI-X).csv"))[cite: 10]
agua_ourense    <- read_csv(paste0(carpeta_capas, "Puntos de auga OURENSE (D XI-XV).csv"))[cite: 10]
agua_pontevedra <- read_csv(paste0(carpeta_capas, "Puntos de auga PONTEVEDRA (D XVI-XIX).csv"))[cite: 10]

todos_puntos_agua <- bind_rows(agua_coruna, agua_lugo, agua_ourense, agua_pontevedra)[cite: 10]
conteo_agua <- todos_puntos_agua %>% group_by(Nome) %>% summarise(total_p_agua = n())[cite: 10]

# Cálculo de la densidad de puntos de agua
concellos_final_agua <- concellos_final %>%
  left_join(conteo_agua, by = c("NAMEUNIT" = "Nome")) %>%
  mutate(total_p_agua = replace_na(total_p_agua, 0),
         densidad_agua = total_p_agua / areakm)[cite: 10]

# ==============================================================================
# 2. ESPACIALIZACIÓN DE VARIABLES HACIA LA MALLA CONTINUA
# ==============================================================================
malla_superposicion <- st_read(paste0(carpeta_capas, "superposi_cortafuegos.gpkg")) %>% mutate(id_malla = row_number())[cite: 10]

malla_centroides_unidos <- malla_superposicion %>% 
  st_centroid() %>%
  st_join(concellos_final_agua, join = st_within) %>%
  st_drop_geometry() %>% select(id_malla, total_p_agua, densidad_agua)[cite: 10]

malla_prev_base <- malla_superposicion %>% left_join(malla_centroides_unidos, by = "id_malla") %>% mutate(ID = row_number())[cite: 10, 11]

# ==============================================================================
# 3. INTERSECCIÓN Y CÁLCULO DE LA DEFENSA PASIVA
# ==============================================================================
malla_forestal <- st_read("C:/Users/MASTER_TIG/Desktop/tfm/capas/dimension_natural/malla_c.gpkg") %>% mutate(ID = row_number())[cite: 11]
defensa_pasiva <- st_read("C:/Users/MASTER_TIG/Desktop/tfm/capas/dimension_prevencion/defensa_pasiva.gpkg") %>% st_transform(st_crs(malla_forestal))[cite: 11]

# Exposición de infraestructuras respecto al área forestal
datos_nuevos_defensa <- st_intersection(malla_forestal, defensa_pasiva) %>%
  mutate(
    area_defensa_km2 = as.numeric(st_area(geom)) / 1000000,
    pct_defensa_forest = (area_defensa_km2 / area_forestal) * 100
  ) %>%
  mutate(pct_defensa_forest = if_else(pct_defensa_forest > 100, 100, pct_defensa_forest)) %>%
  st_drop_geometry() %>% group_by(ID) %>% 
  summarise(nueva_defensa_pasiva = mean(pct_defensa_forest, na.rm = TRUE), .groups = "drop")[cite: 11]

malla_prev_actual <- malla_prev_base %>%
  left_join(datos_nuevos_defensa, by = "ID") %>%
  mutate(nueva_defensa_pasiva = if_else(is.na(nueva_defensa_pasiva), 0, nueva_defensa_pasiva)) %>% select(-ID)[cite: 11]

# ==============================================================================
# 4. NORMALIZACIÓN Y CÁLCULO DEL SUBÍNDICE FINAL DE PREVENCIÓN
# ==============================================================================
# Función para normalizar invirtiendo el Z-score (a mayor prevención, menor vulnerabilidad)
normalizar_prevencion_resvi <- function(columna) {
  if(sd(columna, na.rm = TRUE) == 0 || all(is.na(columna))) return(rep(0, length(columna)))
  z_score <- (columna - mean(columna, na.rm = TRUE)) / sd(columna, na.rm = TRUE)
  z_orientado <- z_score * (-1) 
  min_z <- min(z_orientado, na.rm = TRUE)
  max_z <- max(z_orientado, na.rm = TRUE)
  return((z_orientado - min_z) / (max_z - min_z))
}[cite: 11]

malla_prev_normalizada <- malla_prev_actual %>%
  mutate(
    v_bomberos    = normalizar_prevencion_resvi(densidad_bom),
    v_puntos_agua = normalizar_prevencion_resvi(densidad_agua),
    v_def_pasiva  = normalizar_prevencion_resvi(nueva_defensa_pasiva)
  ) %>%
  rowwise() %>%
  mutate(ReSVI_Prevencion = mean(c(v_bomberos, v_puntos_agua, v_def_pasiva), na.rm = TRUE)) %>%
  ungroup()[cite: 11]

st_write(malla_prev_normalizada, "C:/Users/MASTER_TIG/Desktop/tfm/capas/malla_prevencion_resvi_final.gpkg", delete_dsn = TRUE)[cite: 11]