# ==============================================================================
# SCRIPT 02: MODELO DASIMÉTRICO Y MALLA DE VULNERABILIDAD SOCIAL (ReSVI)
# ==============================================================================

library(dplyr)
library(sf)
library(tidyr)

# 1. CARGA DE DATOS (INE y Cartografía)
estudios_limpios_final <- read.csv2("E:/tfm/capas/renta_persona/estudios_limpios_final.csv")
renta_limpia <- read.csv2("E:/tfm/capas/renta_persona/renta_galicia_limpia.csv") 
catastro_pob <- st_read("E:/tfm/capas/Grupos_Edad/catastro_pob.gpkg")
malla_social_perfecta <- st_read("E:/tfm/capas/Grupos_Edad/malla_social_perfecta.gpkg")

renta_limpia$cusec <- as.character(renta_limpia$cusec)
estudios_limpios_final$cusec <- as.character(estudios_limpios_final$cusec)

# 2. UNIÓN RELACIONAL AL CATASTRO
catastro_actualizado <- catastro_pob %>%
  left_join(renta_limpia, by = c("CUSEC" = "cusec")) %>%
  left_join(estudios_limpios_final, by = c("CUSEC" = "cusec"))

# 3. OPERATIVIZACIÓN DEL MODELO DASIMÉTRICO
catastro_actualizado <- catastro_actualizado %>%
  mutate(
    renta = ifelse(is.na(renta), 0, as.numeric(renta)),
    porcen_estudios_bajos = ifelse(is.na(porcen_estudios_bajos), 0, as.numeric(porcen_estudios_bajos)),
    pob_fin_suma = ifelse(is.na(pob_fin_suma), 0, as.numeric(pob_fin_suma)),
    
    masa_salarial_edi = renta * pob_fin_suma,
    pob_estudios_bajos_edi = (porcen_estudios_bajos / 100) * pob_fin_suma
  )

# 4. ANÁLISIS ESPACIAL: INTERSECCIÓN CON MALLA CONTINUA
catastro_puntos <- st_centroid(catastro_actualizado)
malla_cruce <- st_join(malla_social_perfecta, catastro_puntos, join = st_intersects)

# 5. AGREGACIÓN DE INDICADORES POR CELDA (1x1 km)
malla_social_definitiva_total <- malla_cruce %>%
  st_drop_geometry() %>% 
  group_by(id_malla) %>%
  summarise(
    pob_total_celda = sum(pob_fin_suma, na.rm = TRUE),
    suma_masa_salarial = sum(masa_salarial_edi, na.rm = TRUE),
    suma_pob_estudios = sum(pob_estudios_bajos_edi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    renta_media_malla = ifelse(pob_total_celda > 0, suma_masa_salarial / pob_total_celda, 0),
    porcen_estudios_bajos_malla = ifelse(pob_total_celda > 0, (suma_pob_estudios / pob_total_celda) * 100, 0)
  )

# 6. EXPORTACIÓN DE RESULTADOS
malla_social_final_exportar <- malla_social_perfecta %>%
  left_join(malla_social_definitiva_total, by = "id_malla")

st_write(malla_social_final_exportar, "E:/tfm/capas/renta_persona/malla_social_completa.gpkg", delete_dsn = TRUE)