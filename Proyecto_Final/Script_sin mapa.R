#Librerias utilizadas
library(rgbif) 
library(tidyverse) 
library(sf) 
library(rworldxtra) 
library(geodata) 
library(ggspatial)
library(terra) 
library(tidyterra) 
library(paletteer) 
library(ggcorrplot) 
library(ggfortify)
library(ggplot2)
library(ggridges) 
library(plotly) 
library(patchwork) 
library(magick) 
library(grid)
library(patchwork)
library(factoextra)
library(FactoMineR)
library(vegan)

# === Obtención y Transformación de los datos ===
#---------------------------------------------
#Para la extracción de los registros desde Gbif
#Ejemplo con Catartes aura
Ca_au_sp <- occ_search(
  scientificName = "Cathartes aura",
  hasCoordinate = TRUE,
  hasGeospatialIssue = FALSE
)$data

#Se descargo y guardo cada registro de las 36 especies de manera individual con  el siguiente formato:
#Primeras dos letras del epiteto generico_Primeras dos  letras del epiteto especifico_sp

#Se utilizará la función filter para seleccionar los datos de mayor confianza posible
Ca_au_sp <- Ca_au_sp %>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN"))

#Se convirtira en un archivo espacial para poder extraer los datos climaticos de World clim
Ca_au_sp_vect <- vect(Ca_au_sp, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")
#--------------------------------------------------
#Para extraer las capas clímaticas
env <- worldclim_global(var = "bio", res = 10, path = "datos_wc")

#Para nombrar las capas clímaticas
v_names <- vector()
for(i in 1:19){
  v_names[i] <- paste0("bio_", sprintf("%02d", i))
}

names(env) <- v_names

#Se extraen los datos climaticos a traves del archivo vectorial, ademas de agregar la columna de species
Ca_au_env <- extract(env, Ca_au_sp_vect) 

Ca_au_env$species <- c("Cathartes aura")

#Se seleccionaran solo las 3 variables de nuestro interes
Ca_au_env <- Ca_au_env %>%
  select(species, bio_01, bio_07, bio_12)

#Solo para visualizar el correcto procesamiento
head(Ca_au_env)


#Esta guia de pasos se realizo 36 veces, una vez por cada especie presente en nuestra base de datos

#Estos 36 data frames fueron unidos con la función Aves <- rbind(df1, df2) 

#Vamos a agrupar las observaciones climaticas por especie para despues obtener el promedio de cada una
Aves_clima <- Ca_au_env %>%
  group_by(species) %>%      
  summarise(
    bio_01_prom = mean(bio_01, na.rm = TRUE),
    bio_07_prom = mean(bio_07, na.rm = TRUE),
    bio_12_prom = mean(bio_12, na.rm = TRUE)
  )
#--------------------------------------------------
#Para extraer las capas clímaticas
env <- worldclim_global(var = "bio", res = 10, path = "datos_wc")

#Para nombrar las capas clímaticas
v_names <- vector()
for(i in 1:19){
  v_names[i] <- paste0("bio_", sprintf("%02d", i))
}

names(env) <- v_names

#Se extraen los datos climaticos a traves del archivo vectorial, ademas de agregar la columna de species
Ca_au_env <- extract(env, Ca_au_sp_vect) 

Ca_au_env$species <- c("Cathartes aura")

#Se seleccionaran solo las 3 variables de nuestro interes
Ca_au_env <- Ca_au_env %>%
  select(species, bio_01, bio_07, bio_12)

#Solo para visualizar el correcto procesamiento
head(Ca_au_env)


#Esta guia de pasos se realizo 36 veces, una vez por cada especie presente en nuestra base de datos

#Estos 36 data frames fueron unidos con la función Aves <- rbind(df1, df2) 

#Despues se agruparon las observaciones climaticas por especie para despues obtener el promedio de cada una
Aves_clima <- Ca_au_env %>%
  group_by(species) %>%      
  summarise(
    bio_01_prom = mean(bio_01, na.rm = TRUE),
    bio_07_prom = mean(bio_07, na.rm = TRUE),
    bio_12_prom = mean(bio_12, na.rm = TRUE)
  )

#-------------------------------------------------
#Con esto obtenermos nuestra base de datos climaticos completa.
Aves_clima <- read.csv("Aves_clima.csv")
class(Aves_clima)

#Cargamos el csv correspondiente a la base de datos Obtenida de CONABIO
Aves_CONABIO <- read.csv("Aves_CONABIO.csv")
class(Aves_CONABIO)

#Por ultimo unimos nuestra base climatica con la descargada de CONABIO.
Aves_completa <- full_join(Aves_clima, Aves_CONABIO, by = join_by ("species" == "Especie_cientifica"))

#Guardamos nuestra base de datos
write.csv(Aves_completa, file = "Aves_completa.csv", row.names = FALSE)

Aves_completa <- read.csv("Aves_completa.csv")
class(Aves_completa)

# === Ánalisis generales ===
# Tablas de frecuencia para variables categóricas
table(Aves_completa$UICN)
table(Aves_completa$Gremio_trofico)
table(Aves_completa$Ambiente)
table(Aves_completa$Origen)

# Representación grafica

GG_O <- ggplot(Aves_completa, aes(x=Origen, fill=Origen))+
  geom_bar()+
  theme_minimal()+
  scale_fill_viridis_d(option = "D")+
  labs( title = "Proprción de la distribución de las aves", subtitle = "Datos extraidos de CONABIO", y="Frecuencia")

GG_A <- ggplot(Aves_completa, aes(x=Ambiente, fill=Ambiente))+
  geom_bar()+
  theme_minimal()+
  scale_x_discrete(labels = c(
    "Terrestre" = "T",
    "Dulceacuicola_Terrestre" = "D_T",
    "Dulceacuicola" = "D",
    "Marino_Dulceacuicola_Salobre" = "M_D_S"
  )) +
  scale_fill_viridis_d(option = "D")+
  labs( title = "Diversidad de ambientes ocupados", subtitle = "Datos extraidos de CONABIO", y="Frecuencia")
  
GG_U <- ggplot(Aves_completa, aes(x=UICN, fill=UICN))+
  geom_bar()+
  theme_minimal()+
  scale_fill_viridis_d(option = "D")+
  labs( title = "Proporcion de riesgo que enfrentan las aves", subtitle = "Datos extraidos de CONABIO", y="Frecuecnia")

GG_G <- ggplot(Aves_completa, aes(x=Gremio_trofico, fill=Gremio_trofico))+
  geom_bar()+
  theme_minimal()+
  scale_fill_viridis_d(option = "D")+
  scale_x_discrete(labels = c(
    "Carn_acuatico" = "C_a",
    "Carnivoro" = "C",
    "Frugivoro" = "F",
    "Gran_migratorio" = "G_M",
    "Insectivoro"= "I",
    "Inv_acuatico" = "I_a",
    "Inv_migratorio" ="I_m",
    "Inv_terrestre" ="I_t",
    "Nectarivoro" ="N",
    "Omnivoro" ="O",
    "Piscivoro"= "P"
  )) +
  labs( title = "Proporción del gremio trofico de las aves", subtitle = "Datos extraidos de CONABIO", y="Frecuencia")

#Fusión de los graficos mediante patchword

GG_T <-  wrap_plots(GG_A, GG_U, GG_O, GG_G, ncol = 2) +
  plot_annotation(
    title = "Proporciones generales de las principales variables categoricas",
    subtitle = "Datos obtenidos de CONABIO",
  )

GG_T

# Matriz de correlación entre variables numéricas
cor_aves <- cor(Aves_completa[,c("bio_01", "bio_07", "bio_12", "No_observaciones")])
corrplot::corrplot(cor_aves)

# Cluster por variables ambientales
cluster_aves <- scale(Aves_completa[,c("bio_01", "bio_07","bio_12")])
kmeans_result <- kmeans(cluster_aves, centers = 3)

fviz_cluster(kmeans_result, cluster_aves)

# Boxplots por categorías
ggplot2::ggplot(Aves_completa) +
  geom_boxplot(aes(x = Gremio_trofico, y = bio_01))

# Para variables categóricas
mca_aves <- MCA(Aves_completa[,c("UICN", "Gremio_trofico", "Ambiente", "Origen")])

# Diversidad por categorías
diversidad1 <- diversity(table(Aves_completa$Gremio_trofico, Aves_completa$UICN))
diversidad1
diversidad2 <- diversity(table(Aves_completa$Ambiente, Aves_completa$UICN))
diversidad2
diversidad3 <- diversity(table(Aves_completa$Origen, Aves_completa$UICN))
diversidad3

## Mapas de distribución de condiciones ambientales por gremio trófico
ggplot(Aves_completa) +
  geom_point(aes(x = bio_01, y = bio_12, 
                 color = Gremio_trofico, size = No_observaciones)) +
  facet_wrap(~UICN)

#_-------------
#GLM para ver que afecta el numero de registros
modelo_aves <- glm(No_observaciones ~ bio_01 + bio_07 + bio_12 + 
                     UICN + Gremio_trofico + Ambiente + Origen,
                   data = Aves_completa, family = poisson)
summary(modelo_aves)

# === ánálisis de Componentes Principales ===
#-------------------------------
# Análisis PCA

# transformación de variables categoricas a magnitudes numericas

uicn_orden <- c("LC" = 1,  
                "NT" = 2,  
                "VU" = 3,  
                "EN" = 4,  
                "CR" = 5,  
                "EW" = 6,  
                "EX" = 7)  

Aves_numerica<- Aves_completa %>%
  mutate(UICN_num = uicn_orden[UICN])

gremio_orden <- c("Omnivoro" = 1,
                  "Carnivoro" = 2,
                  "Insectivoro" = 2,
                  "Frugivoro" = 2,
                  "Nectarivoro" = 3,
                  "Piscivoro" = 2,
                  "Carn_acuatico" = 3,
                  "Inv_terrestre" = 3,
                  "Inv_acuatico" = 3,
                  "Inv_migratorio" = 4,
                  "Gran_migratorio" = 4)

Aves_numerica <- Aves_numerica %>%
  mutate(Gremio_num = gremio_orden[`Gremio_trofico`])

Aves_numerica <- Aves_numerica %>%
  mutate(
    Ambiente_num = case_when(
      Ambiente == "Terrestre" ~ 1,
      Ambiente == "Dulceacuicola_Terrestre" ~ 2,
      Ambiente == "Dulceacuicola" ~ 3,
      Ambiente == "Dulceacuicola_Salobre" ~ 4,
      Ambiente == "Marino_Dulceacuicola_Salobre" ~ 5,
      TRUE ~ NA_real_
    )
  )

#Guardado de la base numerica
write_csv(Aves_numerica, "Aves_numerica.csv")

Aves_numerico <- read.csv("Aves_numerica.csv")

data1 <- Aves_numerica[, c(2,3,4,13,11,12)]# Solo las variables numéricas

# Realizar PCA 1 (sin observaciones)
pca1 <- prcomp(data1, scale = TRUE)  # scale = TRUE para estandarizar
pca1
summary(pca1)



#Biplot 
autoplot(pca1, data1 = Aves_numerico, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'darkgreen',
         loadings.label = TRUE, loadings.label.size = 3) +
  theme_minimal()+
  labs( title = "PCA sin considerar las observaciones", subtitle = "Datos extraidos de CONABIO y GBIF")

# Realizar PCA 2 (con observaciones)
data2 <- Aves_numerica[, c(2,3,4,6,13,11,12)]

pca2 <- prcomp(data2, scale = TRUE)  # scale = TRUE para estandarizar
pca2
summary(pca2)

#Biplot 
autoplot(pca2, data2 = Aves_completa, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'darkgreen',
         loadings.label = TRUE, loadings.label.size = 3) +
  theme_minimal()+
  labs( title = "PCA considerando observaciones", subtitle = "Datos extraidos de CONABIO y GBIF")

