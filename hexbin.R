library(ggplot2)
library(sf)
library(maptiles)
library(tidyterra)

setwd("C:/Users/Amat/Proton Drive/amat.canti/My files/@form/0 - UOC/0 - Màster Ciència de Dades/02 - Visualització de dades/PAC2/Hexbin")

# 1. Dades
locals <- read.csv("../Dades/bcn_cens_locals_planta_baixa_2024.csv",
                   fileEncoding = "UTF-8-BOM")
nrow(locals)                                   # 68024

summary(locals$Latitud)
summary(locals$Longitud)
table(locals$Nom_Principal_Activitat)

# 2. Filtre: bars, restaurants i hotels
rest <- subset(locals, grepl("Restaurants", Nom_Grup_Activitat))
nrow(rest)                                     # 10864

b <- ggplot_build(ggplot(rest, aes(Longitud, Latitud)) + geom_hex(bins = 50))$data[[1]]
nrow(b)                                  # 1057 hexàgons
median(b$count)                          # 7
b[b$count >= 50, c("x", "y", "count")]   # els 8 hexàgons

# 3. Mapa de fons
punts <- st_as_sf(rest, coords = c("Longitud", "Latitud"), crs = 4326)
fons <- get_tiles(punts, provider = "Esri.WorldGrayCanvas", zoom = 13, crop = TRUE)

# 4. Noms dels districtes (referència)
districtes <- aggregate(cbind(Longitud, Latitud) ~ Nom_Districte,
                        data = locals, FUN = mean)
districtes[districtes$Nom_Districte == "Eixample", c("Longitud", "Latitud")] <- c(2.1700, 41.3975)
districtes[districtes$Nom_Districte == "Ciutat Vella", c("Longitud", "Latitud")] <- c(2.1665, 41.3768)
districtes                                     # 10 files

# 5. Etiquetes dels punts calents
llocs <- data.frame(
  nom = c("Barri Gòtic i el Raval", "Antiga Esquerra de l'Eixample",
          "Sant Pere, Santa Caterina i la Ribera",
          "La Maquinista, centre comercial (Bon Pastor)"),
  Longitud = c(2.1749, 2.1576, 2.1789, 2.1974),
  Latitud  = c(41.3794, 41.3894, 41.3854, 41.4411),
  costat   = c(0, 0, 0, 1))
llocs$x_text <- llocs$Longitud + ifelse(llocs$costat == 0, 0.004, -0.004)

# 6. Gràfic
g <- ggplot() +
  geom_spatraster_rgb(data = fons) +
  geom_hex(data = rest, aes(x = Longitud, y = Latitud),
           bins = 50, alpha = 0.75) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "Locals per\nhexàgon") +
  geom_label(data = districtes,
             aes(x = Longitud, y = Latitud, label = toupper(Nom_Districte)),
             size = 3, colour = "grey35", fontface = "bold",
             fill = "white", alpha = 0.6, linewidth = 0) +
  geom_label(data = llocs,
             aes(x = x_text, y = Latitud, label = nom, hjust = costat),
             size = 2.8, fill = "white", alpha = 0.8, linewidth = 0.3) +
  labs(title = "On es concentren els bars, restaurants i hotels de Barcelona?",
       subtitle = "10.864 locals en planta baixa, agrupats en hexàgons d'uns 220 m",
       caption = "Dades: Ajuntament de Barcelona, Open Data BCN. Cens de locals en planta baixa 2024 (CC BY 4.0)\nMapa de fons: Tiles © Esri - Esri, DeLorme, NAVTEQ") +
  theme_void()
g

# 7. Desar
ggsave("hexbin.png", g, width = 8, height = 8, dpi = 200, bg = "white")
