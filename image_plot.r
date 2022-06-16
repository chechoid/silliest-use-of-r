library(tidyverse)
library(googlesheets4)
library(gargle)
library(ggimage)
library(class)

datos <- read_sheet("1D_nBkh3nJ5io1-FQ7o1MCfqRNMVH5-nKKmiIe_ZFhEg")

clones <- datos

# Agregar columna de ID
clones$id <- rep(1:nrow(clones))

names(clones)

# Eliminar columnas innecesarias
clones <- clones %>% 
  select(-"Marca temporal", -"Poné lo que quieras... parecidos, chistes, comentarios, etc...")

# Pivotear variables
clones <- clones %>% 
  select(id, everything()) %>% 
  pivot_longer(cols = c("Facha de Keanu": "Copadez de Javier"),
               names_to = "personaje",
               values_to = "puntaje")

clones




# Separar variables categóricas
clones <- clones %>% 
  mutate(personaje = str_remove(personaje, "de "),
         personaje = str_remove(personaje, "del "))

clones %>% 
  group_by(personaje) %>% 
  summarise(valor_promedio = mean(puntaje)) %>% 
  ggplot(aes(x = valor_promedio, y = personaje)) +
  geom_point()

clones <- clones %>% 
  separate(personaje,  into = c("metrica", "persona"))

clones

# Pivotear ancho 

clones <- clones %>% 
  pivot_wider(id_cols = c(id, persona),
              names_from = metrica,
              values_from = puntaje)

clones


resultados <- clones %>% 
  group_by(persona) %>% 
  summarise(facha_promedio = mean(Facha),
            copadez_promedio = mean(Copadez))

ggplot(resultados, aes(x = copadez_promedio, y = facha_promedio, color = persona)) +
  geom_point()

# Creo un dataframe de las fotos ----
persona <- resultados %>% 
  select(persona) %>% 
  pull()

# Creo un vector de imágenes
ruta <- "pics"        # Ruta de las fotos
extension <- "png"   # Extensión de los archivos de imágenes

# nombres de los archivos
imagen <- c("Ben", "Brad", "Javier", "jeff", "keanu", "mono", "nico", 
            "ricky", "roberto", "russell", "sergio")

# Creo el vector de fotos con dirección y extensión completa
foto <- str_c(ruta, imagen, sep = "/")
foto <- str_c(foto, extension, sep = ".")

# Creo el dataframe y lo agrego al dataframe resultados
pics <- data.frame(persona, foto)

resultados <- left_join(resultados, pics)

resultados

ggplot(resultados, aes(x = copadez_promedio, y = facha_promedio)) +
  geom_image(aes(image=foto), size = 0.08) +
  theme_minimal() +
  scale_x_continuous(limits = c(1,10)) +
  scale_y_continuous(limits = c(1,10))

ggsave("clones.png", dpi = 320)
