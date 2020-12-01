library(tidyverse)
library(lubridate)
library(rvest)
library(stringdist)
library(viridis)

# Extrayendo datos de los senadores ---------------------------------------

extraer_links <- function(link){
  links <- read_html(link)
  
  links <- links %>% 
    xml_find_all("//a[contains(@href, 'wiki')]") %>% 
    html_attr(name = "href")
  Sys.sleep(sample(10, 1) * 0.1)
  
  links <-  tibble(link = links) %>% 
    mutate(link = paste0("https://www.bcn.cl", link))
}
extraer_nacimiento <- function(link) {
  pagina_parlamentario <- read_html(link)
  
  #Extraer nombre
  nombre <- pagina_parlamentario %>% 
    xml_find_all("//h2[contains(@itemprop, 'name')]") %>%
    html_text()
  
  #Extraer nacimiento
  nacimiento <- pagina_parlamentario %>% 
    xml_find_all("//span[contains(@property, 'bio:date')]") %>%
    html_text() %>% 
    str_remove_all("\n")
  
  Sys.sleep(sample(10, 1) * 0.1)
  fecha_nacimiento <- tibble(nombre, nacimiento)
}

links_senadores <- extraer_links("https://www.bcn.cl/historiapolitica/resenas_parlamentarias/index.html?categ=en_ejercicio&filtros=2")

datos_senadores <- map_df(links_senadores$link, extraer_nacimiento)
datos_senadores <- datos_senadores %>% 
  mutate(fecha_2012 = dmy(str_replace(nacimiento, pattern = "[0-9]{4}",
                                      replacement = "2012")))

#También queremos hacer una distribución por partido politico

pagina_web <- "https://www.senado.cl/appsenado/index.php?mo=senadores&ac=listado"
pagina_web <- read_html(pagina_web)

nombre <- pagina_web %>% 
  xml_find_all("//td[contains(@style, 'width: 85%')]") %>%
  html_text() %>% 
  str_extract(pattern = "[A-Z].*") %>% 
  str_replace_all(pattern = ",", replacement = "" )

partido <- pagina_web %>% 
  xml_find_all("//td[contains(text(), 'Partido: ')]") %>%
  html_text() %>% 
  str_extract(pattern = "(?<=\\s).*") %>% 
  str_replace_all(pattern = "\\.", replacement = "")
  

partidos <- tibble(nombre, partido)

#Los nombres son distintos en cada tabla. Se hará un matching aproximado
#ordenando las palabras por orden alfabético

partidos$nombre_ordenado <- partidos$nombre %>%
  str_split( pattern = " ") %>% 
  lapply('sort') %>% 
  lapply('paste', collapse = " ") %>% 
  unlist()

datos_senadores$nombre_ordenado <- datos_senadores$nombre %>% 
  str_split( pattern = " ") %>% 
  lapply('sort') %>% 
  lapply('paste', collapse = " ") %>% 
  unlist()
  
datos_senadores$partido <- partidos$partido[amatch(datos_senadores$nombre_ordenado,
                                         partidos$nombre_ordenado,
                                         maxDist = 20)]

# Extrayendo datos de los diputados ---------------------------------------

links_diputados <- paste0("https://www.bcn.cl/historiapolitica/resenas_parlamentarias/index.html?categ=en_ejercicio&filtros=3&pagina=",
       c(1:4), "&K=1#listado_parlamentarios")

links_diputados <- map_df(links_diputados, extraer_links)
datos_diputados <- map_df(links_diputados$link, extraer_nacimiento)

pagina_web <- "https://www.camara.cl/diputados/diputados.aspx"
pagina_web <- read_html(pagina_web)

nombre <- pagina_web %>% 
  xml_find_all("//a[contains(@href, 'detalle/mociones')]") %>%
  html_text()

nombre <- nombre[str_detect(string = nombre,
                            pattern = "\\r\\n", negate = TRUE)]
nombre <- nombre[-c(1:4)]
nombre <- nombre %>% 
  str_extract(pattern = "(?<=Sr. |Sra. ).*")

partido <- pagina_web %>% 
  xml_find_all("//p[contains(text(), 'Partido: ')]") %>%
  html_text()
partido <- partido[-c(1:3)]
partido <- partido %>% 
  str_extract(pattern = "(?<=Partido: ).*")
  
partidos_diputados <- tibble(nombre, partido)

partidos_diputados$nombre_ordenado <- partidos_diputados$nombre %>%
  str_split( pattern = " ") %>% 
  lapply('sort') %>% 
  lapply('paste', collapse = " ") %>% 
  unlist()

datos_diputados$nombre_ordenado <- datos_diputados$nombre %>% 
  str_split( pattern = " ") %>% 
  lapply('sort') %>% 
  lapply('paste', collapse = " ") %>% 
  unlist()

datos_diputados$partido <- partidos_diputados$partido
datos_diputados <- datos_diputados %>% 
  mutate(fecha_2012 = dmy(str_replace(nacimiento, pattern = "[0-9]{4}",
                                      replacement = "2012")))


# Uniendo el congreso -----------------------------------------------------

datos_senadores$camara <- "Senado"
datos_diputados$camara <- "Diputados"

congreso <- rbind(datos_senadores, datos_diputados) %>% 
  select(-nombre_ordenado)

congreso$partido <- if_else(congreso$partido == "Revolución Democrática",
                             true = "RD", false = congreso$partido)

# Incorporando el zodiaco -------------------------------------------------

#Cargando los períodos del zodiaco:

zodiaco <- read_delim("zodiaco.txt", delim = "\t", 
                      col_names = c("signo", "fecha"))

#Pero vienen juntos, así que se separan, y eliminamos la fecha original:

zodiaco <- zodiaco %>% 
  mutate(fecha_inicio = str_sub(fecha, start = 5,
                                end = str_locate(.$fecha,"al")[,1] - 2L),
         fecha_termino = str_sub(fecha, start = str_locate(.$fecha,"al")[,2] + 2L,
                                 end = str_length(.$fecha))
  ) %>% 
  select(-fecha)

#Eventualmente compararemos fechas, así que debemos pasarlo a ese formato.
#Primero debemos agregar un año, porque sin año, no es una fecha.

zodiaco <- zodiaco %>%
  mutate(fecha_inicio = dmy(str_c(fecha_inicio, " de 2012")),
         fecha_termino = dmy(str_c(fecha_termino, " de 2012"))
  ) %>% 
  add_row(signo = "Capricornio", fecha_inicio = ymd("2013-01-01"),
          fecha_termino = ymd("2013-01-20"))


congreso <- congreso %>% 
  mutate(signo = cut.Date(x = fecha_2012, breaks = zodiaco$fecha_inicio,
                          labels = zodiaco$signo[-length(zodiaco$signo)])
         )

# Analisis y graficos -----------------------------------------------------

congreso %>% 
  ggplot(aes(x = signo)) +
  geom_bar(fill = "cornflowerblue") +
  labs(title = "Frecuencia zodiacal del Congreso", x = "Signo Zodiacal", 
       y = "Frecuencia") + 
  geom_text(stat='count', aes(label=..count..), hjust=-0.3) +
  coord_flip()

congreso %>% 
  filter(camara == "Diputados") %>% 
  ggplot(aes(x = signo)) +
  geom_bar(fill = "cornflowerblue") +
  labs(title = "Frecuencia zodiacal de la Cámara de Diputados",
       x = "Signo Zodiacal", 
       y = "Frecuencia") +
    geom_text(stat='count', aes(label=..count..), hjust=-0.3) +
  coord_flip()

congreso %>% 
  filter(camara == "Senado") %>% 
  ggplot(aes(x = signo)) +
  geom_bar(fill = "cornflowerblue") +
  labs(title = "Frecuencia zodiacal del Senado",
       x = "Signo Zodiacal", 
       y = "Frecuencia") +
  geom_text(stat='count', aes(label=..count..), hjust=-0.3) +
  coord_flip()

congreso %>% 
  ggplot(aes(x = partido, fill = signo)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribución al interior de partidos", x = "Partido",
       y = "Porcentaje") +
  coord_flip()
  
