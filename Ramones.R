
library(dplyr)
library(tidyverse)
library(rvest)
library(purrr)
library(stopwords)

pagina <- read_html('https://www.letras.com/ramones/')

pagina #Revisar elementos

nombres_canciones <-pagina %>%
  html_element(xpath = '//div[@class ="cnt-list--alp"]') %>%
  html_children() %>% 
  html_text2()
class(nombres_canciones)

#scrapear todos los link con las canciones
pag_canciones_ramones<-pagina %>% 
  html_element(xpath = '//div[@class ="cnt-list--alp"]') %>% 
  html_nodes("[data-shareurl]") %>%
  html_attr("data-shareurl")


#Ejemplo donde x es cualquier pagina de las rescatadas anteriormente
#pagina_1cancion <- read_html('x')  
  
#pagina_1cancion %>% 
 # html_node(xpath = '//div[@class="cnt-letra"]') %>%
  #html_text2()
  
#Funcion para recorrer paginas de las canciones guardadas
scrape_lyrics <- function(url) {
  webpage <- read_html(url)
  letra <- webpage %>% 
    html_node(xpath = '//div[@class="cnt-letra"]') %>% 
    html_text2()
  return(letra)
}

urls <- pag_canciones_ramones

canciones <-map(setNames(urls, paste0("cancion", seq_along(urls))), scrape_lyrics)

#Tratamiento de canciones. Pasa de Lista a Character para tratar los elementos
canciones<-tolower(canciones)
canciones<- gsub("\\n|\\.|\\,|\\-|\\)|\\(|\\?|\\!", " ", canciones)
#compilacion
ramones<-paste(canciones, collapse = '')

# Tokenización de palabras
palabras <- strsplit(ramones, '\\s+')[[1]]

stop_words <- stopwords("en")

palabras_filtradas <- palabras[!palabras %in% stop_words]

# las palabras filtradas
palabras_filtradas_contadas <- data.frame(table(palabras_filtradas))

# Ordenar de mayor a menor frecuencia y seleccionar las 30 palabras con mayor frecuencia
palabras_filtradas_top <- palabras_filtradas_contadas %>% 
  arrange(desc(Freq)) %>% 
  slice_head(n = 30)

# Crear gráfico de barras
ggplot(palabras_filtradas_top, aes(x = reorder(palabras_filtradas, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Las 30 palabras filtradas con mayor frecuencia en las canciones de Ramones",
       x = "Palabras",
       y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5))



