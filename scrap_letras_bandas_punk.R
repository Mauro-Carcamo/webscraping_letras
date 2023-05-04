install.packages("udpipe")
library(udpipe)
library(rvest)
library(purrr)
library(tibble)
library(tm)
library(gptstudio)




bandas_punk <- c('https://www.letras.com/ramones/',
                 'https://www.letras.com/sex-pistols/',
                 'https://www.letras.com/the-clash/',
                 'https://www.letras.com/damned/',
                 'https://www.letras.com/buzzcocks/',
                 'https://www.letras.com/the-stooges/',
                 'https://www.letras.com/new-york-dolls/')


nombres_bandas<- gsub("https://www.letras.com/|[/\"']|","",bandas_punk)


scrape_lyrics <- function(url) {
  webpage <- read_html(url)
  letra <- webpage %>% 
    html_node(xpath = '//div[@class="cnt-letra"]') %>% 
    html_text2()
  return(letra)
}

bandas_canciones <- map_dfr(bandas_punk, function(banda) {
  pagina <- read_html(banda)
  canciones <- pagina %>%
    html_element(xpath = '//div[@class ="cnt-list--alp"]') %>% 
    html_nodes("[data-shareurl]") %>%
    html_attr("data-shareurl")
  tibble(banda = gsub('https://www.letras.com/|/', '', banda), #obtener solo el nombre de la banda
         letra = map(canciones, scrape_lyrics))
})

head(bandas_canciones)
original_bandas<-bandas_canciones


bandasfactor<-as.factor(bandas_canciones$banda)
summary(bandasfactor)#numero de canciones por banda
class(bandas_canciones)

bandas_canciones<- as.data.frame(bandas_canciones)
solo_letras <- bandas_canciones[-1,]

solo_letras <-trimws(solo_letras)
solo_letras<-tolower(solo_letras)
solo_letras <- gsub("\\n\\n\\n|\\n\\n|\\n|\\\\n", " ", solo_letras)
solo_letras <- gsub("          ", " ", solo_letras)
solo_letras <- gsub("   ", " ", solo_letras)
solo_letras <- gsub("  ", " ", solo_letras)
solo_letras <- gsub("[^[:alnum:]]", " ", solo_letras)


# Tokenización de palabras
solo_palabras <- strsplit(solo_letras, '\\s+')[[1]]



#lematizacion
udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(udmodel$file_model)
text <- solo_palabras
doc <- udpipe_annotate(udmodel, x = text)
doc <- as.data.frame(doc)
lemmas <- doc$lemma


#Stopwords

stop_words <- stopwords("en")

palabras_filtradas <- palabras[!palabras %in% stop_words]

# las palabras filtradas
palabras_filtradas_contadas$palabras_filtradas <- data.frame(table(palabras_filtradas))








#compilacion
#ramones<-paste(bandas_canciones, collapse = '')



solo_letras <- gsub(("\\n|\\.|\\,|\\-|\\)|\\(|\\?|\\!", " ", solo_letras)
bandas_canciones<- gsub("ramones|sex-pistols|the-clash|damned|buzzcocks|the-stooges|new-york-dolls", " ", bandas_canciones)




#compilacion
ramones<-paste(bandas_canciones, collapse = '')

# Tokenización de palabras
palabras <- strsplit(ramones, '\\s+')[[1]]

stop_words <- stopwords("en")

palabras_filtradas <- palabras[!palabras %in% stop_words]

# las palabras filtradas
palabras_filtradas_contadas$palabras_filtradas <- data.frame(table(palabras_filtradas))


palabras_filtradas_contadas$palabras_filtradas<- gsub("\\n|\\n\\n|\\n\\n\\n|\\.|\\,|\\-|\\)|\\(|\\?|\\!|[\"']"," ",palabras_filtradas_contadas$palabras_filtradas)
palabras_filtradas_contadas$palabras_filtradas<- gsub(" ","",palabras_filtradas_contadas$palabras_filtradas)
data <- palabras_filtradas_contadas$palabras_filtradas


corpus <- Corpus(VectorSource(palabras_filtradas_contadas$palabras_filtradas))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)#lematizacion
corpus <- tm_map(corpus, stripWhitespace)

#purbea



view(corpus)


dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(corpus, 0.8)


dtm$dimnames$Terms

data_fullsongs <- dtm$dimnames
texto_total <- data_fullsongs$Terms
texto_total


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




