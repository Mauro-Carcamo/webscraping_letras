install.packages("udpipe")
install.packages("quanteda")
library(udpipe)
library(rvest)
library(purrr)
library(tibble)
library(tm)
library(quanteda)




bandas_grunge <- c('https://www.letras.com/nirvana/',
                 'https://www.letras.com/pearl-jam/',
                 'https://www.letras.com/soundgarden/',
                 'https://www.letras.com/alice-in-chains/',
                 'https://www.letras.com/stone-temple-pilots/',
                 'https://www.letras.com/mudhoney/',
                 'https://www.letras.com/screaming-trees/')

# dr ----------------------------------------------------------------------



nombres_bandas_grunge<- gsub("https://www.letras.com/|[/\"']|","",bandas_grunge)

scrape_lyrics_grunge <- function(url) {

  webpage <- read_html(url)
  letra <- webpage %>% 
    html_node(xpath = '//div[@class="cnt-letra"]') %>% 
    html_text2()
  return(letra)
}

bandas_canciones_grunge <- map_dfr(bandas_grunge, function(banda) {
  pagina <- read_html(banda)
  canciones <- pagina %>%
    html_element(xpath = '//div[@class ="cnt-list--alp"]') %>% 
    html_nodes("[data-shareurl]") %>%
    html_attr("data-shareurl")
  tibble(banda = gsub('https://www.letras.com/|/', '', banda), #obtener solo el nombre de la banda
         letra = map(canciones, scrape_lyrics_grunge))
})

head(bandas_canciones_grunge)
original_bandas_grunge<-bandas_canciones_grunge


bandasfactor_grunge<-as.factor(bandas_canciones_grunge$banda)
summary(bandasfactor_grunge)#numero de canciones por banda
class(bandas_canciones_grunge)

bandas_canciones_grunge<- as.data.frame(bandas_canciones_grunge)
solo_letras_grunge <- subset(bandas_canciones_grunge, select = letra)

solo_letras_grunge <-trimws(solo_letras_grunge)
solo_letras_grunge<-tolower(solo_letras_grunge)
solo_letras_grunge <- gsub("\\n\\n\\n|\\n\\n|\\n|\\\\n", " ", solo_letras_grunge)
solo_letras_grunge <- gsub("          ", " ", solo_letras_grunge)
solo_letras_grunge <- gsub("   ", " ", solo_letras_grunge)
solo_letras_grunge <- gsub("  ", " ", solo_letras_grunge)
solo_letras_grunge <- gsub("[^[:alnum:]]", " ", solo_letras_grunge)

########################################################################################
#_______________________________PROCESO DE TOKENIZACION Y STOPWORDS (((1)))_________________-#
#################################UTILIZANDO LIBRERIA TM#####################






########################################################################################
#_______________________________PROCESO DE TOKENIZACION Y STOPWORDS (((2)))_________________-#
################################UTILIZANDO LIBRERIA QUANTAS#############################
# Convertir el vector en un data frame con una columna llamada "letra"
solo_letras_grunge <- data.frame(letra = solo_letras_grunge)

tokens_df <- tokens(solo_letras_grunge$letra)

dfm_df <- tokens_df %>%
  dfm()

summary(dfm_df)

dim(dfm_df)

featnames(dfm_df)

dfm_df <- dfm_df %>%
  dfm_remove(stopwords("spanish"))

# Obtener los términos más frecuentes
top_terms <- dfm_df %>%
  topfeatures(n = 100)

top_terms <- as.data.frame(top_terms)


# Graficar los términos más frecuentes
barplot(top_terms, horiz = TRUE, las = 1, main = "Términos más frecuentes")