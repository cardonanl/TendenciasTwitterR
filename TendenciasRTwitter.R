library(twitteR)
library(tm)
library(wordcloud)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rtweet)
library(wordcloud2)
library(qdapRegex)


################
##Con Rtwitter##
################

appname <- "XXXX"

api_key <- "XXXXX"

api_secret <- "XXXXXX"

access_token <- "XXXXXX"

access_token_secret <- "XXXXX"

setup_twitter_oauth(api_key1,api_secret1,access_token1,access_token_secret1)


twitter_token <- create_token(
  app = appname,
  consumer_key = api_key1,
  consumer_secret = api_secret1,
  access_token = access_token1, 
  access_secret = access_token_secret1)


############ Busqueda y visualizacion

tweets <- search_tweets(q = "#GradoDeInversion", n = 10000, include_rts = T, token = twitter_token)

#Para establecer una fecha de inicio añadir el siguiente argumento
since <- '2021-06-01'


#### Serie de tiempo con el número de interacciones
#Interacciones = Tweets + RTs + Respuestas + etc (para limitar lo incluído en interacciones ver argumentos como include_rts)
#Notese que para usar ts_plot el dataframe es considerado una serie de tiempo. Este puede ser modelado.

ts_plot(tweets, "hours", lwd = 1, color = "gold3") + theme_bw() + labs(title = "Flujo de tweets con #GradoDeInversion", y = "Número de tweets y RTs (Aprox)", subtitle = "@Cardonanl")


#Tweet con más RTs
granrt <- tweets %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(created_at, screen_name, text, retweet_count)

View(granrt)

#Top N de las cuentas con mayor cantidad de tweets escritos
procedencia1 <- tweets %>%
  filter(screen_name != "", !is.na(screen_name)) %>% 
  count(screen_name) %>% 
  top_n(screen_name, n = 10) %>% 
  ggplot() +
  geom_col(aes(x = reorder(screen_name, n), y = n), fill = "gold3") + 
  coord_flip() +
  labs(title = "Procedencia de tweets #GradoDeInversion",
       x = "Usuario",
       y = "Cantidad", subtitle = "@CardonaNL") + theme_bw()

#Cantidad de tweets por días de la semana
dias1 <- table(weekdays(tweets$created_at))
xy <- as.data.frame(dias1)
xy %>%
  arrange(Freq) %>%
  mutate(Var1 = factor(Var1, levels=c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))) %>%
  ggplot() + 
  geom_col(aes(x = Freq, y = Var1), fill = "gold3") + theme_bw() + labs(title = "Flujo de tweets con mención #GradoDeInversion", x = "Día de la semana", y = "Número de tweets y RTs (Aprox)", subtitle = "@CardonaNl")


#Crear un CVS para descargar (acá se puede ver fácil cuál fue la primera cuenta, etc)
write_as_csv(tweets, "gradodeinversion.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(tweets, "gradodeinversion.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


#WordCloud y palabras mas usadas
text <- str_c(tweets3$text, collapse = "")
text <- 
  text %>%
  str_remove("\\n") %>%
  rm_twitter_url() %>%
  rm_url() %>%
  str_remove_all("#\\S+") %>%
  str_remove_all("@\\S+") %>%
  removeWords(stopwords("english")) %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp"))

textCorpus <- 
  Corpus(VectorSource(text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()
textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)

textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)
wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud

#La siguiente linea es por si la tendencia es demasiado grande. Hace un subdf con las palabras más repetidas
subdatab <- subset(textCorpus, freq >= 10) %>% slice_max(subdatab, n = 10)

ggplot(subdatab, aes(word, freq)) + geom_bar(stat="identity", fill="gold3") + 
  xlab("Palabras") + ylab("Frecuencia") + ggtitle("Palabras más usadas para @DRC_NGO", subtitle = "Kuja Kuja") + theme_bw()



################
##Con TwitterR##
################
#Este paquete dejaron de actualizarlo hace un tiempo pero es más amigable con algunas cosas

#Busqueda con twitterR

xxx <- searchTwitter("@xxx", n = 150000)

xxxx <- searchTwitter("#xxx", n = 15000)

#data frame
txtbl <- str_replace_all(roy$text,"[^[:graph:]]", "")
txtbl <- str_replace_all(roy$text,"[^[:graph:]]", " ") 

#Para Worlcloud y otras gráficas
#limpieza de texto
txtcleanbl <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txtbl) %>% gsub("@\\w+", "", txtcleanbl) %>% gsub("[[:punct:]]", "", txtcleanbl) %>% gsub("[[:digit:]]", "", txtcleanbl)
%>% gsub("http\\w+", "", txtcleanbl) %>% gsub("...", "", txtcleanbl) %>% gsub("í", "", txtcleanbl)

corpusbl <- Corpus(VectorSource(txtcleanbl))

corpusbl <- tm_map(corpusbl, tolower) %>% tm_map(corpusbl, removeWords, c(stopwords("es"))) %>% tm_map(corpusbl, stripWhitespace)

#matriz

tdmbl <- TermDocumentMatrix(corpusbl)

mbl <- as.matrix(tdmbl)

blconteo <- sort(rowSums(mbl), decreasing = T)

#Solo contar algunas palabras/términos

blconteo <- subset(blconteo, blconteo >= 1)
blconteo <- subset(blconteo, blconteo <= 200)

blfrecuencia <- data.frame(word = names(blconteo), freq=blconteo)

subdata <- subset(blfrecuencia)

subdatab <- subset(blfrecuencia, freq > 20)

subdatabb <- subset(subdatab)
subdatabb <- subdatabb[-c(2),] 

#visualizaciones

wordcloud(subdatabb$word, subdatabb$freq, random.order = F, colors = brewer.pal(5, "Dark2"), main = "Worlsaddsf")

library(wordcloud2)

wordcloud2(data=subdata, size = 0.7, shape = 'pentagon')

ggplot(blfrecuencia, aes(word, freq, fill=freq)) + geom_bar(stat="identity") + 
  xlab("Palabras") + ylab("Frecuencia") + ggtitle("Palabras más usadas en xxx") + theme_bw()

