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

appname <- "RTWEET"

api_key <- "XXXXX"

api_secret <- "XXXXXX"

access_token <- "XXXXXX"

access_token_secret <- "XXXXX"


appname <- "XXXX"


setup_twitter_oauth(api_key1,api_secret1,access_token1,access_token_secret1)


twitter_token1 <- create_token(
  app = appname,
  consumer_key = api_key1,
  consumer_secret = api_secret1,
  access_token = access_token1, 
  access_secret = access_token_secret1)


##################### Busqueda y visualizacion con Rtwitter

since <- '2021-06-01'

tweets <- search_tweets(q = "#GradoDeInversion", n = 10000, include_rts = T, token = twitter_token)

tweets2 <- search_tweets(q = "@ACHColombia", n = 10000, include_rts = T, token = twitter_token)

tweets3 <- search_tweets(q = "@DRC_ngo", n = 10000, include_rts = T, token = twitter_token)

tweets4 <- search_tweets(q = "@NRC_LAC", n = 10000, include_rts = T, token = twitter_token1)

tweets5 <- search_tweets(q = "#PicoYGenero", n = 10000, include_rts = T, token = twitter_token1, retryonratelimit = T)

str(tweets)

count(tweets3)

?search_tweets2

ts_plot(tweets, "hours", lwd = 1, color = "gold3") + theme_bw() + labs(title = "Flujo de tweets con #GradoDeInversion", y = "Número de tweets y RTs (Aprox)", subtitle = "@Cardonanl")

granrt <- tweets %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(created_at, screen_name, text, retweet_count)

View(granrt)


?ts_plot

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


dias1 <- table(weekdays(tweets$created_at))
xy <- as.data.frame(dias1)
xy %>%
  arrange(Freq) %>%
  mutate(Var1 = factor(Var1, levels=c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))) %>%
  ggplot() + 
  geom_col(aes(x = Freq, y = Var1), fill = "gold3") + theme_bw() + labs(title = "Flujo de tweets con mención #GradoDeInversion", x = "Día de la semana", y = "Número de tweets y RTs (Aprox)", subtitle = "@CardonaNl")



write_as_csv(tweets, "gradodeinversion.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

save_as_csv(tweets, "gradodeinversion.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

str(tweets)


###WordCloud y palabras mas usadas

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


subdatab <- subset(textCorpus, freq >= 10) %>% slice_max(subdatab, n = 10)
str(subdata)

?top_n

ggplot(subdatab, aes(word, freq)) + geom_bar(stat="identity", fill="gold3") + 
  xlab("Palabras") + ylab("Frecuencia") + ggtitle("Palabras más usadas para @DRC_NGO", subtitle = "Kuja Kuja") + theme_bw()




#########################################
#########Conversiones con TwitterR
######

#####################Busqueda con twitterR

roy <- searchTwitter("@NRC_LAC", n = 150000)

ospina <- searchTwitter("@NRC_LAC", n = 15000)



##data frame


txtbl <- str_replace_all(roy$text,"[^[:graph:]]", "")
txtbl <- str_replace_all(roy$text,"[^[:graph:]]", " ") 

##### inicio limpieza de datos #####
# remueve retweets
txtcleanbl <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txtbl)
# remove @otragente
txtcleanbl <- gsub("@\\w+", "", txtcleanbl)
# remueve simbolos de puntuaci?n
txtcleanbl <- gsub("[[:punct:]]", "", txtcleanbl)
# remove n?meros
txtcleanbl <- gsub("[[:digit:]]", "", txtcleanbl)
# remueve links
txtcleanbl <- gsub("http\\w+", "", txtcleanbl)
txtcleanbl <- gsub("...", "", txtcleanbl)
txtcleanbl <- gsub("í", "", txtcleanbl)

?gsub

##### fin limpieza de datos #####



corpusbl <- Corpus(VectorSource(txtcleanbl))


# convierte a min?sculas
corpusbl <- tm_map(corpusbl, tolower)
corpusbl <- tm_map(corpusbl, removeWords, c(stopwords("es")))
corpusbl <- tm_map(corpusbl, stripWhitespace)


########matriz

tdmbl <- TermDocumentMatrix(corpusbl)

mbl <- as.matrix(tdmbl)

blconteo <- sort(rowSums(mbl), decreasing = T)

###########acotar la lista para la gráfica##########

blconteo <- subset(blconteo, blconteo >= 1)
blconteo <- subset(blconteo, blconteo <= 200)

blfrecuencia <- data.frame(word = names(blconteo), freq=blconteo)

subdata <- subset(blfrecuencia)

subdatab <- subset(blfrecuencia, freq > 20)

subdatabb <- subset(subdatab)
subdatabb <- subdatabb[-c(2),] 

###############visualizacion

wordcloud(subdatabb$word, subdatabb$freq, random.order = F, colors = brewer.pal(5, "Dark2"), main = "Worlsaddsf")
?wordcloud

?brewer.pal

library(wordcloud2)

wordcloud2(data=subdata, size = 0.7, shape = 'pentagon')

ggplot(blfrecuencia, aes(word, freq, fill=freq)) + geom_bar(stat="identity") + 
  xlab("Palabras") + ylab("Frecuencia") + ggtitle("Palabras más usadas en coyuntura Edificio Luz Marina") + theme_bw()

