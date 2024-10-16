# Limpieza de memoria y variables
rm(list = ls(all.names = TRUE))
gc()

# ====================
# =    PAQUETERIAS   =
# ====================

library(syuzhet) # Analisis de sentimientos
library(RColorBrewer) # Graficas (paleta de colores)
library(viridis) # Paleta de colores
library(readxl)
library(tm) # Analisis de sentimientos
library(SentimentAnalysis) # Analisis de sentimientos
library(tidytext)
library(dplyr)
library(wordcloud)
library(readxl)
library(ggplot2) # graficas
library(stringr) # Limpieza de columnas
library(udpipe) # Lematizar las palabras
library(textTinyR) # Permite analisis con lexicos propios

# =======================
# = PROCESAMIENTO DATOS =
# =======================

# Especificamos donde estan nuestros datos
setwd("C:/Users/eduar/OneDrive/Escritorio/ANALISISOSCIEL")

# Lectura de datos
datos <- read.csv('twitter.csv')

# Proceso de limpieza de los datos

# Eliminamos el exceso de columnas
datos_limpios <- datos[, !names(datos) %in% c("T5","T6","T7","T8",
                                              "T9","T10","X","X.1",
                                              "X.2","X.3","X.4","X.5",
                                              "X.6")]

# Eliminamos todas las filas que estan totalmente en blanco
datos_limpios <- datos_limpios[datos_limpios$ID != "" 
                               & datos_limpios$USERNAME != "", ]

# Concatenamos las columnas que tienen la publicacion separada
datos_limpios$TWEET <- paste(datos_limpios$T1,
                             datos_limpios$T2,
                             datos_limpios$T3,
                             datos_limpios$T4)

# Eliminamos las columnas que tenian la publicacion separada
datos_limpios <- datos_limpios[, !colnames(datos_limpios)
                               %in% c("T1", "T2", "T3", "T4")]

# Converimos a fecha la columna DATE
datos_limpios$DATE <- as.POSIXct(datos_limpios$DATE,
                                 format = "%Y-%m-%d-%H:%M:%S")
# Nos quedamos con los datos que solo tengan una fecha
datos_limpios <- subset(datos_limpios, ! is.na(datos_limpios$DATE))

# ===================================
# =    Primer limpieza de datos     =
# ===================================

# Función para limpiar los tweets
limpiar_tweets <- function(tweet) {
  tweet <- iconv(tweet, from = "UTF-8", to = "UTF-8")  # Corregir potenciales problemas de codificación
  tweet <- str_remove_all(tweet, "https?://\\S+|www\\.\\S+")  # Eliminar URLs
  tweet <- str_remove_all(tweet, "#\\S+")  # Eliminar hashtags
  tweet <- str_remove_all(tweet, "@\\w+")  # Eliminar menciones
  tweet <- str_remove_all(tweet, "[^\\w\\s]+")  # Eliminar caracteres especiales
  tweet <- str_remove_all(tweet, "\\d+")  # Eliminar números
  tweet <- tolower(tweet)  # Convertir a minúsculas
  tweet <- str_squish(tweet)  # Eliminar espacios extra
  tweet <- str_replace_all(tweet, pattern = "([a-z])\\1{2,}", "\\1")  # Reducir duplicidad de caracteres
  tweet <- str_remove_all(tweet, "\\s(covid|covid19|covid-19)\\s")  # Eliminar palabras relacionadas con covid
  tweet <- iconv(tweet, to = 'ASCII//TRANSLIT')  # Quitar tildes
  return(tweet)
}

# Aplicar la función de limpieza
datos_limpios$TWEET <- sapply(datos_limpios$TWEET, limpiar_tweets)

# Eliminamos las filas que ya no tienen palabras que analizar
datos_limpios <- datos_limpios[datos_limpios$TWEET != "" , ]


# ==============
# = Stopwords  =
# ==============

# Lista de stopwords del paquete tm
stop_words_es_1 <- stopwords("spanish")
# Lista de stopwrods del equipo
stop_words_es_2 <- c("de", "la", "que", "el", "en", "y", "a", "los", "se", "del", "las",
                   "un", "por", "con", "no", "una", "su", "para", "es", "al", "lo", 
                   "como", "más","pero", "sus", "le", "ya", "o", "este", "sí", "porque",
                   "esta", "entre", "cuando", "muy", "sin", "sobre", "también", "me", 
                   "hasta", "hay", "donde", "quien", "desde", "todo", "nos", "durante", 
                   "todos", "uno", "les", "ni", "contra", "otros", "ese", "eso", "ante",
                   "ellos", "e", "esto", "mí", "antes", "algunos", "qué", "unos", "yo", 
                   "otro", "otras", "otra", "él", "tanto", "esa", "estos", "mucho", 
                   "quienes", "nada", "muchos", "cual", "poco", "ella", "estar", 
                   "estas", "algunas", "algo", "nosotros", "mi", "mis", "tú", "te", 
                   "ti", "tu", "tus", "ellas", "nosotras", "vosotros", "vosotras", "os",
                   "mío", "mía", "míos", "mías", "tuyo", "tuya", "tuyos", "tuyas",
                   "suyo", "suya", "suyos", "suyas", "nuestro", "nuestra", "nuestros", 
                   "nuestras", "vuestro", "vuestra", "vuestros", "vuestras", "esos", 
                   "esas", "estoy", "estás", "está", "estamos", "estáis", "están", 
                   "t.co", "https", "gracias", "cómo", "sabemos", "hacerlo", "hoy", "tiene", "ha", 
                   "han", "si", "son", "días", "ser", "día", "venezuelapatrialibre",
                   "añojudicial2021", "2", "50", "manaos", "mientras", "𝐄𝐒", "cuba",
                   "lea", "notimippci", "horas", "semana", "nueva", "después", "primer",
                   "personal", "ahora", "20", "centro", "2", "casi", "1", "5", "años",
                   "24", "reino", "unido", "8", "viernes", "solo", "trabajadores",
                   "ieopaupkn7", "toda", "todas", "hace", "compra", "así", "mañana", 
                   "nuevo", "ministro", "nuevas", "empresas", "presentan", "cada", 
                   "argentina", "segunda", "mismo", "pone", "da", "estados", "000",
                   "familiares", "parte", "puede", "casa", "comprar", "10", "q", 
                   "null", "n","recibir", "tener", "fue", "chile", "hecho", "madrid",
                   "3", "dijo", "dice", "fútbol", "tras", "través", "pasa", "quedan",
                   "brasil", "01", "dar", "203", "novm7xhq5q", "cien", "partir", "11", "30",
                   "elesequiboesdevenezuela", "miembros", "hora", "tsj", "pdte", "según",
                   "era", "uruguay", "puedan", "cualquier", "podría", "somos", "video", 
                   "venezuela", "mil", "personas", "T O M A", "𝐏𝐑𝐄𝐂𝐀𝐔𝐂𝐈𝐎́𝐍", 
                   "22", "país", "colombia", "mundo", "es")
# Unimos los dos vectores
stop_words <- union(stop_words_es_1, stop_words_es_2)

# =============================
# = EN BUSCA DE MÁS STOPWORDS =
# =============================

# Desanidamos el texto en tokens
tokens <- datos_limpios %>%
  unnest_tokens(TOKENS, TWEET)

# Filtramos las palabras, excluyendo stop words
datos_sin_stopwords <- tokens %>%
  filter(!TOKENS %in% stop_words)

# Hacemos el conteo de la ocurrencia de cada palabra
palabras_frecuentes <- datos_sin_stopwords %>%
  count(TOKENS, sort = TRUE)

# Creación de un vector adicional para eliminar mas stopwords 
stop_words_es_3 <- c("un","covid","19","coronavirus","covid19","sars",
                     "100","cov","min","covid_19","topbqxf65a","dr","ee.uu",
                     "r422zkupwh","2021","d","erik","dos","nelly","va","ola",
                     "nqsxqasxdi","uat3gkhiiu","csic","21","4","2020", "español",
                     "españa","gabrielasjr","vmp0zajvme","oms","sino","cubana",
                     "paris","2º","cpc","3yuggmwqvj","Covid-19","sarscov")

# Hacemos más grande nuestro vector de stop_words
stop_words <- union(stop_words, stop_words_es_3)

# Volvemos a filtrar las palabras sin los nuevos stop_words
datos_sin_stopwords <- tokens %>%
  filter(!TOKENS %in% stop_words)

# Volvemos a hacer el conteo sin los nuevos stop_words
palabras_frecuentes <- datos_sin_stopwords %>%
  count(TOKENS, sort = TRUE)

# ==================================================
# = Analisis de sentimientos con SentimentAnalysis =
# ==================================================

# A los datos limpios le eliminamos las stopwords propias
datos_limpios <- datos_limpios %>%
  mutate(TWEET_CLEAN = str_replace_all(TWEET, paste0("\\b(", paste(stop_words, collapse = "|"), ")\\b"), ""))

# Guardamos los datos limpios
#write.csv(datos_limpios, "datos_limpios.csv")


# Crear un corpus de texto con el paquete "tm"
corpus <- Corpus(VectorSource(datos_limpios$TWEET_CLEAN))

# Preprocesamiento del corpus (mas limpieza de datos)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish")) # Mas limpieza de stopwords

# Extraemos los tokens
datos_procesados <- data.frame(TOKENS = sapply(corpus, as.character))

# Calcular la polaridad usando SentimentAnalysis
polaridad <- analyzeSentiment(corpus, language = "spanish")

# Generamos otros datos
datos2 <- data.frame(TWEET_CLEAN = datos_limpios$TWEET_CLEAN)

# Juntamos los datos
datos2 <- data.frame(datos2,datos_procesados) 

# Agregamos el numero de palabras que tiene cada tweet sin stopwords
datos2$No_Palabras <- polaridad$WordCount

# Agregamos la polaridad para determinar si es un sentimiento positivo, negativo o neutral
datos2$Polaridad <- polaridad$SentimentGI

# Agregamos los valores negativos
datos2$Negativo <- polaridad$NegativityGI

# Agregamos los valores positivos
datos2$Positivo <- polaridad$PositivityGI

# Si la polaridad es igual a cero, entonces sera un sentimiento neutral
datos2$Neutral <- as.integer(datos2[,5] == 0 & datos2[,6] == 0)
# Cambiamos el tipo de numero
datos2$Negativo <- as.integer(datos2[,5] != 0)
datos2$Positivo <- as.integer(datos2[,6] != 0)

# Guardamos los datos con el primer analisis
#write.csv(datos2, "analisis1.csv")
summary(datos2)  # Ver los resultados de interés

# ========================================
# = Analisis de sentimientos con syuzhet =
# ========================================

# Con el diccionario NRC obtenemos un analisis de sentimientos
sentimientos_df <- get_nrc_sentiment(datos_limpios$TWEET_CLEAN, lang="spanish")

# Creamos un dataframe para exportar
datos3 <- data.frame(TWEET_CLEAN = datos_limpios$TWEET_CLEAN)
datos3 <- data.frame(datos3,sentimientos_df)

# Calculamos la polaridad 
datos3$Polarity <- datos3$positive - datos3$negative
# Guardamos los datos con el segundo analisis
#write.csv(datos3, "analisis2.csv")
#datos3 <- read.csv('analisis2.csv', fileEncoding = "UTF-8")
summary(datos3) # Estadisticas de interes


# ============================================================
# =   Analisis con tidytext y el diccionario proporcionado   =
# ============================================================

# Carga el lexico
diccionario <- read.csv("C:/Users/eduar/OneDrive/Escritorio/ANALISISOSCIEL/Lexicos/Diccionario_copalab.csv")
diccionario$Word <- NULL

# Cargamos los datos
# datos_limpios <- read.csv('datos_limpios.csv', fileEncoding = "UTF-8")
datos_limpios$ID <- seq_along(datos_limpios$ID)

# Separar en tokens
tweets_unnested <- datos_limpios %>%
  unnest_tokens(word, TWEET_CLEAN) %>%
  inner_join(diccionario, by = c("word" = "Palabra"))

# Calculando la polaridad de cada tweet
tweets_polarity <- tweets_unnested %>%
  group_by(ID) %>%
  summarise(polarity = sum(Puntuación))

# Agregando la columna de polaridad al DataFrame original
datos_limpios <- left_join(datos_limpios, tweets_polarity, by = "ID")
# Datos con NA = 0, es decir, tendrán una polaridad neutral
datos_limpios$polarity[is.na(datos_limpios$polarity)] <- 0


# Guardamos los datos con el analisis con el diccionario del profesor
#write.csv(datos_limpios, "analisis4.csv")
#datos_limpios <- read.csv('analisis4.csv', fileEncoding = "UTF-8")

# Estadisticas. El diccionario se ajusta bien a esto ?
summary(datos_limpios$polarity)

# Dataframe con el conteo de positivos, negativos y neutros
analisis4_conteo <- data.frame("Positivos" = sum(datos_limpios$polarity > 0),
                               "Negativos" = sum(datos_limpios$polarity < 0),
                               "Neutral" = sum(datos_limpios$polarity == 0)) # Son muchos


# ============================================================
# =   Analisis con tidytext y un diccionario personalizado   =
# ============================================================

# Carga el lexico
# datos_limpios <- read.csv('datos_limpios.csv', fileEncoding = "UTF-8")
diccionario <- read_excel("C:/Users/eduar/OneDrive/Escritorio/ANALISISOSCIEL/Lexicos/senticon_limpio.xlsx")

# Cargamos los datos
datos_limpios$ID <- seq_along(datos_limpios$ID)

# Separar en tokens
tweets_unnested <- datos_limpios %>%
  unnest_tokens(word, TWEET_CLEAN) %>%
  inner_join(diccionario, by = c("word" = "Palabra"))

# Calculando la polaridad de cada tweet
tweets_polarity <- tweets_unnested %>%
  group_by(ID) %>%
  summarise(polarity = sum(Polaridad))

# Agregando la columna de polaridad al DataFrame original
datos_limpios <- left_join(datos_limpios, tweets_polarity, by = "ID")
# Datos con NA = 0, es decir, tendrán una polaridad neutral
datos_limpios$polarity[is.na(datos_limpios$polarity)] <- 0

# Guardamos los datos con el analisis con el Senticon
#write.csv(datos_limpios, "analisis5.csv")
#datos_limpios <- read.csv('analisis5.csv', fileEncoding = "UTF-8")

# Estadisticas. El diccionario se ajusta bien a esto ?
summary(datos_limpios$polarity) # min -2, max 8 pesa a los valores del lexicon

# Dataframe con el conteo de positivos, negativos y neutros (BUENA RELACION)
analisis5_conteo <- data.frame("Positivos" = sum(datos_limpios$polarity > 0),
                               "Negativos" = sum(datos_limpios$polarity < 0),
                               "Neutral" = sum(datos_limpios$polarity == 0))

# Calcular los valores de las barras
valores <- colSums(analisis5_conteo, na.rm = TRUE)

# Convertir los valores en un dataframe
d <- data.frame(Sentimientos = names(valores), Valores = valores)

# gráfico de barras con ggplot
ggplot(d, aes(x = Sentimientos, y = Valores)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = 3, name = "Set2")) +
  labs(
    title = "Análisis de sentimientos en tweets sobre covid/Ciencia/Tecnologia 21-28 enero",
    subtitle = "Léxico ML-SentiCon",
    x = "Sentimientos",
    y = NULL
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Valores), vjust = -0.5, size = 3.3)


# ===================
# =     GRAFICAS    =
# ===================

# = Graf 1 (FRECUENCIA DE FECHAS)
datos_limpios <- read.csv('datos_limpios.csv', fileEncoding = "UTF-8")
# Contar la frecuencia de ocurrencia de cada fecha
frecuencia_fechas <- table(as.Date(datos_limpios$DATE))

# Graficar las fechas en un gráfico de barras
d <- data.frame(Fechas = names(frecuencia_fechas), Frecuencia = frecuencia_fechas)

ggplot(d, aes(x = Fechas, y = Frecuencia.Freq)) +
  geom_bar(stat = "identity", fill = viridis_pal()(8)) +
  labs(
    title = "Frecuencia de Fechas",
    x = "Fechas",
    y = "Frecuencia"
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Frecuencia.Freq), vjust = -0.5, size = 3.3)

# = Graf 2 (20 PALABRAS MÁS REPETIDAS)
# Graficamos las 20 palabras más frecuentes encontradas (palabras_frecuentes.csv)
ggplot(palabras_frecuentes[1:20,], aes(x = reorder(TOKENS, n), y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Palabras", y = "Frecuencia", title = "Las 20 Palabras más Frecuentes") +
  scale_fill_gradient(low = "#B4D4DA", high = "#26456E") +
  theme_light() +
  theme(legend.position = "none")

# = Graf 3 (SentimentAnalysis)
#datos2 <- read.csv('analisis1.csv', fileEncoding = "UTF-8") # Importamos datos correspondientes

# grafica correspondiente a SentimentAnalysis
valores <- colSums(datos2[, 6:8], na.rm = TRUE)

# Convertir los valores en un dataframe
d <- data.frame(Sentimientos = names(valores), Valor = valores)

# Crear el gráfico de barras con ggplot
ggplot(d, aes(x = Sentimientos, y = Valor, fill = Sentimientos)) +
  geom_bar(stat = "identity", width = 0.5, fill = c('#F7F377','#F5A4AB','#7FCDF5')) +
  labs(
    title = "Análisis de sentimientos en tweets sobre covid/Ciencia/Tecnologia 21-28 ENERO",
    subtitle = "Léxico SentimentAnalysis",
    x = "Sentimientos",
    y = NULL
  ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Valor), vjust = -0.5, size = 3.3)

# = Graf 4 (syuzhet)
# Grafica correspondiente al segundo analisis (datos3.csv)
barplot(
  colSums(prop.table(sentimientos_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 2,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Análisis de emociones en tweets sobre covid/Ciencia/Tecnologia 21-28 ENERO \n Léxico syuzhet",
  xlab="EMOCIONES", ylab = NULL)

# ============================
# =     DATOS DE UTILIDAD    =
# ============================

datos_tiempo <- data.frame(datos_limpios,datos_procesados)
# Ordenamos respecto a las fechas (DATE)
datos_ordenados <- datos_tiempo[order(datos_tiempo$DATE), ]

# =================================
# Sentimientos indexados al tiempo
# =================================


# Volvemos a hacer un calculo de emociones pero cronologico
sentimientos_ordenados <- get_nrc_sentiment(datos_ordenados$TOKENS, lang="spanish")
datos4 <- data.frame(datos_ordenados,sentimientos_ordenados)

# guardamos los datos
#write.csv(datos4, "analisis3.csv")
#datos4 <- read.csv('analisis3.csv')

# Calculo de la valencia en los datos ordenados por fechas
sentimientos_valencia_or <- (datos4$negative *-1) + datos4$positive

simple_plot(sentimientos_valencia_or)
