# Packages
library(readr)
library(tm)
library(topicmodels)
library(wordcloud)
library(stringr)
library(tidyr)
library(ggplot2)
library(dplyr)

Sys.setlocale("LC_ALL", "pt_PT.UTF-8")
# Load texts 
data_latercera <- read_csv("data_latercera.csv",locale = locale("es",encoding = "UTF8"))

# fecha
patt= "([0-9]{2})(.+de)(.+)(del.+)([0-9]{4})|([0-9]{2})[/]([0-9]{2})[/]([0-9]{4})"
meses=c("enero","febrero","marzo","abril",
        "mayo","junio","julio","agosto",
        "septiembre","octubre","noviembre","diciembre")
parse_col=str_match(data_latercera$fecha, patt)
dia=ifelse(is.na(parse_col[,2]),parse_col[,7],parse_col[,2])
mes=ifelse(is.na(parse_col[,8]),str_sub(paste0("0",match(trim(parse_col[,4]),meses)),-2),
                                        parse_col[,8])
anio=ifelse(is.na(parse_col[,6]),parse_col[,9],parse_col[,6])
fecha=as.Date(paste0(anio,mes,dia), "%Y%m%d")

# Transform the text to corpus
m=list(id = "date", content = "txt")
myReader=readTabular(mapping = m)
news=Corpus(DataframeSource(data.frame(date=fecha,txt=data_latercera$contenido)), 
                                readerControl = list(reader = myReader))

# Checking
writeLines(as.character(news[[10]]))

## Some preprocessing transformations
# To lower case
news=tm_map(news,content_transformer(tolower))
# comillas
#news=tm_map(news,chartr,old='“',new=' ')
#news=tm_map(news,chartr,old='”',new=' ')
# Discard punctuation
news=tm_map(news,removePunctuation,preserve_intra_word_dashes = TRUE)
# The numbers
news = tm_map(news, removeNumbers)
# The whitespace
news=tm_map(news, stripWhitespace)
# Personal words
#pers_words=c('educación','más', 'también','además')
#news=tm_map(news, removeWords, pers_words)
# the stopwords
news=tm_map(news, removeWords, stopwords("spanish"))
# Step needed before stemming
news = tm_map(news, PlainTextDocument)
# Stemming
#news=tm_map(news, stemDocument, "spanish")

# Checking
writeLines(as.character(news[[10]]))

# frequency
dark2 = brewer.pal(6, "Dark2")   
wordcloud(news,  max.words=100, rot.per=0.2, colors=dark2, random.order = FALSE)

## Now that we have processed the texts we can create the document term matrix
dtmatrix=DocumentTermMatrix(news)

## We may have rows with empty terms, so we delete them
total_row= apply(dtmatrix , 1, sum) 
dtmatrix=dtmatrix[total_row> 0, ]   #remove all docs without words

##### Topic modeling
# Setting parameters for Gibbs sampling
k= 20 # Number of topics
nstart= 3 # Number of repeated random starts. 
seed= 2^(5:(nstart+5-1)) # seed
best= TRUE # if TRUE only the model with the maximum (posterior) likelihood is returned
iter= 1000 # number of Gibbs iterations, by default equals 2000
thin= 1000 # number of omitted in-between Gibbs iterations, by default equals iter.
burnin=1000 #  number of omitted Gibbs iterations at beginning, by default equals 0

# LDA using Gibbs sampling
time=proc.time()
results=LDA(dtmatrix,k, method="Gibbs", 
            control=list(seed = seed,nstart=nstart, 
                         best=best, 
                         iter = iter, 
                         thin=thin,
                         burnin = burnin))
proc.time()-time

# Visualize the first 5 terms in the first 10 topics
fecha2=fecha2[which(total_row>0)]
terms(results,5)

data.frame(fecha=fecha2,results@gamma[,c(1,3,4,5,20)]) %>% 
  group_by(fecha) %>% 
  summarise(proyecto=mean(X1),edu_chilena=mean(X2),movimiento=mean(X3), gratuidad=mean(X4),
            lucro=mean(X5)) %>% 
  gather(topic,probability,-fecha) %>% 
  filter(!is.na(fecha)) %>% 
  ggplot(aes(x=fecha,y=probability,group=topic,color=topic))+geom_point(size=0.1)+
  geom_smooth(method = 'loess',span = 0.2,se = FALSE)+xlab("date") +
  scale_y_continuous(labels = scales::percent,limits=c(0,.2))+
  scale_x_date(date_breaks = "4 months")+theme(axis.text.x=element_text(angle=-45, hjust=0.001))

