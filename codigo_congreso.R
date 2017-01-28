# Packages
library(tidyverse)
library(tm)
library(topicmodels)
library(wordcloud)
library(stringr)
library(vhmo)

Sys.setlocale("LC_ALL", "es_ES.UTF-8")
# Load texts 
data_emol <- read_delim("data_emol3.csv", "\t", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                         na = "NA", trim_ws = TRUE)

data_cnn <- read_delim("data_cnn.csv", "\t", escape_double = FALSE, locale = locale(encoding = "UTF-8"), 
                        na = "NA", trim_ws = TRUE)

data_emol=data_emol %>% na.omit()                   
data_cnn=data_cnn %>% na.omit()                   

data_limpia_emol=data_emol %>% 
  mutate(ciudad=substr(str_match(contenido,"\\w+.-"),1,3)) %>% 
  filter(ciudad %in% c("SAN","VAL",NA,"val","san"))

# Eliminar contenido vacio
data_limpia_emol=data_limpia_emol %>% 
  mutate(enc=Encoding(contenido)) %>% 
  filter(contenido!="" & enc!="unknown")

data_limpia_cnn=data_cnn %>% 
  mutate(enc=Encoding(contenido)) %>% 
  filter(contenido!="" & enc!="unknown")

# fecha emol
patt= "([0-9]{2})(.+de)(.+)(de.+)([0-9]{4})|([0-9]{2})[/]([0-9]{2})[/]([0-9]{4})"
meses=c("enero","febrero","marzo","abril",
        "mayo","junio","julio","agosto",
        "septiembre","octubre","noviembre","diciembre")
parse_col=str_match(data_limpia_emol$fecha, patt)
dia=parse_col[,2]
mes=str_sub(paste0("0",match(tolower(trim(parse_col[,4])),meses)),-2)
anio=parse_col[,6]
fecha_emol=as.Date(paste0(anio,mes,dia), "%Y%m%d")

# fecha cnn
patt= "([0-9]{2})(.+de)(.+)(,.+)([0-9]{4})|([0-9]{2})[/]([0-9]{2})[/]([0-9]{4})"
meses=c("enero","febrero","marzo","abril",
        "mayo","junio","julio","agosto",
        "septiembre","octubre","noviembre","diciembre")
parse_col=str_match(data_limpia_cnn$fecha, patt)
dia=parse_col[,2]
mes=str_sub(paste0("0",match(tolower(trim(parse_col[,4])),meses)),-2)
anio=parse_col[,6]
fecha_cnn=as.Date(paste0(anio,mes,dia), "%Y%m%d")


contenido_emol=enc2utf8(data_limpia_emol$contenido)
contenido_cnn=enc2utf8(data_limpia_cnn$contenido)

contenido_emol[1]
contenido_cnn[1]
## cleaning the content
contenido_emol=tolower(contenido_emol)
contenido_cnn=tolower(contenido_cnn)
# delete location
contenido_emol = str_replace(contenido_emol,"\\w+.-", " ")
# delete &
contenido_emol = str_replace_all(contenido_emol,"&amp", " ")
contenido_cnn = str_replace_all(contenido_cnn,"&amp", " ")
# delete punctuation
contenido_emol = str_replace_all(contenido_emol,"[[:punct:]]", " ")
contenido_cnn = str_replace_all(contenido_cnn,"[[:punct:]]", " ")
# delete urls
contenido_emol = str_replace_all(contenido_emol,"http\\w+", "")
contenido_cnn = str_replace_all(contenido_cnn,"http\\w+", "")
# delete tabs
contenido_emol = str_replace_all(contenido_emol,"[\t]{2,}", "")
contenido_cnn = str_replace_all(contenido_cnn,"[\t]{2,}", "")
# delete spaces
contenido_emol = str_replace_all(contenido_emol,"^\\s+|\\s+$", "")
contenido_cnn = str_replace_all(contenido_cnn,"^\\s+|\\s+$", "")
# stripWhitespace
contenido_emol=stripWhitespace(contenido_emol)
contenido_cnn=stripWhitespace(contenido_cnn)
# delete numbers
contenido_emol=removeNumbers(contenido_emol)
contenido_cnn=removeNumbers(contenido_cnn)

### prueba 
library(tidytext)
stop_list=c(unlist(lapply(stopwords("es"),as.character)),"dijo","dice")

contenido_emol2= str_replace(contenido_emol,"emolmlt.*|tablaennoticia.*|simbologia mapa.*","")
contenido_emol2= str_replace_all(contenido_emol2,"\\bmás\\b","")

lala=data.frame(num=seq(1,length(contenido_emol2)),
                fecha=fecha_emol,contenido=contenido_emol2, stringsAsFactors = FALSE) %>% 
  unite(num_fecha,num,fecha) %>% 
  unnest_tokens(word,contenido) %>% 
  anti_join(data.frame(word=stop_list,stringsAsFactors=FALSE),by="word") %>%
  count(num_fecha, word, sort = TRUE) %>%
  ungroup()

lala %>% 
  filter(word=="más") %>% 
  distinct(num_fecha)

ejemp=data.frame(num=seq(1,length(contenido_emol2)),
                 fecha=fecha_emol,contenido=contenido_emol2, stringsAsFactors = FALSE) %>% 
  filter(num==562)
ejemp$contenido

### fin prueba
  
  


for(word in stop_list){
  pat=paste0("\\b",word,"\\b")
  contenido_emol=str_replace_all(contenido_emol,pat,"")
  contenido_cnn=str_replace_all(contenido_cnn,pat,"")
}

## Plot news vs time
data.frame(fecha_emol, fuente=rep("emol",length(fecha_emol))) %>%
  ggplot(aes(x=fecha_emol, color=fuente))+geom_smooth(stat = "bin",bins=40, method ="loess")+
  geom_smooth(data=data.frame(fecha_cnn, fuente=rep("cnn",length(fecha_cnn))),
              aes(x=fecha_cnn, color=fuente),
               stat = "bin",bins=40, method ="loess")+
  xlab("fecha")+ylab("número de noticias")+
  ggthemes::theme_tufte()
 
# Transform the text to corpus
news_emol=Corpus(VectorSource(contenido_emol))
news_cnn=Corpus(VectorSource(contenido_cnn))

# Checking
writeLines(as.character(news_emol[[1]]))
writeLines(as.character(news_cnn[[1]]))

#news_emol = tm_map(news_emol, PlainTextDocument)
#news_cnn = tm_map(news_cnn, PlainTextDocument)
# Stemming
#news=tm_map(news, stemDocument, "spanish")

# frequency
news_emol2=data.frame(contenido_emol) %>% 
  cbind(data.frame(fecha_emol)) %>% 
  mutate(year=format(fecha_emol,"%Y")) %>% 
  group_by(year) %>% 
  summarise(text_year=paste(contenido_emol,collapse=" "))

news_emol_wd=Corpus(VectorSource(news_emol2$text_year))

tdm_emol = TermDocumentMatrix(news_emol_wd)
tdm_emol = as.matrix(tdm_emol)
colnames(tdm_emol) = factor(news_emol2$year)

len=length(colnames(tdm_emol) )

png("wordcloud_emol.png", width=9,height=6, units='in', res=300)
comparison.cloud(tdm_emol, colors=colorRampPalette(c("blue", "red"))(len), 
#colors = brewer.pal(len, "Set3"),
                 scale = c(4,.1), random.order = FALSE,max.words=2800,
                 title.size = 1)
dev.off()

# Cnn
news_cnn2=data.frame(contenido_cnn) %>% 
  cbind(data.frame(fecha_cnn)) %>% 
  mutate(year=format(fecha_cnn,"%Y")) %>% 
  group_by(year) %>% 
  summarise(text_year=paste(contenido_cnn,collapse=" "))

news_cnn_wd=Corpus(VectorSource(news_cnn2$text_year))

tdm_cnn = TermDocumentMatrix(news_cnn_wd)
tdm_cnn = as.matrix(tdm_cnn)
colnames(tdm_cnn) = factor(news_cnn2$year)

len=length(colnames(tdm_cnn) )

png("wordcloud_cnn.png", width=9,height=6, units='in', res=300)
comparison.cloud(tdm_cnn, colors=colorRampPalette(c("blue", "red"))(len),#colors = brewer.pal(len, "Set3"),
                 scale = c(4,.1), random.order = FALSE,max.words=1000,
                 title.size = 1)
dev.off()

##### TOPIC MODELs
## Now that we have processed the texts we can create the document term matrix
dtmatrix_emol=DocumentTermMatrix(news_emol)
dtmatrix_cnn=DocumentTermMatrix(news_cnn)

############# PRUEBA
lala=c("hola además","esto está de  más ")

lala=str_replace_all(lala,"\\bmás\\b","")
corpus_lala=Corpus(VectorSource(lala))
dtm_lala=DocumentTermMatrix(corpus_lala,
                            control = list(weighting=function(x) weightTfIdf(x, normalize =FALSE),
                                                       stopwords = FALSE))
inspect(dtm_lala[1:2,])
######################  
  
## We may have rows with empty terms, so we delete them
#total_row= apply(dtmatrix , 1, sum) 
#dtmatrix=dtmatrix[total_row> 0, ]   #remove all docs without words

##### Topic modeling
# Setting parameters for Gibbs sampling
k= 10 # Number of topics
nstart= 3 # Number of repeated random starts. 
seed= 2^(5:(nstart+5-1)) # seed
best= TRUE # if TRUE only the model with the maximum (posterior) likelihood is returned
iter= 1000 # number of Gibbs iterations, by default equals 2000
thin= 1000 # number of omitted in-between Gibbs iterations, by default equals iter.
burnin=1000 #  number of omitted Gibbs iterations at beginning, by default equals 0

# LDA using Gibb sampling
# Emol
time=proc.time()
results_emol=LDA(dtmatrix_emol,k, method="Gibbs", 
            control=list(seed = seed,nstart=nstart, 
                         best=best, 
                         iter = iter, 
                         thin=thin,
                         burnin = burnin))
proc.time()-time

# CNN
time=proc.time()
results_cnn=LDA(dtmatrix_cnn,k, method="Gibbs", 
                 control=list(seed = seed,nstart=nstart, 
                              best=best, 
                              iter = iter, 
                              thin=thin,
                              burnin = burnin))
proc.time()-time


# Visualize the first 5 terms in the first 10 topics
#fecha=fecha[which(total_row>0)]
terms(results_emol,5)
terms(results_cnn,5)

write.csv(terms(results,20),"topics_2.csv")

# Heating plot 
data.frame(fecha=fecha,results@gamma) %>% 
  group_by(fecha) %>% 
  gather(topic,probability,-fecha) %>% 
  filter(!is.na(fecha)) %>% 
  ggplot(aes(x=fecha,y=probability,group=topic,color=topic))+geom_point(size=0.1)+
  geom_smooth(method = 'loess',span = 0.2,se = FALSE)+xlab("date") +
  scale_y_continuous(labels = scales::percent,limits=c(0,.2))+
  scale_x_date(date_breaks = "4 months")+theme(axis.text.x=element_text(angle=-45, hjust=0.001))




data.frame(fecha=fecha,results@gamma[,c(1,9,4,5,15)]) %>% 
  group_by(fecha) %>% 
  summarise(ley=mean(X1),bachelet=mean(X2),movimiento=mean(X3), salud=mean(X4),
            congreso=mean(X5)) %>% 
  gather(topic,probability,-fecha) %>% 
  filter(!is.na(fecha)) %>% 
  ggplot(aes(x=fecha,y=probability,group=topic,color=topic))+geom_point(size=0.1)+
  geom_smooth(method = 'loess',span = 0.2,se = FALSE)+xlab("date") +
  scale_y_continuous(labels = scales::percent,limits=c(0,.2))+
  scale_x_date(date_breaks = "4 months")+theme(axis.text.x=element_text(angle=-45, hjust=0.001))
