library("rvest")
library("XML")
library("xml2")
library(dplyr)
library(ggplot2)
library(knitr)
library(recommenderlab)

#fetch data from imdb
url = "http://www.imdb.com/chart/top?ref_=nv_wl_img_3"

sayfa = read_html(url)

film.nodes <- html_nodes(sayfa,'.titleColumn a')

film.link = sapply(html_attrs(film.nodes),`[[`,'href')#Link of movie
film.link = paste0("http://www.imdb.com",film.link)
film.cast = sapply(html_attrs(film.nodes),`[[`,'title')#Team of movie
film.name = html_text(film.nodes)#name of movie

sec <- html_nodes(sayfa,'.secondaryInfo')

yil = as.numeric(gsub(")","",gsub("\\(","",html_text( sec )))) #Year of movie

oran.nodes = html_nodes(sayfa,'.imdbRating')#Rating of movies
xmlTreeParse(oran.nodes[[20]])


oran.nodes = html_nodes(sayfa,'.imdbRating strong')
#All votes
oy = as.numeric(gsub(',','',gsub(' user ratings','',gsub('.*?based on ','',sapply(html_attrs(oran.nodes),`[[`,'title')))))

oran = as.numeric(html_text(oran.nodes))
#set movies dataframe
top250 <- data.frame(film.name, film.cast, film.link,yil,oy,oran)

#Recommender with Collaborative Filtering

film_matrix <- as.matrix(top250)
real_matrix <- as(film_matrix, "realRatingMatrix")
as(real_matrix, "matrix")

#split data into train test set

newData <- evaluationScheme(real_matrix, method="cross-validation",k = 10, train=0.8, given=5,  goodRating=3)
# 10 iterations of run
print(newData)

User_Based <- Recommender(getData(newData, "train"), "UBCF",param=list(normalize = "center",method="pearson"))

#predict ratings
tahmin <- predict(User_Based, getData(newData, "known"), type="ratings")
print(calcPredictionAccuracy(x=tahmin, data= getData(newData, "unknown"), byUser = FALSE))

summary(as.vector(tahmin@data@x))


Item_Based <- Recommender(getData(newData, "train"), method = "IBCF",parameter =list(normalize = "center",method="pearson"))

tahmin_item <- predict(object = Item_Based, newdata=getData(newData, "known"), n=10, type="ratings")

print(calcPredictionAccuracy(x=tahmin_item, data= getData(newData, "unknown"), byUser = FALSE))

summary(as.vector(tahmin_item@data@x))
