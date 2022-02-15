# Filtrado colaborativo
getwd()
setwd('/Users/alberto.deobeso/Downloads') # windows: C:\\  o   C:/
music.usage <- read.csv('lastfm-matrix-germany.csv')
str(music.usage)

# jason mraz, I'm yours. I wont give up

# 1257 obs. of  285 variables
head(music.usage,1)
music.usage[100,]

# Similitud

vec.1 <- c(1,9,6)
vec.2 <- c(1,2,3)
sum(vec.1 * vec.2)


colnames(music.usage)
# 1 agregar mis preferencias
mis.preferencias <- c(10000,rep(0,285))
length(mis.preferencias)
class(mis.preferencias)
names()
names(mis.preferencias) <- colnames(music.usage)
mis.preferencias['abba'] <- 1
mis.preferencias['aerosmith'] <- 1
mis.preferencias['amy.winehouse'] <- 1
mis.preferencias['arctic.monkeys'] <- 1
mis.preferencias['bjork'] <- 1
mis.preferencias['blur'] <- 1

mis.preferencias['coldplay'] <- 1
mis.preferencias['david.bowie'] <- 1
mis.preferencias['depeche.mode'] <- 1
mis.preferencias['gorillaz'] <- 1
mis.preferencias['guns.n.roses'] <- 1
mis.preferencias['interpol'] <- 1

mis.preferencias['james.blunt'] <- 1
mis.preferencias['metallica'] <- 1
mis.preferencias['mika'] <- 1
mis.preferencias['muse'] <- 1
mis.preferencias['nirvana'] <- 1
mis.preferencias['oasis'] <- 1

music.usage <- rbind(music.usage, mis.preferencias)

nrow(music.usage)

music.usage[1258,]

install.packages('scatterplot3d')
install.packages('plot3D')

library(scatterplot3d)
library(plot3D)

?scatterplot3d



dot.prod <- function(v1,v2) {
  return(sum(v1*v2))
}

vec.1 <- c(1,2,3)
vec.2 <- c(1,1,1)
dot.prod(vec.1, vec.2)

magnit.vec <- function(v1) {
  return(sqrt(sum(v1*v1)))
}

magnit.vec(vec.1)

sqrt(1 + 4 + 9)

?arrows
plot(c(0,1,1),c(0,1,1))
arrows(0,0,1,1)

cosine.similarity <-function(v1,v2){
  return (dot.prod(v1,v2)/(magnit.vec(v1)*magnit.vec(v2)))
}

cosine.similarity(c(1,1,1),c(1,1,1))
cosine.similarity(c(1,0,1),c(0,1,0))
cosine.similarity(c(1,0,1),c(-1,0,-1))


scatterplot3d(c(1,0),c(0,1),,c(1,0), highlight.3d=TRUE,
              col.axis="blue", col.grid="lightblue",
              main="scatterplot3d - 2", pch=20)

cosine.similarity(c(1,2,3),c(2,4,6))

music.usage.nouser <- music.usage[,-1]
head(music.usage.nouser,1)

# 2 recalcular matrix cuadrada de similitud > llenar contenedor con NAs
# crear una matriz con la distancia entre los grupos. 
music.usg.dist <- matrix(NA, 
                         nrow=ncol(music.usage.nouser),
                         ncol=ncol(music.usage.nouser),
                         dimname=list(colnames(music.usage.nouser),colnames(music.usage.nouser)))
music.usg.dist[1:8,1:8]
dim(music.usg.dist)
colnames(music.usage.nouser)
music.usage.nouser[,1]
music.usage.nouser[,2]

sum(music.usage.nouser[,1])
sum(music.usage.nouser[,2])

cosine.distance(music.usage.nouser[,68],music.usage.nouser[,90]) 
cosine.distance(music.usage.nouser[,1],music.usage.nouser[,2]) 

cosine.similarity(music.usage.nouser[,5],music.usage.nouser[,37])  #aerosmith vs black.sabbath

cosine.similarity(music.usage.nouser[,3],music.usage.nouser[,24]) # acdc audioslave

cosine.similarity(music.usage.nouser[, 158], music.usage.nouser[, 209])

# 3 recalcular matrix cuadrada de similitud
for (i in 1:ncol(music.usage.nouser)) {
  for (j in 1:ncol(music.usage.nouser)) {
    music.usg.dist[i,j] <- cosine.similarity(music.usage.nouser[,i],music.usage.nouser[,j])
  }
}
music.usg.dist[1:8,1:8]
class(music.usg.dist)
# convertir a DF
music.usg.dist.df <- data.frame(music.usg.dist)
similarity.aerosmith <- music.usg.dist.df$aerosmith
vector.most.similar <- order(similarity.aerosmith, decreasing = T)[2:11]
rownames(music.usg.dist.df[vector.most.similar,])

valores.aerosmith <- music.usg.dist.df[vector.most.similar,'aerosmith']
valores.aerosmith

similarity.beatles <- music.usg.dist.df$the.beatles
vector.most.similar <- order(similarity.beatles, decreasing = T)[2:11]
rownames(music.usg.dist.df[vector.most.similar,])

valores.beatles <- music.usg.dist.df[vector.most.similar,'the.beatles']
valores.beatles

similarity.queen <- music.usg.dist.df$queen
vector.most.similar <- order(similarity.queen, decreasing = T)[2:11]
rownames(music.usg.dist.df[vector.most.similar,])

valores.queen <- music.usg.dist.df[vector.most.similar,'queen']
valores.queen

similarity.coldplay <- music.usg.dist.df$coldplay
vector.most.similar.coldplay <- order(similarity.coldplay, decreasing = T)[2:11]
rownames(music.usg.dist.df[vector.most.similar.coldplay,])

similarity.sinatra <- music.usg.dist.df$frank.sinatra
vector.most.similar.sinatra <- order(similarity.sinatra, decreasing = T)[2:11]
rownames(music.usg.dist.df[vector.most.similar.sinatra,])

music.usg.dist.df[vector.most.similar.sinatra,'frank.sinatra']

similarity.jackson <- music.usg.dist.df$michael.jackson
vector.most.similar.jackson <- order(similarity.jackson, decreasing = T)[2:11]
rownames(music.usg.dist.df[vector.most.similar.jackson,])

# Filtrado colaborativo

# 4 inicializo contenedor para matrix de recomendaciones
user.data <- matrix(NA,
                    nrow=nrow(music.usage),
                    ncol=ncol(music.usage.nouser),
                    dimnames=list(music.usage$user,colnames(music.usage.nouser)))

user.data[1:5, 1:5]

score.recommendation <- function(history, similarities) {
  return(sum(history*similarities)/sum(similarities))
}

# recomendar aerosmith
history.0 <- c(1,0,0,0,0,0,0,0,0,0)
history.1 <- c(1,0,1,0,0,1,0,0,0,0)
history.2 <- c(1,1,1,1,1,1,1,1,1,1)
history.3 <- c(0,0,0,0,0,0,1,1,1,1)

length(history.1)
valores.aerosmith
length(valores.aerosmith)

history.1

score.recommendation(history.0, valores.aerosmith)

score.recommendation(history.1, valores.aerosmith)
score.recommendation(history.2, valores.aerosmith)
score.recommendation(history.3, valores.aerosmith)

# un usuario
head(music.usage, 17,"the.offspring")
music.usage[3,]
colnames(music.usage)

rownames(music.usage)

music.usage[music.usage$the.offspring==1,"the.offspring"]

colnames(user.data)

nrow(user.data)

for (i in 1:nrow(user.data)) {

# 5 focalizo la llamada a filtrado colaborativo con mi ID: 1258
for (i in 1258:1258) {  
 for (j in 1:ncol(user.data)) {
user <- i
artist <- colnames(user.data)[j]
if (music.usage[user,artist]==1) {
  user.data[user,artist] <- -1
} else {
  top.artist <- head(n=6,
                     music.usg.dist.df[order(music.usg.dist.df[,artist],decreasing = T),][artist])
                top.artists.names <- rownames(top.artist)[-1]    
                top.artists.sim  <-(top.artist[,1])[-1]
                
                top.artists.history <- 
                  music.usage[user,top.artists.names]
                
                user.data[user,artist] <- score.recommendation(top.artists.history,top.artists.sim)
  
}
}
}

# 6 mostrar resultados
head(sort(user.data[1258,], decreasing = T))
  
colnames(user.data)
  
user <- 30
artist <- colnames(user.data)[1]
artist <- 'nirvana'

head(user.data,2)

music.usage[30,]

if (music.usage[user,artist]==1) {
  user.data[user,artist] <- -1
}

top.artist <- head(n=11,
                   music.usg.dist.df[order(music.usg.dist.df[,artist],decreasing = T),][artist])
top.artists.names <- rownames(top.artist)[-1]    
top.artists.sim  <-(top.artist[,1])[-1]

top.artists.history <- 
  music.usage[user,top.artists.names]

user.data[user,artist] <- score.recommendation(top.artists.history,top.artists.sim)


load('user.data.R')

user.data[1:10,1:10]

head(user.data)

head(sort(user.data[1,], decreasing = T))
head(sort(user.data[2,], decreasing = T))
head(sort(user.data[50,], decreasing = T))

head(sort(user.data[1258,], decreasing = T))

max <- -1000
for (i in 1:50) {
  print(paste('cliente',i,sep=" "))
  print(head(sort(user.data[i,], decreasing = T)))
}

sum(as.matrix(music.usage.nouser[7,]))



