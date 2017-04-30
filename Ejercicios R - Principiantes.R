####################################################################
###                 EJERCICIOS R - PRINCIPIANTES                 ###
####################################################################

# EJERCICIO 1)

x <- c('a','b','c','a','b','d','e','f','a','b','c','f','g','a')
table(x) # te crea una tabla con la frecuencia de cada uno
fr_ab <- round(table(x)/length(x)*100, digits = 2) # frecuencia relativa, 
                                                  # redondeada y dos decimales
fr_ab


# EJERCICIO 2)

x <- c(20,22,22,27,28,31,33,33,35,36,39,39,39,41,42)
mean(x)   # media
median(x) # mediana

#para la moda no hay un comando definido, hay que calcularlo paso a paso
tmp <- table(as.vector(x))  # sacamos las frecuencias
names(tmp)[tmp == max(tmp)] # sacamos los maximos -> moda

quantile(x)  # cuartiles
var(x)  # varianza
sd(x)   # desviacion estandar


# EJERCICIO 3)

x <- append(x, values = c(53,67) )  #añadir valores al vector x
boxplot(x)
# el valor 67 es outlier


# EJERCICIO 4)

año <- c(2008:2015)
vt <- c(1000, 1200, 1400, 1500, 1200, 1100, 1300, 1400)
gt <- c(120, 130, 135, 140, 120, 120 , 130, 140)
np <- c(50, 52, 54, 44, 43, 48, 46, 48)

#ventas_totales vs gastos_totales:
cov(vt, gt)          #cov>0 : relacion positiva
plot(vt, gt)         # para visualizarlo
q <- lm(gt ~ vt)      # recta de regresion
abline(lm(gt ~ vt))  #añadir linea al grafico


#ventas_totales vs gastos_totales
cov(vt, np)          #cov<0 : relacion negativa
plot(vt, np)         # para visualizarlo
w <- lm(np ~ vt)      # recta de regresion
abline(lm(np ~ vt))  #añadir linea al grafico

# si nos fijamos en los datos de las lineas de regresion de cada caso:
summary(q)
summary(w)
# vemos que en el primer caso las variables estan estadisticamente relacionadas
# (p=0.00147; R=0.8088)

# pero en el segundo caso no existe una relacion estadistica
# (p=0.7266; R=-0.1411)


# EJERCICIO 5)

cor(vt, gt)  #default: pearson, si no 'method = ..."
  #0.9: cerca de 1 y en positivo -> relacion positva

cor(vt, np)
  #-0.14: cerca del 0, no hay relacion


# EJERCICIO 6)

edades <- c(22,24,26,31,34,36,37,38,39,40,43,47,51)
# Datos sin normalizar:
mean(edades)
sd(edades)

# Datos normalizados:    z = (valor - media)/desv estandar
normalized <- (edades - mean(edades))/sd(edades)
mn <- mean(normalized)
sdn <- sd(normalized)


# EJERCICIO 7)

vec1 <- c(10,20,30,15,5)
vec2 <- c('curso','programación', 'R')
vec3 <- vec1*2
vec4 <- vec3 - vec1
vec5 <- vec2[c(1,3)]
vec6 <- vec3[vec3>=20]
length(vec6) 
vec7 <- vec2[nchar(vec2)>=4]

  
# EJERCICIO 8)
 
mat1 <- matrix(nrow=3, ncol=3)
vec1 <- sample(5)
cad1 <- c('red', 'blue', 'yellow')

lista1 <- list(mat1, vec1, cad1)
lista1

lista1[3]       # tercer elemento de la lista
lista1[[2]][3]  # tercer elemento del segundo elemento de la lista


# EJERCICIO 9)

clientes <- c('id001', 'id002', 'id003','id001', 'id002', 'id003')
periodos <- c(201506,201506,201506,201507,201507,201507)
consumo <-  c(20,30,50,10,20,40)

# 9.a
dt <- data.frame(clientes, periodos, consumo)
dt

# 9.b
nreg <- nrow(dt)*ncol(dt)
nreg

# 9.c
dt$año <- 2015
dt

# 9.d
dt_new <- subset(dt, consumo >=20)
dt_new

# 9.e
dt_medias <- data.frame("clientes" = unique(dt$clientes),
                       "media consumo" = unique(ave(dt$consumo, dt$clientes)))

# 9.f
dt_medias$ratio_consumo <- round((dt_medias[ ,2])/sum(dt_medias[ ,2]), digits = 2)
dt_medias

# 9.g
hist(dt$consumo)

# 9.h
dt_descriptivo <- data.frame(periodos=c(201506,201507), 
                            desc_periodo = c('Junio 2015','Julio 2015'))
dt_descriptivo

# 9.i
dt <- merge(dt, dt_descriptivo) #puedes hacer merge porque tienes 'periodos' como Id
dt

# 9.j
dt$agrupado <- ave(dt$consumo,dt$periodos,FUN=cumsum)

# 9.k
library(RColorBrewer)

par(mfrow=c(2,2)) # este comando sirve para mostrar 4 graficos (2x2)
barplot(dt$consumo[dt$desc_periodo=='Junio 2015'], main = 'Junio 2015',
        xlab = 'Clientes', ylab = 'Consumo', col = (brewer.pal(3, 'Set1')))
barplot(dt$consumo[dt$desc_periodo=='Julio 2015'], main = 'Julio 2015', 
        xlab = 'Clientes', ylab = 'Consumo', col = (brewer.pal(3, 'Set1')))
barplot(dt$agrupado[dt$desc_periodo=='Junio 2015'], main = 'Junio 2015',
        xlab = 'Agrupado', ylab = 'Consumo', col = (brewer.pal(3, 'YlOrBr')))
barplot(dt$agrupado[dt$desc_periodo=='Julio 2015'], main = 'Julio 2015', 
        xlab = 'Agrupado',ylab = 'Consumo', col = (brewer.pal(3, 'YlOrBr')))


# EJERCICIO 10)

# 10.a
vec1 <- rep(NA, 10)
vec2 <- rnorm(10, mean = 5, sd = 1)
vec1 <- vec2 - mean(vec2)
vec1

# 10.b
v3 <- scan() # este comando permite introducir valores desde la consola
             # Para acabar de introducir valores pulsa Enter dos veces
v3

v4 = c(rep(NA, length(v3)))

for(i in 1:length(v3)){
  if(i == 1){
    v4[i] = v3[i]
  }
  else{
    v4[i] = v3[i] - v3[i-1] 
  }
}

v4

# 10.c

v5 <- matrix(rnorm(16, mean = 10, sd = 2), nrow = 4)
v5

resul5 <- sum(diag(v5))
resul5

# 10.d

mat4.1 <- matrix(rnorm(16, mean = 10, sd = 2), nrow = 4)
mat4.2 <- matrix(rnorm(16, mean = 10, sd = 2), nrow = 4)
mat4.3 <- mat4.2 - mat4.1

# 10.e

mat5 <- matrix(sample(c('Madrid', 'Lisboa'), 16, replace = T, 
                      prob = c(0.6, 0.4)), nrow = 4)
mat5
table(mat5)

# 10.f

mat6 <- matrix(c(6,3,7,32,14,9,1,5,13,5,21,2), nrow=4, ncol=3)
mat6
t(mat6)  # el resultado que buscamos

mat6.1 = matrix(NA, nrow = 3, ncol = 4) # primero contruimos la matriz vacia

for(i in 1:ncol(mat6)){     # y lo vamos llenando con el "for"
  mat6.1[i, ]= mat6[ ,i]
  }

mat6.1


# EJERCICIO 11)

# 11.a
fun_1 = function(a, b, c){
  if(is.null(a) == T | is.null(b) == T | is.null(c) == T){
    print('hay valores nulos')
  } else {
    vec_1 = c(a, b, c)
    return(vec_1)}
}

fun_1(1, 7 ,3)  # nos retorna lo introducido
fun_1(1, NULL, 3) # nos retorna un error


# 11.b 
fun_2 <- function(a, b, c){
  if(length(a) != 4 | length(b) != 4 | length(b) != 4){
    print('No tiene 4 elementos')
  } else{
    dt_2 = data.frame(a,b,c)
    return(dt_2)
  }
}
  
dataf <- fun_2(c(1, 2, 3, 4), c(4, 6, 7, 8), c(9, 10, 11, 12))


# 11.c
fun_3 = function(x){
  y=3*x +5
  return(y)
}

vec_1 = runif(20, min = 0, max = 5)

fun_3(vec_1)


# 11.d
ciudades <- c('Madrid','Madrid','Madrid','Lisboa','Lisboa','Lisboa')
productos <- c('P1','P1','P2','P3','P3','P4')
ventas <- c(20,22,11,10,15,20)

dataf4 <- data.frame(ciudades, productos, ventas)

m_ventas <- tapply(dataf4$ventas, list(dataf4$ciudades, dataf4$productos), FUN = mean)
                
m_ventas <- data.frame(m_ventas)

m_ventas[is.na(m_ventas)] <- 0


# 11.e
mat = matrix(sample(1:4, 16, replace = T), nrow =4)
mat

fun_5 <- function(x){
  if(x < 4){
  if(x == 1){
    res ='A'
  }
  if(x == 2){
    res = 'B'
  }
  if(x == 3){
    res = 'C'
  }
  }
  else{
    res = 'D'
  }
  return(res)
}

resultado <- apply(mat, c(1,2), FUN = fun_5)


# EJERCICIO 12)

# 12.a
dt_iris <- iris

# 12.b
head(dt_iris, 20) # 20 primeros

ale_50 <- dt_iris[sample(nrow(dt_iris), 50), ] #sample de 50 filas

# 12.c
table(dt_iris$Species)

# 12.d
summary(dt_iris$Petal.Length)

# 12.e
hist(dt_iris$Sepal.Width)
boxplot(dt_iris$Sepal.Width)

# 12.f
compr <- sum(is.na(dt_iris))
compr  # no hay NA


# EJERCICIO 13)
  
periodos<- seq(201501:201509) + 201500
vper<- data.frame(periodo=periodos, ventas = round(rnorm(9,100,20)))

vper$periodo <- as.factor(vper$periodo)


# 13.a
barplot(vper$ventas, names.arg = vper$periodo, col = 'Green')


# 13.b 
barplot(vper$ventas, names.arg = vper$periodo, col = rainbow(length(periodos)))
barplot(vper$ventas, names.arg = vper$periodo, col = heat.colors(length(periodos)))
barplot(vper$ventas, names.arg = vper$periodo, col = gray.colors(length(periodos)))
barplot(vper$ventas, names.arg = vper$periodo, col = topo.colors(length(periodos)))


# 13.c
pie(vper$ventas, periodos, col = rainbow(length(periodos)))


# 13.d
cliente1 <- c(97,80,132,92,106,128,107,119,119)
cliente2 <- c(104,95,167,94,92,96,58,89,109)
cliente3 <- c(115,84,130,108,124,115,113,87,91)
cliente4 <- c(83,90,94,86,129,89,91,105, 85)
cliente5 <- c(80,118,104,72,90,103,105,78,121)
per <- c(201501:201509)

df5 <- data.frame(cliente1, cliente2, cliente3, cliente4, cliente5)
row.names(df5) <- per

df5.2 <- as.matrix(df5) # para hacer un "heatmap" tiene que ser una matriz

heatmap(df5.2, Rowv=NA, Colv=NA, margins = c(6,6))
heatmap(df5.2, Rowv=NA, Colv=NA, margins = c(6,6), col = gray.colors(50))


# 13.e
library(rworldmap)

paises <- c('ESP', 'CHN', 'JPN', 'DEU')
ventas <- c(321, 436, 843, 102)
df_vp <- data.frame(paises, ventas)

#enlazamos los datos con el mapa por país, creando el mapa: map1
map_ventas <- joinCountryData2Map(df_vp,
                                  joinCode="NAME",                          
                                  nameJoinColumn="paises",                          
                                  suggestForFailedCodes = T)
# pintamos el mapa
mapCountryData(map_ventas, 
               nameColumnToPlot="ventas", 
               catMethod='categorical',
               mapTitle="Ventas por países",
               colourPalette=heat.colors(4),
               oceanCol="lightblue", missingCountryCol="white")

# 13.f
library(ggmap)
mapa <- get_map(location = c(lon= -2.671635, lat = 42.846718), maptype = "roadmap")
mapa # el mapa sobre el que poner los puntos

puntos <- data.frame(lon=c(-2.811111, -2.522222, -2.933333),
                     lat=c(42.666666, 42.888888, 43.055555)) # los puntos

ggmap(mapa) + geom_point(data=puntos, aes(x=lon, y=lat),  
                         color="red", size=5, alpha=0.5)


# EJERCICIO 14)

G<-c(20,25,21,30,22,23,19,24,21,23,28,27)
I<-c(229,235,230,242,231,233,226,232,230,232,238,236)

plot(G, I)
abline(lm(I~G))

# podemos poner, ademas, los valores de R y p en el grafico:
sumlr <- summary(lm(I~G))
r2 = sumlr$adj.r.squared   # el valor R
p = sumlr$coefficients[2,4] # el valor p

mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 19, y = 2.5, labels = mylabel)
rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(p, digits = 2)))[2]
legend('bottomright', legend = rp, bty = 'n')


# una forma de calcular los ingresos en el futuro es utilizando la 
# formula y = a*x + b
# en este caso: y = 1'266x + 202.973 (los coeficientes se pueden observar
# en la informacion de la linea de regresion -> lm(I~G))

# por lo tanto, en los siguientes 5 años:
G2 <- c(30,32,34,36,38)
I2 <- (1.266 * G2) + 202.973
I2

# otra forma, sin escribir los valores de coeficientes, sino accediendo a ellos
linea_reg <- lm(I~G)
I3 <- (linea_reg$coefficients[2]*G2) + linea_reg$coefficients[1]
I3


# otra manera es utilizando "predict"
I4 <- predict.lm(linea_reg, data.frame(G=G2))
I4


# EJERCICIO 15)

library(caret)
library(rpart)
library(ElemStatLearn)
library(rattle)

df_SA <- data.frame(SAheart) 
  # 'famhist' ya esta como factor, no hace falta cambiarlo


### Decision tree:
set.seed(123)
m_tree <- rpart(chd ~ .,
                data=df_SA,
                method="class")

fancyRpartPlot(m_tree, cex = 0.7)


### Logistic regression:
set.seed(123)
m_logis <- glm(chd ~ .,
               data=df_SA,
               family="binomial")  

summary(m_logis)


# EJERCICIO 16)

library(forecast)
data(co2)
df_co2 <- data.frame(co2)
plot(df_co2)

# 16.a
stac <- stl(co2, s.window = 'periodic')
plot(stac)

# 16.b
stac$time.series[,1]   # parte estacional
stac$time.series[,2]   # tendencia

# pintamos las diferentes partes sobre la serie temporal
plot(co2) 
lines(stac$time.series[,2],col='red') # Tendencia
lines(stac$time.series[,1]+stac$time.series[,2],col='blue') # Tendencia + estacionaria

# 16.c
acf(co2)

# el lag mide la distancia con intervalos anteriores, siendo 0 (un intervalo
# consigo mismo), saca los años y los qur entre medias
# la correlación decae a largo plazo 

# 16.d
# dividimos en dataset de entrenamiento y test
co2Train <- window(co2,start=1959,end=1995) # de 1959 a 1995
co2Test <- window(co2,start=1995) # de 1995 a 1997

# modelo arima h = 36 , estima 3 años -> 36 meses
co2fit <- forecast(auto.arima(co2Train),h=36)
co2fit # ver intervalos de confianza
plot(co2fit, plot.conf=FALSE,main="Prediccion para emisión de co2")

# comprobar exactitud predicción
accuracy(co2fit, co2Test)


                      #####  THE END!!!  #####