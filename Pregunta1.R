#Solucion Pregunta 1
#Espacio muestral 'S' = {A:tiene mas de 2 teles; B:tiene como max q tele}
#     Pi      x   f(xi)=P(x=xi)
# A   0.32    1       0.32
# B   0.68    0       0.68

#ensayo de Bernoulli
x <- c(0,1)
fx <- c(0.68,0.32)
cbind(x,fx)
plot(x,fx,ylim=c(0,1),type="h",col="red")
points(x,fx,pch=16,col="red")

sample(x,1)
n <- 40000
muestra <- sample(x,n,fx,replace=TRUE)
fi <- table(muestra)/n
br <- barplot(fi) #fecuencia relativa
lines(br,fx,ylim=c(0,1),type="h",col="red") #funcion de masa de probabilidad
points(br,fx,pch=16,col="red")

#datos
xbar <- mean(muestra)

#modelo
mu <- sum(x*fx)
mu

fx[2]

#datos de varianza muestral
ssq <- var(muestra)
ssq

#varianza de la funcion de masa de probabilidad
sigmasq <- sum((x-mu)^2*fx)
sigmasq
fx[1]*fx[2] #en bernoulli


#EXERCICI a)
n <- 43
set.seed(123)
muestra <- sample(x,n,fx,replace=TRUE)
muestra
sum(muestra)

y <- function(i){sum(sample(x,n,fx,replace=TRUE))}
y(2)

# bucle en R
set.seed(123)
m <- 400000 #encuestas de n=43
encuestas <- sapply(1:m,y)
fi <- table(encuestas)/m
data.frame(fi)
dbinom(13,43,0.32)

#tabla de probabilidad
resultados <- 1:43
fy <- dbinom(resultados,43,0.32)
tabladeprob <- cbind(resultados,fy)
tabladeprob

#EXERCICI b)
n <- 44
resultados <- 1:44
fy <- dbinom(resultados,44,0.32)
tabladeprob <- cbind(resultados,fy)
plot(resultados,fy,type='h',col='red',ylim=c(0,0.2))

Fy <- cumsum(fy)
tabladeprob <- cbind(resultados,fy, Fy)
tabladeprob

plot(Fy,type='s',col='red')
# |^ == |v
pbinom(17,44,0.32)

#EXERCICI c)
n <- 24
resultados <- 1:24
fy <- dbinom(resultados,24,0.68)
tabladeprob <- cbind(resultados,fy)
Fy <- cumsum(fy)
tabladeprob <- cbind(resultados,fy, Fy)
tabladeprob
#media = mu = valor esperado
mu <- sum(resultados*fy)
mu
#varianza
sigmasq <- sum((resultados-mu)^2*fy)
sigmasq 
#primer cuartil,(si esta entre medio de dos valores, se coge el mas alto)
plot(Fy,type='s',col='red')
qbinom(0.25,24,0.68)
