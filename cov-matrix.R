#ATIVIDADE 2----

#Quer-se provar a hip?tese de n?o-vi?s do estimador de vari?ncia do MQO
#Pede-se a matriz de vari?ncia-covariancia dos coeficientes 'BETA' da regress?o linear,
#seguido pela variancia amostral em substitui??o da vari?ncia populacional


#CRIANDO OS OBJETOS: MATRIZES X e Y-----

library("car") ##pacote contendo fun??es e conjuntos de dados associados ao livro
##An R Companion to Applied Regression

head(Salaries) #ser? utilizada essa base para criar as vari?veis

x0 <- rep(1, 397)
x1 <- matrix(data=Salaries$yrs.since.phd) #anos de estudo
x2 <- matrix(data=Salaries$yrs.service) #idade
x <- matrix(data=cbind(x0, x1, x2), nrow = 397, ncol = 3) #MATRIZ X
y <- matrix(data=Salaries$salary) #salario (Var dependente) MATRIX Y

#CRIANDO A MATRIZ M----

#A matriz M surge das equa??es normais do MQO ap?s algumas manipula??es alg?bricas.
###Sua f?rumula ? 

#############m = (I - x(x'x)^-1x').

##Portanto, precisamos criar uma identidade (com tra?o igual n? de linhas de X).

I <- diag(rep(1, 397))

m = I-(x%*%(solve(t(x)%*%x))%*%t(x))
m       

#RES?DUOS-----

##Novamente das equa??es normais, os res?duos s?o 

############e = my

#Logo, define-se:


e <- m%*%y
head(e)

#TRA?O DE In e Ik-----

##A vari?ncia amostral tem formula

############### s^2 = e'e/(n - k)

###em que n e k s?o os tra?os da matriz In e Ik = x(x'x)^-1x.

###Precisamos da vari?ncia amostra, pois a vari?ncia populacional ? desconhecida.

###Portanto, vamos definir esses tra?os com a fun??o sum do R:

n <- sum(diag(I))
k <- sum(diag(solve(t(x)%*%x)%*%(t(x)%*%x)))


#s^2-----

#Agora podemos criar um objeto s para a vari?ncia amostral, que segue:

s2 <- (t(e)%*%e)/(n-k)
s2
#Verifica-se que o valor ?                748.412.891

#Vamos comprar com o valor da vari?ncia da regress?o linar com a fun??o summery

summary(lm(y ~ x1 + x2)) 
#erro padr?o dos res?duos: 27360 => s^2 = 748.569.600

#Outra forma de calcular a variancia amostral da regress?o ?

deviance(lm(y~x1+x2))/df.residual(lm(y~x1+x2))


#var(b|B)------

#com a vari?ncia amostral definida, podemos agora, finalmente, encontrar a
###matriz de covariancia dos coeficientes beta.


############### var(b|B) = s^2(X'X)^-1

#Vamos chamar (X'X)^-1 de ssx

ssx <- solve(t(x)%*%x)
ssx

#Para calcular na m?o, ? preciso transformar s2 em um escalar, cuja fun??o ?
##as.numeric

s22 <- as.numeric(s2)
var <- (s22*ssx)
var

#Vamos comprar com os valores entregue pela fun??o lm do R

vcov(lm(y ~ x1 + x2))
