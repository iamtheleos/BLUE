#ATIVIDADE 2----

#Quer-se provar a hipótese de não-viés do estimador de variância do MQO
#Pede-se a matriz de variância-covariancia dos coeficientes 'BETA' da regressão linear,
#seguido pela variancia amostral em substituição da variância populacional


#CRIANDO OS OBJETOS: MATRIZES X e Y-----

library("car") ##pacote contendo funções e conjuntos de dados associados ao livro
##An R Companion to Applied Regression

head(Salaries) #será utilizada essa base para criar as variáveis

x0 <- rep(1, 397)
x1 <- matrix(data=Salaries$yrs.since.phd) #anos de estudo
x2 <- matrix(data=Salaries$yrs.service) #idade
x <- matrix(data=cbind(x0, x1, x2), nrow = 397, ncol = 3) #MATRIZ X
y <- matrix(data=Salaries$salary) #salario (Var dependente) MATRIX Y

#CRIANDO A MATRIZ M----

#A matriz M surge das equações normais do MQO após algumas manipulações algébricas.
###Sua fórumula é 

#############m = (I - x(x'x)^-1x').

##Portanto, precisamos criar uma identidade (com traço igual nº de linhas de X).

I <- diag(rep(1, 397))

m = I-(x%*%(solve(t(x)%*%x))%*%t(x))
m       

#RESÍDUOS-----

##Novamente das equações normais, os resíduos são 

############e = my

#Logo, define-se:


e <- m%*%y
head(e)

#TRAÇO DE In e Ik-----

##A variância amostral tem formula

############### s^2 = e'e/(n - k)

###em que n e k são os traços da matriz In e Ik = x(x'x)^-1x.

###Precisamos da variância amostra, pois a variância populacional é desconhecida.

###Portanto, vamos definir esses traços com a função sum do R:

n <- sum(diag(I))
k <- sum(diag(solve(t(x)%*%x)%*%(t(x)%*%x)))


#s^2-----

#Agora podemos criar um objeto s para a variância amostral, que segue:

s2 <- (t(e)%*%e)/(n-k)
s2
#Verifica-se que o valor é                748.412.891

#Vamos comprar com o valor da variância da regressão linar com a função summery

summary(lm(y ~ x1 + x2)) 
#erro padrão dos resíduos: 27360 => s^2 = 748.569.600

#Outra forma de calcular a variancia amostral da regressão é

deviance(lm(y~x1+x2))/df.residual(lm(y~x1+x2))


#var(b|B)------

#com a variância amostral definida, podemos agora, finalmente, encontrar a
###matriz de covariancia dos coeficientes beta.


############### var(b|B) = s^2(X'X)^-1

#Vamos chamar (X'X)^-1 de ssx

ssx <- solve(t(x)%*%x)
ssx

#Para calcular na mão, é preciso transformar s2 em um escalar, cuja função é
##as.numeric

s22 <- as.numeric(s2)
var <- (s22*ssx)
var

#Vamos comprar com os valores entregue pela função lm do R

vcov(lm(y ~ x1 + x2))
