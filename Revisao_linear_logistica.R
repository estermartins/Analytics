### ANALYTICS
### Ester Pereira Martins
### Exercício - Revisão
### Regressão linear - Regressão logística

install.packages("dplry")
install.packages("descr")
install.packages("ggplot2")
install.packages("texreg")
library(dplyr)
library(ggplot2)
library(texreg)
library(descr)

bd = mtcars
names(bd)

# Parte A - regressão linear

# Monte 3 modelos de regressão. Todos eles devem explicar o consumo do carro (mpg) através de seus atributos.
# No primeiro, utilize o peso do carro (wt) para explicar o consumo. No segundo, use a potência em cavalos (hp)
# Na terceira use as duas variáveis juntas. Apresente uma tabela com os resultados das 3 regressões juntas.

# Regressão linear 
Y = mpg

bd$wtmed = bd$wt - mean(bd$wt) # centralizando a variável
bd$hpmed = bd$hp - mean(bd$hp) # centralizando a variável

reg = lm (mpg ~ wtmed, bd) # 
reg1 = lm (mpg ~ hpmed, bd)
reg2 = lm (mpg ~ wtmed+hpmed, bd)

screenreg(list(reg,reg1,reg2))

# ============================================
#                Model 1    Model 2    Model 3  
# --------------------------------------------
#   (Intercept)  20.09 ***  20.09 ***  20.09 ***
#                (0.54)     (0.68)     (0.46)   
# wtmed          -5.34 ***             -3.88 ***
#                (0.56)                (0.63)   
# hpmed                   -0.07 ***  -0.03 ** 
#                         (0.01)     (0.01)   
# --------------------------------------------
#   R^2           0.75       0.60       0.83    
# Adj. R^2      0.74       0.59       0.81    
# Num. obs.    32         32         32       
# RMSE          3.05       3.86       2.59    
# ============================================
#   *** p < 0.001, ** p < 0.01, * p < 0.05

# 1) Interprete os resultados das 3 regressões
# Para cada peso a mais do carro, espera-se um aumento em média de 5,34 litros de consumo.(peso maior/consumo maior/menos km por litro ele faz)
# Para cada potência em cavalos a mais, espera-se um aumento em média de 0,07 de consumo.
# 

# 2) Qual dos modelos melhor se ajusta aos dados? Por quê?
# Modelo 03, pois o percentual da variancia explicada = 81 %.
# Percentual de variancia explicada pelo modelo é maior.

# 3) Os pressupostos do modelo de regressão estão sendo respeitados no melhor modelo? Mostre.
b0 = reg2$coefficients[1]
b1 = reg2$coefficients[2]
b2 = reg2$coefficients[3]

ggplot(bd, aes(y = mpg, x = wtmed)) +
  geom_point() +
  geom_abline(intercept = b0,
              slope = b1,
              col = "red", lwd = 2)

# Sim. 

# 4) Agora, ao melhor modelo, adicione a variável "transmissão automática" (am). Interprete os resultados 
# deste modelo. Ele é melhor ou pior que o modelo estimado anteriormente? Explique.


# Parte B - Regressão logística

# Agora, vamos utilizar um modelo logístico para explicar as chances de um nenê recém-nascido ter 
# baixo peso ao nascer. Para isso, carregamos o banco de dados lowbt direto do repositório github
install.packages("haven")
library(haven)
bd1 = read_dta("https://github.com/neylsoncrepalde/analytics/blob/master/lowbwt.dta?raw=true")
names(bd1)


# A variável dependente é, portanto, lbw (baixo peso ao nascer). Utilize a variável idade (age) e a variável
# É fumante (smoke) para explicar a dependente. A seguir estime outro modelo utilizando as variáveis
# peso da mãe no último período menstrual (lwt) e É fumante (smoke). Apresente os resultados dos dois modelos
# numa única tabela
reg = glm(lbw ~ age + smoke, bd, family = binomial())
summary(reg)



# 1) Qual dos dois modelos se ajusta melhor aos dados? Por quê? Explique quais foram os critérios que você adotou
# para se decidir.
# 2) Interprete os resultados de maneira substantiva com os coeficientes do jeito como eles vem na tabela.
# 3) Calcule as chances relativas a partir dos coeficientes estimados e interprete os resultados.
# 4) Calcule a probabilidade de uma criança ter baixo peso ao nascer dado que sua mãe tinha peso igual a 120 libras
# e seja não fumante.
# 5) Calcule a probabilidade de uma criança ter baixo peso ao nascer dado que mão tinha pesoa igual a 140 libras
# e seja fumante.