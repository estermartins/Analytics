#########################
# ANALYTICS
# Data: 15/10/2018
# Ester Pereira Martins
########################

# Se as bibliotecas necessárias não estiveram instaladas, instale
if (!"readr" %in% installed.packages()) install.packages("readr")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"haven" %in% installed.packages()) install.packages("haven")
if (!"descr" %in% installed.packages()) install.packages("descr")

# Carregando as bibliotecas necessárias
library(readr)
library(dplyr)
library(ggplot2)
library(haven)
library(descr)

data("diamonds")
dim(diamonds)
names(diamonds)

summary(diamonds$price)
diamonds$expensive = ifelse(diamonds$price > mean(diamonds$price),1,0)
freq(diamonds$expensive, plot = F)

# Intercessão das catégorias 
t = table(diamonds$cut, diamonds$expensive)
t
prop.table(t, 2) * 100 # em percentuais
chisq.test(t)

########################
# Rodando uma regressão logística
# Y = expensive
# X = depth (medida de profundidade do diamante)
# glm = general lines modules
reg = glm(expensive  ~ depth, data = diamonds, family = binomial(link = "logit"))
summary(reg) 

## Rodando uma regressão logística1
# Y = expensive
# X = carat
# glm = general lines modules
reg1 = glm(expensive  ~ carat, data = diamonds, family = binomial(link = "logit"))
summary(reg1) 

### Exercício em casa: rodar a mesma regressão com Y = expensive, X = carat+cut
### E apresentar uma interpretação substantiva do resultado.
