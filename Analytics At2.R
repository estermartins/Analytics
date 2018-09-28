---
title: "Analytics2"
author: "Ester Pereira Martins"
date: "30 de agosto de 2018"
output: html_document
---

```{r}
## Atividade de Analytics: 
# 1) A renda deve ser explicada pela a escolaridade do pai [Logrenda =b0+b1Escpai + e].
# 2) [Logrenda = b0 + b1 isei88pai + e] Interprete os resultados.
# b) Interprete os outputs de ajuste.
# c) Exiba um gráfico de aval. dos pressupostos.
```

```{r}
## Instalar pacotes
if (!"readr" %in% installed.packages()) install.packages("readr")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"haven" %in% installed.packages()) install.packages("haven")
```

```{r}
## Carregando as bibliotecas
library(readr)
library(dplyr)
library(ggplot2)
library(haven)
```

```{r}
## Carregando o banco de dados 
bd = read_sav("https://github.com/neylsoncrepalde/MODUS/blob/master/PNAD2014_30a50_novo4.sav?raw=true")
```

```{r}
## Teste de dimensoes
dim(bd)
```

```{r}
## Modelo de regressao
## X = isei88
## Y = anosesco
reg = lm(anosesco ~ isei88, data = bd)
```

```{r}
## Uso do comando summary para ter aceso às estatísticas descritivas da regressao
summary(reg)

# Comentário: 
# Os valores são estatisticamente significativos (comprovado pelas três estrelas);
# Uma pessoa analfabeta, estima-se um isei de 4 pts em média;
# A cada ano de estudo estima-se 0,1 pts no isei em média;
# O grau de liberdade é 4381 para 3509;
# O isei do individuo explica 0,27% da escolaridade do mesmo;
# A estatística F indica que a hipotese não é nula.
```

```{r}
## Uso do comando confit para analisar o intervalo de confiança
confint(reg)

# Comentário:
# O isei de um analfabeto fica entre 3,8 e 4,2;
# O isei aumenta entre 0,13 e 0,14 pontos por ano de escolaridade do indivíduo.
```

```{r}
## Plotando os gráficos e deefinindo a linearidade 

b0 = reg$coefficients[1]
b1 = reg$coefficients[2]

ggplot(bd, aes(y = anosesco,
                 x = isei88))+
   geom_point() +
   geom_abline(intercept = b0,
               slope = b1,
               col = "blue",
               lwd - 1)

plot(reg)
```









