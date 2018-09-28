---
title: "R Notebook"
output: html_notebook
---

```{r setup}
if (!"readr" %in% installed.packages()) install.packages("readr")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"haven" %in% installed.packages()) install.packages("haven")

library(readr)
library(dplyr)
library(ggplot2)
library(haven)
```

## Regressão Linear

### Regressão Log-Lin

Na aula passada, começamos estudando um modelo de regressão linear que explica a renda através dos anos de escolaridade do indivíduo.
Vimos que esse modelo não funciona bem, pois todos os pressupostos da regressão linear sõa *derespeitados*. Para estimarmos bem a renda do indivíduo, é necessário fazer uma pequena transaformação nos dados da renda, a saber, tranformá-la no seu **logaritmo natural**. Para isso, caso haja valores 0, é preciso acrescentar um centavo para todos os indivíduos porque não existe log de 0!!


## Exercício

Agora que já sabemos estimar um modelo tipo *log-lin*, vamos estimar a renda do indivíduo através do seu status sócio-ocupacional. Lembre-se de que o ISEI não possui valor 0. Portanto, é necessária uma pequena transformação nessa variável para que o intercepto do odelo faça sentido.

```{r reg1}
bd = read_sav("https://github.com/neylsoncrepalde/MODUS/blob/master/PNAD2014_30a50_novo4.sav?raw=true")
dim(bd)

# Transformar a renda
bd$logrenda = log(bd$renda)
# Transformar o ISEI
bd$iseicent = bd$isei88 - mean(bd$isei88)
summary(bd$iseicent)

# Modelo
reg1 <- lm(logrenda ~ iseicent, bd)
summary(reg1)

b0 = reg1$coefficients[1]
b1 = reg1$coefficients[2]
```

Vamos interpretar os coeficientes.

Um indivíduo que possui ISEI médio (iseicent = 0) tem o log da renda esperado de 7,48. Esse valor não é muito informativo. Para analisar o coeficiente de volta à escala em R$, tiramos o exponencial.

Um indivíduo que possui ISEI médio tem a renda esperada de R$ 100,00

Como interpretamos $\hat{\beta}_1$?  ## Latec (escrever fórmulas)

Como interpretamos $\hat{\beta}_1$? B1 é uma taxa simples e mostra a variação no Y em termos percentuais.

Para cada aumento de 1 ponto na escala do ISEI, há um aumento médio de R$ `r round(b1 * 100,2)` %.

Para calcular o rendimento do trabalho esperado para cada ponto da distribuição de X, utilizamos a taxa composta que é calculada pela seguinte fórmula:

$$TaxaComposta = (exp(b_1) - 1) \cdot 100 $$

Para um indivíduo com 5 anos de escolaridade é esperado um rendimento médio de R$ `r round (exp(b0) * exp(b1) * 5, 2)`. 

## Verificando os pressupostos do modelo

1) Linearidade
```{r linearidade}
ggplot(bd, aes(y = logrenda, x = iseicent)) + geom_point() + geom_abline(intercept = b0, slope = b1, col = "red", lwd = 2)
```

2) Normalidade dos erros
```{r normalidade}
e <- residuals(reg1)
qqnorm(e)
```

3)Homoscedasticidade

4) Independência dos erros
```{r homoind}
plot(reg1)
```

