#####################
# Analytics
# Ester Pereira Martins
# 28/09/2018
#####################


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

bd = read_sav("https://github.com/neylsoncrepalde/introducao_ao_r/blob/master/dados/pes_2012.sav?raw=true")
bd
summary(bd$V4720)
bd$V4720[bd$V4720 == 999999999999] = NA
bd$renda = bd$V4720
summary(bd$renda)

bd = bd %>%
  mutate (sexo = case_when(
    V0302 == 2 ~ "Masc",
    V0302 == 4 ~ "Fem"
  ), cor = case_when(
    V0404 == 0 ~ "Indigena",
    V0404 == 2 ~ "Branco",
    V0404 == 4 ~ "Preto",
    V0404 == 6 ~ "Amarelo",
    V0404 == 8 ~ "Pardo"
  ))


summary(bd$V4803)
bd$anosesco = bd$V4803
bd$anosesco[bd$anosesco == 17] = NA
bd$anosesco = bd$anosesco - 1
summary(bd$anosesco)

# Monte um modelo de refressão que explique a escolaridade a partir da idade e do sexo
reg = lm(anosesco ~ V8005 + sexo, bd)
summary(reg)

# Temos que trabalhar a idade cent na media
bd$idadecen = bd$V8005 - mean(bd$V8005)

# Montando o modelo novamente com a idade cent 
reg = lm (anosesco ~ idadecen + sexo, bd)
summary(reg)

# A medida que aumenta cada ano de idade, espera-se um aumento de 0,04 escolaridade em media.
# Sexomasc = homens possuem em media -0,4 anos de escolaridade do que as mulheres.

# Agora vamos elaborar um modelo usando idade e cor
reg2 = lm(anosesco ~ idadecen + cor, bd)
summary(reg2)

# Individuo amarelo com idade media espera-se que ele tenha 8,5 anos de escolaridade em media
# Brancos tem 1,38 anos de escolaridae a menos que o individuo amarelo (VAI JAPA -_-)
# E assim por diante, analisando os anos de escolaridade a menos em relacao ao individuo de cor amarela

# Agora vamos elaborar um modelo usando idade e cor
bd$cor = as.factor(bd$cor)
reg3 = lm(anosesco ~ idadecen + relevel(cor, "Branco"), bd)
summary(reg3)
