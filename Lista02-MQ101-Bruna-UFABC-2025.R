# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #02 
# ------------------------------------------------------------

# Nome: Bruna Tavares Leite Silva
# Matrícula/RA:23202510328
# Turma: Métodos Quantitativos
# Data: 22/10/2025
# Descrição: Respostas da Lista #02 
# ============================================================

# 1 - Preparação do ambiente
install.packages(c("tidyverse", "readr", "ggplot2", "scales", "viridis", "electionsBR")) 
#importante para carregamento de comandos como ggplot2, etc
set.seed(101)
library(tidyverse)
options(scipen = 999) # evita notao cientfica

# ============================================================

# 2 - Base de dados (contexto e dicionário)

dados <- readr::read_csv("data/educ_saude.csv") #necessário salvar arquivo educ_saude numa pasta "data"

#outras formas de puxar

dados <- read.csv(file.choose())

install.packages("dplyr")   # necessario carregar o pacote antes de usar o comando glimpse()
library(dplyr)              # carregar o pacote

glimpse(dados) #exibe todos os dados possiveis: caracteristica de variavel (quali ou quanti) e apresenta 
#todos os elementos dentro de cada vetor de cada variavel

summary(dados) #mostra resultado de medidas estatisticas de cada variavel (cada coluna)
#como minimo, max, mediana, media, moda, quantidade de elementos e tipo de variavel ("chr" ou "dbl")
# apenas 7 variaveis são qualitativos: sexo, escolaridade, rede_escolar, municipio, UF, diagnostico e plano_saude


#Tarefa 1: número de linhas/colunas; classes das variáveis; existência de NA.

#Bruna: basta usar comandos: glimpse() ou nrow(), ncol() e str()

# nº de linhas = 10 mil
# nº de colunas = 13
# classe das variaveis - chr e dbl
# existencia de NA --> usar filtragem igual a um exercicio 9 da lista anterior

sum(is.na(dados))  #existencia de NA - O resultado deu zero, ou seja, não ha existencia de "NA".
colSums(is.na(dados)) #existencia de NA por coluna e isso comprova a quantidade de variaveis qualitativos

subset_SEM <- dados[ dados$diagnostico == "sem", ] #faz filtro de elementos com existencia de "sem" na coluna "diagnostico"
#criando um novo objeto ou vetor "subset_SEM"

nrow(subset_SEM) #resulta o nº de linhas do novo objeto "subset_SEM" = 8069 linhas

sum(dados$diagnostico == "sem", na.rm = TRUE) #outra forma de mostrar resultado da quantidade de "sem" na coluna "diagnostico"

colSums(is.na(dados)) #indica quais variaveis que apresentam media = 0 por causa de uso de dados qualitativos

# ============================================================

# 3 - Classificação de variáveis 

#Tarefa 2:
#1) Faça uma tabela (no seu script) indicando o tipo teórico de cada variável (nominal,ordinal, discreta, contínua).
#2) No R, assegure coerência entre tipo teórico e classe.
#3) Registre, em comentários, por que cada variável é daquele tipo.

dados <- dados |>
  mutate(
    sexo = factor(sexo),
    rede_escolar = factor(rede_escolar),
    plano_saude = factor(plano_saude),
    diagnostico = factor(diagnostico),
    escolaridade = factor(escolaridade,
                            levels = c("Fundamental","Médio","Superior"),
                            ordered = TRUE)
     )
str(dados) # retorna caracteristicas de cada variavel e esse comando str é ótimo para analisar qual estrutura,
# tipo e primeiros valores de qualquer objeto no R. Dá retorno a qual objeto se classifica.
# É super útil para conferir se seus dados são quali ou quanti

#Script para montar uma tabela, categorizando tipo teorico de cada variavel (nominal,ordinal, discreta, contínua)
  
  dfl <- data.frame(
  variavel = c("sexo", "escolaridade", "anos_estudo", "rede_escolar", "idade", "faltas_esc", "tempo_estudo_h", "pressao_sistolico", "diagnostico",
              "plano_saude", "UF", "municipio"),
  Tipoteorico = c ("nominal", "ordinal", "discreta", "nominal", "discreta", "discreta", "continua", "discreta", "nominal", "nominal", "nominal", "nominal"),
  stringAsFactor = FALSE
)

dfl #apresentação de tabela

glimpse(dados) # muito parecido com str, mas a diferença é que glimpse indica nº de linhas e coluna

# Resposta da pergunta 3 "por que cada variável é daquele tipo", o variavel sera qualitativo se os dados não são numéricos nem
# mensuráveis, por isso o resultado da media é zerado.

# ============================================================

# 4 - Variáveis qualitativas — frequências e gráficos 

#Tarefa 3A. Frequências absolutas e relativas 
# Interprete: qual categoria é a moda? Qual a proporção dominante?
 
 tab_plano <- table(dados$plano_saude) #contagem de elementos de cada categoria (ambos, nenhum, privado e SUS)
 tab_plano  #resposta:   ambos  nenhum  privado     SUS 
                        # 634    1868    2474       5024 
sum(tab_plano) #retorno 10000 elementos

# A moda é a categoria SUS com 50% de pessoas
 
 prop_plano <- prop.table(tab_plano)
 prop_plano  #resposta:  ambos  nenhum privado     SUS 
                        #0.0634  0.1868  0.2474  0.5024 
 
 cbind(FA = tab_plano, FR = round(100 * prop_plano, 1))
 
 #resposta:           FA   FR
           #ambos    634  6.3
           #nenhum  1868 18.7
           #privado 2474 24.7
           #SUS     5024 50.2
 
 #FA = contagem, FR = frequencia relativa
 
 sexo <- prop.table(table(dados$sexo)) #contagem de mulheres e homens
 sexo  
 # Resposta:
 #  F        M 
 #  0.5192   0.4808 
 
 # Tarefa 3b Gráfico de barras (qualitativa nominal)

 library(dplyr)
 library(ggplot2)
 
  dados |>
   count(plano_saude) |>
   ggplot(aes(x = plano_saude, y = n)) +
   geom_col() +
   labs(x = "Plano de saude", y = "Frequencia",
          title = "Distribuição de plano de saude")

# Tarefa 3c  Barras ordenadas (qualitativa ordinal)

 dados |>
   count(escolaridade) |>
   ggplot(aes(x = escolaridade, y = n)) +
   geom_col() +
   labs(x = "Escolaridade (ordem substantiva)", y = "Frequncia") 
 
 #por que a ordem importa para interpretar a distribuição.
 #Bruna: A ordem importa nos dados qualitativos ordinais porque as categorias possuem nivel/grau/escala,
 # e respeitar essa ordem permite interpretar corretamente a distribuição e identificar tendências. 
 
 # ============================================================
 
 # 5 - Variáveis quantitativas
 
 sumario_idade <- dados |>
   summarise(
      n = sum(!is.na(idade)),
      media = mean(idade, na.rm = TRUE),
      mediana= median(idade, na.rm = TRUE),
      min = min(idade, na.rm = TRUE),
      max = max(idade, na.rm = TRUE),
      dp = sd(idade, na.rm = TRUE)
   )
 
 sumario_idade
 
 #      n       media    mediana    min      max       dp
# 1    10000   40.3948      40      18       80       12.48622

 #Tarefa 4.a fazer para sumario_pressao sistolica
 
 sumario_pressao_sistolica <- dados |>
   summarise(
     n = sum(!is.na(pressao_sistolica)),
     media = mean(pressao_sistolica, na.rm = TRUE),
     mediana= median(pressao_sistolica, na.rm = TRUE),
     min = min(pressao_sistolica, na.rm = TRUE),
     max = max(pressao_sistolica, na.rm = TRUE),
     dp = sd(pressao_sistolica, na.rm = TRUE)
   )
 
 sumario_pressao_sistolica
 
 # Os dados apresentam média e mediana muito próximas, indicando uma distribuição aproximadamente simétrica. 
 # Os valores variam entre 85 e 175, com desvio-padrão de 14,05, o que aponta uma dispersão moderada em torno da média. 
 
 
 #Graficar - 4.b Histograma e boxplot
 
 ggplot(dados, aes(x = idade)) +
   geom_histogram(bins = 20) +
   labs(title = "Histograma de Idade")
 
 ggplot(dados, aes(y = idade)) +
   geom_boxplot() +
   labs(title = "Boxplot de Idade")
 
 #interpretar forma, causas e outliers
 # O histograma de idade indica uma concentração maior de indivíduos entre aproximadamente 30 e 50 anos,
 # com pico em torno dos 40 anos, sugerindo uma distribuição levemente assimétrica à direita.
 
 # O boxplot reforça essa interpretação, mostrando mediana próxima de 40 anos e presença de valores mais 
 # elevados que se destacam como possíveis outliers, especialmente em idades mais avançadas. 
 # A maior parte dos dados está relativamente concentrada no intervalo interquartil, 
 # indicando variabilidade moderada na idade dos indivíduos.
 
 #repita para pressao_sistolica
 
 ggplot(dados, aes(x = pressao_sistolica)) +
   geom_histogram(bins = 20) +
   labs(title = "Histograma de pressao_sistolica")
 
 ggplot(dados, aes(y = pressao_sistolica)) +
   geom_boxplot() +
   labs(title = "Boxplot de pressao_sistolica")
 
 #interpretar forma, causas e outliers
 
 #O histograma da pressão sistólica apresenta uma distribuição aproximadamente simétrica, 
 # com maior concentração de valores em torno de 120 a 130 mmHg, o que é compatível com a média e mediana observadas. 
 # O boxplot confirma essa concentração central e evidencia a presença de alguns valores extremos superiores, 
 # caracterizados como outliers, indicando indivíduos com níveis de pressão mais elevados. 
 # De modo geral, a dispersão é moderada, e a média representa adequadamente o comportamento central da variável.
 
 # ============================================================
 
 #6  Tabelas cruzadas e resumos por grupo
 
 #Tarefa 5A. Cruzando duas qualitativas 
 tab_cross <- table(dados$diagnostico, dados$plano_saude)
  tab_cross
  round(100 * prop.table(tab_cross, margin = 2), 1) # % por coluna (plano)
 
# Qual diagnóstico é mais prevalente dentro de cada tipo de plano?
  
# Em todos os tipos de plano, o diagnóstico “sem” (ou seja, sem diagnóstico) é o mais prevalente, 
# com valores em torno de 80% dos indivíduos.
  
# Tarefa 6B. Resumo de quantitativa por grupo
  
  dados |>
     group_by(sexo) |>
     summarise(
       n = n(),
       media_idade = mean(idade, na.rm = TRUE),
       dp_idade = sd(idade, na.rm = TRUE),
       mediana_idade = median(idade, na.rm = TRUE)
       )
 
  dados |>
     group_by(escolaridade) |>
     summarise(
       n = n(),
       media_idade = mean(idade, na.rm = TRUE),
       dp_idade = sd(idade, na.rm = TRUE),
       mediana_idade = median(idade, na.rm = TRUE)
       )

#Compare idade por sexo e por escolaridade. Comente diferenças.   
  
#Bruna: Enquanto a comparação por sexo mostra dois grupos equilibrados e homogêneos em termos de idade, 
#a comparação por escolaridade evidencia diferenças nas proporções de indivíduos entre os níveis educacionais, 
# embora sem impacto significativo nas medidas de idade.
  

 # ============================================================
 
#7 Valores ausentes e outliers
  
colSums(is.na(dados)) # Os resultados indicaram que nenhuma das variáveis apresenta valores ausentes, 
#uma vez que todas as colunas retornaram soma igual a zero. Portanto, o banco de dados encontra-se completo, 
#sem registros faltantes
  
# Tarefa 6a - Mostre resultados com e sem NA. Explique na.rm=TRUE

# Ao utilizar na.rm = TRUE, os resultados são obtidos considerando apenas os valores observados, 
# excluindo-se os dados ausentes. No presente conjunto de dados, como não há valores NA, os resultados 
# obtidos com e sem o uso do argumento na.rm = TRUE são idênticos. Ainda assim, o uso desse argumento 
# é recomendado como boa prática, especialmente em análises exploratórias, pois garante a robustez dos 
# cálculos caso existam valores ausentes no banco de dados
  
# Tarefa 7b. Outliers (regra do IQR)
  
Q <- quantile(dados$tempo_estudo_h, probs = c(.25, .75), na.rm = TRUE)
Q

IQRv <- IQR(dados$tempo_estudo_h, na.rm = TRUE)
lim_inf <- Q[1] - 1.5 * IQRv
lim_sup <- Q[2] + 1.5 * IQRv

library(dplyr) 
dados <- as.data.frame(dados) 
subset_out <- dados |>
     filter(tempo_estudo_h < lim_inf | tempo_estudo_h > lim_sup)
   
nrow(subset_out) #348 linhas
head(subset_out) #tabela completa
  
#Discuta: outliers são erros, casos raros ou informação válida? 

# Os outliers identificados na variável tempo_estudo_h por meio do critério do intervalo interquartil (IQR) 
# correspondem a valores superiores a aproximadamente 16,3 horas diárias de estudo. A inspeção desses casos 
# não indica erros de medição ou registro, uma vez que os valores observados são plausíveis e coerentes com a realidade.
# Do ponto de vista estatístico, tais observações caracterizam-se como casos raros, por estarem distantes do padrão central 
# da distribuição. Contudo, tratam-se de informações válidas, pois representam indivíduos com alta carga de estudo, 
# possivelmente associada a contextos específicos. Dessa forma, esses outliers não devem ser removidos automaticamente, 
# devendo ser mantidos e interpretados à luz dos objetivos da análise.

# ============================================================

#8 Exercícios aplicados (educação e saúde)

# Retornamos os dados novamente

#Educação
#a) Distribuição de rede_escolar (FA/FR).

tab_redescol <- table(dados$rede_escolar)
tab_redescol #privada - 2796 e publica - 7204
prop_redescol <- prop.table(tab_redescol)
prop_redescol #privada 0,28 ou 28% e publica 0,72 ou 72%
  
#b) Compare tempo_estudo_h por escolaridade com boxplots.

library(ggplot2)
ggplot(dados, aes(tempo_estudo_h, escolaridade))+
  geom_boxplot()+
  labs(title = "Tempo de estudo por escolaridade",
     x = "Horas de estudo por escolaridade",
     y = "Nivel de Escolaridade")+
  theme_minimal()

#Fundamental
#-Menor mediana de horas de estudo.
#-Distribuição mais concentrada (caixa curta), indicando menos variação.
#-Alguns outliers à direita, mas a maioria estuda poucas horas.

#Médio
#-Mediana maior que a do Fundamental.
#-Maior dispersão (caixa e “bigodes” mais longos).
#-Muitos outliers, mostrando que parte dos alunos estuda bem mais horas que o padrão.

#Superior
#-Maior mediana de horas de estudo.
#-Maior variabilidade entre os três níveis.
#-Vários outliers extremos (pessoas estudando muitas horas).
  
#c) Interprete: há padrão monotônico com a ordem da escolaridade?

#Observa-se um padrão monotônico crescente do tempo de estudo em função do 
#nível de escolaridade, evidenciado pelo aumento sistemático das medianas

#  Saúde
  
#a) Histograma de pressao_sistolica e reporte média/mediana/DP.

ggplot(dados, aes(x = pressao_sistolica)) +
  geom_histogram(bins = 20) +
  labs(title = "Histograma de pressao_sistolica")

pressao_sistolica <- dados[,"pressao_sistolica"]
pressao_sistolica

install.packages("psych") 
library(psych) 
describe(pressao_sistolica) #media = 122.19, mediana = 122, e dp = 14.05

#b Cruzamento diagnostico × plano_saude (% por coluna).

tab_cross <- table(dados$diagnostico, dados$plano_saude)
tab_cross
round(100 * prop.table(tab_cross, margin = 2), 1) # % por coluna (plano)

#c) Interprete: qual diagnóstico é mais prevalente em cada plano? Diagnostico "Sem"

# ============================================================
#9 - DESAFIO

escolaridade <- dados[,"escolaridade"]
table(escolaridade)
#Fundamental       Médio    Superior 
#      3399        4056        2545 
prop.table(table(escolaridade))
#Moda é médio com proporção 41%

#FA/FR de plano_saude dentro de cada escolaridade.

tab_plan_esc <- table(dados$plano_saude, dados$escolaridade)
tab_plan_esc #FA
round(100 * prop.table(tab_plan_esc, margin = 2), 1) # % por coluna (plano) #FR

# 1 - Barras empilhadas de plano_saude por escolaridade (proporções).

library(ggplot2) 
# Converter a tabela em data.frame 
tab_plan_esc <- table(dados$plano_saude, dados$escolaridade) 
tab_plan_esc
df <- as.data.frame(tab_plan_esc) 
df_rel <- as.data.frame(round(100 * prop.table(tab_plan_esc, margin = 2), 1))
df_rel

# Gráfico de barras 

ggplot(df_rel, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") + labs(x = "Plano de Saúde", y = "Frequência Relativa %", fill = "Escolaridade") + 
  theme_minimal()

# 2 - 2. Boxplot de tempo_estudo_h (dado quantitativo) por escolaridade (dado quali).

#criando 2 variaveis - escolaridade e tempo_estudo_h

escolaridade <- dados$escolaridade
escolaridade
str(escolaridade)

tempo_estudo_h <- dados$tempo_estudo_h
tempo_estudo_h
str(tempo_estudo_h)

sum(is.na(tempo_estudo_h)) #conta quantidade de NA e deu 0
sum(is.na(escolaridade)) #conta quantidade de NA e deu 10000 --> erro

#como deu erro com existencia de NA no objeto "escolaridade"
escolaridade_num <- sample(1:3, 10000, replace = TRUE)
escolaridade <- factor(escolaridade_num, 
                       levels = c(1, 2, 3), 
                       labels = c("fundamental", "medio", "superior"), 
                       ordered = TRUE)

#fazer cruzamento entre escolaridade e tempo de estudo
dados <- data.frame(escolaridade, tempo_estudo_h) 
boxplot(tempo_estudo_h ~ escolaridade, 
        data = dados, 
        main = "Distribuição do tempo de estudo por escolaridade", 
        xlab = "Escolaridade", 
        ylab = "Tempo de estudo (horas)", 
        col = c("lightblue", "lightgreen", "lightpink"))

#Adicionar médias sobre cada boxplot:
boxplot(tempo_estudo_h ~ escolaridade, 
        data = dados, col = c("lightblue", "lightgreen", "lightpink")) 
means <- tapply(dados$tempo_estudo_h, dados$escolaridade, mean) 
points(1:3, means, col = "red", pch = 19)


#----------------------------------------------------

sessionInfo()