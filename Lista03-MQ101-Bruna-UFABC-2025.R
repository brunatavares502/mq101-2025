# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #03 
# ------------------------------------------------------------

# Nome: Bruna Tavares Leite Silva
# Matrícula/RA:23202510328
# Turma: Métodos Quantitativos
# Data: 29/10/2025
# Descrição: Respostas da Lista #03

#----------------------------------------------------

#EXERCICIO 0 

# 1 - Preparação do ambiente

install.packages(c("tidyverse", "readr", "ggplot2", "viridis", "electionsBR")) 

#importante para carregamento de comandos como ggplot2, etc

set.seed(101)
library(tidyverse, readr, ggplot2, viridis, electionsBR)

#----------------------------------------------------

# 1 - Tabela vs Grafico

# Carregue a base cars; exiba as 10 primeiras linhas (tabela); produza um gráfico
# de dispersão speed × dist; interprete.
 
 data(cars) # base interna do R
 dim(cars) #retorna nº de linhas/colunas (50x2)
 head(cars, 10) # retorna as 10 primeiras linhas

#produza um gráfico -  de dispersão speed × dist; interprete.   
 
library(ggplot2)
 ggplot(cars, aes(speed, dist))+
  geom_point(alpha = 0.6) +
  labs(title = "speed x distance", x = "speed", y = "distance")+
  theme_minimal()

# O gráfico é um diagrama de dispersão que mostra a relação entre velocidade (speed) no eixo horizontal 
# e distância (distance) no eixo vertical.Existe uma relação positiva entre velocidade e distância:
# quanto maior a velocidade, maior tende a ser a distância. 
# Portanto, As variáveis apresentam uma relação direta (ou correlação positiva)

#opção 2
ggplot(cars, aes(speed, dist))+
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  labs(title = "speed x distance", x = "speed", y = "distance")+
  theme_minimal()


#----------------------------------------------------

# 2 - Distribuições Univariadas e grupos

data(mtcars) # puxa base interna do R
dim(mtcars) #32 x 11

library(dplyr)
mtcars <- mtcars |>
  mutate(cyl = as.factor(cyl)) #faz com que a coluna cyl não é mais numérica, mas sim categórica. 
str(mtcars$cyl) #retorna 3 categorias "4,6 ou 8"

# Histograma de consumo de combustivel - Criando uma tabela de frequencia

library(ggplot2)

ggplot(mtcars, aes(x = mpg))+
  geom_histogram(
    bins = 10, #nº de barras
    fill = "skyblue", #cor
    color = "black", #cor 
    )+
  labs(
    title = "Histograma de consumo de combustivel", 
    x = "Milhas por galão (mpg)",
    y = "Frequencia"
    )+
  theme_minimal()

#Eixo X (mpg) → representa a variável milhas por galão (consumo de combustível).
#Eixo Y (Frequência) → mostra quantos carros (linhas da base mtcars) caem dentro de cada faixa de mpg.
# A maior parte dos carros está concentrada entre 15 e 23 mpg, o que explica as barras mais altas no histograma

# Densidade --> foi possivel ver onde está o pico (quantidade de carro) no intervalo entre 15 e 20 mpg

library(ggplot2)
ggplot(mtcars,aes(x=mpg))+
  geom_density(alpha = 0.4) +
  labs (title = "Densidade do consumo",
        x = "Milhas por galão",
        y = "Densidade")+
  theme_minimal()

# Boxplot de cyl - foi possivel ver qual cyl (4,6 ou 8) que consome mais

ggplot(mtcars, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot (show.legend = TRUE, outlier.alpha = 0.4) +
  labs(
    title = "Consumo de combustivel por cilindros",
    x = "Nº de cilindros",
    y = "MPG" )+
    theme_minimal()

#----------------------------------------------------

# 3 - Série Temporal Simples

# Opção A: AirPassengers (mensal)

library(tibble)
data("AirPassengers")

ap <- tibble(
  date = as.Date(time(AirPassengers)),
  n = as.numeric(AirPassengers),
  year = format(date, "%Y"),
  month = format(date, "%m")
  )

# o script deu erro, fiz outra forma de criar script:

library(tibble)
data("AirPassengers")
ap <- tibble( 
  date = seq.Date(from = as.Date("1949-01-01"), 
                  by = "month", 
                  length.out = length(AirPassengers)), 
  n = as.numeric(AirPassengers), 
  year = format(date, "%Y"), 
  month = format(date, "%m") 
  )
View(ap)

# outra forma de criar script

library(tibble)
data("AirPassengers")
ap_ts <- AirPassengers
st    <- start(ap_ts)          # c(ano, mes) -> ex.: c(1949, 1)
n     <- length(ap_ts)         # número de observações

# 1º dia do primeiro mês da série
first_date <- as.Date(sprintf("%d-%02d-01", st[1], st[2]))

# sequência mensal de datas com o mesmo comprimento da série
date_seq <- seq(first_date, by = "month", length.out = n)

# tibble final
ap <- tibble(
  date  = date_seq,
  n     = as.numeric(ap_ts),
  year  = format(date_seq, "%Y"),
  month = format(date_seq, "%m")
)

head(ap) #retorna matriz 6 x 4

#Visualização de grafico - número de passageiros por mês em cada ano

ggplot(ap, aes(x = month, y = n, group = year)) + 
  geom_line(color = "steelblue") + 
  facet_wrap(~ year, ncol = 3) + # cria um gráfico separado para cada ano 
  labs( title = "Número de passageiros por mês (1949–1960)", x = "Mês", y = "Passageiros" ) + 
  theme_minimal()
  

# ----------------------------------------------------------------------------

# Opção B: Airquality

data("airquality")

library(tidyr)
aq <- airquality |>
  as_tibble() |>
  drop_na(Ozone) |>
  mutate(Month = factor(Month),
           Day = as.integer(Day))
  View(aq)
  
# Grafico de linhas #um panorama de meses 5 a 9 com apresentação de nivel de ozonio por dia
  
ggplot(aq, aes(x = Day, y = Ozone)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  facet_wrap(~ Month, scales = "free_x")
  labs(title = "Niveis de Ozonio",
        x = "Dia",  y = "Ozonio") +
  theme_minimal()

#----------------------------------------------------

# 4  Relações bivariadas e transformações

data (mtcars)
  
#Gerar grafico Peso de carro x mpg  
library (ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "darkred") +
  labs( title = "Relação entre peso do carro e consumo de combustivel",
    x = "Peso do carro (1000lb)", y = "Milhas por galão (mpg)") +
  theme_minimal()

# A tendencia é negativa, quanto maior o peso do carro (lb) menor eficiencia (mpg)

#outra forma de visualização porem menos linearidade e suavidade. A linha do grafico fica meio curvada
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 0.8, color = "darkred") +
  labs( title = "Relação entre peso do carro e consumo de combustivel",
        x = "Peso do carro (1000lb)", y = "Milhas por galão (mpg)") +
  theme_minimal()


#----------------------------------------------------

# 5 Facetas (comparar subgrupos)

str(mtcars$am) # num [1:32] --> dados quantitativos

install.packages("dplyr")
library(dplyr)

mtcars <- mtcars |>
  mutate (am = factor(am, labels = c( "Automatico", "Manual")))

str(mtcars$am) #  Factor w/ 2 levels "Automatico","Manual"

install.packages("tidyverse")
library(tidyverse)

ggplot(mtcars, aes(hp, mpg)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "darkred")+
  facet_wrap(~am)+
    labs( title = "Relação entre potencia do motor (hp) e Consumo de combustivel",
          subtitle= "Os carros automaticos têm menor desempenho que os manuais",
        x = "Potencia do motor (hp", y = "Milhas por galão (mpg)") 


 #----------------------------------------------------
 
#6 Simulação I – Correlação controlada

n <- 1000
rhos <- c(0.2, 0.6, 0.9)

 sim <- purrr::map_dfr(rhos, \(rho) {
   x <- rnorm(n); e <- rnorm(n)
   y <- rho*x + sqrt(1 - rho^2)*e
   tibble(rho = rho, x = x, y = y)
   })

#Visualização
 
 ggplot(sim, aes(x = x, y = y)) +
   geom_point(color = "steelblue", size = 1, alpha = 1) + #pontos de dispersão
   geom_smooth(method = "lm", se = FALSE, color = "darkred") + # Linha de Regressao Linear
   facet_wrap(~ rho, scale = "free") + #separar por rho
   labs( title= "Comparação de dados simulados com diferentes correlações", 
         x= "Variavel X", 
         y= "Variavel y") + 
    theme_minimal()
 
 #rho = 0.2 (correlação Debil positiva): Os pontos estarão muito dispersos e 
 # formar uma nuvem
 
#----------------------------------------------------
 
#7 Simulação II – Diferenças entre grupos

 n <- 1000
 df_grupos <- tibble(
    grupo = rep(c("A","B"), each = n), #criando uma matriz de 2 colunas e 2000 linhas (mil para A e mil para B)
    valor = c(rnorm(n, 0, 1), rnorm(n, 1, 1.8))) #cada elemento um valor
  
# Histograma
  
  ggplot(df_grupos, aes(x = valor, fill = grupo)) +
    geom_histogram(aes( y = after_stat(density)),
                   binwidth = 0.4, alpha = 0.5, position = "identity") +
    labs(
      title = "Comparação de Distribuições",
      x = "Valor", 
      y = "Densidade",
      fill = "Grupo") +
    theme_minimal()

#Interpretação (Histograma):
  #Média: Grupo A concentrado próximo de 0
  #Grupo B deslocado para valores positivos
  #SD: Grupo B ocupa uma faixa mais ampla no eixo X
  #Grupo A mais concentrado
  
# Densidade
  
  ggplot(df_grupos, aes(x = valor, fill = grupo)) +
    geom_density(alpha = 0.2) +
    labs(title = "Comparação entre Densidades",
         x = "Valor", 
         y = "Densidade",
         fill = "Grupo") +
    theme_minimal()

#Interpretação (Densidade):
  #Média: Pico do Grupo A está centrado próximo de 0
  #Pico do Grupo B deslocado para a direita
  #SD: Curva do Grupo A é mais estreita (menor SD)
  #Curva do Grupo B é mais achatada e larga (maior SD)
  
  
# BOXPLOT  
  
  ggplot(df_grupos, aes(x = grupo, y = valor, fill = grupo)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = "Comparação de Distribuições",
      x = "Grupo", 
      y = "Valor",
      fill = "Grupo") +
    theme_minimal()
  
#Interpretação (Boxplot):
  #Média / Mediana: Grupo B tem valores centrais mais altos que o Grupo A
  #SD: Caixa do Grupo B são mais longos
  #Indica maior variabilidade no Grupo B
  #Grupo B apresenta valores extremos mais distantes
  #Grupo B tem maior média e maior SD que o Grupo A
  
# Violin  
  
  ggplot(df_grupos, aes(x = grupo, y = valor, fill = grupo)) +
    geom_violin (fill = "lightblue", color = "steelblue", alpha = 0.4) +
    #geom_bloxplot( width = 0,1, alpha = 0.7)
    labs(
      title = "Comparação de Distribuições",
      x = "Grupo", 
      y = "Valor",
      fill = "Grupo") +
    theme_minimal()

#Interpretação (Violin):
  #Média: Centro do violino do Grupo B está mais alto
  #SD: Violino do Grupo B é mais largo ao longo do eixo Y
  #Indica maior concentração de valores distantes da média
  
#Geral:
# Grupo A: Média: próxima de 0, Desvio-padrão (SD): menor, indicando menor dispersão e Distribuição mais concentrada e simétrica
# Grupo B: Média: maior que a do Grupo A, em torno de 1 a 1,5, Desvio-padrão (SD): maior, indicando maior variabilidade 
#Distribuição mais espalhada e com cauda à direita
 

  
#----------------------------------------------------

# 8 educ_saude.csv – exploração e gráficos

library(readr)
 educ <- read.csv(file.choose())
 glimpse(educ)
   
# faltas_esc, rede_escolar & plano_saude  - escolha de variaveis
   
# Histograma - Escolaridade e Pressao
   
   ggplot(educ, aes(x = pressao_sistolica, fill = escolaridade)) +
     geom_histogram(bins = 10, alpha = 0.5, position = "identity") +
     facet_wrap(~escolaridade)
     labs(
       title = "Relação",
       x = "Valor", 
       y = "Densidade",
       fill = "Escolaridade") +
     theme_minimal()
   
# Densidade Escolaridade x pressao sistolica
     
     ggplot(educ, aes(x = pressao_sistolica, fill = escolaridade)) +
         geom_density(alpha = 0.2)
       labs(
       title = "Relação",
       x = "Valor", 
       y = "Densidade",
       fill = "Escolaridade") +
       theme_minimal()
     
     
# BARRAS -  Plano de saude x Escolaridade
   
       ggplot(educ, aes(x = escolaridade , fill = plano_saude)) +
         geom_histogram(stat = "count", alpha = 0.2) +
         labs(
           title = "Relação",
           x = "Escolaridade", 
           y = "Frequência"
         ) +
         theme_minimal() 
   
#BloxPot Escolaridade x Pressão Sistolica
       
       library(ggplot2) 
       ggplot(educ, aes(escolaridade, pressao_sistolica)) + 
         geom_boxplot(fill = "lightblue", color = "darkblue") + 
         # adicionar média (ponto vermelho) 
         stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") + 
         # adicionar texto com valor da média 
         stat_summary(fun = mean, geom = "text", aes(label = round(..y..,1)), 
                      vjust = -0.5, color = "red") + 
         labs(title = "Pressão sistólica por escolaridade", 
              x = "Nível de Escolaridade", 
              y = "Pressão Sistólica") + 
         theme_minimal() + 
         coord_flip()
       
      
# Dispersões Idade x Tempo de estudo
       
       ggplot(educ, aes(x = tempo_estudo_h, y= idade)) +
         geom_point(size = 1, alpha = 1, color = "steelblue") +
         geom_smooth(method = "lm", se = FALSE, color = "darkred") +
         labs(
           title = "Relação",
           x = "Tempo de estudo", 
           y = "Idade"
           ) +
         theme_minimal() 
  
# Opção 2 Pressao Sistolica x Idade
       
       ggplot(educ, aes(x = idade, y= pressao_sistolica)) +
         geom_point(size = 1.5, alpha = 0.5, color = "darkgrey") +
         geom_smooth(method = "lm", color = "red",  se = TRUE,
          linewidth = 1.2
       ) +
         labs(
           title = "Relação",
           x = "x", 
           y = "y"
         ) +
         theme_minimal() 
       
#Facetas Faltas por escolaridade
       
       str(educ$faltas_esc) # int [1:10000] --> dados quantitativos
       
       library(dplyr) 
       educ <- educ %>% 
         mutate(cat_faltas = case_when( 
           faltas_esc == 0 ~ "Sem faltas", 
           faltas_esc <= 5 ~ "Até 5 faltas", 
           faltas_esc <= 10 ~ "6 a 10 faltas", 
           faltas_esc > 10 ~ "Mais de 10 faltas" ))
       
       install.packages("tidyverse")
       library(tidyverse)
       
       ggplot(educ, aes(x = escolaridade, y = faltas_esc)) + 
         geom_jitter(color = "steelblue", size = 2, alpha = 0.5, width = 0.2) + 
         stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred") + 
         stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkred", linewidth = 0.8) + 
         labs(title = "Relação entre Escolaridade e Quantidade de Faltas", 
              subtitle = "Tendência da quantidade de faltas por nível de escolaridade", 
              x = "Nível de Escolaridade", y = "Quantidade de Faltas") + 
         theme_minimal()
   
 #----------------------------------------------------

#9 educ_saude.csv – figura final

# Contar uma história em um gráfico (título, eixos, legenda, caption, escala adequada).

#O gráfico apresenta a evolução mensal do número de passageiros aéreos entre 1949 e 1960, 
#utilizando painéis separados por ano, o que permite a análise simultânea da tendência temporal 
#e da sazonalidade da série.
       
#O título informa claramente o fenômeno analisado e o período de observação, orientando o 
#leitor quanto ao contexto temporal dos dados. O eixo horizontal (X) representa os meses do ano, 
#mantendo a mesma escala em todos os painéis, o que facilita a comparação intra e interanual. 
#O eixo vertical (Y) indica o número de passageiros, em escala contínua e uniforme entre os anos, 
#permitindo observar o crescimento real da demanda ao longo do tempo, sem distorções visuais.
       
#A organização em painéis por ano dispensa o uso de legenda, uma vez que cada subgráfico 
#está devidamente identificado, reduzindo a sobrecarga visual e aumentando a clareza interpretativa. 
#A escolha de uma escala fixa no eixo Y é fundamental para evidenciar o aumento progressivo do 
#volume de passageiros ao longo dos anos.
       
#A análise do gráfico revela uma tendência crescente no número de passageiros aéreos ao longo 
#do período analisado, indicando expansão consistente do transporte aéreo. Além disso, 
#observa-se um padrão sazonal bem definido, caracterizado por picos recorrentes nos meses 
#intermediários do ano e reduções nos meses iniciais e finais. Esse padrão se mantém estável ao 
#longo do tempo, sugerindo que, apesar do crescimento estrutural da série, a sazonalidade anual 
#permanece constante.
       
#Em síntese, o gráfico comunica de forma visual que o crescimento do número de passageiros é contínuo 
#e sistemático, enquanto a variação sazonal se repete de maneira regular, evidenciando a coexistência 
#de tendência de longo prazo e sazonalidade na série temporal.       
       
#----------------------------------------------------

#10 Desafio (pontos extras) – Eleições 2024 (Município de São Paulo) com TSE + electionsBR 

# (preparar ambiente):

# (baixar resultados por município e zona – 2024, SP):

# (filtrar município de São Paulo):

# (agregar: taxas por zona eleitoral):

# (perfil do eleitor – escolaridade por zona):

# (integrar bases para análise):
#----------------------------------------------------
    

sessionInfo()