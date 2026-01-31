# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #05 Parte 2
# ------------------------------------------------------------

# Nome: Bruna Tavares Leite Silva
# Matrícula/RA:23202510328
# Turma: Métodos Quantitativos
# Data: 17/12/2025
# Descrição: Respostas da Lista #05 - Teste de Hipóteses e Regressão
#----------------------------------------------------

# Preparação do ambiente

install.packages(c("tidyverse", "readr", "ggplot2", "viridis", "electionsBR")) 

#importante para carregamento de comandos como ggplot2, etc

set.seed(101)
library(tidyverse, readr, ggplot2, viridis, electionsBR)

#----------------------------------------------------

knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  dpi = 96
)

library(tidyverse)
library(broom)

# 1.1 Preparação: gerando a base de dados

#2 Geração da base de dados fictícia

set.seed(123)
n <- 400

dados <- tibble(
  id = 1:n,
  idade = round(rnorm(n, mean = 40, sd = 12)),
  sexo = sample(c("F", "M"), n, replace = TRUE, prob = c(0.55, 0.45)),
  renda = round(rlnorm(n, meanlog = log(2500), sdlog = 0.5), 0),
  escolarid = sample(c("Fundamental", "medio", "superior"), 
  n, replace = TRUE, prob = c(0.30, 0.40, 0.30)),
  ideologia = round(runif(n, 0, 10), 0),
  apoio_gov = rbinom(n, 1, plogis(-1 + 0.015 * (idade - 40) + 0.4 * (sexo == "F") + 0.5 * (renda > 3000))),
  satisf_gov = pmin(pmax(round(3 + 2 * apoio_gov + 0.001 * (renda - 2500) + rnorm(n, 0, 2), 0), 0), 10),
  protesto = rbinom(n, 1, plogis(-2 + 0.3 * (ideologia <= 4) - 0.2 * apoio_gov))
)

glimpse(dados)

# 3 Exploração descritiva e gráficos básicos

# média, mediana, desvio-padrão e quartis de idade, renda e satisf_gov;

library(dplyr) 
tabela_resumo <- purrr::map_dfr( 
  .x = c("idade", "renda", "satisf_gov"), 
  .f = function(var) { 
    dados %>% 
      summarise( 
        media = mean(.data[[var]], na.rm = TRUE), 
        mediana = median(.data[[var]], na.rm = TRUE), 
        sd = sd(.data[[var]], na.rm = TRUE), 
        q1 = quantile(.data[[var]], 0.25, na.rm = TRUE), 
        q3 = quantile(.data[[var]], 0.75, na.rm = TRUE) ) %>% 
      mutate(variavel = var, .before = 1) 
    } 
  )

# proporção de respondentes por categoria de sexo e escolarid

prop_sexo <- dados %>% 
  count(sexo) %>% 
  mutate(proporcao = n / sum(n))
print(prop_sexo)

prop_escolaridade <- dados %>% 
  count(escolarid) %>% 
  mutate(proporcao = n / sum(n))
print(prop_escolaridade)

# proporção de respondentes com apoio_gov = 1.

prop_apoio <- mean(dados$apoio_gov == 1, na.rm = TRUE) 
prop_apoio

# 1.2. GRÁFICOS 
# 5 - Histograma de renda

ggplot(dados, aes(x = renda)) + 
  geom_histogram(bins = 20, fill = "steelblue", color = "white") + 
  theme_minimal()

# Interpretação: Há poucos indivíduos com renda elevada, o que pode indicar desigualdade econômica.

# Histograma  de satisf_gov

ggplot(dados, aes(x = factor(satisf_gov))) + 
  geom_bar(fill = "darkgreen") + 
  theme_minimal()

#Interpretação: A maioria dos respondentes está moderadamente insatisfeita com o governo, por causa da
#moda = 4

# 6 - Grafico de  barras - escolaridade

ggplot(dados, aes(x = escolarid)) + 
  geom_bar(fill = "coral") + 
  coord_flip() + 
  theme_minimal()

#Poucos têm apenas ensino fundamental, o que pode indicar um perfil mais escolarizado do que a média nacional.

#1.3 Comentarios:
# Renda: Assimetria positiva (à direita). Média maior que a mediana.
# Satisfação: Moderada/baixa, por causa da moda = 4
# Escolaridade: Equilibrado entre niveis, menos superior com alta contagem

# 7 - Escolha de testes estatísticos

# Identifique o tipo de cada variável: categórica ou contínua.

library(dplyr) 
# Função para classificar variável 

classificar_var <- function(x) { 
  if (is.numeric(x)) { 
    return("Quantitativo") } 
  else if (is.character(x) | is.factor(x)) { 
    return("Qualitativo") } 
  else { return("Outro") } } 

# Aplicar a função em todas as colunas 
tipos_variaveis <- dados %>% 
  summarise(across(everything(), ~classificar_var(.x))) %>% 
  tidyr::pivot_longer(
    cols = everything(), 
    names_to = "variavel", 
    values_to = "tipo") 

tipos_variaveis

#apenas são variaveis com dados qualitativos: sexo e escolarid


# Indique qual teste bivariado é mais adequado para qual caso abaixo
#(análise tabular e qui-quadrado, diferença de médias, correlação, regressão simples)

#Variáveis binárias (como apoio_gov e protesto) são quantitativas discretas, mas em 
#muitos contextos também podem ser tratadas como categóricas (sim/não).

#sexo (F/M) e apoio_gov (0/1). -->duas categóricas --> Analise tabular + Q²
#escolarid (3 categorias) e apoio_gov (0/1). --> duas categóricas --> Analise tabular + Q²
#satisf_gov (0–10) e apoio_gov (0/1). --> quantitativa continua e categorica --> Diferença de médias (teste t de Student)
#satisf_gov (0–10) e renda (contínua). --> quantitativas continuas --> Correlação (Pearson ou Spearman)
#satisf_gov (0–10) e ideologia (0–10). quantitativas continuas --> Correlação (Pearson ou Spearman)


teste_a <- chisq.test(table(dados$sexo, dados$apoio_gov))
teste_c <- t.test(satisf_gov ~ apoio_gov, data = dados)
teste_d <- cor.test(dados$satisf_gov, dados$renda)

# 8 – Teste qui-quadrado: sexo e apoio ao governo

# 9 Tabela de contingência simples
tab_sexo_apoio <- table(dados$sexo, dados$apoio_gov)
tab_sexo_apoio

# 10 Proporções por coluna (exemplo)

prop.table(tab_sexo_apoio, margin = 2)

# Hipoteses se p < 0,05 --> Rejeita HO
# Se rejeitamos H0: isso significa que o apoio ao governo varia conforme o sexo. Em termos simples, 
#homens e mulheres têm padrões diferentes de apoio.
# Se não rejeitamos H0: isso significa que não há diferença estatisticamente significativa entre 
#homens e mulheres quanto ao apoio ao governo; as variações observadas podem ser explicadas pelo acaso.


#Teste q²
chisq.test(tab_sexo_apoio)


#Interpretar
#Qui-quadrado (X-squared = 0.68956): valor baixo, indicando que as frequências observadas não diferem muito das esperadas sob independência.
#Graus de liberdade (df = 1): porque temos uma tabela 2×2 (sexo × apoio).
#Valor-p (p = 0.4063): bem maior que 0,05. --> logo nao rejeitamos HO

# Grafico com barras apoio_gov x sexo
ggplot(dados, aes(x = sexo, fill = factor(apoio_gov))) + 
  geom_bar(position = "fill") + 
  theme_minimal()


# 11 Diferença de médias: renda entre apoiadores e não apoiadores

dados %>%
  group_by(apoio_gov) %>%
  summarise(
    media_renda = mean(renda),
    sd_renda    = sd(renda),
    n           = n()
  )

teste_t_renda <- t.test(renda ~ apoio_gov, data = dados)
print(teste_t_renda)

ggplot(dados, 
       aes(x = as.factor(apoio_gov), 
           y = renda, fill = as.factor(apoio_gov))) + 
  geom_boxplot() + 
  theme_minimal()


# 12 - Correlação: renda, ideologia e satisfação com o governo

dados %>%
  select(renda, ideologia, satisf_gov) %>%
  cor(use = "complete.obs")

ggplot(dados, aes(x = renda, y = satisf_gov)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", col = "blue") + 
  theme_minimal()

# 13 - Regressão linear simples: satisfação e renda

mod1 <- lm(satisf_gov ~ renda, data = dados)
summary(mod1)

tidy(mod1)

tabela_limpa <- tidy(mod1)
tabela_limpa

ggplot(dados, 
       aes(x = renda, y = satisf_gov)) + 
  geom_point(alpha = 0.6, color = "darkgreen") + # pontos da dispersão 
  geom_smooth(method = "lm", se = TRUE, color = "blue") + # reta de regressão linear 
  labs( 
    x = "Renda", 
    y = "Satisfação com o governo", 
    title = "Dispersão entre renda e satisfação com o governo" ) + 
  theme_minimal()
  
# O gráfico de dispersão com a reta azul confirma visualmente o coeficiente positivo.
# Existe uma relação direta: conforme a renda sobe, a satisfação tende a aumentar.
# A dispersão dos pontos indica que a renda explica parte da satisfação, mas não toda ela.

# 14 - Diagnóstico simples do modelo

dados_diag <- augment(mod1)
glimpse(dados_diag)


# 15 - Resíduos vs ajustados

ggplot(dados_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) + geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  labs(title = "Resíduos vs Ajustados") + theme_minimal()

# 16 - QQ-plot

ggplot(dados_diag, aes(sample = .resid)) +
  stat_qq() + stat_qq_line(color = "red") + 
  labs(title = "Normal QQ-Plot") + theme_minimal()

# 17 - Regressão com variável dummy e diferença de médias

mod2 <- lm(satisf_gov ~ apoio_gov, data = dados)
summary(mod2)
t.test(satisf_gov ~ apoio_gov, data = dados)

#----------------------------------------------------

sessionInfo()