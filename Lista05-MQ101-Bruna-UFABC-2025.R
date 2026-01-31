# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #05 
# ------------------------------------------------------------

# Nome: Bruna Tavares Leite Silva
# Matrícula/RA:23202510328
# Turma: Métodos Quantitativos
# Data: 26/11/2025
# Descrição: Respostas da Lista #05 - Probabilidade e Inferência Estatística
#----------------------------------------------------

# Preparação do ambiente

install.packages(c("tidyverse", "readr", "ggplot2", "viridis", "electionsBR")) 

#importante para carregamento de comandos como ggplot2, etc

set.seed(101)
library(tidyverse, readr, ggplot2, viridis, electionsBR)

#----------------------------------------------------

#Exercício 1 – Probabilidade como frequência de longo prazo (moeda viesada)

set.seed(123)

# Probabilidade de cara
p_cara <- 0.3

# Tamanhos de amostra
n_vec  <- c(1, 10, 100, 1000, 10000)

# Data frame para armazenar os resultados
result <- data.frame(
  n      = n_vec,
  p_cara = NA_real_
)

# Completar o loop para simular os lançamentos
for (i in seq_along(n_vec)) {
  x <- rbinom(n=1, size = n_vec[i], prob = 0.3)
  result$p_cara[i] <- x/n_vec[i]
}

#Proporção de caras em cada tamanho da amostra
print(result)

# Gráfico de linha da proporção de caras vs n


plot(result$n, result$p_cara, type = "o", col = "blue",
     xlab = "Tamanho da amostra (n)",
     ylab = "Proporção de caras",
     main = "Proporção de caras vs tamanho da amostra")

# Outra opção de plotar

plot(result$n, result$p_cara, 
     type = "b", 
     pch = 19, 
     col = "blue",
     xlab = "Tamanho da amostra (n)",
     ylab = "Proporção de caras",
     main = "Proporção de caras vs tamanho da amostra")

abline(h = p_cara, lty = 2, col = "red")  # linha da probabilidade teórica

#Interpretação do grafico:
# A trajetória observada começa instável, mas converge para 0,3 
# à medida que o tamanho da amostra aumenta. 
# Isso ilustra perfeitamente como a variabilidade diminui com mais dados.

#----------------------------------------------------

#Exercício 2 - Bernoulli, Binomial e probabilidades exatas (satisfação em saúde)

set.seed(123)

# Parâmetros
p <- 0.65
n <- 20

# (1) Simulação de uma amostra
y <- rbinom(n, size = 1, prob = p) #aleatorio
print(y) #vetor de 20 elementos com p = 0 e 1

# (2) Contar o número de satisfeitos
num_satisfeitos <- sum(y) #contagem de pessoas satisfeitas

# (3a) Probabilidade P(S = 12) pela Binomial
prob_12 <- dbinom(12, size = n, prob = p)
print(prob_12) # P(S - 12) = 0.161351

# (3b) Probabilidade P(S >= 12)
prob_12_ou_mais <- sum(dbinom(12:n, size = n, prob = p))
print(prob_12_ou_mais) #p(S >= 12) = 0.7623776

# ou:
# prob_12_ou_mais <- 1 - pbinom(11, size = n, prob = p)

# TODO: imprimir os resultados e escrever a interpretação fora do código.

print(prob_12)
print(prob_12_ou_mais)

#Interpretação:
# Cada indivíduo da amostra é modelado como uma variável aleatória de Bernoulli, na qual o 
# desfecho assume valor 1 caso o indivíduo esteja satisfeito com o serviço de saúde e 0 caso contrário.
# A probabilidade de sucesso foi fixada em p=0,65,representando a proporção esperada de indivíduos 
#satisfeitos na população.
#Como são 20 individuos, seguimos com a distruibuição binomial:
# A probabilidade exata de que exatamente 12 indivíduos estejam satisfeitos, isto é, P(s=12)
# foi calculada a partir da função de massa de probabilidade da distribuição Binomial, resultando em:
# P(S=12) = 0,161351  
#Esse valor indica que, considerando uma população em que a probabilidade de satisfação individual é 
# de 65%, há aproximadamente 16,1% de chance de que exatamente 12 dos 20 indivíduos estejam satisfeitos
#em uma amostra aleatória desse tamanho.

#Em síntese, a distribuição de Bernoulli descreve o comportamento individual da satisfação, 
#enquanto a distribuição Binomial permite quantificar a incerteza associada ao número total de 
#indivíduos satisfeitos em uma amostra finita. As probabilidades calculadas fornecem uma 
#interpretação exata da variabilidade amostral esperada em estudos de satisfação em saúde.

#----------------------------------------------------

#Exercício 3 - Probabilidade condicional e independência (base saúde)

# Gere uma base sintética de saúde dados_saude

set.seed(123)

N <- 5000

dados_saude <- data.frame(
  sexo       = sample(c("F", "M"), size = N, replace = TRUE, prob = c(0.55, 0.45)), #55% F e %45 M
  fumante    = rbinom(N, 1, 0.22), #Probabilidade de 22% de ser fumante
  hipertenso = rbinom(N, 1, 0.30) #Probabilidade de 30% de ser hipertenso
)

#sample escolhe aleatoriamente entre F e M e faz isso 5000 vezes, replace --> permite repetir
#valores
#prob F = 0,55 e M = 0,45
#rbninom --> gera nºs aleatorios de uma distribuição binomial, 1 - cada responde apenas
# uma vez e 0,22 probabilidade de sucesso e 0,78 de nao sucesso. Isso significa
# que 0 para nao fumante e 1 para fumante com cerca de 22%

# (2) Estimar as probabilidades

P_fumante <- mean(dados_saude$fumante == 1)
print(P_fumante) #0.2112 P = nºobservado/total = 1105/5000 = 22,1%

P_fumante_F <- mean(dados_saude$fumante[dados_saude$sexo == "F"] == 1)
print(P_fumante_F) #0.2178506 ou 21,78%

# 55% F de 5000 = 2740 mulheres
# 22% de 2740 = 605 mulheres e fumantes
# logo P (F e F) = 605/2740 = 21,78%

P_fumante_M <- mean(dados_saude$fumante[dados_saude$sexo == "M"] == 1)
print(P_fumante_M)#.2031042 ou 20,31%

# Interpretação: 𝑃(fumante =1 ∣ sexo= "𝐹") ~ P( fumante =  1 ∣ sexo  =  "𝑀")=  0.22

# (4) Gráfico de barras da proporção de fumantes por sexo

install.packages("dplyr")
install.packages("ggplot2")

# Dica: usar dplyr + ggplot2
library(dplyr) 
library(ggplot2)

tab_fumo <- dados_saude |>
   dplyr::group_by(sexo) |>
   dplyr::summarise(prop_fumante = mean(fumante))

ggplot(tab_fumo, aes(x = sexo, y = prop_fumante, fill=sexo)) +
   geom_col()
   scale_fill_manual(values = c("F" = "pink", "M" = "blue"))  
   labs(
     x = "sexo",
     y = "Proporção de fumantes",
     title ="Proporção de fumantes por sexo")
   
   
# TODO: escrever interpretação fora do código.
   
# Na base simulada, a probabilidade de ser fumante foi definida como 
# 22% para todos os indivíduos, sem levar em conta o sexo.
   
#----------------------------------------------------
   
#Exercício 4– Probabilidade conjunta e regra do produto (saúde)
   
   
# (1) Probabilidades marginais e conjunta
   
 P_hipertenso <- mean(dados_saude$hipertenso == 1)
 print(P_hipertenso) #0.3014 ou 30%
 P_fumante    <- mean(dados_saude$fumante == 1)
 print(P_fumante) #0.2112 ou 21,1%
 P_hip_e_fum  <- mean(dados_saude$hipertenso == 1 & dados_saude$fumante == 1)
 print(P_hip_e_fum) #0.0658 ou 6,6%
 
#Quando duas variáveis são independentes, a probabilidade conjunta é o produto das probabilidades
 
# (2) Probabilidade condicional (proporção dentro do grupo filtrado ou seja (p(f/t)x p(h/t))/p(f)
 
 
  P_hip_dado_fum <- mean(dados_saude$hipertenso[dados_saude$fumante == 1] == 1)
  print(P_hip_dado_fum) #0.311553 ou 31%

#O valor de 31% é a proporção de fumantes que também são hipertensos na sua amostra simulada.
# Ele está muito próximo do valor teórico esperado (30%), porque nesse caso fumante e 
# hipertenso são variáveis independentes
  
# (3) Produto P(hipertenso = 1 | fumante = 1) * P(fumante = 1)
  P_produto <- P_hip_dado_fum * P_fumante
  print(P_produto) #0.0658 ou 6,6%

#----------------------------------------------------
  
#Exercício 5 –  Bayes “de bolso” em triagem de benefícios

set.seed(123)
  
P_F         <- 0.02
P_T_dado_F  <- 0.9
P_T_dado_Fc <- 0.05
P_Fc        <- 1 - P_F
  
# (1) Cálculo analítico de P(F|T)
P_F_dado_T <- (P_T_dado_F * P_F) /
(P_T_dado_F * P_F + P_T_dado_Fc * P_Fc)
print(P_F_dado_T) #0.2686567 ou 26,9%
  
# (2) Simulação
N <- 100000
  
 fraude <- rbinom(N, 1, P_F)
 alerta <- ifelse(
   fraude == 1,
   rbinom(N, 1, P_T_dado_F),
   rbinom(N, 1, P_T_dado_Fc))
  
# (3) Estimar empiricamente P(F|Alerta)
 P_empirico <- mean(fraude[alerta == 1] == 1)
 print(P_empirico) #0.2637992 ou 26,4%
 
# Comparar P_F_dado_T e P_empirico, e interpretar em texto.  
 
 c(Empirico = P_empirico,
   Analitico = P_F_dado_T,
   Diferencia = P_empirico - P_F_dado_T)
 
 #   Empirico    Analitico   Diferencia 
 #   0.263799178  0.268656716 -0.004857538 
 
# Ou seja, o valor empírico está muito próximo do analítico, confirmando a consistência da 
#fórmula de Bayes. 
 
#----------------------------------------------------
 
# Exercício 6 – Teorema Central do Limite com renda
 
 set.seed(123)
 
 N <- 100000
 renda_pop <- rgamma(N, shape = 2, rate = 1/2500)
 
 # Simular 5000 amostras de tamanho n = 30
 # Função auxiliar para simular médias
 
 simular_medias <- function(n, n_rep = 5000) {
   medias <- numeric(n_rep)
   for (i in seq_len(n_rep)) {
      amostra <- sample(renda_pop, n, replace = TRUE)
      medias[i] <- mean(amostra)
   }
   medias 
 }
 
  medias_n30  <- simular_medias(30)
  medias_n200 <- simular_medias(200)
 
 # TODO: produzir histogramas para as distribuições de médias n = 30 e n = 200
  
  par(mfrow = c(2, 1))
  hist(medias_n30,  main = "Médias amostrais de renda (n = 30)")
  hist(medias_n200, main = "Médias amostrais de renda (n = 200)")
  par(mfrow = c(1, 1))

#Interpretação de 2 histogramas n = 30 e n = 200
# A população (renda_pop) tem distribuição assimétrica (Gamma) vide figura 6.n=1
# Com n = 30, a distribuição das médias ainda mostra alguma assimetria, mas já 
# começa a se aproximar de uma forma mais simétrica.
# Com n = 200, a distribuição das médias fica muito próxima de uma Normal, 
#  centrada na média populacional e com menor variabilidade.  
  
#----------------------------------------------------
 
 # Exercício 7 – 
  
  set.seed(123)
  
  # Caso não tenha salvo dados_saude, recriar:
  
  N <- 5000
  dados_saude <- data.frame(
  sexo       = sample(c("F", "M"), size = N, replace = TRUE, prob = c(0.55, 0.45)),
  fumante    = rbinom(N, 1, 0.22),
  hipertenso = rbinom(N, 1, 0.30)
  )
  
  # (1) Amostra aleatória simples n = 400
  n <- 400
  amostra_saude <- dados_saude[sample(1:nrow(dados_saude), n), ]
  
  # (2) Proporção amostral e Intervalo de confiança
  p_hat <- mean(amostra_saude$hipertenso)
  SE_p  <- sqrt(p_hat * (1 - p_hat) / n)
  IC_95 <- c(
   inferior = p_hat - 1.96 * SE_p,
   superior = p_hat + 1.96 * SE_p
  )
  
  # (3) Proporção verdadeira na população
  p_verdadeiro <- mean(dados_saude$hipertenso)
  
  # TODO: comparar p_hat, IC_95 e p_verdadeiro em texto.
 
  print(p_hat) #0.33
  print(p_verdadeiro) #0.30
  print(IC_95)
  # inferior  superior 
  #0.2839192 0.3760808  
  
  #visualização de dados
  
  # Criar gráfico de barras com IC
  barplot(height = p_hat, ylim = c(0, 0.5), 
          names.arg = "Proporção amostral", 
          col = "lightblue", 
          main = "Proporção de hipertensos: amostra vs população", 
          ylab = "Proporção")
  
  # Adicionar intervalo de confiança 
  arrows(x0 = 1, y0 = IC_95[1], x1 = 1, y1 = IC_95[2], 
         angle = 90, code = 3, length = 0.1, col = "red", lwd = 2)
  
  # Adicionar linha da proporção verdadeira 
  abline(h = p_verdadeiro, col = "darkgreen", lwd = 2, lty = 2)
  
  # Legenda 
  legend("topright", legend = c("IC 95% da amostra", "Proporção verdadeira"), 
         col = c("red", "darkgreen"), lty = c(1,2), lwd = 2, bty = "n")

#Interpretação:
  
#  A barra azul mostra a proporção amostral (33%).
#  As setas vermelhas representam o intervalo de confiança (≈ 28% a 37%).
#  A linha verde tracejada mostra a proporção verdadeira da população (30%).

#----------------------------------------------------
 
 # Exercício 8 – 
  
  set.seed(123)
  
  N <- 2000
  
  dados_educacao <- data.frame(
    ideb        = rnorm(N, mean = 5.5, sd = 0.7),
    gasto_aluno = rnorm(N, mean = 6000, sd = 1500)
  )
  
  # Introduzir correlação leve (opcional)
  dados_educacao$ideb <- dados_educacao$ideb +
    0.0002 * (dados_educacao$gasto_aluno - 6000)
  
  # (2) Correlação de Pearson
  cor_ideb_gasto <- cor(dados_educacao$ideb, dados_educacao$gasto_aluno)
  print(cor_ideb_gasto) #0.3785594
  
  # (3) Regressão simples
  
  modelo <- lm(ideb ~ gasto_aluno, data = dados_educacao)
  
  summary(modelo) #cada $ adicional do gasto, espero incrementar o ideb em 0.0001938
                  #cada 1000$ adicional do gasto, o ideb em 0.1938
  
  confint(modelo)
  #                 2.5 %       97.5 %
  # (Intercept) 4.2293148316 4.4854018603
  # gasto_aluno 0.0001730422 0.0002146291
  
  # TODO: interpretar os coeficientes, valor-p e IC em texto.
  
  ggplot(dados_educacao, aes(x = gasto_aluno, y = ideb)) +
    geom_point(alpha = 0.8, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
    labs(title = "Relação entre gasto por aluno e IDEB",
         x = "Gasto por aluno",
         y = "IDEB") +
    theme_minimal()

#Visualizar o grafico  
  
plot(dados_educacao$gasto_aluno, dados_educacao$ideb, 
     main = "Relação entre gasto por aluno e IDEB", 
     xlab = "Gasto por aluno (R$)", ylab = "IDEB", pch = 19, col = rgb(0,0,1,0.3))
abline(modelo, col = "red", lwd = 2)

legend("topleft", legend = c("Observações", "Reta de regressão"), 
       col = c(rgb(0,0,1,0.5), "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2), bty = "n")

# Interpretação:

# O modelo sugere que maior gasto por aluno está associado a IDEB mais alto. 
# O coeficiente de inclinação é positivo e estatisticamente significativo, e 
# o intervalo de confiança confirma que o efeito é consistente e não inclui zero. 
# Embora o efeito seja de magnitude moderada (cerca de 0,2 pontos de IDEB por R$ 1000), 
# ele é relevante porque o IDEB varia em uma escala relativamente estreita (tipicamente entre 0 e 10).

#----------------------------------------------------
 
# Exercício 9 – 
 
  prop_partido <- rbeta(N, shape1 = 10, shape2 = 15)
  
  set.seed(123)
  
  N <- 5000
  
  dados_tse <- data.frame(
    id_mun       = 1:N,
    prop_partido = rbeta(N, shape1 = 10, shape2 = 15)
  )
  
  n_amostra <- 600
  n_rep     <- 1000
  
  # vetor para guardar as proporções amostrais
  p_hat_vec <- numeric(n_rep)
  
  for (r in 1:n_rep) {
    # (3) sortear um município
     id_escolhido <- sample(dados_tse$id_mun, 1)
     p_mun <- dados_tse$prop_partido[dados_tse$id_mun == id_escolhido]
    #
    # # sortear 600 eleitores (Bernoulli)
     votos <- rbinom(n_amostra, size = 1, prob = p_mun)
     p_hat_vec[r] <- mean(votos)
  }
  print(p_hat_vec[r]) #0.395
  
  # (4) margem de erro (pior caso)
  ME <- 1.96 * sqrt(0.25 / n_amostra)
  print(ME) #0.04000833
  
    hist(
    p_hat_vec,
    breaks = 30,
    col    = "skyblue",
    border = "white",
    main   = "Histograma das proporções amostrais (p_hat)",
    xlab   = "Proporção amostral",
    ylab   = "Frequência"
  )
  
# Discutir a relação com a margem de erro.
    
# A margem de erro é uma aproximação teórica que indica o quanto a proporção amostral pode variar em 
# torno da proporção verdadeira.
    
# O histograma mostra a variabilidade empírica das estimativas, confirmando que a maior parte das amostras 
# realmente cai dentro desse intervalo.
    
# Em pesquisas eleitorais, isso significa que, ao entrevistar 600 pessoas, a proporção estimada para um 
# partido pode oscilar até cerca de 4 pontos percentuais para cima ou para baixo, apenas por efeito do sorteio 
# da amostra.    

#----------------------------------------------------
 
 # Exercício 10 – 
    
    renda_media <- rnorm(N, mean = 2500, sd = 600)
    
    prop_partido <- plogis(
      -1 + 0.0006 * renda_media + rnorm(N, 0, 0.3)
    )
 
    set.seed(123)
    
    N <- 5000
    
    dados_tse <- data.frame(
      id_mun      = 1:N,
      renda_media = rnorm(N, mean = 2500, sd = 600)
    )
    
    dados_tse$prop_partido <- plogis(
      -1 + 0.0006 * dados_tse$renda_media + rnorm(N, 0, 0.3)
    )
    
    n_mun_amostra <- 300
    n_eleitores   <- 400
    n_rep         <- 500
    
    coef_angular <- numeric(n_rep)
    
    for (r in 1:n_rep) {
      # (4) Sortear municípios
      mun_sorteados <- sample(dados_tse$id_mun, n_mun_amostra)
      base_pesq <- dados_tse[dados_tse$id_mun %in% mun_sorteados, ]
      
      # # Para cada município, simular votos e calcular proporção p_hat
      p_hat <- numeric(n_mun_amostra)
      #
      for (i in seq_len(n_mun_amostra)) {
       p_true <- base_pesq$prop_partido[i]
       votos  <- rbinom(n_eleitores, size = 1, prob = p_true)
       p_hat[i] <- mean(votos)
       }
      
       base_pesq$p_hat <- p_hat
      
       # (5) Ajustar regressão simples
      modelo <- lm(p_hat ~ renda_media, data = base_pesq)
      coef_angular[r] <- coef(modelo)[2]
    }
    
    # TODO: produzir um histograma de coef_angular,
    # e discutir o que representa essa distribuição.
    
    hist(
      coef_angular,
      breaks = 30,
      col    = "skyblue",
      border = "white",
      main   = "Histograma",
      xlab   = "x",
      ylab   = "y"
    )
 #----------------------------------------------------
 
 # Exercício 11 – 
    
    renda_media <- rnorm(N, mean = 2500, sd = 600)
    
    prop_partido <- plogis(
      -1 + 0.0006 * renda_media + rnorm(N, 0, 0.3)
    )
    set.seed(123)
    
    N <- 5000
    
    dados_tse <- data.frame(
      id_mun      = 1:N,
      renda_media = rnorm(N, mean = 2500, sd = 600)
    )
    
    dados_tse$prop_partido <- plogis(
      -1 + 0.0006 * dados_tse$renda_media + rnorm(N, 0, 0.3)
    )
    
    n_mun_amostra <- 300
    n_eleitores   <- 400
    n_rep         <- 500
    
    coef_angular <- numeric(n_rep)
    
    for (r in 1:n_rep) { 
      mun_sorteados <- sample(dados_tse$id_mun, n_mun_amostra) 
      base_pesq <- dados_tse[dados_tse$id_mun %in% mun_sorteados, ] 
      p_hat <- numeric(n_mun_amostra) 
      for (i in seq_len(n_mun_amostra)) { 
        p_true <- base_pesq$prop_partido[i] 
        votos <- rbinom(n_eleitores, size = 1, prob = p_true) 
        p_hat[i] <- mean(votos) } 
      base_pesq$p_hat <- p_hat 
      modelo <- lm(p_hat ~ renda_media, data = base_pesq) 
      coef_angular[r] <- coef(modelo)[2] 
      }
    
    # TODO: produzir um histograma de coef_angular,
    
    hist(coef_angular, 
         breaks = 30, 
         col = "skyblue", 
         border = "white", 
         main = "Distribuição dos coeficientes angulares", 
         xlab = "Coeficiente angular (inclinação da regressão)", 
         ylab = "Frequência") 
    
    # Adicionar linha da média dos coeficientes 
    abline(v = mean(coef_angular), col = "red", lwd = 2) 
    
    # Legenda 
    legend("topright", legend = c("Média dos coeficientes"), col = "red", lwd = 2, bty = "n")
    
#----------------------------------------------------
 
sessionInfo()
 