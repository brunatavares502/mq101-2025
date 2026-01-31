# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #01 — TEMPLATE DE SCRIPT (R)
# ------------------------------------------------------------

# Nome: Bruna Tavares Leite Silva
# Matrícula/RA:23202510328
# Turma: Métodos Quantitativos
# Data: 24/09/2025
# Descrição: Respostas da Lista #01 (HOPR Partes I–II, caps. 1–8)
# ============================================================


# ===================== 0) PREPARAÇÃO =========================
# (Obrigatório, sem pontuação — verificação do ambiente)

# Versão do R
R.version.string

# Se estiver no RStudio, esta chamada retorna informações do RStudio (pode falhar fora do RStudio).
# tryCatch(RStudio.Version()$mode, error = function(e) "RStudio não detectado")

# Reprodutibilidade global para esta lista (você pode mudar, mas mantenha constante)
set.seed(202501)

install.packages("ggplot2")

# Carregamento opcional do ggplot2 (apenas se desejar usar ggplot para gráficos)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
} else {
  message("Pacote 'ggplot2' não encontrado. Usando gráficos base R (hist).")
}

# Dica: escolha uma pasta de trabalho, se necessário (descomente e ajuste):
# setwd(\"~/caminho/para/sua/pasta\")

# ============================================================
# ===================== EXERCÍCIO 1 (10 pts) =================

# R como calculadora — ordem das operações (HOPR cap. 1)
# Objetivo: executar expressões e entender a ordem das operações.
# ENTREGA: cole os resultados no PDF e escreva, em 3–5 linhas, como os parênteses afetam o resultado.

# TODO: Execute as linhas a seguir e observe os resultados.

10+2
(10 + 2) * 3
((10 + 2) * 3 - 6) / 3

# Minha resposta: Ao observar as três expressões matemáticas, foi possível perceber que o uso de
# parênteses tem prioridade na realização dos cálculos; em seguida, vêm a
# multiplicação/ divisão; e, por último, a adição/ subtração. A resposta de
# cada expressão matemática é 12, 36 e 10, respectivamente. 
# Quanto à relação com o uso de indicadores em PP/CS, é fundamental que o uso correto de parênteses
# garanta que os resultados obtidos representem corretamente o fenômeno social analisado com base nos indicadores.

# ============================================================
# ===================== EXERCÍCIO 2 (10 pts) =================

# Objetos e nomeação (HOPR cap. 1)
# ENTREGA: explique em 2–4 linhas a diferença entre 'Name' e 'name'.

x <- 1:6
Name <- 1
name <- 0
Name + 1
name +1

# Minha Resposta: nenhum deles (Name ou name) é um comando do R; ambos são nomes de variáveis.
# O R diferencia letras maiúsculas de minúsculas, portanto Name e name são
# tratados como objetos distintos (Name ≠ name). Ao serem usados em operações,
# produzem resultados diferentes conforme o valor atribuído a cada um.

# ============================================================
# ===================== EXERCÍCIO 3 (10 pts) =================
# Sorteio (sample) e reprodutibilidade (HOPR cap. 1–2)
# ENTREGA: descreva o papel de set.seed() em 3–5 linhas.

# TODO: Compare com e sem set.seed()

#1 sem set.seed() 

dice <- 1:6
sample(dice, size = 2, replace = TRUE)
#retorno 4 2 (primeira vez)
#retorno 16  (segunda vez)


#2 com set.seed

set.seed(123)
dice <- 1:6
sample(dice, size = 2, replace = TRUE)
#retorno 3 6 (o resultado é o mesmo se clicar "run" varias vezes)


# Minha resposta: Ao executar o código sem o uso de set.seed(), os resultados do comando sample()
# variam a cada execução, pois o sorteio é aleatório. Ao utilizar set.seed() com
# o mesmo valor (123), o resultado se mantém igual em diferentes execuções, garantindo
# a reprodutibilidade do script. Isso permite que outras pessoas obtenham os mesmos
# resultados ao rodar o código. O comando sample() funciona como um sorteio aleatório.

# ============================================================
# ===================== EXERCÍCIO 4 (10 pts) =================
# Sua primeira função (HOPR cap. 1)
# ENTREGA: explique o que faz cada linha da função em 4–6 linhas.
# BÔNUS: implemente 'soma3()' (sorteia 3 números entre 1 e 6 e retorna a soma).

# TODO: Defina a função e teste:


roll2 <- function(bones = 1:6) {   # Sorteia dois valores do vetor 'bones' com reposição e soma.
                                   # 'bones' por padrão é 1:6 (um dado comum).
  set.seed(123)
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll2() #resultado apresentado no console = 5 e com set.seed(123) o retorno deu 9
roll2(1:20) # retorno = 34

# Minha resposta: fiz teste de executar o sample para receber os 2 numeros sorteados da série "bones" (elementos - 1 ate 6). 
# Para garantir a mesma informação de todos, uso o comando set.seed(123). Em seguinte, criei objeto "dice" que equivale a 2 dados
# obtidos do comando sample. Depois usei comando sum(dice) para obter o resultado da soma de 2 elementos sorteados. Criei objeto
# roll2 para executar essa função toda.

# TODO (BÔNUS): implementar soma3()


soma3 <- function(elementos = 1:6) {
  set.seed(123)
  soma <- sample(elementos, size = 3, replace = TRUE)
  sum(soma)
}
soma3() #retorno deu 12.

# ============================================================
# ===================== EXERCÍCIO 5 (10 pts) =================
# Ajuda e exemplos (HOPR cap. 2)
# ENTREGA: resuma argumentos de sample() e como consultar ajuda (4–6 linhas).

# TODO: Consulte a ajuda e rode exemplos
# ?sample
# example(sample)

# Dica: leia os argumentos 'x', 'size', 'replace', 'prob', etc.

?sample
# sample(x, size, replace = FALSE, prob = NULL). 
# x lê-se objeto, size lê-se o numero de elementos dentro de
#objeto (variavel, vetor de elementos), replace é uma função que aceita repetição de mesmo valor de um dos 
# elementos do objeto.Se não usar valor percentual o comando lê-se que é o mesmo valor percentual para cada elemento do vetor.
# para buscar help, basta digitar ?sample e os detalhes explicativos são apresentados na aba "help" no canto inferior direito

# ============================================================
# ===================== EXERCÍCIO 6 (15 pts) =================
# Simulação e histograma (HOPR cap. 1–2)
# ENTREGA: histograma, média, desvio-padrão; interpretação (4–6 linhas).
# Dica: você pode salvar o gráfico com png()... dev.off()

set.seed(42)

somas <- replicate(10000, roll2()) #o programa R busca a função roll2() na biblioteca apos execução da atividade nº4

length(somas)  #length mostra o nº de elementos dentro do vetor "somas", ou seja, somas tem 10000 elementos

hist(somas, main = "Soma de dois dados (10.000 lançamentos)", xlab = "Soma")

mean(somas);  # a media deu 9

sd(somas) # o desvio padrao deu 0

# (Opcional) Salvar figura:
# png(\"ex6_hist_somas.png\", width = 900, height = 600)
# hist(somas, main = \"Soma de dois dados (10.000 lançamentos)\", xlab = \"Soma\")
# dev.off()

png("histograma.png", width = 800, height = 600, res = 100)
hist(somas, main = "Soma de dois dados (10.000 lançamentos)", xlab = "Soma")
dev.off()
getwd() # mostra o caminho da pasta onde o R está salvando seus arquivos


#Anotação da Bruna: somas <- replicate(10000, roll2()) --> executa a função roll2() 
# (que lança dois dados e soma) 10.000 vezes.
#length(): mostra o número de elementos do vetor.Ou seja, somas realmente tem 10.000 resultados.
# hist(): cria um histograma da distribuição dos valores em somas. main = "..." define o título do gráfico.
# xlab = "..." define o nome do eixo X.
# O histograma vai mostrar quais somas aparecem mais vezes (a forma da distribuição).
# replicate() → gera as 10.000 simulações.
#length() → confere o tamanho do vetor.
#hist() → visualiza a distribuição.
#mean() → calcula a média.
#sd() → calcula a variabilidade.

# Extensão (opcional, sem pontos extras): dado viciado favorecendo o 6
prob_vies <- c(rep(1/8, 5), 3/8)
somas_vies <- replicate(10000, sum(sample(1:6, size = 2, replace = TRUE, prob = prob_vies)))
hist(somas_vies, main = "Dado viciado (6 favorecido)", xlab = "Soma")
png("histograma1.png", width = 800, height = 600, res = 100)  # abre o arquivo
hist(somas_vies, main = "Dado viciado (6 favorecido)", xlab = "Soma")  # faz o gráfico
dev.off()  
getwd()

# ============================================================
# ===================== EXERCÍCIO 7 (10 pts) =================
# Tipos básicos (HOPR cap. 3)
# ENTREGA: explique o que 'str()' revela sobre cada tipo e cite um uso prático.

dbl <- c(1.5, 2.0)            # numéricos (double)
int <- c(1L, 2L)              # inteiros
chr <- c("saude", "educacao") # texto
lgl <- c(TRUE, FALSE)         # lógico

str(list(dbl = dbl, int = int, chr = chr, lgl = lgl))

#Bruna: A saída foi assim: 
#List of 4
#$ dbl: num [1:2] 1.5 2
#$ int: int [1:2] 1 2
#$ chr: chr [1:2] "saude" "educacao"
#$ lgl: logi [1:2] TRUE FALSE
# str() analisa qual estrutura, tipo e primeiros valores de qualquer objeto no R. Dá retorno a qual objeto se
#classifica. É super útil para conferir se seus dados estão no formato certo antes de aplicar funções estatísticas ou modelos.

# ============================================================
# ===================== EXERCÍCIO 8 (10 pts) =================
# Data.frame (baralho) e mini-base municipal (HOPR cap. 3)
# ENTREGA: nº de linhas/colunas e breve interpretação do summary().

# Parte 1 - Baralho

faces <- c("ace","two","three","four","five","six","seven",
           "eight","nine","ten","jack","queen","king")
suits <- c("spades","hearts","diamonds","clubs")
deck  <- data.frame(
  face  = rep(faces, times = 4), #Bruna: repete a sequência de 13 faces 4 vezes (total 52).
  suit  = rep(suits, each = 13), #Bruna: repete cada naipe 13 vezes seguidas (spades 13 linhas, depois hearts 13, etc.).
  value = rep(1:13, times = 4) #valores de 1 a 13 repetidos 4 vezes.
)
#deck segue função de data.frame que é montar uma tabela. Nesse caso so tem 3 colunas - face, suit e value

nrow(deck) #numero de linhas = 52
ncol(deck) #numero de colunas = 3
head(deck, 10) #Mostra as 10 primeiras linhas (útil para inspecionar).

#Bruna: Dica de checagem: stopifnot(nrow(deck)==52, ncol(deck)==3) e resultado: Se tudo estiver correto, não aparece nada

# Parte 2 - Mini-base municipal (dados simulados)

set.seed(2025)
municipios <- paste0("Mun_", sprintf("%02d", 1:10)) #Bruna: cria sequencia “Mun_01”, “Mun_02”, …, “Mun_10”.
dados_munic <- data.frame(
  municipio        = municipios,
  gasto_saude_pc   = round(runif(10, 200, 1200), 2), 
  #10 valores uniformes entre 200 e 1200 (todos os valores no intervalo têm a mesma chance);
  taxa_evasao      = round(runif(10, 0.00, 0.20), 3), 
  #10 valores uniformes entre 0 e 0,20 (0%–20%); 3 casas decimais.
  taxa_desemprego  = round(rnorm(10, 0.12, 0.03), 3) 
  #10 valores de uma normal (rnorm) com média 0,12 (12%) e desvio-padrão 0,03 
)
head(dados_munic); #por padrão mostra as 6 primeiras linhas. Se eu quiser ver 10 linhas digita head(dados_minic, 10)
summary(dados_munic) #mostra resultados como mediana, media, primeiro e terceiro elemento e valor maximo de cada coluna
                      # gasto_saude_pc,  taxa_evasao e  taxa_desemprego

nrow(dados_munic) # nº de linhas = 10
ncol(dados_munic) # nº de colunas = 4

#data.frame monta a tabela com 10 linhas (uma por município) e 4 colunas (municipio, gasto, taxa ev e tava des)

# ============================================================
# ===================== EXERCÍCIO 9 (10 pts) =================
# Seleção e filtros (HOPR cap. 4)
# ENTREGA: descreva os retornos e quantos municípios têm taxa_evasao > 0.10.

#Parte 1 - Baralhos

deck[1, ] #Retorna a linha 1 inteira
deck[c(1,3,5), c("face","suit")] #Retorna 3 linhas x 2 colunas: apenas face e suit das linhas 1, 3 e 5.
deck[-(1:48),] #Retorna as 4 últimas linhas, lembrando que são 52 linhas, por isso sobra 4 linhas apos
#retirada de 48 linhas com uso de subtração

subset_hearts <- deck[ deck$suit == "hearts", ] #faz filtro de somente de hearts 



#Parte 2 - Base Municipios

evaz_alta <- dados_munic[dados_munic$taxa_evasao > 0.10,] #filtra somente "Mun_" cuja a taxa_evasaoé >0.10
evaz_alta
nrow(evaz_alta) #somente 4 Municipios tem taxa_evasao > 0.10

# ============================================================
# ===================== EXERCÍCIO 10 (10 pts) ================
# Modificando valores e NA (HOPR cap. 5)
# ENTREGA: explique o efeito de na.rm = TRUE e quando usá-lo.

# Modificando valores (ases = 14)

deck2 <- deck # $ extrai coluna value da matriz deck para criar um novo vetor
deck2$value[c(13, 26, 39, 52)] <- 14 #trocando os elementos das posições 13, 26, 39 e 52 por 14
print(deck2) #so para ver como seria o novo vetor e conferi que as posições sofreram mofificações. 
#Realmente as ases nao sofreram modificações e sim KINGs
head(deck2, 13) #retorna matriz ate 13ª linha

# Valores ausentes
vals <- c(NA, 1:5) #vetor de 6 elementos, sendo que um deles é NA, valor ausente
mean(vals)                 # retorna o valor medio = NA
mean(vals, na.rm = TRUE)   # ignora NA e retorna o valor medio = 3 (15/5=3)

dados_m2 <- dados_munic
dados_m2$taxa_evasao[3] <- NA # 3º elemento da coluna taxa_evasão foi trocado por NA
dados_m2$gasto_saude_pc[7] <- NA # 7º elemento da coluna gasto_saude_pc foi trocado por NA

mean(dados_m2$taxa_evasao)             # NA --> qualquer NA no vetor faz o resultado da media tornar igual a NA
mean(dados_m2$taxa_evasao, na.rm=TRUE) # média sem NA e retorna 0.09633333

# ============================================================
# ========== EXERCÍCIO 11 (OPCIONAL, até 10 pts) ============
# Funções que "guardam estado" (HOPR cap. 6)
# ENTREGA: explique o conceito e dê exemplo análogo em PP/CS.

setup <- function(deck_init) {
  DECK <- deck_init  # cópia interna (estado)

  DEAL <- function() {
    # Devolve a primeira carta e atualiza o baralho interno removendo-a.
    card <- DECK[1, , drop = FALSE] #1ª linha, todas as colunas mantendo matriz e nao como vetor
    DECK <<- DECK[-1, , drop = FALSE] 
    return(card)
  }

  SHUFFLE <- function() {
    # Reembaralha o baralho interno
    idx <- sample(seq_len(nrow(deck_init)), size = nrow(deck_init))
    DECK <<- deck_init[idx, , drop = FALSE]
    invisible(NULL)
  }

  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
cards$deal(); cards$shuffle(); 

#Retorno do script todo:
#  face   suit value
#   1  ace spades     1
# face   suit value
#   2  two spades     2
# face   suit value
#  25 queen hearts    12


#Explicação sobre conceito:
# O script define uma função geradora de funções setup(), que:
# Cria um estado interno (DECK),
# Retorna funções internas (DEAL, SHUFFLE) que manipulam esse estado,
# Preservam memória entre as chamadas — mesmo depois de setup() já ter terminado.

#Analogia com o SUS:
#setup(): criação do SUS em 1988 — define princípios, base normativa e estrutura.
#DECK: conjunto de políticas, profissionais, recursos, fluxos administrativos (estado interno do sistema).
#DEAL(): execução de uma política. Por exemplo, distribuir vacinas, realizar consultas, ou implementar 
#o programa “Agora tem especialistas”.

#SHUFFLE(): reorganização do sistema — quando há mudanças de governo, portarias, descentralização, mas mantendo 
#a estrutura básica (a “memória institucional” do SUS).

#Mesmo quando muda o ministro ou a política específica, o SUS preserva parte do estado interno — 
#isso é comparável ao fechamento em R, onde a função interna continua acessando e alterando o mesmo DECK.

# ============================================================
# ================= EXERCÍCIO 12 (15 pts) ====================
# Mini-projeto integrador: Saúde
# ENTREGA: 2 histogramas, diferença de médias e interpretação (6–8 linhas).

set.seed(123)
pressao_saude <- function() {
  demanda <- sample(1:6, 1, TRUE)
  equipe  <- sample(1:6, 1, TRUE)
  insumos <- sample(1:6, 1, TRUE)
  demanda + equipe + insumos
}

#Parte 2 - Simulando 10.000 situações onde a pressão sobre o sistema de saúde é resultado de três fatores aleatórios
# demanda, equipe e insumos
# Cada fator pode variar de 1 a 6
# a pressao_saude é a soma de 3 fatores.

prs <- replicate(10000, pressao_saude())
hist(prs, main = "Pressão no sistema de saúde (simulada)", xlab = "Índice")
mean(prs);  # 10.4759
sd(prs)     # 2.97498
#em média, a pressão total esperada é 10,48 pontos, com desvio padrão de 2,97
# o que mostra moderada variabilidade entre as situações.

# Viés em demanda (favorece 6)
prob_demanda <- c(rep(1/8, 5), 3/8)
pressao_vies <- function() {
  demanda <- sample(1:6, 1, TRUE, prob = prob_demanda)
  equipe  <- sample(1:6, 1, TRUE)
  insumos <- sample(1:6, 1, TRUE)
  demanda + equipe + insumos
}
prs_bias <- replicate(10000, pressao_vies())
hist(prs_bias, main = "Pressão no sistema de saúde (com viés)", xlab = "Índice")

mean(prs_bias) - mean(prs) #0.6377



# (Opcional) Salvar figuras:
png("ex12_hist_semvies.png", width = 900, height = 600); hist(prs); dev.off()
png("ex12_hist_comvies.png", width = 900, height = 600); hist(prs_bias); dev.off()

#Interpretação
# O experimento computacional simula a pressão sobre o sistema de saúde como resultado de três componentes — demanda, equipe e insumos.
# No cenário neutro, a média da pressão total foi de 10,48. Ao introduzir um viés de aumento na demanda, a média subiu para 11,12,
# representando um acréscimo médio de 0,64 ponto (aproximadamente 6,1%). Essa diferença evidencia que pequenas alterações estruturais
# na demanda podem gerar aumento expressivo na carga sobre o sistema, demonstrando a importância de políticas públicas capazes de
# equilibrar recursos humanos e materiais diante de variações na procura por serviços. Em termos de governança, o resultado 
# reforça a necessidade de planejamento estatal que antecipe sobrecargas e assegure equidade no acesso.

#---------------------------------------------
sessionInfo()