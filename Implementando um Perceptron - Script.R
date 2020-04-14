# Carregando pacote
library(tidyverse)

# Carregando dados de iris
irisF <- iris %>% as_tibble()

# Rápida exploração dos dados
Hmisc::describe(irisF)

irisF %>% 
  pivot_longer(-Species, 
               names_to = "Variavel", 
               values_to = "Valor") %>% 
  ggplot(aes(x = Variavel, y = Valor, fill = Species)) + 
  geom_boxplot()


# Tranformando os dados
irisF <- irisF %>% filter(Species != "virginica") %>% 
  mutate(Species = ifelse(Species == "versicolor", 1, 0))

x <- irisF[, 1:4]
y <- irisF$Species

# Amostrando teste e treino
trn <- sample(1:100, 80)
x_trn <- x[trn, ]
x_tst <- x[-trn, ]
y_trn <- y[trn]
y_tst <- y[-trn]

# Contruindo funções
perceptron <- function(dados_x, pesos, vies){
  
  # Calcula resultado
  res <- as.matrix(dados_x) %*% pesos + vies
  
  # Passa pela função ativação
  act <- 1/(1 + exp(res))
  return(act)
}

custo <- function(y_real, y_prob){
  
  loss <- -(y_real*log(y_prob) + (1-y_real)*log(1-y_prob))
  
  cost <- mean(loss)
  
  return(cost)
}

treina_rn <- function(x_trn, x_tst, y_trn, y_tst, learn_rate, epochs){
  
  # Inicia variáveis
  w <- rep(0, ncol(x_trn))
  b <- 0
  costs <- tibble(trn = rep(0, epochs),
                  tst = rep(0, epochs))
  
  # Ajusta pesos
  for(i in 1:epochs){
    
    # Calcula resultados
    pred_trn <- perceptron(x_trn, w, b)
    pred_tst <- perceptron(x_tst, w, b)
    
    # Calcula e imprime custo
    costs$trn[i] <- custo(y_trn, pred_trn)
    costs$tst[i] <- custo(y_tst, pred_tst)
    print(sprintf("Custos Treino/Teste: %f / %f", costs$trn[i], costs$tst[i]))
    
    # Ajusta pesos
    dif <- y_trn - pred_trn
    for(i in 1:4) w[i] <- w[i] - learn_rate*mean(x_trn[[i]] * dif[, 1])
    b <- b - learn_rate*mean(dif)
  }
  
  return(list(pesos = w,
              vies = b,
              custos = costs))
}

# Treinando a rede
botanista <- treina_rn(x_trn, x_tst, y_trn, y_tst, 0.1, 300)

# Visualizando perda
botanista$custos %>% 
  mutate(iter = 1:nrow(.)) %>% 
  pivot_longer(-iter, names_to = "dados", values_to = "custo") %>% 
  ggplot(aes(x = iter, y = custo, color = dados)) + 
  geom_line()

# Verificando separação
irisF <- irisF %>% mutate(prob = perceptron(x, botanista$pesos, botanista$vies),
                          pred = ifelse(prob > 0.5, 1, 0))

ggplot(irisF, aes(x = Species, y = prob)) + 
  geom_point() + 
  geom_hline(yintercept = 0.5)

# Testando a rede
table(irisF$Species[trn], irisF$pred[trn])
table(irisF$Species[-trn], irisF$pred[-trn])
