# Nome: Leonardo de Salles Amaral - Trabalho de Graduação A
# Códigos TCC - Modelagem da taxa de mortes da COVID 19 por país

# ----------------------------------------------------

# Liberando os pacotes
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(gamlss)

# Visualizando o banco de dados
View(BaseTGFiltroFinal)
# ----------------------- DESCRITIVA ------------------

#Selecionando as covariaveis para nossa análise descritiva
select_dataset<-dplyr::select(BaseTGFiltroFinal,	
                              Mortes1MilhãoDePop,
                              TxMortCard,
                              TaxaMortalidadeInfantil,
                              TaxaFertilidade	,
                              TaxaPopUrb,
                              TaxaPopRural,
                              IDH,
                              ExpectatVida,
                              PIB)

# Renomeando as variáveis
select_dataset <- dplyr::rename(select_dataset,
                                Mortes = Mortes1MilhãoDePop,	
                                TMC = TxMortCard,	
                                TMI = TaxaMortalidadeInfantil,
                                TF = TaxaFertilidade,
                                TU = TaxaPopUrb,
                                TR = TaxaPopRural,
                                IDH = IDH,
                                EV = ExpectatVida,
                                PIB = PIB)


# Calcule a matriz de correlação
matriz_cor <- cor(select_dataset)
print(matriz_cor)
ggcorr(select_dataset, label=T)

# ---------------------------------------- Variável  Resposta -------

# Boxplot
p <- ggplot(BaseTGFiltroFinal, aes(y = Mortes1MilhãoDePop)) +
  geom_boxplot(fill = "darkblue", color = "black") +
  labs(y = "Mortalidade por COVID-19",
       x = "") +
  theme_minimal()

print(p)
summary(BaseTGFiltroFinal$Mortes1MilhãoDePop)

# Histograma
hist(BaseTGFiltroFinal$Mortes1MilhãoDePop, breaks = 10, freq = TRUE, col = "darkblue", main = "",
     xlab = "Mortalidade por COVID-19", ylab = "Frequência")

# ------------------------- Variáveis em estudo --------------

ggplot(BaseTGFiltroFinal, aes(x = TxMortCard, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Taxa de Mortalidade Cardíaca",
    y = "Mortalidade por COVID-19",
  )

# ------------------------------------------------------

ggplot(BaseTGFiltroFinal, aes(x = TaxaMortalidadeInfantil, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Taxa de Mortalidade Infantil",
    y = "Mortes por 1 Milhão de População"
  )

# -------------------------------------------------------

ggplot(BaseTGFiltroFinal, aes(x = TaxaFertilidade, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Taxa de fertilidade",
    y = "Mortes por 1 Milhão de População"
  )
# --------------------------------------------------------

ggplot(BaseTGFiltroFinal, aes(x = TaxaPopRural, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Porcentagem da pop. rural",
    y = "Mortes por 1 Milhão de População"
  )
#--------------------------------------------------------

ggplot(BaseTGFiltroFinal, aes(x = TaxaPopUrb, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Porcentagem da pop. urbana",
    y = "Mortes por 1 Milhão de População"
  )
# -------------------------------------------------------

ggplot(BaseTGFiltroFinal, aes(x = IDH, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "IDH",
    y = "Mortes por 1 Milhão de População"
  )

# ----------------------------------------------------------

ggplot(BaseTGFiltroFinal, aes(x = ExpectatVida, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Esperança de vida ao nascer",
    y = "Mortes por 1 Milhão de População"
  )

# -------------------------------------------------------

ggplot(BaseTGFiltroFinal, aes(x = PIB, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "PIB Per Capita",
    y = "Mortes por 1 Milhão de População"
  )

# -------------------------------------------------------------

# ------------------- DENSIDADES DE PROBABILIDADE  -----------

# LIBERANDO OS PACOTES
library(ggplot2)
library(actuar)
#------------------------------- GAMA ------------------------

# Função para calcular shape e scale dados mu e sigma2 para Gamma
calc_parameters_gamma <- function(mu, sigma2) {
  shape <- (mu^2) / sigma2
  scale <- sigma2 / mu
  list(shape = shape, scale = scale)
}

# Parâmetros fornecidos
params1 <- calc_parameters_gamma(mu = 10, sigma2 = 4)
params2 <- calc_parameters_gamma(mu = 20, sigma2 = 4)
params3 <- calc_parameters_gamma(mu = 10, sigma2 = 25)

# Dados para o gráfico
x <- seq(0, 40, length.out = 100)
pdf_values1 <- dgamma(x, shape = params1$shape, scale = params1$scale)
pdf_values2 <- dgamma(x, shape = params2$shape, scale = params2$scale)
pdf_values3 <- dgamma(x, shape = params3$shape, scale = params3$scale)

# Criar o gráfico
data <- data.frame(
  x = x,
  pdf_values = c(pdf_values1, pdf_values2, pdf_values3),
  Distribution = factor(rep(c("Média = 10\nVariância = 4", 
                              "Média = 20\nVariância = 4", 
                              "Média = 10\nVariância = 25"), each = length(x)))
)

ggplot(data) +
  geom_line(aes(x = x, y = pdf_values, linetype = Distribution), size = 1) +
  labs(
    x = "Valores da variável",
    y = "f(x)",
    linetype = "Legenda:"
  ) +
  scale_linetype_manual(values = c("Média = 10\nVariância = 4" = "solid",
                                   "Média = 20\nVariância = 4" = "dashed",
                                   "Média = 10\nVariância = 25" = "dotted")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Coloca a legenda na parte inferior
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.box = "vertical") # Garante que a legenda esteja em uma caixa vertical
    
# ------------------------- WEIBULL ---------------


# Função para calcular lambda e k dados mu e sigma2 para Weibull
calc_parameters <- function(mu, sigma2) {
  objective_function <- function(params) {
    lambda <- params[1]
    k <- params[2]
    
    mean_k <- lambda * gamma(1 + 1 / k)
    var_k <- lambda^2 * (gamma(1 + 2 / k) - gamma(1 + 1 / k)^2)
    
    # Função objetivo para minimizar a diferença
    abs(mu - mean_k) + abs(sigma2 - var_k)
  }
  
  result <- optim(par = c(mu, 1), fn = objective_function, method = "L-BFGS-B", lower = c(0.1, 0.1))
  lambda <- result$par[1]
  k <- result$par[2]
  
  list(lambda = lambda, k = k)
}

# Parâmetros fornecidos
params1 <- calc_parameters(mu = 10, sigma2 = 4)
params2 <- calc_parameters(mu = 20, sigma2 = 4)
params3 <- calc_parameters(mu = 10, sigma2 = 25)

# Dados para o gráfico
x <- seq(0, 40, length.out = 100)
pdf_values1 <- dweibull(x, shape = params1$k, scale = params1$lambda)
pdf_values2 <- dweibull(x, shape = params2$k, scale = params2$lambda)
pdf_values3 <- dweibull(x, shape = params3$k, scale = params3$lambda)

# Criar o gráfico
data <- data.frame(
  x = x,
  pdf_values = c(pdf_values1, pdf_values2, pdf_values3),
  Distribution = factor(rep(c("Média = 10\nVariância = 4", 
                              "Média = 20\nVariância = 4", 
                              "Média = 10\nVariância = 25"), each = length(x)))
)

ggplot(data) +
  geom_line(aes(x = x, y = pdf_values, linetype = Distribution), color = "blue", size = 1.2) +
  scale_linetype_manual(values = c("Média = 10\nVariância = 4" = "solid",
                                   "Média = 20\nVariância = 4" = "dashed",
                                   "Média = 10\nVariância = 25" = "dotted")) +
  labs(
    x = "Valores da variável",
    y = "f(x)",
    linetype = "Legenda"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Coloca a legenda na parte inferior
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# ------------------ SOBREPOSIÇÃO ------------

# Função para calcular shape e scale dados mu e sigma2 para Gamma
calc_parameters_gamma <- function(mu, sigma2) {
  shape <- (mu^2) / sigma2
  scale <- sigma2 / mu
  list(shape = shape, scale = scale)
}

# Parâmetros
params_weibull <- list(
  calc_parameters_weibull(mu = 10, sigma2 = 4),
  calc_parameters_weibull(mu = 20, sigma2 = 4),
  calc_parameters_weibull(mu = 10, sigma2 = 25)
)

params_gamma <- list(
  calc_parameters_gamma(mu = 10, sigma2 = 4),
  calc_parameters_gamma(mu = 20, sigma2 = 4),
  calc_parameters_gamma(mu = 10, sigma2 = 25)
)

x <- seq(0, 40, length.out = 100)
data <- data.frame(
  x = rep(x, 6),
  pdf_values = c(
    dweibull(x, shape = params_weibull[[1]]$k, scale = params_weibull[[1]]$lambda),
    dweibull(x, shape = params_weibull[[2]]$k, scale = params_weibull[[2]]$lambda),
    dweibull(x, shape = params_weibull[[3]]$k, scale = params_weibull[[3]]$lambda),
    dgamma(x, shape = params_gamma[[1]]$shape, scale = params_gamma[[1]]$scale),
    dgamma(x, shape = params_gamma[[2]]$shape, scale = params_gamma[[2]]$scale),
    dgamma(x, shape = params_gamma[[3]]$shape, scale = params_gamma[[3]]$scale)
  ),
  Distribution = factor(rep(c(
    "Weibull: Média = 10\nVariância = 4", 
    "Weibull: Média = 20\nVariância = 4", 
    "Weibull: Média = 10\nVariância = 25",
    "Gamma: Média = 10\nVariância = 4",
    "Gamma: Média = 20\nVariância = 4",
    "Gamma: Média = 10\nVariância = 25"
  ), each = length(x)))
)

ggplot(data) +
  geom_area(aes(x = x, y = pdf_values, fill = Distribution), alpha = 0.5) +
  geom_line(aes(x = x, y = pdf_values, color = Distribution), size = 1) +
  scale_fill_manual(name = "Distribuição", values = c(
    "Weibull: Média = 10\nVariância = 4" = "blue",
    "Weibull: Média = 20\nVariância = 4" = "blue",
    "Weibull: Média = 10\nVariância = 25" = "blue",
    "Gamma: Média = 10\nVariância = 4" = "black",
    "Gamma: Média = 20\nVariância = 4" = "black",
    "Gamma: Média = 10\nVariância = 25" = "black"
  )) +
  scale_color_manual(name = "Distribuição", values = c(
    "Weibull: Média = 10\nVariância = 4" = "blue",
    "Weibull: Média = 20\nVariância = 4" = "blue",
    "Weibull: Média = 10\nVariância = 25" = "blue",
    "Gamma: Média = 10\nVariância = 4" = "black",
    "Gamma: Média = 20\nVariância = 4" = "black",
    "Gamma: Média = 10\nVariância = 25" = "black"
  )) +
  labs(
    x = "Valores da variável",
    y = "f(x)",
    fill = "Legenda",
    color = "Legenda"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(), # Fundo do gráfico sem preenchimento
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# ------------------ MAPA MUNDI ------------
  
# liberando os pacotes necessários
library(ggplot2)
library(maps)
library(dplyr)
library(viridis)
library(RColorBrewer)


world_map <- map_data("world")
countries <- c( "Afghanistan", "Albania", "Algeria", "Angola", "Argentina", 
                "Armenia", "Australia", "Austria", "Azerbaijan", "Bahrain", 
                "Bangladesh", "Barbados", "Belarus", "Belgium", "Benin", 
                "Bolivia", "Botswana", "Brazil", "Bulgaria", "Burundi", 
                "Cambodia", "Cameroon", "Canada", "Chad", "Chile", "China", 
                "Colombia", "Comoros", "Congo", "Croatia", "Cyprus", 
                "Denmark", "Djibouti", "Ecuador", "Egypt", "Estonia", 
                "Eswatini", "Ethiopia", "Finland", "France", "Gabon", 
                "Gambia", "Georgia", "Germany", "Ghana", "Guatemala", 
                "Guinea", "Guinea-Bissau", "Haiti", "Honduras", "Hungary", 
                "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", 
                "Israel", "Italy", "Jamaica", "Japan", "Jordan", 
                "Kazakhstan", "Kenya", "Kuwait", "Kyrgyzstan", "Laos", 
                "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", 
                "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Mali", 
                "Malta", "Mauritania", "Mauritius", "Mexico", "Moldova", 
                "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", 
                "Namibia", "Nepal", "Netherlands", "Nicaragua", "Niger", 
                "Nigeria", "Norway", "Oman", "Panama", "Paraguay", "Peru", 
                "Philippines", "Poland", "Portugal", "Qatar", "Romania", 
                "Russia", "Rwanda", "Senegal", "Serbia", "Seychelles", 
                "Singapore", "Slovakia", "Slovenia", "Spain", "Sweden", 
                "Switzerland", "Tajikistan", "Tanzania", "Thailand", 
                "Tunisia", "Peruvian", "Uganda", "Ukraine", "Uruguay", "Uzbekistan", 
                "Vietnam", "Zambia", "Zimbabwe", "USA", "India", 
                "South Korea", "United Kingdom", "Greece", "South Africa", 
                "New Zealand", "Pakistan", "Lithuania", "Costa Rica", 
                "Cuba", "United Arab Emirates", "Saudi Arabia", 
                "Dominican Republic", "Venezuela")

values <- c(
  196, 1258, 152, 55, 2841, 2953,935,2486,1010,882,176,
  2250,755,2946,13,1868,1147,3303,5661,3,178,71,1536,
  11,3350, 4,2780,177,67,4604,1116,1511,186,1990,232,
  2270,1204,63,2153,2556,132,145,4317,2181,45,1092,34,
  86,74,1092,5106,663,379,581,1707,602,1891,1363,3260,
  1258,595,1371,721,101,587,445,101,3630,1638,332,56,
  914,1918,49,133,1126,35,1993, 203,824,2546,3044,676,
  4532,432,68,353,1559,398,1336,33,12,15,1204,869,
  1962,2759,6595,594,3196,2773,232,3622,2762,108,112,2087,
  1730,341,3887,3417,2606,2680,1647,13,13,493,2442,
  1.1940,
  75,2603,2192,48,437,209,374,3642,379,700,3389,3671,
  1689,1163,134,3718,1819,754,233,269,397,200)


if (length(values) != length(countries)) {
  stop("O número de valores não corresponde ao número de países.")
}

country_data <- data.frame(
  region = countries,
  value = values
)

map_data_with_values <- world_map %>%
  left_join(country_data, by = c("region" = "region"), relationship = "many-to-many")

ggplot(data = map_data_with_values, aes(x = long, y = lat, group = group, fill = value)) +
  geom_polygon(color = "black") +
  scale_fill_gradientn(colors = brewer.pal(9, "Blues"), values = scales::rescale(c(min(values), max(values))), name = "Mortes") +
  theme_minimal() +
  labs(title = )
