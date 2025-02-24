# NOME: LEONARDO DE SALLES AMARA, RA: 770617 - TRABALHO DE GRADUAÇÃO

# LIBERANDO OS PACOTES  


library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(gamlss)

# VISUALIZANDO O BANCO DE DADOS   

View(BASETGB)


#  ANÁLISE DESCRITIVA 

#Selecionando as covariaveis para nossa análise descritiva
select_dataset<-dplyr::select(BASETGB,	
                              Mortes1MilhãoDePop,
                              TxMortCard,
                              TaxaMortalidadeInfantil,
                              TaxaFertilidade	,
                              TaxaPopUrb,
                              IDH,
                              ExpectatVida,
                              PIB,
                              Turismo,
                              Densidade)

# Renomeando as variáveis
select_dataset <- dplyr::rename(select_dataset,
                                Mortes = Mortes1MilhãoDePop,	
                                TMC = TxMortCard,	
                                TMI = TaxaMortalidadeInfantil,
                                TF = TaxaFertilidade,
                                TU = TaxaPopUrb,
                                IDH = IDH,
                                EV = ExpectatVida,
                                PPC = PIB,
                                TUR= Turismo,
                                DEN=Densidade)




# MATRIZ DE CORRELAÇÕES
matriz_cor <- cor(select_dataset)
print(matriz_cor)
ggcorr(select_dataset, label=T)
ggcorr(select_dataset, label = TRUE, label_round = 2, label_size = 3, label_color = "darkblue", palette = "Blues")

# ESTUDO DA VARIÁVEL RESPOSTA

# BOXPLOT
p <- ggplot(BASETGB, aes(y = Mortes1MilhãoDePop)) +
  geom_boxplot(fill = "darkblue", color = "black") +
  labs(y = "Mortalidade por COVID-19",
       x = "") +
  theme_minimal()

print(p)
summary(BASETGB$Mortes1MilhãoDePop)

# HISTOGRAMA
hist(BASETGB$Mortes1MilhãoDePop, breaks = 10, freq = TRUE, col = "darkblue", main = "",
     xlab = "Mortalidade por COVID-19", ylab = "Frequência")


# COVARIÁVEIS EM ESTDUO
ggplot(BASETGB, aes(x = TxMortCard, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Taxa de Mortalidade Cardíaca",
    y = "Mortalidade por COVID-19",
  )

# -------

ggplot(BASETGB, aes(x = TaxaMortalidadeInfantil, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Taxa de Mortalidade Infantil",
    y = "Mortes por 1 Milhão de População"
  )

# -------

ggplot(BASETGB, aes(x = TaxaFertilidade, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Taxa de fertilidade",
    y = "Mortes por 1 Milhão de População"
  )

# -------

ggplot(BASETGB, aes(x = TaxaPopUrb, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Porcentagem da pop. urbana",
    y = "Mortes por 1 Milhão de População"
  )

# -------

ggplot(BASETGB, aes(x = IDH, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "IDH",
    y = "Mortes por 1 Milhão de População"
  )

# -------

ggplot(BASETGB, aes(x = ExpectatVida, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Esperança de vida ao nascer",
    y = "Mortes por 1 Milhão de População"
  )

# -------

ggplot(BASETGB, aes(x = PIB, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "PIB Per Capita",
    y = "Mortes por 1 Milhão de População"
  )

# -------

ggplot(BASETGB, aes(x = Turismo, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Taxa de turistas",
    y = "Mortes por 1 Milhão de População"
  )

# -------

View(BASETGB)
ggplot(BASETGB, aes(x = Densidade, y = Mortes1MilhãoDePop)) +
  geom_point(color = "darkblue") +
  labs(
    x = "Densidade populacional",
    y = "Mortalidade por COVID-19",
  )



# CONSTRUINDO O MODELO 



modelo_NO2   <- gamlss( formula = BASETGB$Mortes1MilhãoDePop ~ 	
                                      BASETGB$PIB + BASETGB$IDH + BASETGB$TaxaPopUrb + BASETGB$TaxaFertilidade,
                                    
                                    sigma.formula=~BASETGB$PIB + BASETGB$IDH + BASETGB$TaxaPopUrb + BASETGB$TaxaFertilidade,
                                    family = NO2,
                                    data = na.omit(BASETGB),
                                    trace = FALSE
)
summary(modelo_NO2)
plot(modelo_NO2)
wp(modelo_NO2, ylim.all = 3)
residuos_modelo_NO2 <- residuals(modelo_NO2)
shapiro.test(residuos_modelo_NO2)
ks.test(residuos_modelo_NO2, "pnorm", mean = mean(residuos_modelo_NO2), sd = sd(residuos_modelo_NO2))



#  ANALISANDO OS RESÍDUOS DOS PAÍSES  


plot(residuos_modelo_NO2_IDH_FERT111)
residuos_modelo_NO2_IDH_FERT111
sort(residuos_modelo_NO2_IDH_FERT111)
fitted(modelo_NO2_IDH_FERT111)


# CRIANDO O GRÁFICO

par(mar = c(5, 4, 4, 2) - 0.1)
plot(residuos_modelo_NO2_IDH_FERT111, 
     ylab = "Resíduos", 
     xlab = "Países", 
     pch = 16, col = "black")


indices_menores <- order(residuos_modelo_NO2)[1]
indices_maiores <- order(residuos_modelo_NO2, decreasing = TRUE)[1:2]


points(indices_menores, 
       residuos_modelo_NO2_IDH_FERT111[indices_menores], 
       col = "blue", 
       pch = 19)


points(indices_maiores, 
       residuos_modelo_NO2_IDH_FERT111[indices_maiores], 
       col = "red", 
       pch = 19)


text(indices_menores, 
     residuos_modelo_NO21[indices_menores], 
     labels = indices_menores, 
     pos = 2, col = "blue")


text(indices_maiores, 
     residuos_modelo_NO2[indices_maiores], 
     labels = indices_maiores, 
     pos = 2, col = "red")


