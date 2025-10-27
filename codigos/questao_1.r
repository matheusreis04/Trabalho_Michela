dados <- scan("dados/emissoes.txt")

# 1.1
media <- mean(dados)
mediana <- median(dados)
moda <- function(x){
    tabela <- table(x)
    as.numeric(names(tabela)[tabela == max(tabela)])
}
moda <- moda(dados)

sumario_central <- data.frame(
    Media = media,
    Mediana = mediana,
    Moda = moda
)
sumario_central
 
amplitude <- range(dados)[2] - range(dados)[1]
variancia <- var(dados)
desvio_padrao <- sd(dados)
coef_variacao <- (desvio_padrao/media) * 100

sumario_dispersao <- data.frame(
    Amplitude = amplitude,
    Variancia = variancia,
    `Desvio Padrao` = desvio_padrao,
    `Coef. de Variacao` = coef_variacao
)
sumario_dispersao

# 1.2
pdf("graficos/graficos_questao1.pdf")
par(mfrow = c(1,2))

hist(dados,
     main = "Histograma das Emissões Diárias",
     xlab = "Unidades de Emissão",
     ylab = "Frequência",
     col = "lightblue",
     border = "black"
)

boxplot(dados,
        main = "Boxplot das Emissões Diárias",
        ylab = "Emissões (unidades)",
        col = "lightgreen",
        border = "black"
)

dev.off()


# 1.3
q1 <- as.numeric(quantile(dados, probs = c(1/4)))
q2 <- as.numeric(quantile(dados, probs = c(2/4)))
q3 <- as.numeric(quantile(dados, probs = c(3/4)))

iqr <- IQR(dados)

limite_inferior <- q1 - (1.5 * iqr)
limite_superior <- q3 + (1.5 * iqr)

outliers <- dados[dados < limite_inferior | dados > limite_superior]

sumario_quartis <- data.frame(
    Q1 = q1,
    Q2 = q2,
    Q3 = q3,
    `Limite Sup.` = limite_superior,
    `Limite Inf.` = limite_inferior,
    `N° de outliers` = length(outliers)
)
sumario_quartis

# 1.4
excederam <- dados[dados > 25]
proporcao <- length(excederam)/length(dados) * 100
proporcao