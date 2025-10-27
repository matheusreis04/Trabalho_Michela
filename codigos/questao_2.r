dados <- data.frame(
  idade = c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46),
  nacionalidade = c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola", "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa", "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa", "Italiana", "Alemana", "Italiana"),
  renda_desejada = c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1, 2.5, 2.0, 1.7, 2.1, 3.2),
  experiencia_anos = c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
)
dados

# 2.1
media_idade <- mean(dados$idade)
mediana_idade <- median(dados$idade)
desvio_idade <- sd(dados$idade)

media_renda <- mean(dados$renda_desejada)
mediana_renda <- median(dados$renda_desejada)
desvio_renda <- sd(dados$renda_desejada)

media_experiencia <- mean(dados$experiencia_anos)
mediana_experiencia <- median(dados$experiencia_anos)
desvio_experiencia <- sd(dados$experiencia_anos)

tabela <- data.frame(
  Medida = c("Media", "Mediana", "Desvio Padrao"),
  Idade = c(media_idade, mediana_idade, desvio_idade),
  Renda = c(media_renda, mediana_renda, desvio_renda),
  Experiencia = c(media_experiencia, mediana_experiencia, desvio_experiencia)
)
tabela

# 2.2
renda_media_por_nacionalidade = aggregate(renda_desejada ~ nacionalidade,
                                    data = dados,
                                    FUN = mean
                                   )
experiencia_media_por_nacionalidade = aggregate(experiencia_anos ~ nacionalidade,
                                    data = dados,
                                    FUN = mean
                                   )
renda_media_por_nacionalidade
experiencia_media_por_nacionalidade


maior_renda_media <- renda_media_por_nacionalidade[which.max(renda_media_por_nacionalidade$renda_desejada)]
maior_experiencia_media <- experiencia_media_por_nacionalidade[which.max(experiencia_media_por_nacionalidade$renda_desejada)]

# 2.3
correlacao_pearson <- cor(dados$experiencia_anos, dados$renda_desejada, method = "pearson")
correlacao_pearson

pdf("graficos/graficos_questao2.pdf")

plot(dados$experiencia_anos, dados$renda_desejada,
     main = "Gráfico de Dispersão: Experiência vs Renda Desejada",
     xlab = "Anos de Experiência",
     ylab = "Renda Desejada (mil euros)",
     pch = 19, 
     col = "blue"
)

abline(lm(renda_desejada ~ experiencia_anos, data = dados), col = "red")

# 2.4
candidatos_priorizados <- dados[dados$renda_desejada < 2 & dados$experiencia_anos >= 10,]
candidatos_priorizados

#2.5
par(mfrow = c(2, 1))
boxplot(idade ~ nacionalidade,
        data = dados,
        main = "Distribuição da Idade por Nacionalidade",
        xlab = "",
        ylab = "Idade",
        col = terrain.colors(6), 
        las = 2 
)

boxplot(renda_desejada ~ nacionalidade,
        data = dados,
        main = "Distribuição da Renda Desejada por Nacionalidade",
        xlab = "",
        ylab = "Renda Desejada (mil euros)",
        col = heat.colors(6),
        las = 2
)
dev.off()