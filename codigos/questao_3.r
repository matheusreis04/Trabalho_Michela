dados <- read.csv("dados/HW1_bike_sharing.csv")

# 3.1
numero_total_de_observacoes <-nrow(dados)
numero_total_de_observacoes

inicio_da_amostra <- min(dados$dteday)
inicio_da_amostra

fim_da_amostra <- max(dados$dteday)
fim_da_amostra

# 3.2
variaveis_relevantes <- dados[c("temp", "casual", "registered")]

resumo_estatistico <- summary(variaveis_relevantes)
resumo_estatistico

dados$season <- factor(dados$season, 
                       levels = c(1, 2, 3, 4),
                       labels = c("Primavera", "Verão", "Outono", "Inverno")
                )
dados$weathersit <- factor(dados$weathersit, 
                       levels = c(1, 2, 3, 4),
                       labels = c("Céu Limpo", "Nublado", "Chuva Fraca", "Chuva Forte")
                ) 

dados$total_users <- dados$casual + dados$registered

total_por_estacao <- aggregate(total_users ~ season,
                               data = dados,
                               FUN = sum)             
total_por_condicao <- aggregate(total_users ~ weathersit,
                               data = dados,
                               FUN = sum)

pdf("graficos/graficos_questao3.pdf")
par(mfrow = c(2, 1))
barplot(total_por_estacao$total_users,
        names.arg = total_por_estacao$season,
        main = "Total de Usuários por Estação do Ano",
        ylab = "Total de usuários",
        xlab = "Estações",
        col = c("lightgreen", "yellow", "orange", "lightblue")
        )

barplot(total_por_condicao$total_users,
        names.arg = total_por_condicao$weathersit,
        main = "Total de Usuários por Condição Metereológica",
        ylab = "Total de usuários",
        xlab = "Condições metereológicas",
        col = c("lightgreen", "yellow", "orange", "lightblue")
        )



dados$dteday <- as.Date(dados$dteday)

par(mar = c(5, 4, 4, 5) + 0.1)
plot(dados$dteday, dados$total_users,
     type = "l",
     col = "blue",
     xlab = "Data",
     ylab = "Total de Usuários",
     main = "Usuários Totais ao Longo do Tempo"
)

plot(dados$dteday, dados$temp,
     type = "l",
     col = "red",
     xlab = "Data",
     ylab = "Temperatura",
     main = "Temperatura ao Longo do Tempo"
)

dev.off()