## Librerias

library(tidyverse)
library(modelr)
library(dplyr)

## Carga de dataset

orodf <- read_csv('GOLD/GC=F_hist.csv')
btcdf <- read_csv('BTC/BTC-USD_hist.csv')
glimpse(orodf)
## Operaciones

orodf <- orodf %>% mutate(Diff = High - Low)
btcdf <- btcdf %>% mutate(Diff = High - Low)


## Graficos 

ggplot(orodf, aes(x = Date, y = Diff)) +
  geom_point() +
  stat_smooth() +
  labs(title = "Gráfico de diferencia de precios de Oro",
       subtitle = "(High - Low)",
       x = "Fecha [Diario]",
       y = "Diferencia de precios [USD]")
  


library(ggplot2)

ggplot(btcdf, aes(x = Date, y = Diff)) +
  geom_point() +
  stat_smooth() +
  coord_cartesian(ylim = c(0, 5000)) +
  labs(title = "Gráfico de diferencia de precios de BTC",
       subtitle = "(High - Low)",
       x = "Fecha [Diario]",
       y = "Diferencia de precios [USD]")


medianabtc <- median(btcdf$Diff)  # Calcular la mediana de la columna "Diff"


ggplot(btcdf, aes(x = Date, y = Diff)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.5) +
  geom_text(aes(label = sprintf("%.2f", medianabtc)), 
            x = max(btcdf$Date), y = medianabtc, 
            vjust = -1, hjust = 5.4, color = "blue", size = 4) +
  coord_cartesian(ylim = c(0, 5000)) +
  labs(title = "Diferencia de precios diarios de BTC",
       subtitle = "(High - Low)",
       x = "Fecha",
       y = "Diferencia de precios (USD)") +
  theme_minimal()





medianaoro <- median(orodf$Diff)



ggplot(orodf, aes(x = Date, y = Diff)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.5) +
  geom_text(aes(label = sprintf("%.2f", medianaoro)), 
            x = max(orodf$Date), y = medianaoro, 
            vjust = -1, hjust = 8.3, color = "blue", size = 4) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Mediana de la Diferencia de precios diarios de Oro",
       subtitle = "(High - Low)",
       x = "Fecha",
       y = "Diferencia de precios (USD)") +
  theme_minimal()
