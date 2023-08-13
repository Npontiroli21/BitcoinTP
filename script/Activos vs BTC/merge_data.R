## Cómo afectan los precios de distintos activos financieros al precio
## del BTC

## Librerias 
library(tidyverse)
library(modelr)
library(lubridate)
library(reshape2)

data_folder <- "data"


## Carga de csv
btc_data <- read_csv(file.path(data_folder, 'BTC/BTC-USD_hist.csv'))
oro_data <- read_csv(file.path(data_folder, 'GOLD/GC=F_hist.csv'))
brent_data <- read_csv(file.path(data_folder, 'BRENT/BZ=F_hist.csv'))
wti_data <-read_csv(file.path(data_folder, 'WTI/CL=F_hist.csv'))
nasdaq_data <- read_csv(file.path(data_folder, 'NASDAQ/^IXIC_hist.csv'))
sp500_data <- read_csv(file.path(data_folder, 'SP500/^GSPC_hist.csv'))



# Operaciones

oro_data$Date <- as.Date(oro_data$Date)
brent_data$Date <- as.Date(brent_data$Date)
wti_data$Date <- as.Date(wti_data$Date)
nasdaq_data$Date <- as.Date(nasdaq_data$Date)
sp500_data$Date <- as.Date(sp500_data$Date)
# Unimos todo en un unico dataframe

btc_data <- subset(btc_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
oro_data <- subset(oro_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
brent_data <- subset(brent_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
wti_data <- subset(wti_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
sp500_data <- subset(sp500_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
nasdaq_data <- subset(nasdaq_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))


# Unimos todo en un unico dataframe
combined_data <- inner_join(btc_data, oro_data, by = "Date", suffix = c("_btc", "_oro"))
combined_data <- inner_join(combined_data, brent_data, by = "Date", suffix = c("", "_brent"))
combined_data <- inner_join(combined_data, wti_data, by = "Date", suffix = c("", "_wti"))
combined_data <- inner_join(combined_data, sp500_data, by = "Date", suffix = c("", "_sp500"))
combined_data <- inner_join(combined_data, nasdaq_data, by = "Date", suffix = c("", "_nasdaq"))

# Supongamos que quieres cambiar el nombre de la columna "Open" a "Open_brent"
# Asegúrate de que el nombre de la columna sea el correcto

combined_data <- as.data.frame(combined_data)
names(combined_data)[names(combined_data) == "Open"] <- "Open_brent"
names(combined_data)[names(combined_data) == "High"] <- "High_brent"
names(combined_data)[names(combined_data) == "Low"] <- "Low_brent"
names(combined_data)[names(combined_data) == "Close"] <- "Close_brent"
names(combined_data)[names(combined_data) == "Volume"] <- "Volume_brent"
names(combined_data)[names(combined_data) == "Dividens"] <- "Dividens_brent"
names(combined_data)[names(combined_data) == "Stock Splits"] <- "Stock Splits_brent"

# Gráficos

# Precio Bitcoin a lo largo del tiempo
ggplot(combined_data, aes(x = Date, y = Close_btc)) +
  geom_line() +
  labs(x = "Fecha", y = "Precio BTC [USD]", title = "Precio del Bitcoin a lo largo del tiempo [2014-2023]")

ggplot(combined_data, aes(x = Date, y = Close_sp500)) +
  geom_line() +
  labs(x = "Fecha", y = "Precio Sp500 [USD]", title = "Precio del Sp500 a lo largo del tiempo [2014-2023]") #+
  #coord_cartesian(ylim = c(2000, 2350))

ggplot(combined_data, aes(x = Date, y = Close_nasdaq)) +
  geom_line() +
  labs(x = "Fecha", y = "Precio Nasdaq [USD]", title = "Precio del Nasdaq a lo largo del tiempo [2014-2023]")
# Matriz de correlacion

cor_data <- select(combined_data, Close_btc, Close_oro, Close_brent, Close_wti, Close_sp500, Close_nasdaq)
correlation_matrix <- cor(cor_data)
print(correlation_matrix)


# Convertir la matriz de correlación en un dataframe para facilitar el gráfico
cor_df <- as.data.frame(as.table(correlation_matrix))
cor_df <- cor_df %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq)

# Gráfico de barras de la matriz de correlación
ggplot(cor_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red",
                      limits = c(0, 1),
                      na.value = "transparent") +
  labs(x = "Activos", y = "Activos", fill = "Correlación", title = "Matriz de Correlación", subtitle = "del Precio Cierre de los Activos") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
