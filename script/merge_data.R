## Cómo afectan los precios de distintos activos financieros al precio
## del BTC

## Librerias 
library(tidyverse)
library(modelr)
library(reshape2)
library(scales)
data_folder <- "data"


## Carga de csv
btc_data <- read_csv(file.path(data_folder, 'BTC/BTC-USD_hist.csv'))
oro_data <- read_csv(file.path(data_folder, 'GOLD/GC=F_hist.csv'))
brent_data <- read_csv(file.path(data_folder, 'BRENT/BZ=F_hist.csv'))
wti_data <-read_csv(file.path(data_folder, 'WTI/CL=F_hist.csv'))
nasdaq_data <- read_csv(file.path(data_folder, 'NASDAQ/^IXIC_hist.csv'))
sp500_data <- read_csv(file.path(data_folder, 'SP500/^GSPC_hist.csv'))
fed_data <- read_csv(file.path(data_folder, 'FED/FEDFUNDS.csv'))
pbi_data <- read_csv(file.path(data_folder, 'FED/GDP.csv'))
desempleo <- read_csv(file.path(data_folder, 'FED/unemployment.csv'))


# Operaciones

oro_data$Date <- as.Date(oro_data$Date)
brent_data$Date <- as.Date(brent_data$Date)
wti_data$Date <- as.Date(wti_data$Date)
nasdaq_data$Date <- as.Date(nasdaq_data$Date)
sp500_data$Date <- as.Date(sp500_data$Date)

# Volatilidad con respecto a los demas activos:

oro_data <- oro_data %>% mutate(Diff_oro = High - Low)
btc_data <- btc_data %>% mutate(Diff_btc = High - Low)
sp500_data <- sp500_data %>% mutate(Diff_sp500 = High - Low)
nasdaq_data <- nasdaq_data %>% mutate(Diff_nasdaq = High - Low)
wti_data <- wti_data %>% mutate(Diff_wti = High - Low)
brent_data <- brent_data %>% mutate(Diff_brent = High - Low)

ggplot(combined_data) +
  geom_line(aes(x = Date, y = Diff_btc / Close_btc, color = "BTC")) +
  geom_line(aes(x = Date, y = Diff_sp500 / Close_sp500, color = "S&P500")) +
  geom_line(aes(x = Date, y = Diff_oro / Close_oro, color = "ORO")) +
  geom_line(aes(x = Date, y = Diff_nasdaq / Close_nasdaq, color = "NASDAQ")) +
  geom_line(aes(x = Date, y = Diff_brent / Close_brent, color = "Brent")) +
  geom_line(aes(x = Date, y = Diff_wti / Close_wti, color = "WTI")) +
  #coord_cartesian(ylim = c(-0.09,0.40)) +
  labs(x = "Fecha [Días]", y = "Diferencia [USD]", title = "Volatilidad de precio", subtitle = "Comparación de activos", colour = "Activos") +
  scale_color_manual(values = c("S&P500" = "blue", "NASDAQ" = "red", "Brent" = "violet", "WTI" = "purple", "ORO" = "gold", "BTC" = "green"))

max(diff_wti)

diff_btc <- btc_data$Diff_btc / btc_data$Close
diff_sp500 <- median(sp500_data$Diff_sp500 / sp500_data$Close) * 100
diff_nasdaq <- median(nasdaq_data$Diff_nasdaq / nasdaq_data$Close) * 100
diff_oro <- median(oro_data$Diff_oro / oro_data$Close) * 100
diff_wti <- wti_data$Diff_wti / wti_data$Close
diff_brent <- median(brent_data$Diff_brent / brent_data$Close) * 10

wti_data <- mutate(wti_data, volatilidad = Diff_wti / Close)

normalizado_btc <- btc_data$Diff_btc / btc_data$Close

# Unimos todo en un unico dataframe

btc_data <- subset(btc_data, select = c("Date", "Open", "High", "Low", "Close", "Volume", "Diff_btc"))
oro_data <- subset(oro_data, select = c("Date", "Open", "High", "Low", "Close", "Volume", "Diff_oro"))
brent_data <- subset(brent_data, select = c("Date", "Open", "High", "Low", "Close", "Volume", "Diff_brent"))
wti_data <- subset(wti_data, select = c("Date", "Open", "High", "Low", "Close", "Volume", "Diff_wti"))
sp500_data <- subset(sp500_data, select = c("Date", "Open", "High", "Low", "Close", "Volume", "Diff_sp500"))
nasdaq_data <- subset(nasdaq_data, select = c("Date", "Open", "High", "Low", "Close", "Volume", "Diff_nasdaq"))


# Unimos todo en un unico dataframe
combined_data <- inner_join(btc_data, oro_data, by = "Date", suffix = c("_btc", "_oro"))
combined_data <- inner_join(combined_data, brent_data, by = "Date", suffix = c("", "_brent"))
combined_data <- inner_join(combined_data, wti_data, by = "Date", suffix = c("", "_wti"))
combined_data <- inner_join(combined_data, sp500_data, by = "Date", suffix = c("", "_sp500"))
combined_data <- inner_join(combined_data, nasdaq_data, by = "Date", suffix = c("", "_nasdaq"))
combined_data_FED <- inner_join(fed_data, pbi_data, by = "DATE")
combined_data_FED <- inner_join(combined_data_FED, desempleo, by = "DATE")
# Supongamos que quieres cambiar el nombre de la columna "Open" a "Open_brent"
# Asegúrate de que el nombre de la columna sea el correcto

combined_data <- as.data.frame(combined_data)
combined_data_FED <- as.data.frame(combined_data_FED)
names(combined_data)[names(combined_data) == "Open"] <- "Open_brent"
names(combined_data)[names(combined_data) == "High"] <- "High_brent"
names(combined_data)[names(combined_data) == "Low"] <- "Low_brent"
names(combined_data)[names(combined_data) == "Close"] <- "Close_brent"
names(combined_data)[names(combined_data) == "Volume"] <- "Volume_brent"
names(combined_data)[names(combined_data) == "Dividens"] <- "Dividens_brent"
names(combined_data)[names(combined_data) == "Stock Splits"] <- "Stock Splits_brent"
names(combined_data_FED)[names(combined_data_FED) == "GDP"] <- "PBI"
names(combined_data_FED)[names(combined_data_FED) == "UNRATE"] <- "desempleo"
names(combined_data_FED)[names(combined_data_FED) == "DATE"] <- "Date"
names(combined_data_FED)[names(combined_data_FED) == "FEDFUNDS"] <- "FED"
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


ggplot(combined_data) +
  geom_line(aes(x = Date, y = Close_btc, color = "Bitcoin")) +
  geom_line(aes(x = Date, y = Close_sp500, color = "S&P 500")) +
  geom_line(aes(x = Date, y = Close_nasdaq, color = "NASDAQ")) +
  geom_line(aes(x = Date, y = Close_brent, color = "Brent")) +
  geom_line(aes(x = Date, y = Close_wti, color = "WTI")) +
  geom_line(aes(x = Date, y = Close_oro, color = "ORO")) +
  #coord_cartesian(ylim = c(,)) +
  labs(x = "Fecha", y = "Precio de cierre [USD]", title = "Precios de activos Historico [2014-2023]", subtitle = "S&P500, NASDAQ, BTC, Brent, WTI y ORO", color = "Activos") +
  scale_color_manual(values = c("Bitcoin" = "blue", "S&P 500" = "red", "NASDAQ" = "green", "Brent" = "orange", "WTI" = "purple", "ORO" = "gold"))



ggplot(combined_data, aes(x = Date, y = Close_sp500)) +
  geom_line() +
  labs(x = "Fecha", y = "Precio Sp500 [USD]", title = "Precio del Sp500 a lo largo del tiempo [2014-2023]") #+
#coord_cartesian(ylim = c(2000, 2350))

ggplot(combined_data, aes(x = Date, y = Close_nasdaq)) +
  geom_line() +
  labs(x = "Fecha", y = "Precio Nasdaq [USD]", title = "Precio del Nasdaq a lo largo del tiempo [2014-2023]")





# Se incluye la Tasa de interes
# Filtrar los datos antes de 2014
filtered_data <- combined_data_FED[combined_data_FED$Date >= as.Date("2014-01-01"), ]
filtered_data <- as.data.frame(filtered_data)
# Crear el gráfico utilizando los datos filtrados
ggplot(filtered_data, aes(x = Date, y = FED)) +
  geom_line() +
  labs(x = "Fecha", y = "Tasa de interés [%]", title = "Tasa de interés histórico")

filtered_data_combined <- inner_join(btc_data, filtered_data, by = "Date")
filtered_data_combined <- as.data.frame(filtered_data_combined)


# Matriz de correlacion

cor_data <- select(combined_data, Close_btc, Close_oro, Close_brent, Close_wti, Close_sp500, Close_nasdaq)
correlation_matrix <- cor(cor_data)
print(correlation_matrix)

cor_data1 <- select(filtered_data_combined, Close, FED)
correlation_matrix1 <- cor(cor_data1)
print(correlation_matrix1)
# Convertir la matriz de correlación en un dataframe para facilitar el gráfico
cor_df <- as.data.frame(as.table(correlation_matrix))
cor_df <- cor_df %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq)

cor_df1 <- as.data.frame(as.table(correlation_matrix1))
cor_df1 <- cor_df1 %>%
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

ggplot(cor_df1, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red",
                      limits = c(-1, 1),
                      na.value = "transparent") +
  labs(x = "Activos", y = "Activos", fill = "Correlación", title = "Matriz de Correlación", subtitle = "del Precio Cierre vs Tasa de Interes") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

