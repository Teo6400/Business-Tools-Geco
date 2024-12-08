#Correzione file prova futuro oro

# Importazione delle librerie necessarie
library(readxl)
library(forecast)
library(dplyr)
library(fpp2)
library(zoo)  # Potrebbe essere utile per operazioni future
library(ggplot2)
library(lubridate) #per utilizzare le date

# Caricamento del dataset e preparazione dei dati
ORO <- read.csv("C:/Users/teo_b/OneDrive/Desktop/lavoro qui - tools/amCharts aggiornato.csv")

# Selezione delle colonne utili (Date e close)
ORO <- ORO %>% select(Date, close)

# Formattazione della colonna Date come formato data corretto
ORO$Date <- as.Date(ORO$Date, format = "%Y-%m-%d")

# Filtriamo i dati per mantenere solo quelli a partire dal 7 Novembre 2021
ORO <- ORO %>% filter(Date >= as.Date("2021-12-02"))

# Rinominiamo la colonna "close" in "Price" per maggiore chiarezza
ORO <- ORO %>% rename(Price = close)

# Ordiniamo i dati per data crescente
ORO <- ORO %>% arrange(Date)

# Controlliamo i primi valori e la struttura del dataset
head(ORO)
str(ORO)

# Preparazione della serie temporale e decomposizione

# Converte il dataset in una serie temporale (ts) con frequenza giornaliera (365 giorni all'anno)
Gold_ts <- ts(ORO$Price, frequency = 260, start = c(year(min(ORO$Date)), yday(min(ORO$Date))))

# Decomposizione della serie temporale usando STL (Seasonal and Trend decomposition using Loess)
decomposition <- stl(Gold_ts, s.window = "periodic")

# Creazione dei grafici
par(mfrow = c(4, 1), mar = c(4, 4, 2, 1))

# Grafico della serie temporale originale
plot(Gold_ts, main = "Original Time Series for Gold Price", ylab = "Price dell'Oro", col = "blue", lwd = 2)

# Grafico del Trend
plot(decomposition$time.series[, "trend"], main = "Trend", ylab = "Trend", col = "darkgreen", lwd = 2)

# Grafico della Componente Stagionale
plot(decomposition$time.series[, "seasonal"], main = "Seasonal", ylab = "Seasonal", col = "purple", lwd = 2)

# Grafico dei Residui
plot(decomposition$time.series[, "remainder"], main = "Residuals", ylab = "Residuals", col = "red", lwd = 2)

# Ripristina la finestra grafica
par(mfrow = c(1, 1))

# Librerie necessarie
library(ggplot2)

# Residui dalla decomposizione STL
residuals <- decomposition$time.series[, "remainder"]

# Istogramma dei residui
ggplot(data = data.frame(residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.6) +
  labs(title = "Residuals Histogram", x = "Residuals", y = "Relative Frequency") +
  theme_minimal()


# Grafico Q-Q dei residui
# Creiamo una nuova finestra grafica per il Q-Q plot
par(mfrow = c(1, 1))  # Reset della disposizione dei grafici

qqnorm(residuals, main = "Q-Q Plot of Residuals", col = "blue")
qqline(residuals, col = "red", lwd = 2)  # Aggiunge la linea di riferimento

# Libreria necessaria (acf è parte del pacchetto base di R, quindi non è necessario caricare librerie esterne)
# Residui dalla decomposizione STL
residuals <- decomposition$time.series[, "remainder"]

# Grafico ACF dei residui (non differenziati)
acf(Gold_diff, main = "ACF Plot of Residuals", col = "blue", lwd = 2)


# Differenziazione della serie temporale per renderla stazionaria
Gold_diff <- diff(Gold_ts)

# Controlliamo la nuova serie differenziata
plot(Gold_diff, main = "Differenced Time Series", ylab = "Differenced Price dell'Oro", col = "blue", lwd = 2)

# Controllo della struttura della serie differenziata
str(Gold_diff)


# Grafico ACF della serie differenziata
acf(Gold_diff, main = "ACF Plot of Differenced Time Series", col = "blue", lwd = 2)

# Grafico PACF della serie differenziata
pacf(Gold_diff, main = "PACF Plot of Differenced Time Series", col = "blue", lwd = 2)


###Predizione###


#Prova 4

library(forecast)

# Supponiamo che il dataset si chiami 'ORO' e contenga le colonne 'Date' e 'Price'
# Dividiamo i dati in set di addestramento e set di test
train_size <- floor(0.80 * nrow(ORO))  # 80% per l'addestramento
y_train <- ORO$Price[1:train_size]  # Set di addestramento (80%)
y_test <- ORO$Price[(train_size + 1):nrow(ORO)]  # Set di test (20%)
dates_test <- ORO$Date[(train_size + 1):nrow(ORO)]  # Data per il set di test

# Selezione automatica del miglior modello ARIMA
ARIMA_Model <- auto.arima(y_train, seasonal=TRUE)
print("Modello ARIMA selezionato automaticamente:")
print(ARIMA_Model)

# Creiamo una serie vuota per memorizzare i valori previsti (per la validazione walk-forward)
y_prediction_auto <- numeric(0)
history <- y_train

# Eseguiamo la previsione con la validazione walk-forward per il modello selezionato automaticamente
###qui aspettare quale secondo###
for (i in 1:length(y_test)) {
  ARIMA_Model <- auto.arima(history, seasonal=TRUE)
  next_prediction <- forecast(ARIMA_Model, h=1)$mean  # Predizione a 1 passo
  y_prediction_auto <- c(y_prediction_auto, next_prediction)
  history <- c(history, ORO$Price[train_size + i])
}

# Creiamo un dataframe con le previsioni automatiche, i valori reali e le date
df_results_auto <- data.frame(
  Date = dates_test,
  Actual_Gold_Price = y_test,
  Predicted_Gold_Price = y_prediction_auto
)

print(df_results_auto)

# Calcoliamo l'errore assoluto medio (MAE) per il modello automatico
mae_auto <- mean(abs(df_results_auto$Actual_Gold_Price - df_results_auto$Predicted_Gold_Price))
print(paste("Errore Assoluto Medio (MAE) - Modello automatico:", mae_auto))

# Calcolo del RMSE
rmse_auto <- sqrt(mean((df_results_auto$Actual_Gold_Price - df_results_auto$Predicted_Gold_Price)^2))
print(paste("Errore Quadratico Medio (RMSE) - Modello automatico:", rmse_auto))


# Ora utilizziamo i parametri manuali scelti (puoi sostituirli con quelli selezionati o altri)
# Parametri manuali scelti
p_manual <- 0
d_manual <- 1
q_manual <- 4

# Creiamo una serie vuota per memorizzare i valori previsti (per la validazione walk-forward)
y_prediction_manual <- numeric(0)
history <- y_train  # Set iniziale di addestramento

# Eseguiamo la previsione con la validazione walk-forward per il modello ARIMA manuale
for (i in 1:length(y_test)) {
  # Creiamo e alleniamo il modello ARIMA con i parametri manuali su history
  ARIMA_Manual_Model <- Arima(history, order=c(p_manual, d_manual, q_manual), seasonal=TRUE)
  
  # Effettuiamo la previsione per il prossimo timestamp
  next_prediction <- forecast(ARIMA_Manual_Model, h=1)$mean
  
  # Aggiungiamo la previsione alla serie delle predizioni
  y_prediction_manual <- c(y_prediction_manual, next_prediction)
  
  # Aggiorniamo il set di addestramento aggiungendo il prossimo valore dei dati
  history <- c(history, ORO$Price[train_size + i])
}

# Creiamo un dataframe con le previsioni manuali, i valori reali e le date
df_results_manual <- data.frame(
  Date = dates_test,
  Actual_Gold_Price = y_test,
  Predicted_Gold_Price = y_prediction_manual
)

print(df_results_manual)

# Calcoliamo l'errore assoluto medio (MAE) per il modello manuale
mae_manual <- mean(abs(df_results_manual$Actual_Gold_Price - df_results_manual$Predicted_Gold_Price))
print(paste("Errore Assoluto Medio (MAE) - Modello manuale:", mae_manual))

# Calcolo del RMSE
rmse_auto <- sqrt(mean((df_results_manual$Actual_Gold_Price - df_results_auto$Predicted_Gold_Price)^2))
print(paste("Errore Quadratico Medio (RMSE) - Modello automatico:", rmse_auto))

# Mostriamo il dataframe con i risultati del modello manuale
print("Risultati modello manuale:")
print(df_results_manual)
print(ARIMA_Manual_Model)

# Creiamo il grafico delle previsioni del modello automatico e dei dati reali
ggplot(df_results_auto, aes(x = Date)) +
  geom_line(aes(y = Actual_Gold_Price, color = "Real Prices"), size = 1) +
  geom_line(aes(y = Predicted_Gold_Price, color = "Predicted Prices"), size = 1) +
  labs(title = "Previsioni del Price dell'Oro vs Price Reale - Modello Automatico",
       x = "Data",
       y = "Price dell'Oro",
       color = "Legenda") +
  theme_minimal() +
  scale_color_manual(values = c("Real Prices" = "blue", "Predicted Prices" = "red"))

# Creiamo il grafico delle previsioni del modello manuale e dei dati reali
ggplot(df_results_manual, aes(x = Date)) +
  geom_line(aes(y = Actual_Gold_Price, color = "Real Prices"), size = 1) +
  geom_line(aes(y = Predicted_Gold_Price, color = "Predicted Prices"), size = 1) +
  labs(title = "Previsioni del Price dell'Oro vs Price Reale - Modello Manuale",
       x = "Data",
       y = "Price dell'Oro",
       color = "Legenda") +
  theme_minimal() +
  scale_color_manual(values = c("Real Prices" = "blue", "Predicted Prices" = "red"))

### Correzione per previsioni future ###

library(forecast)
library(ggplot2)

# Dividiamo i dati in set di addestramento e test
train_size <- floor(0.80 * nrow(ORO))
y_train <- ORO$Price[1:train_size]
y_test <- ORO$Price[(train_size + 1):nrow(ORO)]
dates_test <- ORO$Date[(train_size + 1):nrow(ORO)]

# Modello ARIMA automatico
ARIMA_Model <- auto.arima(y_train, seasonal = TRUE)
print("Modello ARIMA automatico selezionato:")
print(ARIMA_Model)

# Validazione walk-forward (previsioni per dati di test)
y_prediction_auto <- numeric(0)
history <- y_train
###aspettare qualche secondo###
for (i in 1:length(y_test)) {
  ARIMA_Model <- auto.arima(history, seasonal = TRUE)
  next_prediction <- forecast(ARIMA_Model, h = 1)$mean
  y_prediction_auto <- c(y_prediction_auto, next_prediction)
  history <- c(history, y_test[i])
}

ARIMA_Model <- auto.arima(y_train, seasonal = TRUE)

# Creiamo un dataframe con i risultati
df_results_auto <- data.frame(
  Date = dates_test,
  Actual_Gold_Price = y_test,
  Predicted_Gold_Price = y_prediction_auto
)
print(df_results_auto)

# Previsioni future (iterative)
horizon <- 15
future_predictions <- numeric(horizon)
current_history <- c(y_train, y_test)

#aspettare qualche secondo per addestramento dino ad orizzonte richiesto
for (i in 1:horizon) {
  ARIMA_Model <- auto.arima(current_history, seasonal = TRUE)
  next_prediction <- forecast(ARIMA_Model, h = 1)$mean
  future_predictions[i] <- next_prediction
  current_history <- c(current_history, next_prediction)
}

ARIMA_Model <- auto.arima(y_train, seasonal = TRUE)

# Creiamo un dataframe per le previsioni future
future_dates <- seq(from = max(ORO$Date), by = "days", length.out = horizon)
future_results <- data.frame(
  Date = future_dates,
  Predicted_Price = future_predictions
)
print(future_results)

# Grafico delle previsioni
ggplot() +
  geom_line(data = df_results_auto, aes(x = Date, y = Actual_Gold_Price, color = "Price Reale"), size = 1) +
  geom_line(data = df_results_auto, aes(x = Date, y = Predicted_Gold_Price, color = "Previsione Test"), size = 1) +
  geom_line(data = future_results, aes(x = Date, y = Predicted_Price, color = "Previsione Futuro"), size = 1) +
  labs(title = "Previsioni del Price dell'Oro",
       x = "Data", y = "Price (€)", color = "Legenda") +
  theme_minimal() +
  scale_color_manual(values = c("Price Reale" = "blue", "Previsione Test" = "red", "Previsione Futuro" = "green"))

#zoom


# Definiamo i limiti dell'asse Y
y_lower_limit <- 75  # Limite inferiore dell'asse Y
y_upper_limit <- 85  # Limite superiore dell'asse Y

# Definiamo la data di inizio per novembre (assicurati che le date siano nel formato corretto)
november_start <- as.Date("2024-11-01")  # Modifica questa data a seconda di quando inizia novembre nei tuoi dati

# Grafico delle previsioni con limiti sugli assi
ggplot() +
  geom_line(data = df_results_auto, aes(x = Date, y = Actual_Gold_Price, color = "Price Reale"), size = 1) +
  geom_line(data = df_results_auto, aes(x = Date, y = Predicted_Gold_Price, color = "Previsione Test"), size = 1) +
  geom_line(data = future_results, aes(x = Date, y = Predicted_Price, color = "Previsione Futuro"), size = 1) +
  labs(title = "Previsioni del Price dell'Oro",
       x = "Data", y = "Price (€)", color = "Legenda") +
  theme_minimal() +
  scale_color_manual(values = c("Price Reale" = "blue", "Previsione Test" = "red", "Previsione Futuro" = "green")) +
  xlim(november_start, max(future_results$Date)) +  # Limita l'asse x da novembre in poi
  ylim(y_lower_limit, y_upper_limit)  # Imposta i limiti dell'asse Y tra 75 e 85



###proviamo a vedere un po meglio

future_predictions <- forecast(ARIMA_Model, h = horizon)
plot(future_predictions, xlim = c(600, 635), ylim = c(62, 75), ylab = "Price €")#nooo

