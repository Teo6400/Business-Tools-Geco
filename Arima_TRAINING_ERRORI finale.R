#PRIMA TROVIAMO I DATI, BASTANO QUESTI SENZA APRIRE ALTRI FILE PER FARE L'ANALISI IN QUESTO DOCUMENTO

library(readxl)
library(forecast)
library(dplyr)
library(fpp2)
library(zoo)
library(ggplot2)
library(fpp2)
library(lubridate)
library(stats)

ORO <- read.csv("C:/Users/famdr/Downloads/amCharts.csv")

# selezioniamo solo le colonne utili
ORO <- ORO%>%select(Date, close) #selezionate la colonna delle date e la colonna del prezzo di chiusura

# formattiamo la colonna Date di modo che sia riconosciuto che sono date
ORO$Date <- as.Date(ORO$Date, format = "%Y-%m-%d")

#eliminiamo le righe da prima del 20 Novembre 2014
ORO <- ORO %>% filter(Date >= as.Date("2014-11-20"))

#rinominiamo la colonna close con il nome "Prezzo"
names(ORO) <- c("Date", "Prezzo")

# Ordiniamo i dati per data crescente
ORO <- ORO[order(ORO$Date), ] #per precauzione più che altro perché erano già ordinati

#--------------------------------------------------------------------
#FINE LAVORO CON I DATI PRESO DA FILE GITHUB,
#ORA SI TAGLIA ULTERIORMENTE IL DATASET E POI SI PROCEDE A FARE TRAINING E TEST ARIMA


#Per implementare Arima e non mandarlo in overfitting tagliamo il dataset
ORO <- ORO %>% filter(Date >= as.Date("2021-11-09")) #(3 anni/periodi, ognuno da 261 osservazioni)

# Dividiamo i dati in set di addestramento e set di test
train_size <- floor(0.80 * nrow(ORO))                # 80% dei dati è destinata all'addestramento
y_train <- ORO$Prezzo[1:train_size]                  # Set per l'addestramento (80%)
y_test <- ORO$Prezzo[(train_size + 1):nrow(ORO)]     # Set per il test (20%)

# Estraiamo la colonna delle date per il set di test
dates_test <- ORO$Date[(train_size + 1):nrow(ORO)]   # Data per il set di test




#######[questa parte è diversa da TEO, non faccio auto.arima]#####

# Inizializzazione delle variabili
p_values <- seq(0, 2, by = 1)                        #sequenza di valori da 0 a 2 con passo 1
q_values <- seq(0, 4, by = 1)
d_value <- 1

#inizializziamo anche 2 dataframe, uno per gli errori, uno per i risultati del forecasting
error_data <- data.frame(
  Model = character(),
  LL = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  MAE = numeric(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)
test_results <- data.frame("Date" = dates_test, "Actual Gold Price" = y_test)


#NB questo comando ci mette qualche minuto per finire. 
#A un certo punto manda un messaggio di errore (pianificato)

# Ciclo per calcolare ARIMA(p, d, q):
for (p in p_values) {
  for (q in q_values) {
    tryCatch({
      # Creazione e allenamento del modello ARIMA
      ARIMA_Model <- Arima(y_train, order = c(p, d_value, q), seasonal = TRUE) #training di ARIMA
      
      # Calcoliamo gli errori (LL, AIC, BIC)     #Errori
      model_LL <- logLik(ARIMA_Model)
      model_AIC <- AIC(ARIMA_Model)
      model_BIC <- BIC(ARIMA_Model)
      
      #walkforward (ci serve per il ciclo for subito sotto)
      y_prediction <- numeric(0)
      history <- y_train  # Set iniziale di addestramento
      
      for (i in 1:length(y_test)) {
        ARIMA_Fit <- Arima(history, order = c(p, d_value, q), seasonal = TRUE)   #TEST di Arima
        next_prediction <- forecast(ARIMA_Fit, h = 1)$mean
        y_prediction <- c(y_prediction, next_prediction)
        history <- c(history, ORO$Prezzo[train_size + i])
      }
      
      # Calcolo MAE e RMSE
      MAE <- mean(abs(y_test - y_prediction))
      RMSE <- sqrt(mean((y_test - y_prediction)^2))
      
      # Nome del modello
      model_name <- paste("ARIMA(", p, ",", d_value, ",", q, ")", sep = "")
      
      # Aggiungiamo i risultati al dataframe errori
      error_data <- rbind(error_data, data.frame(
        Model = model_name,
        LL = model_LL,
        AIC = model_AIC,
        BIC = model_BIC,
        MAE = MAE,
        RMSE = RMSE
      ))
      
      # Aggiungiamo le previsioni al dataset test_results
      test_results[[model_name]] <- y_prediction
    }, error = function(e) {
      # Se il modello non converge, stampa un messaggio e continua (non lo aggiunge nemmeno a nessun dataframe)
      message(paste("Errore per ARIMA(", p, ",", d_value, ",", q, "):", e$message))
    })
  }
}

# Visualizza il dataframe degli errori dei modelli ARIMA
View(error_data)

# Visualizza il dataframe con le previsioni per ogni modello ARIMA
View(test_results)

#PRIMA DI ELIMINARE O ALTRO FACCIO DELLE COPIE DEI DUE DATAFRAME PERCHé MI SONO STUFATA DI RIRUNNARE IL CODICE 
a <- error_data
b <- test_results


#Eliminiamo da error_data tutti modelli che hanno AIC > di 800:
error_best <- error_data[error_data$AIC <= 800, ]

View(error_best)
#I modelli rimanenti sono: 
        error_best$Model
  #"ARIMA(0,1,4)" "ARIMA(1,1,3)" "ARIMA(1,1,4)" "ARIMA(2,1,2)" "ARIMA(2,1,3)"

#Ora che abbiamo ridotto a 5 modelli possiamo fare una selezione manuale
#Teoricamente con una previsione finanziaria di questo tipo dovremmo dare più
#peso a BIC e MAE (fonte Chat GPT)

#BIC:
#I modelli: 
#(0,1,4) = 817 ...
#(2,1,2) = 819 ...

#MAE:
#I modelli hanno una differenza troppo piccola (millesimi):
#(1,1,3) = 0,531...
#(0,1,4) = 0,532...

#AIC:
#I modelli: 
#(0,1,4) = 795,1 ...
#(2,1,3) = 795,8 ...

#RMSE:
#I modelli anche qui hanno una differenza di millesimi:
#(1,1,3) = 0,671...
#(0,1,4) = 0,673...

#NB i valori sopra riportati non sono appropriatamente approssimati ma solamente troncati per una migliore visibilità.

#A seguito di questa analisi (manuale purtroppo non sono riuscita ad automatizzarla)
#possiamo concludere che il modello che performa bene in ogni categoria è il
#modello ARIMA(0,1,4)

#              "Il modello migliore è ARIMA(0,1,4)"



#-----------------
# Mi creo una tabella con gli errori evidenziando il nostro modello ottimo:

# Carica la libreria formattable
install.packages("formattable")
library(formattable)

# Carica la libreria gt
install.packages("gt")
library(gt)


############TABELLA 1
#[dal dataframe error_data]
# Tabella con evidenziazione della riga ottimale per noi 
table_with_highlight <- error_data %>%
  gt() %>%
  # Evidenziazione dell'intera riga con il modello "ARIMA(0,1,4)"
  tab_style(
    style = list(
      cell_fill(color = "lightblue") # Colore giallo chiaro per la riga evidenziata (non c'è il trasparente per questa funzione)
    ),
    locations = cells_body(
      rows = error_data$Model == "ARIMA(0,1,4)"
    )
  ) %>%
  # Aggiungere bordi verticali (inclusa l'ultima colonna)
  tab_style(
    style = list(
      cell_borders(
        sides = c("left", "right"), # Linee verticali incluse quelle finali
        color = "black",
        weight = px(1)
      )
    ),
    locations = cells_body(
      columns = everything()
    )
  )

# Visualizza la tabella
table_with_highlight



############TABELLA 2  
#stessa tabella ma [dal dataframe error_best] 
table_with_highlight1 <- error_best %>%
  gt() %>%
  tab_style(
    style = list(
      cell_fill(color = "lightblue")
    ),
    locations = cells_body(
      rows = error_best$Model == "ARIMA(0,1,4)"
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("left", "right"),
        color = "black",
        weight = px(1)
      )
    ),
    locations = cells_body(
      columns = everything()
    )
  )

table_with_highlight1
#------------------------------------

#ORA PRENDO TEST_RESULTS E TOLTO QUELLO CHE NON SERVE
#E POI GRAFICO IL VERO PREZZO CON QUELLO DEL MODELLO NEI MESI DEL TEST

predizione_test <- test_results%>%select(Date, Actual.Gold.Price, `ARIMA(0,1,4)`)
View(predizione_test)

ggplot(predizione_test, aes(x = Date)) +
  geom_line(aes(y = Actual.Gold.Price, color = "Actual Gold Price"), size = 1, alpha = 1) +     # Linea blu
  geom_line(aes(y = `ARIMA(0,1,4)`, color = "ARIMA(0,1,4) Prediction"), size = 1, alpha = 1) +  # Linea rossa
  labs(
    title = "Prezzo dell'Oro: Valori Reali e Previsioni",
    x = "Data",
    y = "Prezzo dell'Oro",
    color = "Legenda"
  ) +
  scale_color_manual(values = c("Actual Gold Price" = "darkblue", "ARIMA(0,1,4) Prediction" = "red")) +
  theme_minimal() +
  theme(
    text = element_text(size = 11)  # Dimensione del testo
  )
#------------------------------------------
