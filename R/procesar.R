library(xlsx)
library(janitor)
library(tidyverse)
library(rvest)

# Funciones
# -------------------
leer_y_limpiar <- function(file) {
  df <- read.csv(paste0(file), header = T)
  df <- df[df$ticker != "",]
  df
}

get_cedears <- function(ARK, cedears) {
  df <- cedears[cedears$Ticker.en.Mercado.de.Origen %in% ARK$ticker,]
  df <- merge(df, ARK, by.x = "Ticker.en.Mercado.de.Origen", by.y = "ticker")
  df
}


# Bajo archivos de ARK
download.file("https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_INNOVATION_ETF_ARKK_HOLDINGS.csv", "ARKK.csv")
download.file("https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_AUTONOMOUS_TECHNOLOGY_&_ROBOTICS_ETF_ARKQ_HOLDINGS.csv", "ARKQ.csv")
download.file("https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_GENOMIC_REVOLUTION_MULTISECTOR_ETF_ARKG_HOLDINGS.csv", "ARKG.csv")
download.file("https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_NEXT_GENERATION_INTERNET_ETF_ARKW_HOLDINGS.csv", "ARKW.csv")
download.file("https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_FINTECH_INNOVATION_ETF_ARKF_HOLDINGS.csv", "ARKF.csv")

# Leo lista de cedears del banco comafi
cedears <- read.xlsx("data/cedears.xlsx", sheetIndex = 1)
#volumen <- read.xlsx("data/volumen_operado.xlsx", sheetIndex = 1)

# Leo el volumen de invertironline
content <- read_html("https://www.invertironline.com/mercado/cotizaciones/argentina/acciones/cedears")
tables <- content %>% html_table(fill = TRUE)
first_table <- tables[[1]]
nombres <- first_table[1,]
first_table <- first_table[-1,]
first_table <- first_table %>% clean_names()
colnames(first_table) <- nombres
volumen <- first_table
tickers <- unlist(lapply(strsplit(as.character(volumen$Símbolo), "  "), '[[', 1))
tickers <- gsub("\\r","", tickers)
tickers <- gsub("\\n","", tickers)
volumen$Símbolo <- tickers

volumen <- volumen[c("Símbolo", "ÚltimoOperado", "MontoOperado")]
volumen$ÚltimoOperado <- gsub("\\.","",volumen$ÚltimoOperado)
volumen$ÚltimoOperado <- as.numeric(as.character(gsub("\\,",".",volumen$ÚltimoOperado)))

volumen$MontoOperado <- gsub("\\.","",volumen$MontoOperado)
volumen$MontoOperado <- as.numeric(as.character(gsub("\\,",".",volumen$MontoOperado)))

# Mergeo volumen y cedears
merged <- merge(cedears, volumen, by.x = "Símbolo.BYMA", by.y = "Símbolo")

ARKK <- leer_y_limpiar("ARKK.csv")
ARKG <- leer_y_limpiar("ARKG.csv")
ARKF <- leer_y_limpiar("ARKF.csv")
ARKQ <- leer_y_limpiar("ARKQ.csv")
ARKW <- leer_y_limpiar("ARKW.csv")

# Cedears de ARK
cedears_ARKK <- get_cedears(ARKK, merged)
cedears_ARKG <- get_cedears(ARKG, merged)
cedears_ARKF <- get_cedears(ARKF, merged)
cedears_ARKQ <- get_cedears(ARKQ, merged)
cedears_ARKW <- get_cedears(ARKW, merged)

# Unifico cedears ARK
cedears_all_ARK <- rbind(cedears_ARKF, cedears_ARKG, cedears_ARKK, cedears_ARKQ, cedears_ARKW)
nrow(cedears_all_ARK)

# Hay que quedarse con los unicos
# Primero elimino columnas que no me interesan
cedears_all_ARK <- cedears_all_ARK[!colnames(cedears_all_ARK)
                                   %in% c("fund", "shares","cusip", "market.value...", "CUSIP.No.")]

# Sumo los pesos en distintos fondos
# columnas <- colnames(cedears_all_ARK)
# columnas <- columnas[columnas != "weight..."]
cedears_all_ARK.pesado <- cedears_all_ARK %>% group_by(Símbolo.BYMA) %>% summarise(PesoCompuesto = sum(weight...))
cedears_all_ARK.pesado <- unique(cedears_all_ARK.pesado)

# Agrego el peso ARK teniendo en cuenta los 5 ETF's
mergeado_peso <- merge(merged, cedears_all_ARK.pesado, by = "Símbolo.BYMA")

