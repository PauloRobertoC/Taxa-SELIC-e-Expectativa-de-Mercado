# Analise de Conjuntura Economica 
# Coleta de Dados Macroeconomicos
# Paulo Roberto Carneiro de Sá

# Pacotes -----------------------------------------------------------------

# Carregar pacotes/dependências

# Carregar pacotes/dependências
library(GetBCBData) # BCB
library(rbcb)       # BCB
library(ipeadatar)  # IPEADATA
library(sidrar)     # SIDRA
library(rdbnomics)  # FMI, OCDE e outros
library(WDI)        # Banco Mundial
library(dplyr)      # pacotes de tratamento de dados
library(stringr)    # Tratar dados do tipo texto, string, character
library(tidyr)      # Transformacao e tratamento de dados
library(magrittr)   # Para utilizar o operador pipe
library(tsibble)    # 
library(ggplot2)    # Criar graficos

?GetBCBData::gbcbd_get_series # lendo a documentacao da biblioteca do SGS

# Dados do BCB ------------------------------------------------------------

# Site: https://www3.bcb.gov.br/sgspub/
# Coletar dados da SELIC no SGS/BCB

dados_sgs <- GetBCBData::gbcbd_get_series(
  id          = 432,            # código da série no SGS/BCB
  first.date  = "2020-01-01",   # filtro de período
  last.date   = Sys.Date()
)

View(dados_sgs)
head(dados_sgs)

# Criando um grafico de serie temporal

ggplot(dados_sgs, aes(x = ref.date, y = value)) +
  geom_line(color = "blue") +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 15, 5), limits = c(0, 15)) +
  labs(title = "Taxa Selic", x = "Data", y = "Valor") +
  theme(plot.title = element_text(hjust = 0.5))

# Coletar múltiplas séries do SGS

dados_sgs <- GetBCBData::gbcbd_get_series(
  id          = c("Dólar" = 3698, "IBC-Br" = 24363, "Resultado Primário" = 5793),
  first.date  = "2020-01-01",
  last.date   = Sys.Date(),
  format.data = "wide"     # formato de dados "wide"
)

head(dados_sgs)
View(dados_sgs)

# Desabilitar sistema de cache de dados (SGS)
dados_sgs <- GetBCBData::gbcbd_get_series(
  id          = c("Dólar" = 3698, "IBC-Br" = 24363, "Resultado Primário" = 5793),
  use.memoise = FALSE
)

head(dados_sgs)
tail(dados_sgs)

# Tratamento de dados
dados_sgs %>%      # formato long p/ wide
  tidyr::pivot_wider(
    id_cols = "ref.date",
    names_from = "series.name",
    values_from = "value"
  ) %>%
  tidyr::drop_na()

# Site: https://www3.bcb.gov.br/expectativas2/#/consultaSeriesEstatisticas
# Coletar dados de expectativas de mercado

dados_focus <- rbcb::get_market_expectations(
  type       = "annual",
  indic      = "IPCA",
  start_date = "2020-01-01"
)

head(dados_focus) # vendo os 10 primeiros dados
tail(dados_focus) # vendo os 10 ultimos dados

# tratamento de dados 

colnames(dados_focus)

dados_focus %>%
  dplyr::filter(baseCalculo == 0) %>%
  dplyr::select("data" = "Data",
                "data_ref" = "DataReferencia",
                "mediana" = "Mediana"
                ) %>% 
  dplyr::group_by(data_ref, ano_mes = tsibble::yearmonth(data)) %>%
  dplyr:: summarise(expectativa_mensal = mean(mediana, na.rm = TRUE),
                    .groups = "drop") 
  
### Criando um grafico para a expectativa de mercado IPCA com intervalo de confianca

# Criar um objeto para os dados da expectativa mensal
df <- dados_focus %>%
  dplyr::filter(baseCalculo == 0) %>%
  dplyr::select("data" = "Data",
                "data_ref" = "DataReferencia",
                "mediana" = "Mediana"
  ) %>% 
  dplyr::group_by(data_ref, ano_mes = tsibble::yearmonth(data)) %>%
  dplyr:: summarise(expectativa_mensal = mean(mediana, na.rm = TRUE),
                    erro_padrao = sd(mediana, na.rm = TRUE)/sqrt(n()))

# Plotar um gráfico de linhas para as expectativas mensais
ggplot(df, aes(x = data_ref, y = expectativa_mensal)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = expectativa_mensal - 1.96*erro_padrao,
                  ymax = expectativa_mensal + 1.96*erro_padrao), alpha = 0.2, fill = "blue") +
  geom_point(size = 3, color = "blue") +
  labs(x = "Data de Referência", y = "Expectativa Mensal", title = "Expectativa de Mercado - IPCA") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
































































































































