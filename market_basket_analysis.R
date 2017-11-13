### My Market Basket Analysis
# Load the libraries
library(arules)
library(arulesViz)
library(caret)
library(tidyverse)
library(bigQueryR)
library(openxlsx)

### setwd('C:/Users/imbrigliaf/Documents')


### Autenticazione
### bqr_auth()

### Setto la query
### query <- bqr_query(projectId = 'yoox-bq-export',
###                   datasetId = '96332349',
###                   query = "SELECT
###                    fullVisitorId AS UserID,
###                    visitNumber as VisitNumber,
###                    hits.type as Type,
###                    hits.eventInfo.eventCategory as Category,
###                    hits.eventInfo.eventAction as Action,
###                    hits.eventInfo.eventLabel as Label,
###                    hits.hitNumber AS HitNumber
###                    FROM (TABLE_DATE_RANGE([yoox-bq-export:96332349.ga_sessions_], TIMESTAMP('2017-07-01'), TIMESTAMP('2017-09-30'))) 
###                    WHERE
###                    hits.eventInfo.eventAction IN ('AddToCart', 'Zoom') 
###                    AND hits.eventInfo.eventLabel != 'Error'
###                    ORDER BY UserID ASC, VisitNumber ASC"
###                  )

### query$cod8 <- substr(query$Label, 1, 8)


### Scrivo csv in output
### write.csv(query, '//yoox.net/ydata/Shared Data/DIGITAL MARKETING IN SEASON/DM/Analisi e report/1. Analisi In Season/Web Analytics-Repository_All/Analisi/Armani/QBR/adhoc/armani_q32017.csv', row.names = FALSE)

### Leggo dati GA e tabella di decodifica
armani <- read.csv('//yoox.net/ydata/Shared Data/DIGITAL MARKETING IN SEASON/DM/Analisi e report/1. Analisi In Season/Web Analytics-Repository_All/Analisi/Armani/QBR/adhoc/armani_q32017.csv', stringsAsFactors = FALSE)
c10 <- read.xlsx('//yoox.net/ydata/Shared Data/DIGITAL MARKETING IN SEASON/DM/Analisi e report/1. Analisi In Season/Web Analytics-Repository_All/Analisi/Armani/QBR/adhoc/armani_c10.xlsx', startRow = 4)

final <- c10 %>% inner_join(armani, by = c("Code.10" = "Label"))
final %>% mutate(MicroDivision = paste0(Division, ' ', Micro),
                 MicroDivision2 = paste0(Sale.Line, ' ', Micro),
                 MacroDivision = paste0(Division, ' ', Macro),
                 MacroDivision2 = paste0(Sale.Line, ' ', Macro),
                 UserID = as.character(UserID)) -> final

show_apriori <- function(col1 = 9, col2 = 1, support = 0.001, season = 'e mi') {
  
  final2 <- final %>% filter(Season == season)
  final2 <- final2[,c(col1, col2)]
  
  write.csv(final2, 'final.csv', row.names = FALSE)
  
  ### Carico le transazioni su base c10 (eventualmente cambiare l'indice di colonna da 5 a 7 per i c8)
  mba <- read.transactions('final.csv'
                           , format = 'single'
                           , sep = ","
                           , cols = c(1, 2))
  
  ### Ispeziono gli item più ricorrenti
  print(itemFrequencyPlot(mba, topN=20, type="relative"))
  
  ### Ricavo le regole
  rules <- apriori(mba, parameter = list(supp = support, conf = 0.3))
  return(rules)
  
  print(plot(rules, method = 'graph', interactive = TRUE))
  ###inspectDT(rules)
}

### col = 16 -> Division + Category per stagione SS17
### season = 'e mi' per stagione SS17
### col = 17 -> Division + Category per stagione FW17
### season = 'FW17' per l'omonima stagione
