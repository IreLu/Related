### My Market Basket Analysis
# Load the libraries
library(arules)
library(arulesViz)
library(caret)
library(tidyverse)
library(bigrquery)
library(openxlsx)
### Connecting to MS Sql Server - Shared Tables
library(RODBC)
library(tidyverse)


### Query BQ
q <- c("SELECT  
          fullVisitorId AS UserID,
          visitNumber as VisitNumber,
          hits.type as Type,
          hits.eventInfo.eventCategory as Category,
          hits.eventInfo.eventAction as Action,
          hits.eventInfo.eventLabel as Label,
          hits.hitNumber AS HitNumber
        FROM 
          (TABLE_DATE_RANGE([yoox-bq-export:91899902.ga_sessions_], TIMESTAMP('2017-05-01'), TIMESTAMP('2017-12-04'))) 
        WHERE
          hits.eventInfo.eventAction IN ('AddToCart', 'ZoomOpen', 'AddToWishList') 
        AND 
          hits.eventInfo.eventLabel != 'Error'
        ORDER BY 
          UserID ASC, VisitNumber ASC")

query <- query_exec(query = q, project = 'yoox-bq-export', max_pages = Inf)


### Query SQL
conn <- odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};server=YAPP14;database=SharedTables;trusted_connection=yes;")

queryArticoli <- c("SELECT  
	              [ID_Articolo]
               ,[Divisione_ID]
               ,[Descrizione_Report]
               ,[Codice10]
               ,[Sesso_ID]
               ,[LineeDiVendita_ID]
               ,[Stagione_di_Vendita_ID]
               ,[DescrizioneAbbreviata_EN]
               ,[DataImmissione]
               ,[DataImmissione_ID]
               FROM [SharedTables].[ods].[Articolo]
               LEFT JOIN [SharedTables].[ods].[Stagioni]
               ON Stagione_di_Vendita_ID = ID_Stagioni 
               LEFT JOIN [SharedTables].[ods].[Divisione]
               ON Divisione_ID = ID_Divisione
               WHERE Descrizione_Report = 'REDVALENTINO' AND DescrizioneAbbreviata_EN = 'FW17'
               ORDER BY DataImmissione ASC")

articoli <- sqlQuery(conn, queryArticoli)
articoli <- articoli %>% mutate_if(is.factor, as.character)

### Join dati BQ e SQL and .csv output
queryJoined <- query %>% inner_join(articoli, c('Label' = 'Codice10'))
queryJoined %>% select(UserID, Label) %>% write.csv('red.csv', row.names = FALSE)

### Reading transactions
mba <- read.transactions('red.csv'
                         , format = 'single'
                         , sep = ","
                         , cols = c(1, 2))

### Printing most frequent items
itemFrequencyPlot(mba, topN=20, type="relative")

### Building rules (support threshold by trial and error)
rules <- apriori(mba, parameter = list(supp = 0.0005, conf = 0.3))

### Plotting graph and table
plot(rules, method = 'graph', interactive = TRUE)
inspectDT(rules)

### Converting rules to df
source('rules_to_df.R')
rulesDf <- arules.to.df(rules)
