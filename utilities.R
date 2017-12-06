### High level functions for:
### 1. Gathering product data from MS SQL Server (sqlToDf)
### 2. Joining product data to rules_to_df output
### 3. Filtering rules, based on product attributes


sqlToDf <- function(ofs, season) {

  ### Libraries
  require(RODBC)
  require(tidyverse)
  
  ### Query SQL
  conn <- odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};server=YAPP14;database=SharedTables;trusted_connection=yes;")
  queryArticoli <- paste0(
                    c("SELECT  
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
                     WHERE Descrizione_Report = '"),
                     as.character(ofs),
                    c("' AND DescrizioneAbbreviata_EN = '"),
                     as.character(season),
                    c("' ORDER BY DataImmissione ASC"))
  
  articoli <- sqlQuery(conn, queryArticoli)
  articoli <- articoli %>% mutate_if(is.factor, as.character)
  return(articoli)
}

bqWithSqlData <- function(bq, sql){
  
  ### Libraries
  require(tidyverse)
  
  ### Joining BQ with SQL data
  joined <- bq %>% inner_join(sql, c('Label' = 'Codice10'))
  return(joined)
}

bqToRules <- function(dataset, startdate, enddate, support = 0.001){
  
  ### Libraries
  require(arules)
  require(bigrquery)
  require(tidyverse)
  
  ### Downloading BQ data
  query <- paste0("SELECT
                 fullVisitorId AS UserID,
                 visitNumber as VisitNumber,
                 hits.type as Type,
                 hits.eventInfo.eventCategory as Category,
                 hits.eventInfo.eventAction as Action,
                 hits.eventInfo.eventLabel as Label,
                 hits.hitNumber AS HitNumber
              FROM (TABLE_DATE_RANGE([yoox-bq-export:",
                  dataset,
                  ".ga_sessions_], TIMESTAMP('",
                  startdate,
                  "'), TIMESTAMP('",
                  enddate,
                  "'))) 
              WHERE
                 hits.eventInfo.eventAction IN ('AddToCart', 'Zoom', 'ZoomOpen', 'AddToWishList') 
                 AND hits.eventInfo.eventLabel != 'Error'
              ORDER BY UserID ASC, VisitNumber ASC")
  
  bq <- query_exec(query, project = 'yoox-bq-export', max_pages = Inf)
  
  ### Loading bq as transactions
  bq %>% select(UserID, Label) %>% write.csv('bq.csv', row.names=FALSE)
  bqT <- read.transactions('bq.csv'
                           , format = 'single'
                           , sep = ","
                           , cols = c(1, 2))
  
  ### Creating rules
  rules <- apriori(bqT, parameter = list(supp = support, conf = 0.3))
  
  ### Returning rules and printing rules
  return(rules)
}
