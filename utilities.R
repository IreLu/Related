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