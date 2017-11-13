### My Market Basket Analysis
# Load the libraries
library(arules)
library(arulesViz)
library(caret)
library(tidyverse)
library(bigQueryR)
library(openxlsx)

### setwd('YOUR PATH')


### Auth to BQ
### bqr_auth()

### Query
### query <- bqr_query(projectId = 'YOURPROJECT',
###                   datasetId = 'YOURDATASET',
###                   query = "SELECT
###                    fullVisitorId AS UserID,
###                    visitNumber as VisitNumber,
###                    hits.type as Type,
###                    hits.eventInfo.eventCategory as Category,
###                    hits.eventInfo.eventAction as Action,
###                    hits.eventInfo.eventLabel as Label,
###                    hits.hitNumber AS HitNumber
###                    FROM (TABLE_DATE_RANGE([yoox-bq-export:YOURDATASET.ga_sessions_], TIMESTAMP('2017-07-01'), TIMESTAMP('2017-09-30'))) 
###                    WHERE
###                    hits.eventInfo.eventAction IN ('AddToCart', 'Zoom') 
###                    AND hits.eventInfo.eventLabel != 'Error'
###                    ORDER BY UserID ASC, VisitNumber ASC"
###                  )

### Encoding product variant to product
### query$cod8 <- substr(query$Label, 1, 8)

### Writing query to .csv in order not to cache it on local
### write.csv(query, myquery.csv', row.names = FALSE)

### Reading gadata previously queryied from BQ and a product data mapping files with other information (Category, Division, Seasonality, etc...)
gadata <- read.csv('myquery.csv'.csv', stringsAsFactors = FALSE)
productdata <- read.xlsx('productdata.xlsx', startRow = 4)

### Joining gadata and productdata
### Creating Sales Line + Macro Category and Micro Category columns
final <- productdata %>% inner_join(gadata, by = c("Code.10" = "Label"))
final %>% mutate(MicroDivision = paste0(Division, ' ', Micro),
                 MicroDivision2 = paste0(Sale.Line, ' ', Micro),
                 MacroDivision = paste0(Division, ' ', Macro),
                 MacroDivision2 = paste0(Sale.Line, ' ', Macro),
                 UserID = as.character(UserID)) -> final

### Final function to be used after pre-process phase
show_apriori <- function(col1 = 9, col2 = 1, support = 0.001, season = 'e mi') {
  
  final2 <- final %>% filter(Season == season)
  final2 <- final2[,c(col1, col2)]
  
  write.csv(final2, 'final.csv', row.names = FALSE)
  
  ### Loading transactions 
  mba <- read.transactions('final.csv'
                           , format = 'single'
                           , sep = ","
                           , cols = c(1, 2))
  
  ### Printing most frequent items
  print(itemFrequencyPlot(mba, topN=20, type="relative"))
  
  ### Building rules and returning it
  rules <- apriori(mba, parameter = list(supp = support, conf = 0.3))
  return(rules)
  
  ### Plotting graph and table
  print(plot(rules, method = 'graph', interactive = TRUE))
  inspectDT(rules)
}
