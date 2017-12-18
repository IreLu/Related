library(ggplot2)
library(bigrquery)
library(plyr)
library(tidyverse)
library(readxl)
library(stats)
library(arules)
library(ggplot2)
library(arulesViz)

setwd(myDirectory)



# query <- paste0("
# 
#     # standardSQL
# 
#     SELECT
#     fullVisitorId AS visitorId,
#     visitNumber,
#     hits.hitNumber AS hitN,
#     hits.transaction.transactionId AS IdTrans,
#     pr.productSKU AS C10,
#     pr.productPrice AS Price
# 
#     FROM `", projectName,".", DataSetId, ".", "ga_sessions_2017*` AS GA, UNNEST(GA.hits) AS hits,
#     UNNEST(hits.product) AS pr
#     WHERE _TABLE_SUFFIX BETWEEN '0901' AND '1130'
#     AND hits.transaction.transactionId IS NOT NULL
# ")
# 
# query_res <- query_exec(query, projectName, max_pages = Inf, use_legacy_sql = FALSE)
# write.csv(query_res, "query_res.csv", row.names=F)

query_res <- read_csv(paste(myDirectory, "/query_res.csv", sep=""), 
                      col_types = cols(visitorId = col_character()))
summary(query_res)

raw <- as.tibble(query_res) %>% mutate(Price=round(Price*10^(-6),2)) %>% filter(!(is.na(Price)))
raw <- raw %>% arrange(visitorId, visitNumber, hitN)

## Passing over product variants
  gaData <- raw %>% mutate(c8=substr(C10,1,8)) %>% select(visitorId, c8)

## Joining with external product data, filtering for specific Season
  productsData <- as.tibble(read.csv("ProductsData.csv", sep=";")) %>% 
    mutate(Code.8=as.character(Code.8),
           Sale.Line=substr(Sale.Line, 4, nchar(as.character(Sale.Line))))
  
  finalData <- inner_join(gaData, productsData, by=c("c8"="Code.8"))


## Transactions
transactions <- finalData %>% select(visitorId, c8)  
write.csv(transactions, "transactions.csv", row.names = F)
mba <- read.transactions("transactions.csv", format="single", sep=",", cols=c(1,2), skip=1)

## Rules
itemFrequencyPlot(mba, topN=20, type='relative')
rules <- apriori(mba, parameter = list(supp=0.0005, conf=0.2))

inspect(rules)
print(plot(rules, method = 'graph', engine="interactive"))
inspectDT(rules)
db_rules <- as.data.frame(inspect(rules))
write.csv(db_rules, "rules.csv", row.names = F)


## apriori algorithm doesn't analysis temporal sequences, so changing transactions' order doesn't affect the output.


rawinfoC8 <- finalData %>% group_by(c8, Macro, Micro, Sale.Line, Sex, Age.Range) %>% summarise(Count=n())

par(mar=c(8,3,1,0) + 0.1)
barplot(sort(table(infoC8$Macro), decreasing = T), las=2, main="Prodotti Acquistati per Macro")
dev.off()

data.frame(table(infoC8$Sex, infoC8$Age.Range)) -> df
colnames(df) <- c("Gender", "Age", "Freq")
df
df %>% ggplot(aes(x=Age, y=Freq)) +
  geom_bar(stat="identity", aes(fill=Gender))


sx <- data.frame(rules@lhs@itemInfo$labels[rules@lhs@data@i[1:45]+1])
dx <- data.frame(rules@rhs@itemInfo$labels[rules@rhs@data@i[1:45]+1])
colnames(sx) <- "c8"
colnames(dx) <- "c8"

sx_info <- inner_join(sx, infoC8, by="c8")
dx_info <- inner_join(dx, infoC8, by="c8")

str(rules)
copyRules <- rules
copyRules@lhs@itemInfo$labels[copyRules@lhs@data@i[1:45]+1] <- as.character(paste(sx_info$Sex, sx_info$Macro, sep="_"))
inspect(copyRules)
copyRules@rhs@itemInfo$labels[copyRules@rhs@data@i[1:45]+1] <- as.character(paste(dx_info$Sex, dx_info$Macro, sep="_"))
inspect(copyRules)
print(plot(copyRules, method = 'graph', engine="interactive"))
inspectDT(copyRules)
