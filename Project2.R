##PROJECT2
##GENE PARK
library(dplyr)
library(tidyverse)
library(readxl)

file <- read.csv("./conn_real_estate_data") ##Conneticut Real Estate Data
View(file)


## Get rid of data that doesn't have information on sale amount, assessed value, and town
## As they are all essential.


clean <- file %>% 
  filter(!is.na(Sale.Amount) & Sale.Amount != ""
         & Town != "***Unknown***")

print(nrow(file)) ##1054159
print(nrow(clean)) ##1054158



## Seeing if this dataset contains all data for 169 Towns in conneticut
unique_towns <- unique(clean$Town)
unique_towns_df <- data.frame(Town = unique_towns)
View(unique_towns_df)

##IT DOES


#RENAME
second <- clean %>%
  rename(
    AssessedValue = Assessed.Value,
    SaleAmount = Sale.Amount,
    ResidentialType = Residential.Type,
    ListYear = List.Year
  )




##Getting median of each town
average_prices <- second %>%
  group_by(Town, ListYear) %>%
  summarize(average_price = median(SaleAmount, na.rm = TRUE))

View(average_prices)


##Ranking by highest amount
data_new <- average_prices %>%
  group_by(ListYear) %>%
  arrange(ListYear , -average_price) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 30)


view(data_new)

## THIS IS FOR THE MEDIAN PRICE
write.csv(data_new, file = "average_prices.csv", row.names = FALSE)


## NOW FOR THE RATIO PLOT
 

## USING FILE AGAIN
clean2 <- file %>% 
  filter(!is.na(List.Year) & List.Year != ""  & !is.na(Sales.Ratio) & Sales.Ratio != "")


print(nrow(clean2))


clean2 <- clean2 %>%
  select(Sales.Ratio, List.Year)

clean2 <- clean2 %>%
  rename (
    Year = List.Year,
    SalesRatio = Sales.Ratio
  )


filtered <- clean2 %>%
  filter(SalesRatio < 2.5 & SalesRatio >= 0)

## THIS IS THE HISTOGRAM PLOT

write.csv(filtered, file = "histogram_estate.csv", row.names = FALSE)





