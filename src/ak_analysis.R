library(plyr)
cl.data <- read.csv("data/clean_data2.csv")
new_cname <- c('Register Num', 'Annual DFO Num',	'Glide Num',	'Country',	'Other',
               'Detailed Locations', 'Validation',	'Began', 'Ended',	'Duration',
               'Dead',	'Displaced',	'Damage', 'Severity', 'Affected',
               'Magnitude', 'Centroid X', 'Centroid Y',	'News if validated',	'M>6',
               'Total annual floods M>6',	'M>4', 'Total annual floods M>4', 'Total floods M>6',	'Total floods M>4',
               'Main Cause 1', 'Main Cause 2', 'Main Cause 3', 
               'Began Year', 'Began Month', 'End Month')
colnames(cl.data) <- new_cname

gdp_data <- read.csv("data/gdppc.csv", header=TRUE)
colnames(gdp_data) <- c('Country', 'GDP')
gdp_data$GDP <- as.numeric(gdp_data$GDP)

gdp_data$Class <- "GDP: $6,000 ~ $35,000"
gdp_data$Class[gdp_data$GDP > 35000] <- "GDP: Above $35,000"
gdp_data$Class[gdp_data$GDP < 6000] <- "GDP: Below $6,000"

df23 <- merge(cl.data, gdp_data, by = 'Country')
df23 <- df23[order(df23$`Register Num`), ]

df23$`Annual DFO Num` <- NULL
df23$`Glide Num` <- NULL
df23$Validation <- NULL
df23$`News if validated` <- NULL
df23$Other <- NULL
df23$`Detailed Locations` <- NULL
df23$`Main Cause 2` <- NULL
df23$`Main Cause 3` <- NULL
colnames(df23)[20] <- 'Cause'

df23 <- df23[, c(2:23, 1, 24:25)]
df23 <- df23[!(is.na(df23$Cause)), ]

as.data.frame(table(df23$Class))

temp <- df23[, c(23, 25)]

library(dplyr)
temp2 <- temp %>% distinct(Country)
as.data.frame(table(temp2$Class))

write.csv(df23, file= "data/gdp_data.csv", row.names=FALSE)
