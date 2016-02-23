#```{r warning=FALSE, echo=FALSE, results='hide', message=FALSE}
library(readxl) # for 'read_excel'
library(gdata) # for 'grep'
library(splitstackshape) # for 'cSplit()'
library(xlsx)

# Read xlsx file
raw.data <- read_excel("data/GlobalFloodsRecord.xls", sheet="MasterTable")

# Remove columns (repetitive, NAs, notes )
dataFlood<- raw.data[-c(6, 7, 27, 30, 31)]
colnames(dataFlood) <- c('Register Num', 'Annual DFO Num',	'Glide Num',	'Country',	'Other', 
                         'Detailed Locations', 'Validation',	'Began',
                         'Ended',	'Duration in Days', 'Dead',	'Displaced',	'Damage (USD)',
                         'Main cause',	'Severity', 'Affected (sq km)', 'Magnitude (M)', 'Centroid X',
                         'Centroid Y',	'News if validated',	'M>6', 'Total annual floods M>6',	'M>4',
                         'Total annual floods M>4', 'Total floods M>6',	'Total floods M>4')

# Remove records with almost no data
dataFlood <- dataFlood[!is.na(dataFlood$Country),]
dataFlood <- dataFlood[(dataFlood$Magnitude>0),]
dataFlood <- dataFlood[!(dataFlood$Country=="error"),]
dataFlood <- dataFlood[!is.na(dataFlood$`Register Num`),]

dataFlood$Other[dataFlood$Other==0] <- NA
dataFlood$`Glide Num`[dataFlood$`Glide Num`==0] <- NA
dataFlood$Validation <- gsub("error", NA, dataFlood$Validation)
dataFlood$`News if validated` <- gsub("error", NA, dataFlood$`News if validated`)
dataFlood$`News if validated` <- gsub("x", NA, dataFlood$`News if validated`)

# Convert Began and Ended into Date type
# http://stackoverflow.com/questions/15686451/dates-from-excel-to-r-platform-dependency
# Need to double check depend on the OS you're using.
# If the first row Began is 2015-12-05, it is right.
dataFlood$Began <- as.Date(dataFlood$Began, origin = "1899-12-30")
dataFlood$Ended <- as.Date(dataFlood$Ended, origin = "1899-12-30")
dataFlood$Began <- format(dataFlood$Began, "%d-%b-%y")
dataFlood$Ended <- format(dataFlood$Ended, "%d-%b-%y")


# Clean up Country Column
dataFlood$Country <- trim(dataFlood$Country)
dataFlood$Country <- gsub(" and ", "-", dataFlood$Country)
dataFlood$Country <- gsub("/", "-", dataFlood$Country)
dataFlood$Country <- gsub(", ", "-", dataFlood$Country)

dataFlood$Country <- gsub("Inda", "India", dataFlood$Country)
dataFlood$Country <- gsub("Malayasia", "Malaysia", dataFlood$Country)
dataFlood$Country <- gsub("Moldava", "Moldova", dataFlood$Country)
dataFlood$Country <- gsub("Moldovo", "Moldova", dataFlood$Country)
dataFlood$Country <- gsub("Papua New Gunea", "Papua New Guinea", dataFlood$Country)
dataFlood$Country <- gsub("Texas", "USA", dataFlood$Country)
dataFlood$Country <- gsub("United Kingdom", "UK", dataFlood$Country)
dataFlood$Country <- gsub("Bangaldesh", "Bangladesh", dataFlood$Country)
dataFlood$Country <- gsub("Bangledesh", "Bangladesh", dataFlood$Country)
dataFlood$Country <- gsub("Uruguay,", "Uruguay", dataFlood$Country)
dataFlood$Country <- gsub("USA.", "USA", dataFlood$Country)
dataFlood$Country <- gsub("Viet Nam", "Vietnam", dataFlood$Country)
dataFlood$Country <- gsub("Zimbawe", "Zimbabwe", dataFlood$Country)
dataFlood$Country <- gsub("Venezulea", "Venezuela", dataFlood$Country)
dataFlood$Country <- gsub("Thiland", "Thailand", dataFlood$Country)
dataFlood$Country <- gsub("Boliva", "Bolivia", dataFlood$Country)
dataFlood$Country <- gsub("El Savador", "El Salvador", dataFlood$Country)
dataFlood$Country <- gsub("Columbia", "Colombia", dataFlood$Country)
dataFlood$Country <- gsub("Tajikstan", "Tajikistan", dataFlood$Country)
dataFlood$Country <- gsub("Domnican Republic", "Dominican Republic", dataFlood$Country)
dataFlood$Country <- gsub("Burkino Faso", "Burkina Faso", dataFlood$Country)
dataFlood$Country <- gsub("Guatamala", "Guatemala", dataFlood$Country)
dataFlood$Country <- gsub("Myanamar", "Myanmar", dataFlood$Country)
dataFlood$Country <- gsub("Kazahkstan", "Kazakhstan", dataFlood$Country)
dataFlood$Country <- gsub("Camaroun", "Cameroon", dataFlood$Country)

to_replace <- grepl("Bosnia-+", dataFlood$Country, perl=TRUE)
dataFlood$Country[to_replace] <- "Bosnia-Herzegovina"

to_replace <- grepl("Phil+", dataFlood$Country, perl=TRUE)
dataFlood$Country[to_replace] <- "Philippines"

to_replace <- grepl("Congo+", dataFlood$Country, perl=TRUE)
dataFlood$Country[to_replace] <- "Democratic Republic of Congo"

dataFlood$Country[dataFlood$Country==0] <- NA


# Clean up Main Cause Column
dataFlood$`Main cause`[dataFlood$`Main cause`==0] <- NA

dataFlood$`Main cause` <- gsub(" and ", ", ", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub(";", ", ", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("  ", " ", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("heavy", "Heavy", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("HeavyRain", "Heavy Rain", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Heay Rain", "Heavy Rain", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("rain", "Rain", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Rains", "Rain", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("Heavy seasonal Rain", "Monsoonal Rain", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Heavy monsoon Rain", "Monsoonal Rain", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("Monoonal Rain", "Monsoonal Rain", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("monsoonal Rainfall", "Monsoonal Rain", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("torrential", "Torrential", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Torrrential Rain", "Torrential Rain", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("storm", "Storm", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("storms", "Storms", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("stroms", "Storms", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("snow", "Snow", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Snow Melt", "Snowmelt", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("snowmelt", "Snowmelt", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("cyclone", "Cyclone", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("surge", "Surge", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Jams", "jam", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Tides", "Tide", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("Heavy Rain Snowmelt Dam B", 
                               "Heavy Rain, Snowmelt, Dam B", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Avalance Breach", 
                               "Avalanche related", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("Dam/Levy, break or release",
                               "Dam/Levee - Break or Release", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Cylone Tasha, Monsoon Rain, Cyclone", 
                               "Cylone, Monsoonal Rain", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("see notes", "Volcano", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Hgh", "High", dataFlood$`Main cause`)

to_replace <- grepl("Ice+", dataFlood$`Main cause`, perl=TRUE)
dataFlood$`Main cause`[to_replace] <- "Ice jam/break-up"

to_replace <- grepl("Hurricane+", dataFlood$`Main cause`, perl=TRUE)
dataFlood$`Main cause`[to_replace] <- "Hurricane"

to_replace <- grepl("Tropical Storms+", dataFlood$`Main cause`, perl=TRUE)
dataFlood$`Main cause`[to_replace] <- "Tropical Storm"

to_replace <- grepl("Tropical Cyclone+", dataFlood$`Main cause`, perl=TRUE)
dataFlood$`Main cause`[to_replace] <- "Tropical Cyclone"

to_replace <- grepl("Typhoon+", dataFlood$`Main cause`, perl=TRUE)
dataFlood$`Main cause`[to_replace] <- "Typhoon"




# Split the main cause into 3 columns
df1 <- cSplit(dataFlood, 'Main cause', sep=c(","))[,lapply(.SD,as.character)]

to_replace <- grepl("Monsoo+", df1$`Main cause_1`, perl=TRUE)
df1$`Main cause_1`[to_replace] <- "Monsoonal Rain"
to_replace <- grepl("Monsoo+", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Monsoonal Rain"
to_replace <- grepl("Monsoo+", df1$`Main cause_3`, perl=TRUE)
df1$`Main cause_3`[to_replace] <- "Monsoonal Rain"

to_replace <- grepl("Tropical Storm+", df1$`Main cause_1`, perl=TRUE)
df1$`Main cause_1`[to_replace] <- "Tropical Storm"
to_replace <- grepl("Tropical Storm+", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Tropical Storm"
to_replace <- grepl("Tropical Storm+", df1$`Main cause_3`, perl=TRUE)
df1$`Main cause_3`[to_replace] <- "Tropical Storm"

to_replace <- grepl("Snow+", df1$`Main cause_1`, perl=TRUE)
df1$`Main cause_1`[to_replace] <- "Snowmelt"
to_replace <- grepl("Snow+", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Snowmelt"
to_replace <- grepl("Snow+", df1$`Main cause_3`, perl=TRUE)
df1$`Main cause_3`[to_replace] <- "Snowmelt"

to_replace <- grepl("Dam+", df1$`Main cause_1`, perl=TRUE)
df1$`Main cause_1`[to_replace] <- "Dam/Levee - Break or Release"
to_replace <- grepl("Dam+", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Dam/Levee - Break or Release"
to_replace <- grepl("Dam+", df1$`Main cause_3`, perl=TRUE)
df1$`Main cause_3`[to_replace] <- "Dam/Levee - Break or Release"

to_replace <- grepl("Levee failure", df1$`Main cause_1`, perl=TRUE)
df1$`Main cause_1`[to_replace] <- "Dam/Levee - Break or Release"
to_replace <- grepl("ulhlaup", df1$`Main cause_1`, perl=TRUE)
df1$`Main cause_1`[to_replace] <- "Jokulhlaup"

to_replace <- grepl("Heavy monso", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Monsoonal Rain"
to_replace <- grepl("early monsoonal Rain", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Monsoonal Rain"

to_replace <- grepl("Hea+", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Heavy Rain"

to_replace <- grepl("began+", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- NA

df1$`Main cause_2`[df1$`Main cause_2`=="Tropical Cycl"] <- "Tropical Cyclone"

# Produce the cleaned data file
write.csv(df1, file= "data/clean_data.csv", row.names=FALSE)

cl.data <- read.csv("data/clean_data.csv")
new_cname <- c('Register Num', 'Annual DFO Num',	'Glide Num',	'Country',	'Other',
               'Detailed Locations', 'Validation',	'Began', 'Ended',	'Duration',
               'Dead',	'Displaced',	'Damage', 'Severity', 'Affected',
               'Magnitude', 'Centroid X', 'Centroid Y',	'News if validated',	'M>6',
               'Total annual floods M>6',	'M>4', 'Total annual floods M>4', 'Total floods M>6',	'Total floods M>4',
               'Main Cause 1', 'Main Cause 2', 'Main Cause 3')
colnames(cl.data) <- new_cname

#```