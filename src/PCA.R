#```{r warning=FALSE, echo=FALSE, results='hide', message=FALSE}
library(readxl) # for 'read_excel'
library(gdata) # for 'grep'
library(splitstackshape) # for 'cSplit()'
library(xlsx)
#The package factoextra is used for the visualization of the principal component analysis results.
# install.packages("devtools")
devtools::install_github("kassambara/factoextra")

# load
library("factoextra")
library(plyr)

# Read xlsx file
raw.data <- read_excel("data/GlobalFloodsRecord.xls", sheet="MasterTable")

# Remove columns (repetitive, NAs, notes )
dataFlood<- raw.data[-c(6, 7, 27, 30, 31)]
colnames(dataFlood) <- c('Register Num', 'Annual DFO Num',        'Glide Num',	'Country',	'Other', 
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
dataFlood$`Main cause` <- gsub("Avalanche related", 
                               "Avalanche", dataFlood$`Main cause`)

dataFlood$`Main cause` <- gsub("Dam/Levy, break or release",
                               "Dam/Levee - Break or Release", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("Cylone Tasha, Monsoon Rain, Cyclone", 
                               "Tropical Cyclone, Monsoonal Rain", dataFlood$`Main cause`)
dataFlood$`Main cause` <- gsub("see notes", "ETC", dataFlood$`Main cause`) # Volcano
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

to_replace <- grepl("Levee failure", df1$`Main cause_1`, perl=TRUE)
df1$`Main cause_1`[to_replace] <- "Dam/Levee - Break or Release"

to_replace <- grepl("Dam+", df1$`Main cause_1`, perl=TRUE)
df1$`Main cause_1`[to_replace] <- "Dam/Levee"
to_replace <- grepl("Dam+", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Dam/Levee"
to_replace <- grepl("Dam+", df1$`Main cause_3`, perl=TRUE)
df1$`Main cause_3`[to_replace] <- "Dam/Levee"


to_replace <- grepl("ulhlaup", df1$`Main cause_1`, perl=TRUE)
df1$`Main cause_1`[to_replace] <- "ETC" # Jokulhlaup

to_replace <- grepl("Heavy monso", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Monsoonal Rain"
to_replace <- grepl("early monsoonal Rain", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Monsoonal Rain"

to_replace <- grepl("Hea+", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- "Heavy Rain"

to_replace <- grepl("began+", df1$`Main cause_2`, perl=TRUE)
df1$`Main cause_2`[to_replace] <- NA

df1$`Main cause_2`[df1$`Main cause_2`=="Tropical Cycl"] <- "Tropical Cyclone"

colnames(df1)

df1<-rename(df1, c("Duration in Days"="Duration", "Affected (sq km)"="Affected", "Magnitude (M)"="Magnitude","Centroid X"="Lat","Centroid Y"= "Long"))

x<-subset(df1, select = c(Began,Duration,Dead,Displaced,Affected,Magnitude,Lat,Long ))

for(i in 1:length(x$Began)){
       str <- strsplit(x$Began[i], "-")
       month<-  unlist(str)[2]
       if(month == "Jan" ){
               x$Began[i] =as.numeric(1) 
       }
       else
       if(month == "Feb" ){
               x$Began[i] =as.numeric(2 )
       }
       else
               if(month == "Mar" ){
                       x$Began[i] =as.numeric(3)
               }
       else
               if(month == "Apr" ){
                       x$Began[i] =as.numeric(4 )
               }
       else
               if(month == "May" ){
                       x$Began[i] =as.numeric(5 )
               }
       else
               if(month == "Jun" ){
                       x$Began[i] =as.numeric(6 )
               }
       else
               if(month == "Jul" ){
                       x$Began[i] =as.numeric(7 )
               }
       else
               if(month == "Aug" ){
                       x$Began[i] =as.numeric(8)
               }
       else
               if(month == "Sep" ){
                       x$Began[i] =as.numeric(9 )
               }
       else
               if(month == "Oct" ){
                       x$Began[i] =as.numeric(10) 
               }
       else
               if(month == "Nov" ){
                       x$Began[i] =as.numeric(11)
               }
       else
               if(month == "Dec" ){
                       x$Began[i] =as.numeric(12) 
               }

}

x$Began <- as.numeric(x$Began )
x$Duration <- as.numeric(x$Duration )
x$Dead <- as.numeric(x$Dead )
x$Displaced <- as.numeric(x$Displaced )
x$Affected <- as.numeric(x$Affected )
x$Magnitude <- as.numeric(x$Magnitude )
x$Lat <- as.numeric(x$Lat )
x$Long <- as.numeric(x$Long )



pca <-prcomp(x, scale = FALSE)
names(pca)
head(pca$sdev)
pca$rotation

#eigen values -> variance in each direction
eig <-pca$sdev^2

# percentage of variance vector
variance <- eig*100/sum(eig)

# Cumulative variances
cumvar <- cumsum(variance)

eig.flood <- data.frame(eig = eig, variance = variance, cumvariance = cumvar)



barplot(eig.flood[, 2], names.arg=1:nrow(eig.flood), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.flood), 
      eig.flood[, 2], 
      type="b", pch=19, col = "red")

#Coordinates of variables on the principal components :The correlation between variables and principal components is used as coordinates

# Helper function : 
# Correlation between variables and principal components
var_cor_func <- function(var.loadings, comp.sdev){
        var.loadings*comp.sdev
}

# Variable correlation/coordinates
loadings <- pca$rotation
sdev <- pca$sdev

var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
var.coord[, 1:4]

#Graph of variables using R base graph
# Plot the correlation circle
a <- seq(0, 2*pi, length = 100)
plot( 10*cos(a), 10*sin(a), type = 'l', col="gray",xlab = "PC1",  ylab = "PC2")

abline(h = 0, v = 0, lty = 2)

# Add active variables
arrows(0, 0, var.coord[, 1], var.coord[, 2], 
       length = 0.1, angle = 15, code = 2)

# Add labels
text(var.coord, labels=rownames(var.coord), cex = 1, adj=1)

# we observe that the severity of flood islargely distated Area affected (sq km)
# variable duration , affected , magnitude correlated positively in PCA2 . Also these
# these variables are most closely related to PC1 as compared to other variables 


fviz_pca_var(pca, col.var="contrib")+ scale_color_gradient2(low="white", mid="blue", high="red", midpoint=55) 
+ theme_minimal()

#```