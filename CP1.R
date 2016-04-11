# Reproducible Research
# Course Project 2
# Lindsay Spencer
# 9 APRIL 2016

# R version 3.2.4 Revised

# Ensure packages needed are installed
list.of.packages <- c("lubridate", "sqldf", "ggplot2", "gridExtra")
new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()
                       [, "Package"])]
if (length(new.packages))
    install.packages(new.packages)
rm("list.of.packages")
rm("new.packages")
library(lubridate)
library(sqldf)
library(ggplot2)
library(gridExtra)

# Get data
fileUrl <-
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
fileName = "stormData.csv"
if (!file.exists(fileName)) {
    download.file(fileUrl, fileName)
}
stormActivityData <- read.csv(fileName)

# Subset only the stuff we care about
relevantData <- c(
    "BGN_DATE",
    "EVTYPE",
    "FATALITIES",
    "INJURIES",
    "PROPDMG",
    "PROPDMGEXP",
    "CROPDMG",
    "CROPDMGEXP"
)
relevantDataSubset <- stormActivityData[relevantData]

getMultiplier <- function(multiplier) {
    ifelse(
        multiplier == 'K' || multiplier == 'k',
        mult <- 1000,
        ifelse(
            multiplier == 'M' || multiplier == 'm',
            mult <- 1000000,
            ifelse(
                multiplier == 'B' ||
                    multiplier == 'b',
                mult <- 1000000000,
                mult <- 1
            )
        )
    )
    mult
}

# Get total damages done
relevantDataSubset$propDamage <- relevantDataSubset$PROPDMG *
    as.integer(lapply(relevantDataSubset$PROPDMGEXP, getMultiplier))
relevantDataSubset$cropDamage <- relevantDataSubset$CROPDMG *
    as.integer(lapply(relevantDataSubset$CROPDMGEXP, getMultiplier))
relevantDataSubset$totalDamage <-
    relevantDataSubset$propDamage + relevantDataSubset$cropDamage

# Calculate cutoff year
relevantDataSubset$YEAR <-
    year(as.Date(relevantDataSubset$BGN_DATE, '%m/%d/%Y'))
hist(
    relevantDataSubset$YEAR,
    xlab = "Year",
    main = "Number of Events per Year.",
    col = "green"
)
cutOffYear <- quantile(relevantDataSubset$YEAR, c(.5))


selectStatement <- paste(
    "SELECT ",
    "EVTYPE AS Event,",
    "SUM(FATALITIES) AS Fatalities,",
    "SUM(INJURIES) AS Injuries,",
    "SUM(TotalDamage) AS Damage",
    "FROM relevantDataSubset ",
    "WHERE YEAR >= '",
    cutOffYear,
    "'",
    "GROUP BY EVTYPE"
)

finalDataSet <- sqldf(selectStatement)

topFatalEvents <-
    head(finalDataSet[order(-finalDataSet$Fatalities),], 5)

g1 <-
    ggplot(topFatalEvents, aes(x = factor(Event, levels = unique(Event)), y = Fatalities)) +
    geom_bar(stat = "identity", fill = "blue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Event")

topInjuryEvents <-
    head(finalDataSet[order(-finalDataSet$Injuries),], 5)

g2 <-
    ggplot(topInjuryEvents, aes(x = factor(Event, levels = unique(Event)), y = Injuries)) +
    geom_bar(stat = "identity", fill = "red") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Event")


grid.arrange(g1, g2, ncol = 2)


economicDamageEvents <-
    head(finalDataSet[order(-finalDataSet$Damage),], 5)

ggplot(economicDamageEvents,
       aes(x = factor(Event, levels = unique(Event)), y = Damage)) +
    geom_bar(stat = "identity", fill = "orange") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Event", y = "$ Damage (in Billions)") +
    scale_y_continuous(
        labels = function(n) {
            format(n / 1000000000, scientific = FALSE)
        }
    )
