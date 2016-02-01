################################################################################
#                 Foundation to Strategic Business Analytics                   #
#                                                                              #
#                Recommendation production and prioritization                  #
#                                                                              #
#                              Peer Assignment                                 #
#                                                                              #
#        https://www.coursera.org/specializations/strategic-analytics          #
#                                                                              #
#              Author: Maximilian H. Nierhoff (http://nierhoff.info)           #
#                                                                              #
################################################################################


# Financial Crisis Overview Plot
library(Ecdat)
library(ggplot2)

data(bankingCrises)
bankingCrisesSubset <- bankingCrises[101:211,]
numberCrises <- rowSums(bankingCrisesSubset[-1], na.rm = TRUE)
plot(bankingCrisesSubset$year, numberCrises, 
     type = "l",
     main = "Number of Financial Crises per Year (1900 - 2010)",
     #sub = "Financial crisis data was taken from 70 countries, which representing over 90% of the worlds GDP.",
     ylab = "",
     xlab = "",
     col = "blue")
abline(v = c(1945, 1971), lty = "dashed", col = "darkred")
text(1958, 20, "Bretton", srt = 0, cex = .80, col = "darkred")
text(1958, 15, "Woods", srt = 0, cex = .80, col = "darkred")
text(1958, 10, "System", srt = 0, cex = .80, col = "darkred")


# World Map Plot GDP Growth

library(readr)
library(rworldmap)

indicators <- read_csv("PeerAssignment/WDI/Indicators.csv")

# GDP growth (annual %)
indicatorCode <- "NY.GDP.MKTP.KD.ZG"
indicatorYear <- 2014

filteredData <- indicators[indicators$IndicatorCode == indicatorCode &
                               indicators$Year == indicatorYear,]


sPDF <- joinCountryData2Map(filteredData,
                            joinCode = "ISO3",
                            nameJoinColumn = "CountryCode",
                            verbose = TRUE)

png("PeerAssignment/PA_ESSEC_Viz2.png", width = 600, height = 275)
mapCountryData(sPDF, nameColumnToPlot = "Value", 
               mapTitle = "GDP Growth (annual %) for 2014")
dev.off()
