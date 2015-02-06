## Idea of this R script: perform some analysis on the Swiss statistics data
# (years 2003 - 2011), as provided in the Open Data library here:
# http://www.opendata.admin.ch/de/organization/kanton-zurich?license_id=STAT-ZH+Lizenz
#
## Functions to implement:
## V1.0   - DONE
# getdata(), transform()
# If the files are not available in the current working directory, they will be downloaded
# and cleansed automatically (e.g. the Income_groups.csv file needs the removal of quotation marks)
## V1.1   - DONE
# plot_income(), query_income()
# The plotting funcionality is currently based on the "primitive" plot(), 
# instead of lattice or ggplot2. I'm on it, the Coursera M4 W1 will explain exactly these parts. 
## V1.2   - TBD, another set of files, same functionality
# plot_wealth(), query_wealth()
# upgrade to lattice, introduce dynamic charts (shiny?), maybe histograms and box plots instead of lines.

## setting the URLs
url_incomeEN <- "http://bar-opendata-ch.s3.amazonaws.com/Kanton-ZH/Statistik/Distribution_of_income.csv"
f_incomeEN <- "incomeEN.csv"
url_incomeGroup <- "http://bar-opendata-ch.s3.amazonaws.com/Kanton-ZH/Statistik/Income_groups.csv"
f_incomeGroup <- "incomeGrp.csv"
# Omitted: "http://bar-opendata-ch.s3.amazonaws.com/Kanton-ZH/Statistik/Tax_rates.csv"
# --> two-line CSV file: 1 = Single household; 2 = Married household

library(data.table)
library(methods)

getdata <- function(url, file, special = 0) {
    setClass("remove.quotes", contains=character())
    setAs("character", "remove.quotes", function(from) as.character(gsub("\"", "", from) ) )
    # get file
    if (!file.exists(file)) {
        fileURL <- url
        download.file(fileURL, destfile = file, method = "curl")
        dateDownloaded <- date()
        print(dateDownloaded)
    }
    # process file
    DT <- data.table()
    if (file == "incomeGrp.csv") {
        # remove quotation marks, convert INCOME_GROUP_CODE to integer
        setAs("character", "remove.quotes", function(from) as.character(gsub("\"", "", from) ) )
        DT <- read.csv(file, sep=",", header=T, quote="", colClasses=c(special,special))
        DT$INCOME_GROUP_CODE <- as.integer(DT$INCOME_GROUP_CODE)
    } else DT <- read.csv(file, sep=",", header=T)
    as.data.table(DT)
}

transform <- function(tab1, tab2, column) {
    setkeyv(tab1,column)
    setkeyv(tab2,column)
    merge(tab1, tab2)   
}

query_income <- function (year = 2011, city = "Zürich", married = F) {
    if (married == FALSE) { TRC <- 1 } else {TRC <- 2}
    subset(DTjoin, subset = DTjoin$YEAR == year & DTjoin$MUNICIPALITY == city & DTjoin$TAX_RATE_CODE == TRC)
}

DTincome <- getdata(url_incomeEN, f_incomeEN)
DTincomeGrp <- getdata(url_incomeGroup, f_incomeGroup, "remove.quotes")
DTjoin <- transform(tab1 = DTincome, tab2 = DTincomeGrp, column = "INCOME_GROUP_CODE")

plot_income <- function (min = 1999, max = 2011) {
    step <- 10 # poor man's solution to color cycling
    for (i in min:max) {
        dataset <- query_income(year = i)
        if (step == 10) {
            plot (dataset$NUMBER_OF_TAXPAYERS, type = "b", col = step, ylab = "Number of taxpayers", main = c("Taxpayers in years ", min, " to ", max), xaxt = "n")
        } else {
            points (dataset$NUMBER_OF_TAXPAYERS, col = step, pch = i - min)
            lines (dataset$NUMBER_OF_TAXPAYERS, col = step, pch = i - min)
        }
        step <- step + 10
    }
    axis(1, 1:nrow(dataset), labels = dataset$INCOME_GROUP_DESCRIPTION, las = 2)
    legendMax <- max(dataset$NUMBER_OF_TAXPAYERS)
    legend (40,legendMax - 100, min:max, pch = seq_along(min:max))
}

## Example usage: plot the number of single taxpayers, as a trend comparison between years 2005 to 2011
plot_income(2005,2011)

