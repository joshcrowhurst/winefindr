######Libraries######
library(rvest)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(here)
source(here("scripts","WineFunctions.R"))

# TO DO: 
# - fully projectize and put this on GitHub
# - optimize what columns are surfaced and their order in the output file
# - add checks for whether the product is even in stock 

#####SETTINGS AND INPUT#####
priceLow <- 200 # these will be used to sanity check and tidy the data up a little bit at the end 
priceHigh <- 350 
# ENTER YOUR WATSONS WINE URL HERE #
queryURL <- "https://www.watsonswine.com/c/root?q=%3Aprice-asc%3ApriceValue%3A%5B150+TO+350%5D%3AtypeName%3A10010207&text=&resultsForPage=48&sort=&CSRFToken=7b566aa5-a5c4-46b4-9d17-7e7a3cb01cfe#"

#####SCRIPT BODY - SCRAPING #####
queryURL <- transformInputURL(queryURL)
countWatsonsPages(queryURL)
if (watsonsPageCount > 1) {
  htmlList <-
    lapply(paste0(queryURL, "&page=", 0:(watsonsPageCount - 1)),
           read_html)
} else {
  htmlList <- list()
  htmlList[1] <- lapply(queryURL,
                        read_html)
} #Load all of the HTML for all pages into memory
watsonsTable.df <- lapply(htmlList, scrapeWatsonsPage) %>% do.call(what = rbind) #Parse into a table... Wine name (product ID?), cost, sale cost, ratings if avail, in stock status
suppressWarnings(
  vivinoResults.df <-
    lapply(watsonsTable.df$product, scrapeVivinoPages) %>% do.call(what = rbind)
)
combinedResults.df <- cbind(watsonsTable.df, vivinoResults.df) %>% filter(reviews > 0)

####EXPORT AND VISUALIZE####
write.table(
  combinedResults.df,
  file = here("data","processed",paste0("Output ", Sys.Date(), ".csv")),
  sep = ',',
  dec = '.',
  row.names = FALSE
)
# plot(x = jitter(vivino.df$vivNumReviews), y=jitter(vivino.df$vivRating))
# with(vivino.df,text(vivRating~vivNumReviews, labels = vivino.df$vivProdName),cex=0.1)
# 
# s <- subset(vivino.df, vivNumReviews > 30 & vivRating >= 3.8)
# with(s, plot(vivNumReviews, vivRating,xlim=c(40,4000),ylim=c(3.8,5)))
# with(s,text(vivRating~vivNumReviews, labels = s$vivProdName, pos = 3))

#Sort and rank