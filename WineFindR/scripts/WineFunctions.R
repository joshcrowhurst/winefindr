# Functions for Wine FindR v2 are contained in this file.

transformInputURL <- function(inputURL) {  
  inputURL <- str_replace(inputURL,
                          "&CSRFToken=.*#*",
                          "") 
} # knock off the CS ID, if it exists
countWatsonsPages <- function(inputURL) {
  rawHTML <- read_html(inputURL)
  watsonsPageCount <<- html_nodes(rawHTML,'#productListingTwoColumn .page span+ span') %>% html_text() %>% as.double()
} # globally define the number of Watson's results pages 
scrapeWatsonsPage <- function(inputHTML) {
  extractedPrices <- html_nodes(inputHTML,'.price .s1') %>% 
    html_text() %>% 
    gsub(pattern = " |\r|\n|\t|,",replacement = "")
  extractedPrices <- substr(extractedPrices, start = (regexpr(pattern = 'HK',text = extractedPrices)+3),stop = 99) %>%
    as.double()
  #extractedPrices <- extractedPrices[-1]
  extractedProducts <- html_nodes(inputHTML,'.productName') %>% 
    html_text() %>%
    gsub(pattern = "\n|                |                    |            | .Limited time offer item – other promotion offers not applicable.*| .Special offer item – other promotion offers not applicable.*", replacement = "")
  # Get the HREFs of the products
  extractedURLs <- inputHTML %>% 
    html_nodes("a") %>% # get the CSS nodes
    html_attr("href") %>% # extract the URLs
    unique()
  extractedURLs <- extractedURLs[grepl(pattern = "\\/en\\/wine\\/",x = extractedURLs)]
  #extractedURLs <- extractedURLs[-1]
  results <- data.frame(url = extractedURLs, product = extractedProducts, price = extractedPrices)
} # extract product prices, urls, and names from a given Watsons results page
scrapeVivinoPages <- function(watsonsProduct) {
  vivinoURL <- paste0("https://www.vivino.com/search/wines?q=",gsub(" ", "+",watsonsProduct))
  print(vivinoURL)
  inputList <- tryCatch({
    Sys.sleep(runif(1)*10)
    read_html(vivinoURL)
  }, warning = function(w) {
    print("WARNING")
  }, error = function(e) {
    print("ERROR")
  },
  finally = {
    print("Okay")
  })
  if (inputList == "ERROR") {
    vivinoAvgRating <- 0
    vivinoNumReviews <- 0
    vivinoProductName <- "No results on Vivino"
  } else {
    vivinoAvgRating <- html_nodes(inputList,'.average__container:nth-child(1) .average__number') %>%
      html_text() %>%
      gsub(pattern = " |\r|\n|\t|,", replacement = "") %>%
      as.double()
    vivinoNumReviews <- html_nodes(inputList,'.text-micro') %>%
      html_text() %>%
      #gsub(pattern = " |\r|\n|\t|,", replacement = "") %>%
      gsub(pattern = "ratings", replacement = "") %>%
      as.double()
    vivinoProductName <- html_nodes(inputList,'.link-color-alt-grey .bold') %>%
      html_text() %>%
      gsub(pattern = "\r|\n|\t|,", replacement = "")
  }
  data.frame(rating = vivinoAvgRating[1], reviews = vivinoNumReviews[1], vProductName <- vivinoProductName[1], vUrl <- vivinoURL)
} # Generate a list of Vivino URLs and then scrape the sumbitches
