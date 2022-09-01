# Author Dre Dyson
# Github link: https://github.com/ergosumdre
# 
# TODO -- Clean this up
# 
# This script will return All Sold Items on Ebay given a Sold/Completed URL. 
# Pseudo code:
# 1. go to page
#  # get number of pages
# 2. scrape listings
#  # add the following to dataframe: title, price, date sold, link
# 3. click on next page
#  # repeat 1 and 2. Append rows to previous dataframe

library(RSelenium)
library(lubridate)
library(rvest)
library(stringr)
library(dplyr)





# Pagination
main_page <- "https://www.ebay.com/sch/i.html?_from=R40&_trksid=p2334524.m570.l1313&_nkw=1oz+.999&_sacat=0&LH_TitleDesc=0&_odkw=1+oz+999+silver+round&_osacat=0&LH_Complete=1&LH_Sold=1"
rD <- rsDriver(browser="firefox", port=2848L, verbose=F)
remDr <- rD[["client"]]
remDr$navigate(main_page)
html <- remDr$getPageSource()[[1]]

numOfListings <-  read_html(html) %>%
  html_nodes("span.BOLD")
numOfListings <- numOfListings[grep(",", numOfListings)] %>% gsub("<span class=\"BOLD\">|</span>|,", "", .) %>% as.numeric()


moreThanTenK <- ifelse(numOfListings > 10000,
                       yes = TRUE,
                       no = FALSE)



page_links <- read_html(html) %>%
  html_nodes("a.pagination__item") %>%
  str_extract(., "https://www.ebay.com/sch/i.html.*pgn") %>%
  gsub("&", "", .) %>% 
  data.frame()
colnames(page_links) = "link"
link_ending <- paste0("=", seq(length(page_links$link)))
links <- paste0(page_links$link, link_ending)
goToPage <- c(main_page, links)
goToPage <- gsub("amp;", "&", goToPage)

if(moreThanTenK == FALSE){
  link_ending <- paste0("=", seq(length(page_links$link)))
  links <- paste0(page_links$link, link_ending)
  goToPage <- c(main_page, links)
  goToPage <- gsub("amp;", "&", goToPage)
} else{
  link_ending <- paste0("=", seq(49))
  links <- paste0(page_links$link, link_ending)
  goToPage <- c(main_page, links)
  goToPage <- gsub("amp;", "&", goToPage)
}



listings <- function(link){
  remDr$navigate(link) # go to page
  html <- remDr$getPageSource()[[1]]
  title <- read_html(html) %>% # parse HTML
    html_nodes("div.s-item__title.s-item__title--has-tags") %>% # type of table/class and title of table/class
    rvest::html_text() %>% # get text
    stringr::str_squish() # remove any whitespaces
  price_sold <- read_html(html) %>%
    html_nodes("span.s-item__price") %>%
    rvest::html_text() %>%
    stringr::str_squish()
  date_sold <- read_html(html) %>%
    html_nodes("div.s-item__title--tag") %>%
    rvest::html_text() %>%
    gsub("Sold |Sold Item", "", .) %>%
    lubridate::mdy()
  link <- read_html(html) %>%
    html_nodes("a.s-item__link") %>% 
    str_extract(., "https://www.ebay.com/itm/.*\\?") %>% 
    gsub("\\?", "", .)
  listings <- data.frame("title" = title,
                         "price_sold" = price_sold[-1], # weird HTML code
                         "date_sold" = date_sold,
                         "item_link" = link[-1]) # weird HTML code
  return(listings)
}



all_listings <- lapply(goToPage, listings) # do function

all_sold_items_df <- do.call(rbind.data.frame, all_listings) # unlist data into dataframe
pivot_table <- all_sold_items_df %>% group_by(title) %>% tally() %>% arrange(desc(n))
