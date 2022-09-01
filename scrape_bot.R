# Author Dre Dyson
# Github link: https://github.com/ergosumdre
# 
# 
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

# start Selenium
main_page <- "https://www.ebay.com/sch/i.html?_from=R40&_nkw=1+Troy+oz+999+SILVER+Round&_sacat=0&rt=nc&LH_Sold=1&LH_Complete=1&_ipg=240"
rD <- rsDriver(browser="firefox", port=2948L, verbose=F)
remDr <- rD[["client"]]
remDr$navigate(main_page)
html <- remDr$getPageSource()[[1]]

# Pagination
page_links <- read_html(html) %>%
  html_nodes("a.pagination__item") %>%
  str_extract(., "https://www.ebay.com/sch/i.html.*pgn") %>%
  gsub("&", "", .) %>% 
  data.frame()
colnames(page_links) = "link"
link_ending <- paste0("=", seq(length(page_links$link)))
links <- paste0(page_links$link, link_ending)
goToPage <- c(main_page, links)
goToPage <- gsub("amp;", "&", goToPage) # remove ampersand symbols

# Listings to dataframe
listings <- function(link){
  remDr$navigate(link) # go to page
  html <- remDr$getPageSource()[[1]]
  title <- read_html(html) %>% # parse HTML
    html_nodes("div.s-item__title.s-item__title--has-tags") %>% # type of table/class and title of table/class
    rvest::html_text() %>% # get text
    stringr::str_squish() # remove any whitespaces
  sold_price <- read_html(html) %>%
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
                         "sold_price" = sold_price[-1], # weird HTML code
                         "date_sold" = date_sold,
                         "item_link" = link[-1]) # weird HTML code
  return(listings)
}



all_listings <- lapply(goToPage, listings) # do function

all_sold_items_df <- do.call(rbind.data.frame, all_listings) # unlist data into dataframe
all_sold_items_df %>% group_by(title) %>% tally() %>% View() # pivot table
