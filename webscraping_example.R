# Install packages  
## Optional: "scales" 
install.packages(c("tidyverse", "rvest", "stringr", "xml2", "scales"))

# Load libraries 
library(tidyverse)
library(rvest)
library(stringr)
library(purrr)
library(xml2)

# Example 1: Assign URL to a variable
## Input your URL of choice 
url <- "https://www.booking.com/searchresults.html?aid=355028;sid=39674d46b7ffda1e1865c0f078db5716;city=20123908;from_idr=1&;ilp=1;d_dcp=1"
webpage <- read_html(url)

# Collect the right HTML nodes
## Change '.sr_item_no_dates' to a node of your choosing using the Selector Gadget
title_html <- html_nodes(webpage, '.sr_item_no_dates')
# Pass the nodes into an HTML parser 
title <- html_text(title_html)
head(title)[1]

# Tidying up the text
## Deleting all instances of \n and \t 
title <- str_replace_all(title, '\n', '') 
title <- str_replace_all(title, '\t', '')
title[1]

## Putting space between lowercase and uppercase words that were combined together using regex
title <- gsub("([a-z])([A-Z])", "\\1 \\2", title)
## Deleting extra trailing space
title <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", title, perl=TRUE)
title[1]

# Writing title to dataframe
title <- as.tibble(title)

# Extracting hotel names from url
hotel_name <- url %>%
  read_html() %>%
  html_nodes(".sr-hotel__name") %>%
  html_text()

# Binding the hotel_name list to our title dataframe
title <- cbind(title, hotel_name)

# Attempting to extract another node is incredibly difficult because Bookings updates it's information via API and the HTML/CSS is bound with Javascript. Information is constantly changing. 
url %>%
  read_html() %>%
  html_nodes(".strong") %>%
  html_attrs()

# Example 2: Scraping bookings with pricing 
## Make sure to choose a series of dates and collect that URL 
url2 <- "https://www.booking.com/searchresults.en-us.html?aid=355028&sid=248cf8bb675e984f2217abf643b58f79&sb=1&src=index&src_elem=sb&error_url=https%3A%2F%2Fwww.booking.com%2Findex.html%3Faid%3D355028%3Bsid%3D248cf8bb675e984f2217abf643b58f79%3Bsb_price_type%3Dtotal%26%3B&ss=Nashville%2C+Tennessee%2C+USA&is_ski_area=&checkin_year=2019&checkin_month=4&checkin_monthday=4&checkout_year=2019&checkout_month=4&checkout_monthday=30&group_adults=2&group_children=0&no_rooms=1&b_h4u_keep_filters=&from_sf=1&ss_raw=Nashville&ac_position=0&ac_langcode=en&ac_click_type=b&dest_id=20123908&dest_type=city&iata=BNA&place_id_lat=36.16223&place_id_lon=-86.77435&search_pageview_id=6cd9611bb32b0045&search_selected=true&search_pageview_id=6cd9611bb32b0045&ac_suggestion_list_length=5&ac_suggestion_theme_list_length=0"
webpage2 <- read_html(url2)
webpage2

# Collect the right HTML nodes
## My selector gadget wasn't able to find the right path (.sr_flex_layout came up empty: NA), so I'm making an educated guess that the node is ".sr_item"
## I can also see, by right clicking on the page > inspect, that the class .sr_item repeats often in that <div>
title_html2 <- html_nodes(webpage2, ".sr_item")

# Pass the nodes into an HTML text parser 
title2 <- html_text(title_html2)
head(title2)[1]

# Tidying up the text
## Deleting all instances of \n and \t 
title2 <- str_replace_all(title2, '\n', '') 
title2 <- str_replace_all(title2, '\t', '')
title2[1]

## Putting space between lowercase and uppercase words that were combined together using regex
title2 <- gsub("([a-z])([A-Z])", "\\1 \\2", title2)
## Deleting extra trailing space
title2 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", title2, perl=TRUE)
title2[1]

# Writing title2 to a dataframe 
title2 <- as.tibble(title2)

# Next step, using regex to pull out: 
## a) reviews: pattern is a float (decimal number) followed by word "Review"
## b) score: pattern is a 2 or 3 digit integer preceded by word "score"
## c) price: this would need to be an if/else statement because some listings don't have price included. If the listing has a price, collect it. If not (else), move onto next listing. The pattern is a $ followed by a 3-4 digit number.
## d) max people: string "max people" followed by one digit number 


# Webscraping multiple pages using purrr
moisture <- "https://www.sephora.com/shop/moisturizing-cream-oils-mists" 
map_df(1:14, function(i) {
  # simple but effective progress indicator
  cat(".")
  Sys.sleep(10.0)
  page <- read_html(sprintf(moisture, i))
  data.frame(title=html_text(html_nodes(page, ".OneLinkNoTx")),
             price=html_text(html_nodes(page, ".css-68u28a")),
             description=html_text(html_nodes(page, ".css-1eueg40")),
             stringsAsFactors=FALSE)
}) -> sephora
dplyr::glimpse(sephora)

# But why am I getting the same 12 results repeating? They're blocking my request to scrape: 404 Error -- see Examples 2-3
## Example 1
makeup <- lapply(paste0('https://www.sephora.com/shop/moisturizing-cream-oils-mists', 1:14),
                 function(url){
                   Sys.sleep(10.0)
                   url %>% read_html() %>% 
                     html_nodes(".OneLinkNoTx") %>% 
                     html_text()
                   })
## Example 2
sephora_moisture <- lapply(paste0('https://www.sephora.com/shop/moisturizing-cream-oils-mists&page=', 1:14),
                function(url){
                  url %>% read_html() %>% 
                    html_nodes(".OneLinkNoTx") %>% 
                    html_text()
                })

## Example 3
sapply(sephora_moisture, function(url){
  tryCatch(
    url %>%
      as.character() %>% 
      read_html() %>% 
      html_nodes('.OneLinkNoTx') %>% 
      html_text(), 
    error = function(e){NA}    
    )
  })


# Introducing a lag to fool the website 
## IP could already be blacklisted as a bot -- wait 24 hours (or use Tor or VPN)
## Still fails 
to_get <- seq(0, 150, 10)
pb <- progress_estimated(length(to_get))

map_chr(to_get, function(i) {
  pb$tick()$print()
  searchURL <- paste("https://www.sephora.com/shop/moisturizing-cream-oils-mists", i, sep="")
  htmlWeb <- read_html(searchURL)
  nodeWeb <- html_node(htmlWeb, "td > .OneLinkNoTx > a")
  textWeb <- html_text(nodeWeb)
  Sys.sleep(20.0)
  textWeb
}) -> titles
print(trimws(titles))


# Winemag *is* scrapable
## Example 1: 
wines2 <- lapply(paste0('http://www.winemag.com/?s=washington%20merlot&drink_type=wine&page=', 1:12),
                function(url){
                  url %>% read_html() %>% 
                    html_nodes(".review-listing .title")  
                    html_text()
                })
# Data cleaning: 
## Checking the structure of our data: it's a list of lists (or nested lists)
str(wines2)
wines2[1]

## Use purrr library to flatten our lists so they're no longer bunched into groups (pages) of 12
wines2 <- wines2 %>% flatten() 
str(wines2)
class(wines2)

## To transform the flattened list into a dataframe, I need to unlist by the number of rows (i.e., data elements/wine labels) we have. I'm going to check the length of the list using the length() function and pass that number into data.frame(). 
## We can repeat this process with individual nodes and append them into a single dataset, or...
length(wines2)
wines2 <- data.frame(matrix(unlist(wines2), nrow=233, byrow=T),stringsAsFactors=FALSE)

## Example 2:
merlot_base <- "http://www.winemag.com/?s=washington merlot&drink_type=wine&page=%d"
map_df(1:12, function(i) {
  cat(".")
  pg <- read_html(sprintf(merlot_base, i))
  data.frame(wine=html_text(html_nodes(pg, ".review-listing .title")),
             excerpt=html_text(html_nodes(pg, "div.excerpt")),
             rating=gsub(" Points", "", html_text(html_nodes(pg, "span.rating"))),
             appellation=html_text(html_nodes(pg, "span.appellation")),
             price=gsub("\\$", "", html_text(html_nodes(pg, "span.price"))),
             stringsAsFactors=FALSE)
  
}) -> merlot 


## Cleaning up our cost -- adding a $
### Do not run dollar() if you want to visualize data
library(scales)
merlot$price <- as.numeric(merlot$price)
merlot$price <- dollar(merlot$price)

# Visualization and exploration 
library(ggplot2)
ggplot(merlot, aes(price)) +
  geom_histogram(bins = 35) +
  theme(axis.text.x = element_text(angle = 45))

merlot %>%
  select(price, appellation, wine) %>%
  filter(price > 50) %>%
  count(appellation)

## Extract dates using regex
merlot$dates <- str_extract(merlot$wine,"\\d{4}")

## Transform dates to an integer and visualize 
merlot$dates <- as.integer(merlot$dates)
ggplot(merlot, aes(dates, fill=appellation)) +
  geom_histogram(bins = 35)

merlot %>%
  select(price, appellation, wine, dates) %>%
  filter(dates < 2005) %>%
  count(appellation)

# Exporting our dataframe (merlot) to a .csv
write.csv(merlot, "merlot.csv")
