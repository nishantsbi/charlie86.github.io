---
layout: post
title: \#TweetOrDie
subtitle: Measuring geographic candidate sentiment with Twitter
bigimg: /img/candidates.jpg
---

For my submission to Ari Lamstein's R Shapefile Contest, I looked at Twitter's sentiment towards Hillary Clinton and Donald Trump. Using Twitter's geotagging feature, I mapped current sentiment expressed in tweets across the United States and examined the effect of keynote speeches at the Democratic National Convention on tweet sentiment.

Database architecture
---------------------

The tweets, henceforth referred to as twitts, are stored on a MySQL database I have on an AWS RDS instance. I have a script (included at the bottom of the post) hosted on EC2 to pull down live twitts mentioning Clinton or Trump's Twitter handles using the `streamR` package (I begain tracking the evening of Monday, July 25th, 2016).

Before writing the twitts to the database, I used `httr` and the US Census API to reverse geocode the twitts for which lat/lon tracking is enabled (around 3% of all twitts). Additionally, I used `tidytext` to count the number of "positive" and "negative" words for each twitt and detect which candidate the twitt refers to. In cases where both candidates were mentioned, I did not calculate sentiment.

Load up
-------

    library(chuckr) # package for pulling from my MySQL database
    library(RMySQL)
    library(DBI)
    library(scales)
    library(tidyr)
    library(dplyr)
    library(lubridate)
    library(RColorBrewer)
    library(highcharter)

Twitter candidate sentiment by state
------------------------------------

    twitts_og <- chuckr::cQuery("SELECT 
            candidate,
           `State.name` AS state_name,
            avg(positive - negative) AS score
           FROM twitts
            WHERE candidate IS NOT NULL
            AND `State.name` IS NOT NULL
           GROUP BY 1,2")

To quantify sentiment, I took the mean difference in positive and negative words for each candidate per state. To compare states' candidate preferences, I then took the differences in the candidates' scores within states. 

    twitts <- twitts_og %>%
        spread(candidate, score, fill = 0) %>% 
        mutate(diff = round(Clinton - Trump, 2),
               name = state_name)

To create the map, I used jbkunst's `higcharter` package, which comes preloaded with U.S. geojson files.

States with positive values tend to have more positive language when tweeting about Clinton than Trump and are colored blue, while states with negative values are in red, indicating relatively more postive tweets for Trump.

    data("usgeojson")
    highchart() %>% 
        hc_add_series_map(
            usgeojson, twitts,
            name = "Tweet Score", value = "diff", joinBy = "name") %>% 
        hc_colorAxis(dataClasses = color_classes(breaks = c(-1, 0, 1), colorRampPalette(c('#E91D0E', '#232066'))(3))) %>% 
        hc_legend(layout = "vertical", align = "right", floating = TRUE)

![](/img/tweetordie/state_map.png)

Using almost identical code, I further broke it down to the county level. However, because I had less than a week of data, and such a small portion of twitts were geotagged, the map is fairly empty.

One note here is that sparsity of the county level data led to more extreme values. Because all I looked at was whether or not the differences in scores were positive or negative, I normalized the scores to fit within negative and positive one.

    county_twitts_og <- cQuery("SELECT 
           candidate,
            `County.FIPS` AS fips,
            avg(positive - negative) AS score
           FROM twitts
            WHERE candidate IS NOT NULL
            AND `County.FIPS` IS NOT NULL
           GROUP BY 1,2")

    county_twitts <- county_twitts_og %>%
        mutate(score = rescale(score, to = c(-1, 1))) %>% 
        spread(candidate, score, fill = 0) %>% 
        mutate(diff = round(Clinton - Trump, 2))

    highchart() %>% 
        hc_add_series_map(
            uscountygeojson, county_twitts,
            name = "Tweet Score", value = "diff", joinBy = "fips") %>% 
        hc_colorAxis(dataClasses = color_classes(breaks = c(-1, 0, 1), colorRampPalette(c('#E91D0E', '#232066'))(3))) %>% 
        hc_legend(layout = "vertical", align = "right", floating = TRUE)

![](/img/tweetordie/county_map.png)

Here we see a more balanced breakout of Trump vs Clinton sentiment. With national polls being so tight at the moment, the overwhelmingly blue state map could be due to Clinton's "convention bounce", as the DNC was this week. To further explore this, I took a look at the overall sentiment over time. For interpretation's sake, I again normalized the score to fit within negative and positive one.

    stuff <- cQuery("SELECT
            concat(date(created_at), ' ', hour(created_at), ':00') AS time,
            candidate,
            avg(positive - negative) AS score
            FROM twitts
            WHERE candidate IS NOT NULL
           GROUP BY 1,2")

    stuff_plot <- stuff %>% 
        mutate(time = as.numeric(as.POSIXct(time)),
               score = round(rescale(score, to = c(-1, 1)), 2)) %>% 
        arrange(time)
    hchart(x = time, y = score, group = candidate, type = 'spline', object = stuff_plot)

![](/img/tweetordie/time_chart.png) 
There definitely appear to be spikes in Clinton's sentiment during the primetime DNC events! Specifically, Michelle Obama's midnight speech on Monday, the roll call vote ending at 7pm on Tuesday, Barack Obama's speech at midnight on Wednesday, and Hillary's formal acceptance speech midnight Thursday.

Below is the script I use to pull in twitts and write them to RDS.

Thank you for reading! If you have any questions please feel free to
email me anytime at <charles.thompson@barcelonagse.eu>.

pullTwitts.R
------------

    library(dplyr)
    library(stringr)
    library(lubridate)
    library(streamR)
    library(sp)
    library(tidyr)
    library(tidytext)
    library(httr)
    library(RMySQL)

    candidates <- data.frame(Name = c('Hillary Clinton', 'Donald Trump')) %>%
        mutate(username = str_replace(Name, ' ', ''),
               username = ifelse(username == 'DonaldTrump', paste0('real', username), username))
    chuckDB <- dbConnect(MySQL(),
                      user = 'chuckteezy',
                      password = 'xxxxxxx',
                      dbname = 'xxxxxxx',
                      host = 'xxxxxxx')
    load('data/my_oauth.RData')
    twitts <- filterStream(file.name = '', track = candidates$username, timeout = 5, oauth = my_oauth)
    if (length(twitts) > 0) {
        
     # format candidate names
     candidates <- data.frame(Name = c('Hillary Clinton', 'Donald Trump')) %>%
         mutate(username = str_replace(Name, ' ', ''),
                username = ifelse(username == 'DonaldTrump', paste0('real', username), username))
     
     bing <- sentiments %>%
         filter(lexicon == 'bing') %>%
         select(word, sentiment)
     
     make.sentiment <- function(positive, negative) {
         if (positive > negative) {
             sent <- 'positive'
         } else if (positive < negative) {
             sent <- 'negative'
         } else if (positive == negative) {
             sent <- 'neutral'
         }
         return(sent)
     }
     
     twitts <- parseTweets(twitts) %>%
         mutate(created_at = as.POSIXct(created_at, format = '%a %b %d %T +0000 %Y') - hours(4),
                rownum = row_number())
     
     coords <- twitts %>%
         filter(!is.na(place_lat))
     
     ##### Geocoding
     geocodes <- do.call(rbind, lapply(seq_len(nrow(coords)), function(x) {
         lat <- coords$place_lat[x]
         lon <- coords$place_lon[x]
         geos <- GET(paste0('http://data.fcc.gov/api/block/2010/find?latitude=', lat, '&longitude=', lon, '&format=json')) %>%
             content %>%
             unlist %>%
             t %>%
             as.data.frame
         if (is.null(geos$Block.FIPS)) {
             df <- data.frame(Block.FIPS = NA,
                              County.FIPS = NA,
                              County.name = NA,
                              State.FIPS = NA,
                              State.code = NA,
                              State.name = NA, stringsAsFactors = F)
             geos <- cbind(df, geos)
         }
         return(geos %>% mutate(rownum = coords$rownum[x]))
     }))
     
     sent <- twitts %>%
         filter(lang == 'en') %>%
         select(text, created_at, id_str)
     invisible(vapply(word(candidates$Name, 2), function(x) {
         sent[, x] <<- str_detect(tolower(str_replace_all(sent$text, '[^[:graph:]]', ' ')), tolower(x))
     }, logical(nrow(sent))))
     
     sent <- sent %>%
         rowwise %>%
         filter(sum(Clinton, Trump) == 1) %>%
         ungroup %>%
         gather(candidate, mention, c(Clinton, Trump)) %>%
         filter(mention == T) %>%
         select(-mention) %>%
         mutate(time = max(created_at),
                text = str_replace_all(text, '[[:punct:]]|[[:cntrl:]]|\\d+', ''),
                text = iconv(text, 'UTF-8', 'ASCII')) %>%
         unnest_tokens(word, text) %>%
         filter(!is.na(candidate)) %>%
         anti_join(stop_words, by = 'word') %>%
         left_join(bing, by = 'word') %>%
         mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
         count(id_str, candidate, sentiment) %>%
         spread(sentiment, n, fill = 0) %>%
         mutate(sentiment = make.sentiment(positive, negative)) %>%
         ungroup
     
     twitts <- twitts %>%
         left_join(geocodes, by = 'rownum') %>%
         left_join(sent, by = 'id_str')
     
     dbWriteTable(conn = chuckDB, name = 'twitts', value = twitts, append = T, overwrite = F)
    }
