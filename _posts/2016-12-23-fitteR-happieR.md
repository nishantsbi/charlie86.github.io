---
layout: post
title: fitteR happieR
subtitle: Finding the most depressing Radiohead song with R, using the Spotify and Genius Lyrics APIs
bigimg: /img/ifwesmilecanwegohd.jpg
---
Radiohead has been my favorite band for several years, so I am used to people politely suggesting that I play something "less depressing". Much of Radiohead's music is undeniably sad, and this post catalogs my journey to quantify that sadness, concluding in a data-driven determination of their most depressing song.

## Getting Data

Spotify's [Web API](https://developer.spotify.com/web-api/){:target="_blank"} provides detailed audio statistics for each song in their library. One of these metrics, "valence", measures a song's positivity. From the offical API documentation:

> A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).

So valence provides a measure of how sad a song *sounds* from a musical perspective. Another key component of a song's sentiment is its lyrics, and it just so happens that Genius Lyrics also has an [API](https://docs.genius.com/){:target="_blank"} to pull track-level data. To determine a song's sadness, I calculated a weighted average of valence and lyrical sentiment. But first, I had to get the data.

### Spotify Web API

Spotify's API is well documented, but it's still a pretty involved process to grab all songs for a given artist. In short, Spotify has separate API endpoints for tracks, albums, and artists, each of which needs their own identifying "uri" to access. To start, I created a function to query the `search` endpoint to grab the `artist_uri` for a given artist name.

```r
library(tidyverse)
library(httr)
library(stringr)

get_artists <- function(artist_name) {
    
    # Search Spotify API for artist name
    res <- GET('https://api.spotify.com/v1/search', query = list(q = artist_name, type = 'artist')) %>%
        content %>% .$artists %>% .$items
    
    # Clean response and combine all returned artists into a dataframe
    artists <- map_df(seq_len(length(res)), function(x) {
        list(
            artist_name = res[[x]]$name,
            artist_uri = str_replace(res[[x]]$uri, 'spotify:artist:', ''), # remove meta info from the uri string
            artist_img = ifelse(length(res[[x]]$images) > 0, res[[x]]$images[[1]]$url, NA)
        )
    })
    
    return(artists)
}
artist_info <- get_artists('radiohead')

str(artist_info)

Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	2 obs. of  3 variables:
 $ artist_name: chr  "Radiohead" "Radiohead Tribute Band"
 $ artist_uri : chr  "4Z8W4fKeB5YxbusRsdQVPb" "0ADkBHZhR2cVfANgK5gHQO"
 $ artist_img : chr  "https://i.scdn.co/image/afcd616e1ef2d2786f47b3b4a8a6aeea24a72adc"

# Filter out other artist matches
artist_info = artist_info %>% 
    filter(artist_name == 'Radiohead')
```

Next, I used the `artist uri` obtained above to search for all of Radiohead's albums.

```r
library(lubridate)

get_albums <- function(artist_uri) {
    albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist_uri,'/albums')) %>% content
    
    map_df(1:length(albums$items), function(x) {
        tmp <- albums$items[[x]]
        
        # Make sure the album_type is not "single"
        if (tmp$album_type == 'album') {
            data.frame(album_uri = str_replace(tmp$uri, 'spotify:album:', ''),
                       album_name = str_replace_all(tmp$name, '\'', ''),
                       album_img = albums$items[[x]]$images[[1]]$url,
                       stringsAsFactors = F) %>%
                mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', str_replace(tmp$uri, 'spotify:album:', ''))) %>% content %>% .$release_date, # you need a separate call to on "albums" to get release date.
                       album_release_year = ifelse(nchar(album_release_date) == 4, year(as.Date(album_release_date, '%Y')), year(as.Date(album_release_date, '%Y-%m-%d'))) # not all album_release_dates have months, so I created album_release year for sorting
                       )
        } else {
            NULL
        }
        
    }) %>% filter(!duplicated(tolower(album_name))) %>%  # Sometimes there are multiple versions (just with different capitalizations) of the same album
        arrange(album_release_year)
}

album_info <- get_albums(artist_info$artist_uri)

str(album_info)

'data.frame':	13 obs. of  5 variables:
 $ album_uri         : chr  "6400dnyeDyD2mIFHfkwHXN" "500FEaUzn8lN9zWFyZG5C2"
 $ album_name        : chr  "Pablo Honey" "The Bends" "OK Computer" "Kid A" ...
 $ album_img         : chr  "https://i.scdn.co/image/e17011b2aa33289dfa6c08 ... " 
 $ album_release_date: chr  "1993-02-22" "1995-03-28" "1997-05-28" ...
 $ album_release_year: num  1993 1995 1997 2000 2001 ...

# Filter out remixes and EPs
non_studio_albums <- c('TKOL RMX 1234567', 'In Rainbows Disk 2', 'Com Lag: 2+2=5', 'I Might Be Wrong')
album_info <- album_info %>% filter(!album_name %in% non_studio_albums)
```

Armed with all of the `album uris`, I pulled the track info for each album.

```r
get_tracks <- function(artist_info, album_info) {
    
    # You'll have to set up a dev account with Spotify here: https://developer.spotify.com/my-applications/#!/applications
    client_id <- 'xxxxxxxxxxxxxxxxxx'
    client_secret <- 'xxxxxxxxxxxxxxxxxx'
    access_token <- POST('https://accounts.spotify.com/api/token',
                         accept_json(), authenticate(client_id, client_secret),
                         body = list(grant_type='client_credentials'),
                         encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
    
    track_info <- map_df(album_info$album_uri, function(x) {
        tracks <- GET(paste0('https://api.spotify.com/v1/albums/', x, '/tracks')) %>% 
            content %>% 
            .$items 
        
        uris <- map(1:length(tracks), function(z) {
            gsub('spotify:track:', '', tracks[z][[1]]$uri)
        }) %>% unlist %>% paste0(collapse=',')
        
        res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
                   query = list(access_token = access_token)) %>% content %>% .$audio_features
        df <- unlist(res) %>% 
            matrix(nrow = length(res), byrow = T) %>% 
            as.data.frame(stringsAsFactors = F)
        names(df) <- names(res[[1]])
        df <- df %>% 
            mutate(album_uri = x,
                   track_number = row_number()) %>% 
            rowwise %>% 
            mutate(track_name = tracks[[track_number]]$name) %>%
            ungroup %>% 
            left_join(album_info, by = 'album_uri') %>% 
            rename(track_uri = id) %>% 
            select(-c(type, track_href, analysis_url, uri))
        return(df)
    }) %>%
        mutate(artist_img = artist_info$artist_img) %>% 
        mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
        mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'album_release_year',
                    'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) # for some reason parse_number() from readr doesn't work here
    return(track_info)
}

spotify_df <- get_tracks(artist_info, album_info)

str(spotify_df)

Classes ‘tbl_df’, ‘tbl’ and 'data.frame': 101 obs. of  22 variables:
 $ danceability      : num  0.223 0.515 0.185 0.212 0.364 0.294 0.256 0.384 0.25 0.284 ...
 $ energy            : num  0.706 0.43 0.964 0.696 0.37 0.813 0.906 0.717 0.62 0.825 ...
 $ key               : num  9 7 9 2 7 4 2 6 0 7 ...
 $ loudness          : num  -12.01 -9.94 -8.32 -10.06 -14.13 ...
 $ mode              : num  1 1 1 1 1 0 1 1 1 1 ...
 $ speechiness       : num  0.0581 0.0369 0.084 0.0472 0.0331 0.0547 0.0548 0.0339 0.0611 0.0595 ...
 $ acousticness      : num  0.000945 0.0102 0.000659 0.000849 0.704 0.000101 NA 0.00281 0.000849 0.00968 ...
 $ instrumentalness  : num  0.0068 0.000141 0.879 0.0165 NA 0.000756 0.366 0.569 0.0848 0.3 ...
 $ liveness          : num  0.109 0.129 0.107 0.129 0.0883 0.333 0.322 0.187 0.175 0.118 ...
 $ valence           : num  0.305 0.096 0.264 0.279 0.419 0.544 0.258 0.399 0.278 0.269 ...
 $ tempo             : num  112.9 91.8 147.4 122.4 103.4 ...
 $ track_uri         : chr  "1MyqLTRhgyWPw7v107BEuI" "6b2oQwSGFkzsMtQruIWm2p" "71wIOoaoVMUwskK5yCXZL4" ...
 $ duration_ms       : num  208667 238640 132173 325627 161533 ...
 $ time_signature    : num  3 4 4 4 4 4 4 4 4 4 ...
 $ album_uri         : chr  "6400dnyeDyD2mIFHfkwHXN" "6400dnyeDyD2mIFHfkwHXN" "6400dnyeDyD2mIFHfkwHXN" ...
 $ track_number      : num  1 2 3 4 5 6 7 8 9 10 ...
 $ track_name        : chr  "You" "Creep" "How Do You?" "Stop Whispering" ...
 $ album_name        : chr  "Pablo Honey" "Pablo Honey" "Pablo Honey" "Pablo Honey" ...
 $ album_img         : chr  "https://i.scdn.co/image/e17011b2aa33289dfa6c0828a0e40d6b56ad8820" ...
 $ album_release_date: chr  "1993-02-22" "1993-02-22" "1993-02-22" "1993-02-22" ...
 $ album_release_year: num  1993 1993 1993 1993 1993 ...
 $ artist_img        : chr  "https://i.scdn.co/image/afcd616e1ef2d2786f47b3b4a8a6aeea24a72adc" ...

```
Note that this returns more fields than necessary for this particular analysis, but I figured it was worth keeping those extra metrics in the function for future use.

### Genius Lyrics API

While this data proved to be slightly easier to pull, it was still a multi-step process. Similar to with Spotify, I first used the `search` API call to get the `artist_id`. 

```r
token <- 'xxxxxxxxxxxxxxxxxxxx'

genius_get_artists <- function(artist_name, n_results = 10) {
    baseURL <- 'https://api.genius.com/search?q=' 
    requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                         '&per_page=', n_results,
                         '&access_token=', token)
    
    res <- GET(requestURL) %>% content %>% .$response %>% .$hits
    
    map_df(1:length(res), function(x) {
        tmp <- res[[x]]$result$primary_artist
        list(
            artist_id = tmp$id,
            artist_name = tmp$name
        )
    }) %>% unique
}

genius_artists <- genius_get_artists('radiohead')
genius_artists

# A tibble: 1 × 2
  artist_id artist_name
      <int>       <chr>
1       604   Radiohead

```

Next, I looped through the contents of the `songs` endpoint (the limit is 50 per page), pulling down each result (a list containing the url of the tracks' lyrics) until the `next_page` parameter was null.

```r
baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
    tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
    track_lyric_urls <- c(track_lyric_urls, tmp$songs)
    if (!is.null(tmp$next_page)) {
        i <- tmp$next_page
    } else {
        break
    }
}

length(track_lyric_urls)
[1] 219

summary(track_lyric_urls[[1]])

                             Length Class  Mode     
annotation_count             1      -none- numeric  
api_path                     1      -none- character
full_title                   1      -none- character
header_image_thumbnail_url   1      -none- character
header_image_url             1      -none- character
id                           1      -none- numeric  
lyrics_owner_id              1      -none- numeric  
path                         1      -none- character
pyongs_count                 1      -none- numeric  
song_art_image_thumbnail_url 1      -none- character
stats                        3      -none- list     
title                        1      -none- character
url                          1      -none- character
primary_artist               8      -none- list  
```
From here, I used `rvest` to scrape the "lyrics" elements from the urls provided above.

```r
library(rvest)

lyric_scraper <- function(url) {
    read_html(url) %>% 
        html_node('lyrics') %>% 
        html_text
}

genius_df <- map_df(1:length(track_lyric_urls), function(x) {
    lyrics <- lyric_scraper(track_lyric_urls[[x]]$url)
    # strip out non-lyric text and extra spaces
    lyrics <- str_replace_all(lyrics, '\\[(Verse [[:digit:]]|Chorus|Outro|Verse|Refrain|Hook|Bridge|Intro|Instrumental)\\]|[[:digit:]]|[[:punct:]]', '')
    lyrics <- str_replace_all(lyrics, '\\n', ' ')
    lyrics <- str_replace_all(lyrics, '([A-Z])', ' \\1')
    lyrics <- str_replace_all(lyrics, ' {2,}', ' ')
    lyrics <- str_trim(lyrics)
    tots <- list(
        track_name = track_lyric_urls[[x]]$title,
        lyrics = lyrics
    )
    return(tots)
})

str(genius_df)

Classes ‘tbl_df’, ‘tbl’ and 'data.frame':   219 obs. of  2 variables:
 $ track_name: chr  "15 Step" "2 + 2 = 5" "4 Minute Warning" "Airbag" ...
 $ lyrics    : chr  "How come I end up where I started How come I end ... "
```
After bit of name-standardizing between Spotify and Genius, I left joined `genius_df` onto `spotify_df` by `track_name` (The album info will come in handy later).

```r
genius_df$track_name[genius_df$track_name == 'Packt Like Sardines in a Crushd Tin Box'] <- 'Packt Like Sardines in a Crushed Tin Box'
genius_df$track_name[genius_df$track_name == 'Weird Fishes/Arpeggi'] <- 'Weird Fishes/ Arpeggi'
genius_df$track_name[genius_df$track_name == 'A Punchup at a Wedding'] <- 'A Punch Up at a Wedding'
genius_df$track_name[genius_df$track_name == 'Dollars and Cents'] <- 'Dollars & Cents'
genius_df$track_name[genius_df$track_name == 'Bullet Proof...I Wish I Was'] <- 'Bullet Proof ... I Wish I was'

track_df <- spotify_df %>%
  mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>%
  left_join(genius_df %>% mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', '')), by = 'track_name_join')) %>%
  select(track_name, valence, duration_ms, lyrics, album_name, album_release_year, album_img)

str(track_df)

'data.frame': 101 obs. of  7 variables:
 $ track_name        : chr  "You" "Creep" "How Do You?" "Stop Whispering" ...
 $ valence           : num  0.305 0.096 0.264 0.279 0.419 0.544 0.258 0.399 0.278 0.269 ...
 $ duration_ms       : num  208667 238640 132173 325627 161533 ...
 $ lyrics            : chr  "You are The sun and moon And stars are you And I could never ..."
 $ album_name        : chr  "Pablo Honey" "Pablo Honey" "Pablo Honey" "Pablo Honey" ...
 $ album_release_year: num  1993 1993 1993 1993 1993 ...
 $ album_img         : chr  "https://i.scdn.co/image/e17011b2aa33289dfa6c0828a0e40d6b56ad ... "
```
Now onto the analysis!

## Quantifying Sentiment
Using valence alone, calculating the saddest song is pretty straightforward - the song with the lowest valence wins.

```r
track_df %>% 
    select(track_name, valence) %>%
    arrange(valence) %>% 
    slice(1:10)

                         track_name valence
1               We Suck Young Blood  0.0378
2                   True Love Waits  0.0378
3                       The Tourist  0.0400
4         Motion Picture Soundtrack  0.0425
5                  Sail To The Moon  0.0458
6                         Videotape  0.0468
7              Life In a Glasshouse  0.0516
8   Tinker Tailor Soldier Sailor...  0.0517
9                       The Numbers  0.0545
10    Everything In Its Right Place  0.0585
```

Would that it were so simple. "True Love Waits" and "We Suck Young Blood" tie here, each with a valence of 0.0378, further illustrating the need to factor in lyrics. 

While valence serves as an out-of-the box measure of musical sentiment, the emotions behind song lyrics are much more elusive. To find the most depressing song, I used sentiment analysis to pick out words associated with sadness. Specifically, I used `tidytext` and the NRC lexicon, based on a crowd-sourced [project](http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm){:target="_blank"} by the National Research Council Canada. This lexicon contains an array of emotions (sadness, joy, anger, surprise, etc.) and the words determined to most likely elicit them.

To quantify sad lyrics, I calculated the share of "sad" words per song. While the case could be made for only including unique words, I would argue that the overall sadness of a song is influenced by repetition - repeating a sad lyric can multiply its emotional effect. Furthermore, valence analyzes the sounds of the entire song, necessarily including repetitive hooks and choruses.

```r
library(tidytext)

sad_words <- sentiments %>% 
    filter(lexicon == 'nrc', sentiment == 'sadness') %>% 
    select(word) %>% 
    mutate(sad = T)

sent_df <- track_df %>% 
    unnest_tokens(word, lyrics) %>%
    left_join(sad_words, by = 'word') %>%
    group_by(track_name) %>% 
    summarise(pct_sad = sum(sad, na.rm = T) / n(),
              word_count = n()) %>% 
    ungroup

sent_df %>% 
    arrange(-pct_sad) %>% 
    head(10)

                   track_name    pct_sad word_count
                        <chr>      <dbl>      <int>
1             True Love Waits 0.16129032         62
2   Packt Like Sardines in... 0.12790698        172
3                High And Dry 0.10000000        200
4     Exit Music (For a Film) 0.09433962        106
5           Give Up The Ghost 0.08947368        190
6                  Backdrifts 0.08904110        146
7                    Let Down 0.07453416        161
8                   Identikit 0.06691450        269
9                Karma Police 0.06293706        143
10  Motion Picture Soundtrack 0.05714286         70

```

By the percentage of total words that were sad, "True Love Waits" wins, with over 16% of its lyrics containing sad words. Specifically, the algorithm picked out the words "drown", "killing", "crazy", "haunted", and "leave" - the last of which was repeated six times in the chorus ("Just don't leave. Don't leave").

Given that "True Love Waits" also tied for lowest valence, we techincally already have our winner, but I still went ahead and calculated the weighted "sentiment score" to see how the rest of the tracks stacked up.

## Lyrical Density
To combine lyrical and musical sadness, I turned to a fellow R Blogger's [analysis](https://www.r-bloggers.com/everything-in-its-right-place-visualization-and-content-analysis-of-radiohead-lyrics/){:target="_blank"}, which coincidentally also dealt with Radiohead lyrics. They explored the concept of "lyrical density", which is, according to their definition - "the number of lyrics per song over the track length". One way to interpret this is how "important" lyrics are to a given song, making it the perfect weighting metric for my analysis.

Using track duration and word count, I calculated lyrical density for each track. To create the final "sentiment score", I took the average of valence and the percentage of sad words per track, weighted by lyrical density.

<img src="/img/posts/fitterhappier/sentimentscore.png">

I also rescaled the metric to fit within 1 and 100, so that the saddest song had a score of 1 and the least sad song scored 100.

```r
library(scales)

track_df <- track_df %>% 
    left_join(sent_df, by = 'track_name') %>% 
    mutate_at(c('pct_sad', 'word_count'), funs(ifelse(is.na(.), 0, .))) %>% 
    mutate(lyrical_density = word_count / duration_ms * 1000,
           sentiment_score = round(rescale(1 - ((1 - valence) + (pct_sad * (1 + lyrical_density))) / 2, to = c(1, 100)), 2)) 

track_df %>%
    select(track_name, sentiment_score) %>%
	arrange(sentiment_score) %>%
	head(10)

                         track_name sentiment_score
                              <chr>           <dbl>
1                   True Love Waits         1.00000
2         Motion Picture Soundtrack        13.21053
3               We Suck Young Blood        15.90489
4                      Pyramid Song        16.88393
5                         Videotape        17.40243
6                 Give Up The Ghost        17.48944
7  Tinker Tailor Soldier Sailor ...        17.80405
8                   Dollars & Cents        18.08028
9                          Let Down        18.22576
10             Life In a Glasshouse        18.74126
```

As expected, "True Love Waits" is officially the single most depressing Radiohead song to date. To see how sentiment evoloved across all nine albums, I calculated the average sentiment score per album and plotted each song by album release date. To spice up the plot a bit, I created a custom tooltip incorporating the `album_img` from Spotify.

```r
library(RColorBrewer)
library(highcharter)

plot_df <- track_df %>% 
    rowwise %>% 
    mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 6, 50), 'px">', # dynamic sizing
                            '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                            '<b>Album:</b> ', album_name,
                            '<br><b>Track:</b> ', track_name)) %>% 
    ungroup

avg_line <- plot_df %>% 
    group_by(album_release_year, album_name, album_img) %>% 
    summarise(avg = mean(sentiment_score)) %>% 
    ungroup %>% 
    transmute(x = as.numeric(as.factor(album_release_year)), 
              y = avg,
              tooltip = paste0('<a style = "margin-right:50px">',
                               '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                               '<b>Album:</b> ', album_name,
                               '<br><b>Average Sentiment Score:</b> ', round(avg, 2),
                               '</a>'))
plot_track_df <- plot_df %>% 
    mutate(tooltip = paste0(tooltip, '<br><b>Sentiment Score:</b> ', sentiment_score, '</a>'),
           album_number = as.numeric(as.factor(album_release_year))) %>% 
    ungroup

album_chart <- hchart(plot_track_df, x = as.numeric(as.factor(album_release_year)), y = sentiment_score, group = album_name, type = 'scatter') %>% 
    hc_add_series_df(data = avg_line, type = 'line') %>%
    hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
    hc_colors(c(sample(brewer.pal(n_distinct(track_df$album_name), 'Paired')), 'black')) %>% 
    hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>% 
    hc_yAxis(max = 100, title = list(text = 'Sentiment Score')) %>% 
    hc_title(text = 'Data Driven Depression') %>% 
    hc_subtitle(text = 'Radiohead song sentiment by album') %>% 
    hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[10]]$name <- 'Album Averages'
album_chart
```
<iframe src="/htmlwidgets/fitterhappier/album_chart.html" height="400px"></iframe>
Of all nine studio albums, Radiohead's latest release, "A Moon Shaped Pool" boasts the saddest average sentiment score. This is driven largely by the fact that its finale, "True Love Waits", was the saddest song overall - exclude it, and "Amnesiac" takes the cake. It's also apparent that "A Moon Shaped Pool" broke the trend of increased average sentiment beginning with 2003's "Hail to the Thief".

This was a really fun dataset to work with, and there are plenty of other interesting things to explore here (artist comparisons, within album sentiment, additional song features, etc.). Thanks for reading, and I'm sure I'll revisit this soon!