---
layout: post
title: fitteR happieR
subtitle: Finding the most depressing Radiohead song with R, using the Spotify and Genius Lyrics APIs
bigimg: /img/ifwesmilecanwegohd.jpg
---
Radiohead has been my favorite band for several years, so I am used to people politely suggesting that I play something "less depressing". Much of Radiohead's music is undeniably sad, and this post catalogs my journey to quantify that sadness, including a data-driven determination of their single most depressing song.

## Getting Data
Spotify recently released their [Web API](https://developer.spotify.com/web-api/){:target="_blank"}, which provides detailed audio statistics for each song in their library. One of these metrics, "valence", measures a song's positivity. From the offical API documentation:

> A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).

So valence provides a measure of how sad a song *sounds* from a musical perspective. Another key component of a song's sentiment is its lyrics, and it just so happens that Genius Lyrics also has an [API](https://docs.genius.com/){:target="_blank"} to pull track-level data. In my analysis I used a combination of both valence and lyrical sentiment, and the code for retrieiving both data sets is included at the end of the post. I used the resulting dataframes, `sound_df` and `lyrics_df`, for the following analysis.

```r
head(sound_df)
head(lyrics_df)
```

## Quantifying Sentiment
Using valence alone, calculating the saddest song is pretty straightforward - the song with the lowest valence wins.

```r
sound_df %>% 
    arrange(valence) %>% 
    slice(1:10) %>% 
    select(track_name, valence)

                                                        track_name valence
1                                              We Suck Young Blood  0.0378
2                                                  True Love Waits  0.0378
3                                                      The Tourist  0.0400
4                                        Motion Picture Soundtrack  0.0425
5                                                 Sail To The Moon  0.0458
6                                                        Videotape  0.0468
7                                             Life In a Glasshouse  0.0516
8  Tinker Tailor Soldier Sailor Rich Man Poor Man Beggar Man Thief  0.0517
9                                                      The Numbers  0.0545
10                                   Everything In Its Right Place  0.0585
```

Would that it were so simple. "True Love Waits" and "We Suck Young Blood" tie here, each with a valence of 0.0378, further illustrating the need to bring in additional metrics. 

While valence serves as an out-of-the box measure of musical sentiment, the emotions behind song lyrics are much more elusive and difficult to pin down. To find the most depressing song, I used sentiment analysis to pick out words associated with sadness. Specifically, I used `tidytext` and the NRC lexicon, which is based on a crowd-sourced [project](http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm){:target="_blank"} by researchers Saif Mohammad and Peter Turney. This lexicon contains an array of emotions (sadness, joy, anger, surprise, etc.) and the words determined to most likely elicit them.

For my specific metric, I chose the number of "sad" words as a share of all words in a song. While the case could be made for only including unique words, I would argue that the overall sadness of a song is influenced by repetition - repeating a sad lyric can multiply its emotional effect. Furthermore, valence analyzes the sounds of the entire song, necessarily including repetitive hooks and choruses.

```r
library(tidytext)

nrc <- sentiments %>% 
    filter(lexicon == 'nrc',
           sentiment == 'sadness') %>% 
    select(word) %>% 
    mutate(sad = T)

lyrics_sent <- lyrics_df %>% 
    unnest_tokens(word, lyrics) %>%
    left_join(nrc, by = 'word') %>%
    group_by(track_name) %>% 
    summarise(word_count = n(),
              pct_sad = sum(sad, na.rm = T) / word_count) %>% 
    ungroup

arrange(lyrics_sent, -pct_sad)

# A tibble: 208 x 3
                                 track_name word_count    pct_sad
                                      <chr>      <int>      <dbl>
1                         Give Up The Ghost        190 0.17368421
2                           True Love Waits         62 0.16129032
3  Packt Like Sardines in a Crushed Tin Box        172 0.12790698
4                              High and Dry        200 0.10000000
5                                       Fog         73 0.09589041
6                    How I Made My Millions         42 0.09523810
7                   Exit Music (For a Film)        106 0.09433962
8                                 The Thief        200 0.09000000
9                                Backdrifts        146 0.08904110
10                                 Let Down        161 0.07453416
# ... with 198 more rows
```

So by the percentage of total words that were sad, "Give Up The Ghost" wins, with over 17% of its lyrics containing sad words, but "True Love Waits" is a close second! To combine musical and lyrical sentiment, I turned to a previous R Blogger's analysis.

## Lyrical Density
In the strangest coincidence, it turns out that a fellow R Blogger previously came up with a concept of "lyrical density" in their [analysis](https://www.r-bloggers.com/everything-in-its-right-place-visualization-and-content-analysis-of-radiohead-lyrics/){:target="_blank"} of...Radiohead! Lyrical density is, according to their definition - "the number of lyrics per song over the track length". One way to interpret this is how "important" lyrics are to a given song, making it the perfect weighting metric for my analysis.

Fortunately, track duration was included in the Spotify dataset, so after a simple join I calculated lyrical density for each track and created my final measure of sonic sadness, taking the average of valence and the percentage of sad words weighted by lyrical density. I also rescaled the metric to fit within 0 and 1, so that the saddest song had a score of 0 and the least sad song scored 1.

```r
library(scales)

# Before, joining, I reconciled the differences between a few of the track names
lyrics_sent$track_name[lyrics_sent$track_name == 'Packt Like Sardines in a Crushd Tin Box'] <- 'Packt Like Sardines in a Crushed Tin Box'
lyrics_sent$track_name[lyrics_sent$track_name == 'Weird Fishes/Arpeggi'] <- 'Weird Fishes/ Arpeggi'
lyrics_sent$track_name[lyrics_sent$track_name == 'A Punchup at a Wedding'] <- 'A Punch Up at a Wedding'
lyrics_sent$track_name[lyrics_sent$track_name == 'Dollars and Cents'] <- 'Dollars & Cents'
lyrics_sent$track_name[lyrics_sent$track_name == 'Bullet Proof...I Wish I Was'] <- 'Bullet Proof ... I Wish I was'

track_df <- sound_df %>% 
    mutate(track_name_join = tolower(gsub('[[:punct:]]', '', track_name))) %>% 
    left_join(lyrics_sent %>% mutate(track_name_join = tolower(gsub('[[:punct:]]', '', track_name))) %>% select(-track_name), by = 'track_name_join') %>% 
    mutate(word_count = ifelse(is.na(word_count), 0, word_count),
           pct_sad = ifelse(is.na(pct_sad), 0, pct_sad),
           lyrical_density = word_count / duration_ms * 1000,
           sentiment_score = rescale(1 - ((1 - valence) + (pct_sad * (1 + lyrical_density))) / 2)) 
```

Drum Roll...

```r
track_df %>%
	arrange(sentiment_score) %>%
	head(10)

                                                        track_name sentiment_score
1                                                  True Love Waits          1.0000
2                                                Give Up The Ghost          3.7846
3                                        Motion Picture Soundtrack         13.2105
4                                              We Suck Young Blood         16.0205
5                                                     Pyramid Song         16.8839
6                                                        Videotape         17.4024
7  Tinker Tailor Soldier Sailor Rich Man Poor Man Beggar Man Thief         17.8040
8                                                  Dollars & Cents         18.0803
9                                                         Let Down         18.2258
10                                            Life In a Glasshouse         18.7413
```

We have a winner! "True Love Waits" is officially the single most depressing Radiohead song to-date. To visualize the results, I plotted the sentiment score of each song with the `highcharter` package.


```r
track_df %>% 
    rowwise %>% 
    mutate(tooltip = paste0('<a style = "margin-right:', max(nchar(track_name), nchar(album_name)) * 9, 'px">',
                            '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                            '<b>Album:</b> ', album_name,
                            '<br><b>Track:</b> ', track_name,
                            '<br><b>Valence:</b> ', sentiment_score),
           '</a>') %>% 
    ungroup %>% 
    select(track_name, tooltip, sentiment_score) %>% 
    arrange(-sentiment_score) %>% 
    hchart(x = track_name, y = sentiment_score, type = 'bar') %>%
    hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
    hc_yAxis(plotLines = list(list(label = list(text = 'Average', verticalAlign = 'middle', y = 50),
                                   color = 'black',
                                   width = 2,
                                   value = mean(track_df$sentiment_score, na.rm = T),
                                   zIndex = 4)
    ),
    max = 100
    ) %>% 
    hc_xAxis(title = list(enabled = F)) %>% 
    hc_yAxis(title = list(text = 'Sentiment Score')) %>% 
    hc_title(text = 'Sentiment of Radiohead Songs') %>% 
    hc_subtitle(text = 'Average of track valence and lyrical sentiment, weighted by lyrical density') %>% 
    hc_add_theme(hc_theme_smpl())
```
<iframe src="/htmlwidgets/fitterhappier/track_sentiment_bar.html"></iframe>

## If you think this is over, then you're wrong
It would be a shame to throw away all of this data without digging a little deeper. While searching for the most depressing song, I found a number of other interesting questions to explore.

### Has Radiohead become sadder over time?
```r
library(RColorBrewer)

plot_df <- track_df %>% 
    rowwise %>% 
    mutate(tooltip = paste0('<a style = "margin-right:', max(nchar(track_name), nchar(album_name)) * 9, 'px">',
                            '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                            '<b>Album:</b> ', album_name,
                            '<br><b>Track:</b> ', track_name)) %>% 
    ungroup
avg_line <- plot_df %>% 
    group_by(album_release_year, album_name, album_img) %>% 
    summarise(avg = mean(sentiment_score)) %>% 
    ungroup %>% 
    transmute(x = as.numeric(as.factor(album_release_year)), y = avg,
              tooltip = paste0('<a style = "margin-right:', nchar(album_name) * 10, 'px">',
                               '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                               '<b>Album:</b> ', album_name,
                               '<br><b>Average Track Sentiment Score:</b> ', round(avg, 4),
                               '</a>'))
plot_track_df <- plot_df %>% 
    mutate(tooltip = paste0(tooltip, '<br><b>Sentiment Score:</b> ', sentiment_score,
                            '</a>'),
           album_number = as.numeric(as.factor(album_release_year))) %>% 
    ungroup

hc <- hchart(plot_track_df, x = as.numeric(as.factor(album_release_year)), y = sentiment_score, group = album_name, type = 'scatter') %>% 
    hc_add_series_df(data = avg_line, type = 'line') %>%
    hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
    hc_colors(c(brewer.pal(n_distinct(track_df$album_name), 'Set3'), 'black')) %>% 
    hc_xAxis(title = list(enabled = F)) %>% 
    hc_yAxis(max = 100, title = list(text = 'Sentiment Score')) %>% 
    hc_title(text = 'Data Driven Depression') %>% 
    hc_subtitle(text = 'Radiohead sentiment by album') %>% 
    hc_add_theme(hc_theme_smpl())
hc$x$hc_opts$series[[10]]$name <- 'Album Averages'
```
<iframe src="/htmlwidgets/fitterhappier/album_chart.html" height="400px"></iframe>

### How does sentiment evolve within Radiohead's albums?

<!-- ```r
library(purrr)
library(htmltools)
track_num_album <- track_df %>% 
    rowwise %>% 
    mutate(tooltip = paste0('<a style = "margin-right:', max(nchar(track_name), nchar(album_name)) * 9, 'px">',
                        '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                        '<b>Album:</b> ', album_name,
                        '<br><b>Track:</b> ', track_name)) %>% 
    ungroup %>% 
    group_by(album_name) %>% 
    mutate(tooltip = paste0(tooltip,
                        '<br><b>Album Position:</b> ', track_number, '/', max(track_number),
                        '<br><b>Sentiment Score:</b> ', sentiment_score,
                        '</a>'
                            )) %>% 
    ungroup

map(track_num_album %>% arrange(album_release_date) %>% select(album_name) %>% unique %>% .[[1]], function(x) {
    hchart(track_num_album %>% filter(album_name == x), x = track_number, y = sentiment_score, type = 'line') %>% 
        hc_title(text = x) %>% 
        hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
        hc_yAxis(max = 100, title = list(text = 'Sentiment Score')) %>% 
        hc_xAxis(title = list(text = 'Track Number'),
                 allowDecimals = F) %>% 
        hc_add_theme(hc_theme_smpl())
}) %>% hw_grid(rowheight = 200, ncol = 3) %>% browsable
```
<iframe src="/htmlwidgets/fitterhappier/track_num_album"></iframe> -->

```r
track_order <- track_df %>% 
    group_by(album_name, album_img) %>% 
    mutate(scaled_track_number = rescale(track_number / max(track_number), c(0, 1))) %>% 
    ungroup %>% 
    rowwise %>% 
    mutate(tooltip = paste0('<a style = "margin-right:', max(nchar(track_name), nchar(album_name)) * 9, 'px">',
                        '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                        '<b>Album:</b> ', album_name,
                        '<br><b>Track:</b> ', track_name)) %>% 
    ungroup %>% 
    group_by(album_name) %>% 
    mutate(tooltip = paste0(tooltip,
                        '<br><b>Album Position:</b> ', track_number, '/', max(track_number),
                        '<br><b>Sentiment Score:</b> ', sentiment_score,
                        '</a>'
                            )) %>% 
    ungroup %>% 
    arrange(scaled_track_number) %>% 
    mutate(smooth_track_order = loess(sentiment_score ~ scaled_track_number, data = .) %>% .$fitted)

track_num_combined <- highchart() %>% 
    hc_add_series_df(data = track_order %>% arrange(album_release_year) %>% select(x = scaled_track_number, y = sentiment_score, group = album_name, tooltip = tooltip), group = group, type = 'scatter') %>% 
    hc_add_series_df(data = track_order %>% select(x = scaled_track_number, y = smooth_track_order) %>% mutate(tooltip = 'Smooth Fitted Line'), type = 'line') %>% 
    hc_colors(c(brewer.pal(n_distinct(track_df$album_name), 'Set3'), 'black')) %>% 
    hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
    hc_yAxis(max = 100, title = list(text = 'Sentiment Score')) %>% 
    hc_xAxis(labels = list(formatter = JS(paste0("function() {return this.value * 100 + '%';}"))),
             title = list(text = 'Album Position Percentile')) %>% 
    hc_title(text = 'Everything in its Right Place') %>% 
    hc_subtitle(text = 'Track Sentiment by Album Position') %>% 
    hc_add_theme(hc_theme_smpl())
track_num_combined$x$hc_opts$plotOptions$series$marker$symbol <- 'circle'
track_num_combined$x$hc_opts$series[[10]]$name <- 'Smooth Fitted Line'
track_num_combined
```
<iframe src="/htmlwidgets/fitterhappier/track_num_combined"></iframe>

### What is the prototypical Radiohead song?

## Data Appendix

### Spotify Web API

The Spotify Web API itself has pretty good documentation, but it's still a pretty involved process to grab all songs for a given artist. In short, Spotify segments the API calls into track, album, and artist hierarchies, each of which need their own identifying "uri" to access. To get track info, you need the `track uri`, which can be found within the `albums` section of API. To get there, you need the `album uri` from the `artists` section, for which you need the `artist uri`. To get that, you can use the `search` API call to look for "radiohead". Note that you can also find any single uri by right clicking on the track, album, or artist directly within the Spotify app, but that would be a huge pain in the ass for this use case.

First, I created a function to search for artist names.

```r
get_artists <- function(artist_name) {
    
    # Search Spotify API for artist name
    res <- GET('https://api.spotify.com/v1/search', query = list(q = artist_name, type = 'artist')) %>%
        content %>% .$artists %>% .$items
    
    # Clean response and combine all returned artists into a dataframe
    artists <- map_df(seq_len(length(res)), function(x) {
        list(
            artist_name = res[[x]]$name,
            artist_uri = gsub('spotify:artist:', '', res[[x]]$uri), # remove meta info from the uri string
            artist_img = ifelse(length(res[[x]]$images) > 0, res[[x]]$images[[1]]$url, NA) # we'll grab this just for fun
        )
    })
    
    return(artists)
}

get_artists('radiohead') %>% 
    mutate(artist_img = paste0('<img src=', artist_img, ' height="50">')) %>% 
    datatable(escape=F,rownames=F)

# Filter out the tribute band
artist_info <- get_artists('radiohead') %>% 
    filter(artist_name == 'Radiohead')
```

Next, I used the `artist uri` obtained above to search for all of Radiohead's albums.

```r
get_albums <- function(artist_uri) {
    albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist_uri,'/albums')) %>% content
    
    map_df(1:length(albums$items), function(x) {
        tmp <- albums$items[[x]]
        
        # Make sure the album_type is not "single"
        if (tmp$album_type == 'album') {
            data.frame(album_uri = tmp$uri %>% gsub('spotify:album:', '', .),
                       album_name = gsub('\'', '', tmp$name),
                       album_img = albums$items[[x]]$images[[1]]$url,
                       stringsAsFactors = F) %>%
                mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', tmp$uri %>% gsub('spotify:album:', '', .))) %>% content %>% .$release_date, # yep, you need a separate call to on "albums" to get release date.
                       album_release_year = ifelse(nchar(album_release_date) == 4, year(as.Date(album_release_date, '%Y')), year(as.Date(album_release_date, '%Y-%m-%d'))) # not all album_release_dates have months, so I created album_release year for sorting
                       )
        } else {
            NULL
        }
        
    }) %>% filter(!duplicated(tolower(album_name))) %>%  # Sometimes there are multiple versions (just with different capitalizations) of the same album
        arrange(album_release_year)
}

album_info <- get_albums(artist_info$artist_uri)

album_info %>% 
    mutate(album_img = paste0('<img src=', album_img, ' height="50">')) %>% 
    datatable(escape=F,rownames=F)

# Some remixes and EPs snuck through
non_studio_albums <- c('TKOL RMX 1234567', 'In Rainbows Disk 2', 'Com Lag: 2+2=5', 'I Might Be Wrong')
album_info <- album_info %>% filter(!album_name %in% non_studio_albums)
```

Armed with all of the `album uris`, I pulled the track info for each album.

```r
get_tracks <- function(artist_info, album_info) {
    
    # You'll have to set up a dev account with Spotify here:
    client_id <- 'c857dcec62a74825985e4749ef531abe'
    client_secret <- '46cb88674ec641a0ab124aa190060b70'
    access_token <- POST('https://accounts.spotify.com/api/token',
                         accept_json(), authenticate(client_id, client_secret),
                         body = list(grant_type='client_credentials'),
                         encode = 'form') %>% content %>% .$access_token
    
    track_info <- map_df(album_info$album_uri, function(x) {
        tracks <- GET(paste0('https://api.spotify.com/v1/albums/', x, '/tracks')) %>% content %>% .$items
        
        map_df(1:length(tracks), function(y) {
            Sys.sleep(.1)
            tmp <- tracks[y][[1]]
            res <- GET(paste0('https://api.spotify.com/v1/audio-features/', gsub('spotify:track:', '', tmp$uri)),
                       query = list(access_token = access_token)) %>% content
            res$album_uri <- x
            res$track_uri <- res$id
            res$album_name <- album_info$album_name[album_info$album_uri == x]
            res$album_release_date <- album_info$album_release_date[album_info$album_uri == x]
            res$album_release_year <- album_info$album_release_year[album_info$album_uri == x]
            res$album_img <- album_info$album_img[album_info$album_uri == x]
            res$artist_name <- artist_info$artist_name
            res$artist_img <- artist_info$artist_img
            res$track_name <- tmp$name
            res$track_number <- tmp$track_number
            
            res <- as.data.frame(res, stringsAsFactors = F) %>% 
                select(-c(type, id, track_href, analysis_url, uri))
            
            return(res)
        })
        
    }) %>%
        mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
        mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'album_release_year',
                    'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) # for some reason parse_number() from readr doesn't work here
    return(track_info)
}

track_info <- get_tracks(artist_info, album_info)
```

### Genius Lyrics API

I used the Genius Lyrics API, and while this data proved to be slightly easier to pull, it is still a multi-step process. Similar to with Spotify, you first need to use the `search` call to get the `artist_uri`. 
```r
library(rvest)

artist_name <- 'radiohead'
n_results <- 10
token <- '0WblX68Oq3W8IqgBwBDL8MqY0cSPy5hOsD7L3aLQLd6rXxQYdh0Fs_ZbhtgeGn9O'

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
```
From here, I used `rvest` to scrape the "lyrics" elements from the urls provided above.

```r
lyric_scraper <- function(url) {
    read_html(url) %>% 
        html_node('lyrics') %>% 
        html_text
}

lyrics_df <- map_df(1:length(track_lyric_urls), function(x) {
    lyrics <- lyric_scraper(track_lyric_urls[[x]]$url)
    lyrics <- str_replace_all(lyrics, '\\[Verse [[:digit:]]\\]|\\[Chorus\\]|\\[Outro\\]|\\[Verse\\]|\\[Refrain\\]|\\[Hook\\]|\\[Bridge\\]|\\[Intro\\]|\\[Instrumental\\]', '') # I'm kind of a regex n00b, I'm sure there's a better way to do this.
    lyrics <- str_replace_all(lyrics, '\\n', ' ')
    lyrics <- str_replace_all(lyrics, '([A-Z])', ' \\1')
    lyrics <- str_replace_all(lyrics, ' {2,}', ' ')
    lyrics <- str_trim(lyrics)
    tots <- list(
        track_name = track_lyric_urls[[x]]$title,
        lyrics = lyrics
    )
    print(tots$track_name)
    return(tots)
})
```
