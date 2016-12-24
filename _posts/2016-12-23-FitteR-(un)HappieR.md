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
With valence alone, calculating the saddest song is pretty straightforward.

```r
sound_df %>% 
    filter(valence == min(valence)) %>% 
    select(track_name, valence)
```
<iframe src="/htmlwidgets/fitterhappier/valence_chart.html" width: "100%" height: "650px"></iframe>
Wow, I guess not it's not so simple! "True Love Waits" and "We Suck Young Blood" tie here, further illustrating the need for bringing in additional metrics. 

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

lyrics_sent$track_name[lyrics_sent$track_name == 'Packt Like Sardines in a Crushd Tin Box'] <- 'Packt Like Sardines in a Crushed Tin Box'
lyrics_sent$track_name[lyrics_sent$track_name == 'Weird Fishes/Arpeggi'] <- 'Weird Fishes/ Arpeggi'
lyrics_sent$track_name[lyrics_sent$track_name == 'A Punchup at a Wedding'] <- 'A Punch Up at a Wedding'
lyrics_sent$track_name[lyrics_sent$track_name == 'Dollars and Cents'] <- 'Dollars & Cents'
lyrics_sent$track_name[lyrics_sent$track_name == 'Bullet Proof...I Wish I Was'] <- 'Bullet Proof ... I Wish I was'

arrange(lyrics_sent, -pct_sad)
```

So by the percentage of total words that were sad, "Give Up The Ghost" wins, with over 17% of its lyrics containing sad words, but "True Love Waits" is a close second! To get to the one true most depressing song, I had to find some way to combine the two metrics using some weighting metric. 

## Lyrical Density
In the strangest coincidence, it turns out that a fellow R Blogger previously came up with a concept of "lyrical density" in their [analysis](https://www.r-bloggers.com/everything-in-its-right-place-visualization-and-content-analysis-of-radiohead-lyrics/){:target="_blank"} of...Radiohead! As they describe it - "the number of lyrics per song over the track length". One way to interpret this is how "important" lyrics are to a given song, making it the perfect weighting metric for my analysis.

Recall that track duration was included in the Spotify dataset, so after a simple join I calculated lyrical density for each track and created my final measure of sonic sadness, taking the average of valence and the percentage of sad words weighted by lyrical density. I also rescaled the metric to fit within 0 and 1, so that the saddest song had a score of 0 and the least sad song scored 1.

```r
library(scales)
track_df <- sound_df %>% 
    mutate(track_name_join = tolower(gsub('[[:punct:]]', '', track_name))) %>% 
    left_join(lyrics_sent %>% mutate(track_name_join = tolower(gsub('[[:punct:]]', '', track_name))) %>% select(-track_name), by = 'track_name_join') %>% 
    mutate(word_count = ifelse(is.na(word_count), 0, word_count),
           pct_sad = ifelse(is.na(pct_sad), 0, pct_sad),
           lyrical_density = word_count / duration_ms * 1000,
           combined_sadness = rescale(1 - ((1 - valence) + (pct_sad * (1 + lyrical_density))) / 2))
```

Drum Roll...

```r
track_df %>% 
    arrange(combined_sadness) %>% 
    head
```

We have a winner! "True Love Waits" is officially the single most depressing Radiohead song to-date.

## If you think this is over, then you're wrong
It would be a shame to throw away all of this data without digging a little deeper. While searching for the most depressing song, I found a number of other interesting questions to explore.

### Has Radiohead gotten sadder over time?

### What is the prototypical Radiohead song?

## Data Appendix
The Spotify Track API itself has pretty good documentation, but it's still a pretty involved process to grab all songs for a given artist. In short, Spotify segments the API calls into track, album, and artist hierarchies, each of which need their own identifying "uri" to access. To get track info, you need the `track uri`, which can be found within the `albums` section of API. To get there, you need the `album uri` from the `artists` section, for which you need the `artist uri`. To get that, you can use the `search` API call to look for "radiohead". Note that you can also find any single uri by right clicking on the track, album, or artist directly within the Spotify app, but that would be a huge pain in the ass for this use case.

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
track_info %>% 
    datatable(rownames=F,escape=F)
```
