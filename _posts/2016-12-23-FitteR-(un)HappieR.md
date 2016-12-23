---
layout: post
title: fitteR happieR
subtitle: Finding the most depressing Radiohead song with R
bigimg: /img/ifwesmilecanwegohd.jpg
---
Radiohead has been my favorite band for several years, so I am used to people politely suggesting that I play something "less depressing". Much of Radiohead's music is undeniably sad, and this post catalogs my journey to quantify that sadness, including a data-driven determination of their single most depressing song.

## Getting Data
Spotify recently released their Track API, which provides detailed audio statistics for each song in their library. One of these metrics, "valence", measures a song's positivity. From the offical API documentation:
> A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).

So valence provides a measure of how sad a song *sounds* from a musical perspective. Another key component of a song's sentiment is its lyrics, and it just so happens that Genius Lyrics also has an API to pull track-level data. In my analysis I used a combination of both valence and lyrical sentiment, and the code for retrieiving both data sets is included at the end of the post.

## Quantifying Sentiment
With valence alone, calculating the saddest song is pretty straightforward.
{% highlight javascript %}
head(sound_df)

sound_df %>% 
    filter(valence == min(valence)) %>% 
    select(track_name, valence)
{% endhighlight %}
Wow, I guess not it's not so simple! "True Love Waits" and "We Suck Young Blood" tie here, further illustrating the need for bringing in additional metrics. 

While valence serves as an out-of-the box measure of musical sentiment, the emotions behind song lyrics are much more elusive and difficult to pin down. To find the most depressing song, I used sentiment analysis to pick out words associated with sadness. Specifically, I used `tidytext` and the NRC lexicon, which is based on a crowd-sourced project by researchers Saif Mohammad and Peter Turney. This lexicon contains an array of emotions (sadness, joy, anger, surprise, etc.) and the words determined to most likely elicit them.

For my specific metric, I chose the number of "sad" words as a share of all words in a song. While the case could be made for only including ___unique___ words, I would argue that the overall sadness of a song is influenced by repetition - repeating a sad lyric can multiply its emotional effect. Furthermore, valence analyzes the sounds of the entire song, necessarily including repetitive hooks and choruses.

{% highlight javascript %}
library(tidytext)

head(lyrics_df)

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
{% endhighlight %}

So by the percentage of total words that were sad, "Give Up The Ghost" wins, with over 17% of its lyrics containing sad words, but "True Love Waits" is a close second! To get to the one true most depressing song, I had to find some way to combine the two metrics using some weighting metric. 

## Lyrical Density
In the strangest coincidence, it turns out that a fellow R Blogger previously came up with a concept of "lyrical density" in their [analysis](https://www.r-bloggers.com/everything-in-its-right-place-visualization-and-content-analysis-of-radiohead-lyrics/) of...Radiohead! As they describe it - "the number of lyrics per song over the track length". You can interpret this as how "important" lyrics are to a given song, making it the perfect weighting metric for my analysis.

Recall that track duration was included in the Spotify dataset, so after a simple join I calculated lyrical density for each track and created my final measure of sonic sadness.
