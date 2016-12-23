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

So valence provides a measure of how sad a song *sounds* from a musical perspective. Another key component of a song's sentiment is lyrics, and it just so happens that Genius Lyrics also has an API to pull track-level data. In my analysis I used a combination of both valence and lyrical sentiment, and the code for retrieiving both data sets is included at the end of the post.
