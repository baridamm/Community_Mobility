dCOVID EFFECT
================

The goal of this project is to analyze the effect of COVID-19 on
communities in different countries. For this purpose we use community
mobility reports data. These Community Mobility Reports aim to provide
insights into what has changed in response to policies aimed at
combating COVID-19. The reports chart movement trends over time by
geography, across different categories of places such as retail and
recreation, groceries and pharmacies, parks, transit stations,
workplaces, and residential.

The project utilizes unsupervised machine learning and time series
analysis to analyze the data.

Libraries

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(tidyr)
```

Importing and exploring the data

``` r
global_df <- read.csv("~/Downloads/Global_Mobility_Report.csv")

str(global_df)
```

    ## 'data.frame':    8156037 obs. of  15 variables:
    ##  $ country_region_code                               : chr  "AE" "AE" "AE" "AE" ...
    ##  $ country_region                                    : chr  "United Arab Emirates" "United Arab Emirates" "United Arab Emirates" "United Arab Emirates" ...
    ##  $ sub_region_1                                      : chr  "" "" "" "" ...
    ##  $ sub_region_2                                      : chr  "" "" "" "" ...
    ##  $ metro_area                                        : chr  "" "" "" "" ...
    ##  $ iso_3166_2_code                                   : chr  "" "" "" "" ...
    ##  $ census_fips_code                                  : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ place_id                                          : chr  "ChIJvRKrsd9IXj4RpwoIwFYv0zM" "ChIJvRKrsd9IXj4RpwoIwFYv0zM" "ChIJvRKrsd9IXj4RpwoIwFYv0zM" "ChIJvRKrsd9IXj4RpwoIwFYv0zM" ...
    ##  $ date                                              : chr  "2020-02-15" "2020-02-16" "2020-02-17" "2020-02-18" ...
    ##  $ retail_and_recreation_percent_change_from_baseline: int  0 1 -1 -2 -2 -2 -3 -2 -1 -3 ...
    ##  $ grocery_and_pharmacy_percent_change_from_baseline : int  4 4 1 1 0 1 2 2 3 0 ...
    ##  $ parks_percent_change_from_baseline                : int  5 4 5 5 4 6 6 4 3 5 ...
    ##  $ transit_stations_percent_change_from_baseline     : int  0 1 1 0 -1 1 0 -2 -1 -1 ...
    ##  $ workplaces_percent_change_from_baseline           : int  2 2 2 2 2 1 -1 3 4 3 ...
    ##  $ residential_percent_change_from_baseline          : int  1 1 1 1 1 1 1 1 1 1 ...

``` r
length(unique(global_df$date))
```

    ## [1] 679

``` r
length(unique(global_df$country_region))
```

    ## [1] 135

The data set contains places of 135 different countries for 6 different
categories over a period of almost 2 years, from 2020/2/15 to
2021/12/24.

Data cleaning

As there are many missing values in the data set we only use places that
have more than 100 days of observation for the analysis. We prepare the
data for clustering by calculating the average value of each variable
for each unique place.

``` r
global_df$date <- as.Date(global_df$date)

global_df %>%
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))/nrow(global_df)))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ##   country_region_code country_region sub_region_1 sub_region_2 metro_area
    ## 1        0.0006316793              0            0            0          0
    ##   iso_3166_2_code census_fips_code place_id date
    ## 1               0        0.7902429        0    0
    ##   retail_and_recreation_percent_change_from_baseline
    ## 1                                          0.3807526
    ##   grocery_and_pharmacy_percent_change_from_baseline
    ## 1                                         0.4075194
    ##   parks_percent_change_from_baseline
    ## 1                          0.5267573
    ##   transit_stations_percent_change_from_baseline
    ## 1                                     0.5032563
    ##   workplaces_percent_change_from_baseline
    ## 1                              0.03663201
    ##   residential_percent_change_from_baseline
    ## 1                                0.3946868

``` r
effect_df <- global_df %>% select('country_region_code', 'country_region', 'date','place_id' ,  'retail_and_recreation_percent_change_from_baseline', 'grocery_and_pharmacy_percent_change_from_baseline', 'parks_percent_change_from_baseline', 'transit_stations_percent_change_from_baseline', 'workplaces_percent_change_from_baseline', 'residential_percent_change_from_baseline' )

#summarizing number of values for each column
grouped_df <- effect_df %>% group_by(country_region, place_id) %>% summarise_all(funs(n()-sum(is.na(.))))

#subseting the data to places with more than 100 observations
filtered_df <- subset(grouped_df, grouped_df$residential_percent_change_from_baseline>100 & grouped_df$retail_and_recreation_percent_change_from_baseline>100 & grouped_df$grocery_and_pharmacy_percent_change_from_baseline>100 & grouped_df$parks_percent_change_from_baseline>100 & grouped_df$transit_stations_percent_change_from_baseline>100)

#subseting the initial data frame to places with more than 100 observations
clustering_df <- subset(effect_df, effect_df$place_id %in% filtered_df$place_id)

#calculating the average of each feature for each place
clustering_mean_df <- clustering_df %>% 
  group_by(country_region, place_id) %>% summarise(across(retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline, funs(mean(.,na.rm=TRUE))))
```

    ## `summarise()` has grouped output by 'country_region'. You can override using the `.groups` argument.

Clustering

For clustering we only use the average of each feature for the
countries. As mentioned, only places with more than 100 days of
observation are taken into consideration. The purpose is to find similar
countries based on effects of COVID-19 on the countries economy. 6
features are taken into consideration for each country.

``` r
#creating a new data frame with the average value for each country
cluster_final_df <- clustering_mean_df %>% group_by(country_region) %>% summarise(across(retail_and_recreation_percent_change_from_baseline_mean:residential_percent_change_from_baseline_mean, funs(mean(.,na.rm=TRUE))))

dim(cluster_final_df)
```

    ## [1] 132   7

The final data consists of 132 countries.

We use k\_means and hierarchical algorithms for clustering. For finding
the best clustering method and number of clusters we use 2 internal
validation metrics, average Silhouette width and Dunn index.

``` r
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
#using elbow method
fviz_nbclust(cluster_final_df[2:7], kmeans, method = "wss")
```

![](covid_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The elbow method shows that the best number of clusters for the k\_mean
algorithm is 3. However, we will calculate both average Silhouette width
and Dunn index for k from 2 to 5, and for both algorithms to find the
best number of clusters. As all the features are on the same scale
(percentage change from the baseline), there is no need to scale the
data.

K-mean Clustering

``` r
library(ClusterR)
```

    ## Loading required package: gtools

``` r
library(cluster)
library(fpc)
set.seed(2021)

k_mean_sil <- c()
k_mean_dunn <- c()
#k_mean clustering
for(i in 2:5){
  km <- kmeans(cluster_final_df[,2:7], i , nstart=20) 
  cluster_final_df$km_class <- km$cluster
  sil_km <- silhouette(cluster_final_df$km_class, dist(cluster_final_df[,2:7]))
  sil_km_sum <- summary(sil_km)
  k_mean_sil <- c(k_mean_sil,sil_km_sum$avg.width) 
  
  km_stats <- cluster.stats(dist(cluster_final_df[,2:7]), cluster_final_df$km_class)
  k_mean_dunn <- c(k_mean_dunn, km_stats$dunn)
}

k_mean_sil
```

    ## [1] 0.3572688 0.3941130 0.3726242 0.2896702

``` r
k_mean_dunn
```

    ## [1] 0.06877339 0.08756453 0.12801891 0.09656946

For the k\_mean algorithm we use k=3 for the number of clusters based on
internal validation metrics.

Internal validation summary

``` r
#average Silhouette width
km <- kmeans(cluster_final_df[,2:7], 3 , nstart=20) 
  cluster_final_df$km_class <- km$cluster
  sil_km <- silhouette(cluster_final_df$km_class, dist(cluster_final_df[,2:7]))
  sil_km_sum <- summary(sil_km)
  fviz_silhouette(sil_km) 
```

    ##   cluster size ave.sil.width
    ## 1       1   23          0.23
    ## 2       2   50          0.36
    ## 3       3   59          0.49

![](covid_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

The average silhouette with is 0.39 for the k\_mean algorithm with k=3.
This metric is calculated as follows:

s(i) := (b(i) - a(i)) / max(a(i), b(i)).

a(i) = average dissimilarity between i and all other points of the
cluster to which i belongs For all other clusters C, put d(i,C) =
average dissimilarity of i to all observations of C. The smallest of
these d(i,C) is b(i)

The average Silhouette width is the mean of all the s(i). This metric
should be maximized.

``` r
#Dunn index
km_stats <- cluster.stats(dist(cluster_final_df[,2:7]), cluster_final_df$km_class)
km_stats$dunn
```

    ## [1] 0.08756453

The Dunn index is 0.087 Dunn index is calculated as follows:

D= min.separation/max.diameter

This metric should be maximized.

``` r
#number of countries in each cluster
sil_km_sum$clus.sizes
```

    ## cl
    ##  1  2  3 
    ## 23 50 59

Scatter plots for visualizing different features of different clusters

``` r
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
plot_ly(cluster_final_df, type="scatter", x=cluster_final_df$retail_and_recreation_percent_change_from_baseline_mean_mean, y=cluster_final_df$grocery_and_pharmacy_percent_change_from_baseline_mean_mean, color=factor(cluster_final_df$km_class), alpha=0.8) %>% layout(xaxis=list(title="retail and recreation percent change from baseline"), yaxis=list(title="grocery and pharmacy percent change from baseline"))
```

    ## No scatter mode specifed:
    ##   Setting the mode to markers
    ##   Read more about this attribute -> https://plotly.com/r/reference/#scatter-mode

![](covid_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

As shown by the scatter plot COVID\_19 hasn’t had a negative impact on
grocery and pharmacy and retail and recreation on most of the countries
in class 1. The plot shows that COVID\_19 has had a minor negative
impact on countries retail and recreation in class 2. Grocery and
pharmacy has stayed almost the same for countries in this class. Class 3
are the countries with the most negative impacts.

``` r
plot_ly(cluster_final_df, type="scatter", x=cluster_final_df$parks_percent_change_from_baseline_mean_mean, y=cluster_final_df$transit_stations_percent_change_from_baseline_mean_mean, color=factor(cluster_final_df$km_class), alpha=0.8) %>% layout(xaxis=list(title="parks percent change from baseline"), yaxis=list(title="transit stations percent change from baseline"))
```

    ## No scatter mode specifed:
    ##   Setting the mode to markers
    ##   Read more about this attribute -> https://plotly.com/r/reference/#scatter-mode

![](covid_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

This scatter plots shows that countries in class 3 have been most
influenced in parks and transit station areas by COVID\_19. The plot
illustrates that some of the countries have seen more than 40 percent
change in both areas. Unlike the last plot, countries in class 2 are
clearly divided from countries in class 3.

Hierarchical Clustering

This time we will use the hierarchical algorithm for clustering the
countries

``` r
set.seed(2021)

hc_sil <- c()
hc_dunn <- c()
#hierarchical clustering
for(i in 2:5){
  hc <- hclust(dist(cluster_final_df[,2:7]), method="complete")
  cluster_final_df$hc_class <- cutree(hc, i)
  sil_hc <- silhouette(cluster_final_df$hc_class, dist(cluster_final_df[,2:7]))
  sil_hc_sum <- summary(sil_hc)
  hc_sil <- c(hc_sil,sil_hc_sum$avg.width) 
  
  hc_stats <- cluster.stats(dist(cluster_final_df[,2:7]), cluster_final_df$hc_class)
  hc_dunn <- c(hc_dunn, hc_stats$dunn)
}

hc_sil
```

    ## [1] 0.3482170 0.3370441 0.3459815 0.2781309

``` r
hc_dunn
```

    ## [1] 0.1016339 0.1302105 0.1470190 0.1142418

The metrics for 2 and 3 are really close as we used k=3 for the k\_mean
algorithm we will try the same to be able to compare the results better.

``` r
#average Silhouette width
hc <- hclust(dist(cluster_final_df[,2:7]), method="complete")
cluster_final_df$hc_class <- cutree(hc, 3)
sil_hc <- silhouette(cluster_final_df$hc_class, dist(cluster_final_df[,2:7]))
fviz_silhouette(sil_hc) 
```

    ##   cluster size ave.sil.width
    ## 1       1   77          0.20
    ## 2       2   54          0.54
    ## 3       3    1          0.00

![](covid_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
sil_hc_sum <- summary(sil_hc)
```

When dividing the countries to 3 clusters, the hierarchical algorithm
provides a cluster with only 1 observation, therefore we will divide the
countries into 2 clusters.

``` r
hc <- hclust(dist(cluster_final_df[,2:7]), method="complete")
cluster_final_df$hc_class <- cutree(hc, 2)
sil_hc <- silhouette(cluster_final_df$hc_class, dist(cluster_final_df[,2:7]))
fviz_silhouette(sil_hc) 
```

    ##   cluster size ave.sil.width
    ## 1       1   78          0.21
    ## 2       2   54          0.55

![](covid_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
sil_hc_sum <- summary(sil_hc)
```

``` r
hc_stats <- cluster.stats(dist(cluster_final_df[,2:7]), cluster_final_df$hc_class)
hc_stats$dunn
```

    ## [1] 0.1016339

``` r
sil_hc_sum$clus.sizes
```

    ## cl
    ##  1  2 
    ## 78 54

The hierarchical algorithm divides the countries into 2 clusters with
size 78 and 54.

Visualizing the dendrogram

``` r
library(dendextend)
```

    ## 
    ## ---------------------
    ## Welcome to dendextend version 1.15.2
    ## Type citation('dendextend') for how to cite the package.
    ## 
    ## Type browseVignettes(package = 'dendextend') for the package vignette.
    ## The github page is: https://github.com/talgalili/dendextend/
    ## 
    ## Suggestions and bug-reports can be submitted at: https://github.com/talgalili/dendextend/issues
    ## You may ask questions at stackoverflow, use the r and dendextend tags: 
    ##   https://stackoverflow.com/questions/tagged/dendextend
    ## 
    ##  To suppress this message use:  suppressPackageStartupMessages(library(dendextend))
    ## ---------------------

    ## 
    ## Attaching package: 'dendextend'

    ## The following object is masked from 'package:stats':
    ## 
    ##     cutree

``` r
dend <- as.dendrogram(hclust(dist(cluster_final_df[,2:7])))
dend <- set(dend, "labels_cex", 0.5)
labels(dend) <- cluster_final_df$country_region
dend1 <- color_branches(dend, k=2) %>% 
  color_labels(dend, k=2)
plot(dend1)
```

![](covid_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

TIME SERIES ANALYSIS

We use time series analysis to understand how has COVID-19 effected
different countries over time. The goal is to compare different
countries from different clusters, look for trends in the data and
forecast the values over the next month.

``` r
set.seed(10)
top_1 <- subset(cluster_final_df, cluster_final_df$km_class==1)
sample <- as.data.frame(sample_n(top_1, 3))
ts_1 <- subset(clustering_df, clustering_df$country_region %in% sample$country_region)


places_1 <- unique(ts_1 %>% select("country_region", "place_id")) %>% distinct(country_region, .keep_all=TRUE)
ts_1 <- subset(ts_1, ts_1$place_id %in% places_1$place_id)
```

![](covid_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

The plot illustrates retail and recreation percent change from a
baseline, for 3 countries in one cluster, since the start of the
pandemic. They all experienced a drastic decrease at the start of the
pandemic in April of 2020. It can be seen that Iraq follows an upward
trend until the end of the period. Ghana has the same trend, but with a
slower increase.

``` r
set.seed(1)
top_3 <- subset(cluster_final_df, cluster_final_df$km_class==3)
sample <- as.data.frame(sample_n(top_3, 3))
ts_3 <- subset(clustering_df, clustering_df$country_region %in% sample$country_region)


places_3 <- unique(ts_3 %>% select("country_region", "place_id")) %>% distinct(country_region, .keep_all=TRUE)
ts_3 <- subset(ts_3, ts_3$place_id %in% places_3$place_id)
```

![](covid_files/figure-gfm/unnamed-chunk-22-1.png)<!-- --> This plot
illustrates countries in cluster 2. They have the same the drastic
decrease as the other class. The difference is they all mostly remain
below zero until October of 2021. These countries mostly follow the same
patterns.

``` r
set.seed(10)
top_2 <- subset(cluster_final_df, cluster_final_df$km_class==2)
sample <- as.data.frame(sample_n(top_2, 3))
ts_2 <- subset(clustering_df, clustering_df$country_region %in% sample$country_region)


places_2 <- unique(ts_2 %>% select("country_region", "place_id")) %>% distinct(country_region, .keep_all=TRUE)
ts_2 <- subset(ts_2, ts_2$place_id %in% places_2$place_id)
```

![](covid_files/figure-gfm/unnamed-chunk-24-1.png)<!-- --> The plot for
countries in class 3 is different than the other classes. They only
manage to go above 0 for a brief period of time. It can be seen that
they have 2 high peaks.The heavy fluctuation in December and January is
for the holidays.

In the rest of the project Canada is used for the analysis.

The plots below show the time series for all the categories in Canada.

![](covid_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
library(xts)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

``` r
library(forecast)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
canada_retail <- subset(ts_2, ts_2$country_region=="Canada") %>% select("date", "retail_and_recreation_percent_change_from_baseline")

canada_ts <- xts(canada_retail$retail_and_recreation_percent_change_from_baseline, canada_retail$date, start(2020,2))
names(canada_ts) <- "retail_and_recreation_percent_change_from_baseline"
plot(canada_ts)
```

![](covid_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
ggAcf(canada_ts, lag=120)
```

![](covid_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->

Top 10 days lowest percentage

``` r
canada %>% select("date", "retail_and_recreation_percent_change_from_baseline") %>% arrange(retail_and_recreation_percent_change_from_baseline) %>% head(10)
```

    ##          date retail_and_recreation_percent_change_from_baseline
    ## 1  2020-12-25                                                -85
    ## 2  2020-04-12                                                -74
    ## 3  2021-01-01                                                -74
    ## 4  2020-04-10                                                -70
    ## 5  2020-04-05                                                -63
    ## 6  2020-04-19                                                -60
    ## 7  2020-03-28                                                -59
    ## 8  2020-03-29                                                -59
    ## 9  2020-12-26                                                -59
    ## 10 2020-04-04                                                -57

``` r
canada %>% select("date", "grocery_and_pharmacy_percent_change_from_baseline") %>% arrange(grocery_and_pharmacy_percent_change_from_baseline) %>% head(10)
```

    ##          date grocery_and_pharmacy_percent_change_from_baseline
    ## 1  2020-12-25                                               -78
    ## 2  2021-01-01                                               -67
    ## 3  2020-04-12                                               -62
    ## 4  2020-04-05                                               -45
    ## 5  2020-04-10                                               -43
    ## 6  2020-04-19                                               -43
    ## 7  2020-12-26                                               -40
    ## 8  2020-04-26                                               -39
    ## 9  2021-04-04                                               -36
    ## 10 2020-03-29                                               -35

Comparing the result for top 10 lowest days in grocery and pharmacy in
comparison to retail and recreation, it can be seen that new year’s day
and Christmas day are among top 3 for both. Other days are mostly in
April 2020 for both categories which illustrates high COVID-19 cases in
that month.

Time series forecasting

Next we will use ARIMA model to predict the percentages for the next 30
days.

``` r
d.arima <- auto.arima(canada_ts, seasonal=FALSE)
d.forecast <- forecast(d.arima, h=30) 
plot(d.forecast, include=90, xaxt = 'n') 
axis(1, at=seq(600, 700,20) , las=2, labels=seq(as.Date('2021-10-06'), as.Date('2022-01-14') , length.out=6) )
```

![](covid_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
library(nonlinearTseries)
```

    ## 
    ## Attaching package: 'nonlinearTseries'

    ## The following object is masked from 'package:grDevices':
    ## 
    ##     contourLines

``` r
rqa.analysis=rqa(time.series = canada_ts, embedding.dim=2, time.lag=1,
                 radius=3,lmin=1,do.plot=TRUE,distanceToBorder=2)
```

![](covid_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->
