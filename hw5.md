# hw5
Wade  
October 14, 2017  


```r
suppressWarnings(library(tidyverse))
suppressWarnings(library(gapminder))
suppressWarnings(library(forcats))
suppressWarnings(library(readxl))
```

For this assignment, I have been given orders to make several changes to the gapminder dataset, beginning with this one:

"<b>Drop Oceania.</b> Filter the Gapminder data to remove observations associated with the continent of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors."


```r
filter(gapminder, continent=="Oceania")
```

```
## # A tibble: 24 × 6
##      country continent  year lifeExp      pop gdpPercap
##       <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
## 1  Australia   Oceania  1952   69.12  8691212  10039.60
## 2  Australia   Oceania  1957   70.33  9712569  10949.65
## 3  Australia   Oceania  1962   70.93 10794968  12217.23
## 4  Australia   Oceania  1967   71.10 11872264  14526.12
## 5  Australia   Oceania  1972   71.93 13177000  16788.63
## 6  Australia   Oceania  1977   73.49 14074100  18334.20
## 7  Australia   Oceania  1982   74.74 15184200  19477.01
## 8  Australia   Oceania  1987   76.32 16257249  21888.89
## 9  Australia   Oceania  1992   77.56 17481977  23424.77
## 10 Australia   Oceania  1997   78.83 18565243  26997.94
## # ... with 14 more rows
```

```r
#View(gapminder$continent)
length(gapminder$continent)
```

```
## [1] 1704
```

Before I make changes to the dataset, note that we have 1704 observations.


```r
gap<-gapminder %>% filter(continent!="Oceania")
filter(gap, continent=="Oceania")
```

```
## # A tibble: 0 × 6
## # ... with 6 variables: country <fctr>, continent <fctr>, year <int>,
## #   lifeExp <dbl>, pop <int>, gdpPercap <dbl>
```

```r
length(gap$continent)
```

```
## [1] 1680
```

Here we see that the rows associated with Oceania have been removed and the dataset contains 1680 observations, down from 1704.

"Additionally, remove unused factor levels." 
"...address the number of rows and the levels of the affected factors."


```r
names(gapminder)
```

```
## [1] "country"   "continent" "year"      "lifeExp"   "pop"       "gdpPercap"
```

```r
sapply(data, levels)
```

```
## $...
## NULL
## 
## $list
## NULL
## 
## $package
## NULL
## 
## $lib.loc
## NULL
## 
## $verbose
## NULL
## 
## $envir
## NULL
## 
## [[7]]
## NULL
```

```r
str(gap)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1680 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
##  $ gdpPercap: num  779 821 853 836 740 ...
```

I do not see any unused factor levels, so I will move on to the next step.

"Reorder the levels of country or continent. Use the forcats package to change the order of the factor levels, based on a principled summary of one of the quantitative variables. Consider experimenting with a summary statistic beyond the most basic choice of the median."







"Remake at least one figure or create a new one, in light of something you learned in the recent class meetings about visualization design and color."

This one will be fun. I have certainly learned a lot of new techniques in this class for displaying data in an aesthetically pleasing way. Let's try remaking my first plot using what I have learned.

The first plot I made back in hw2 was this one:


```r
pie(summary(gapminder$continent), main="Continents")
```

![](hw5_files/figure-html/continents-1.png)<!-- -->

It looks a bit plain, and it suffers from, well, being a pie chart. Pie charts are poor for a number of reasons. For one, as Tamara Munzner mentioned in her guest lecture, pie charts show proportions using areas and angles. The human perceptual system is not designed to handle very well. But I also saw a way to redeem pie charts...



```r
g<-  gapminder %>% 
  group_by(continent, country, year)%>%
  summarize(max_GDP=max(gdpPercap))
View(g)
```



```r
ggplot(data=g,aes(x=year,y=max_GDP,fill=continent))+
        geom_bar(stat="identity")+
        coord_polar()+
        scale_fill_brewer(palette="Greens")+xlab("")+ylab("")
```

![](hw5_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

It looks cool, but I don't think we have the right color scheme. This color scheme shows a progression by continent which doesn't make sense. Also, the y-axis values do not look good and, worse, may even be misleading. Since we are interested mostly in showing an overall trend with this type of plot, I think it's best to remove the values on the y-axis altogether.


```r
ggplot(data=g,aes(x=year,y=max_GDP,fill=continent))+
        geom_bar(stat="identity")+
        coord_polar()+
        scale_fill_brewer(palette="Set2")+xlab("Year")+ylab("Max GDP per Capita") +
        theme_bw() +
        theme(axis.text.y = element_blank())
```

![](hw5_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
        labs(fill="Continent")
```

```
## $fill
## [1] "Continent"
## 
## attr(,"class")
## [1] "labels"
```

This looks pretty good. Admittedly, it doesn't fully redeem the pie chart, but this type of pie chart (called a rose plot) is certainly much better than a standard pie chart.

An even better way of displaying this data might be a stacked bar plot.


```r
ggplot(data=g,aes(x=year,y=max_GDP,fill=continent))+
        geom_bar(stat="identity")+
        scale_fill_brewer(palette="Set2")+xlab("Year")+ylab("Max GDP per Capita") +
        theme_bw() +
        theme(axis.text.y = element_blank())
```

![](hw5_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
        labs(fill="Continent")
```

```
## $fill
## [1] "Continent"
## 
## attr(,"class")
## [1] "labels"
```

Even this chart doesn't perfectly display this data. It may be an ill-posed problem, with no true solution. But I will argue that these charts display the data well enough for how complex the relationships are and are miles above the plain bar chart I produced at the beginning of this term!

# may want to remove y scale





















