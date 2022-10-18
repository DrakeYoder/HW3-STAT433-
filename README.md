HW3
================
2022-10-17

[HW3](https://github.com/DrakeYoder/HW3-STAT433-.git)

``` r
#Q1)
flight = flights %>% group_by(dest) %>% summarise(mean_arrival_delay = mean(arr_delay, na.rm = T), n = n())

airports %>% inner_join(flight, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat, colour = mean_arrival_delay)) +
    borders("state") +
    geom_point() +
    coord_quickmap() +
    ggtitle("Destination of Flight & Average Arrival Delay")
```

![unnamed-chunk-1-1](https://user-images.githubusercontent.com/78119439/196495939-a013b006-34f6-4f4b-a375-709414bf7fd2.png)

``` r
#Q2)
location_airports = airports %>% select(lat, lon, faa)
location_airports
```

    ## # A tibble: 1,458 × 3
    ##      lat    lon faa  
    ##    <dbl>  <dbl> <chr>
    ##  1  41.1  -80.6 04G  
    ##  2  32.5  -85.7 06A  
    ##  3  42.0  -88.1 06C  
    ##  4  41.4  -74.4 06N  
    ##  5  31.1  -81.4 09J  
    ##  6  36.4  -82.2 0A9  
    ##  7  41.5  -84.5 0G6  
    ##  8  42.9  -76.8 0G7  
    ##  9  39.8  -76.6 0P2  
    ## 10  48.1 -123.  0S9  
    ## # … with 1,448 more rows

``` r
origin = flights %>% left_join(location_airports, by = c("origin" = "faa"))
origin
```

    ## # A tibble: 336,776 × 21
    ##     year month   day dep_time sched_de…¹ dep_d…² arr_t…³ sched…⁴ arr_d…⁵ carrier
    ##    <int> <int> <int>    <int>      <int>   <dbl>   <int>   <int>   <dbl> <chr>  
    ##  1  2013     1     1      517        515       2     830     819      11 UA     
    ##  2  2013     1     1      533        529       4     850     830      20 UA     
    ##  3  2013     1     1      542        540       2     923     850      33 AA     
    ##  4  2013     1     1      544        545      -1    1004    1022     -18 B6     
    ##  5  2013     1     1      554        600      -6     812     837     -25 DL     
    ##  6  2013     1     1      554        558      -4     740     728      12 UA     
    ##  7  2013     1     1      555        600      -5     913     854      19 B6     
    ##  8  2013     1     1      557        600      -3     709     723     -14 EV     
    ##  9  2013     1     1      557        600      -3     838     846      -8 B6     
    ## 10  2013     1     1      558        600      -2     753     745       8 AA     
    ## # … with 336,766 more rows, 11 more variables: flight <int>, tailnum <chr>,
    ## #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
    ## #   minute <dbl>, time_hour <dttm>, lat <dbl>, lon <dbl>, and abbreviated
    ## #   variable names ¹​sched_dep_time, ²​dep_delay, ³​arr_time, ⁴​sched_arr_time,
    ## #   ⁵​arr_delay

``` r
origin_destination = left_join(origin, location_airports, by = c("dest" = "faa"), suffix = c(".origin", ".dest"))
origin_destination
```

    ## # A tibble: 336,776 × 23
    ##     year month   day dep_time sched_de…¹ dep_d…² arr_t…³ sched…⁴ arr_d…⁵ carrier
    ##    <int> <int> <int>    <int>      <int>   <dbl>   <int>   <int>   <dbl> <chr>  
    ##  1  2013     1     1      517        515       2     830     819      11 UA     
    ##  2  2013     1     1      533        529       4     850     830      20 UA     
    ##  3  2013     1     1      542        540       2     923     850      33 AA     
    ##  4  2013     1     1      544        545      -1    1004    1022     -18 B6     
    ##  5  2013     1     1      554        600      -6     812     837     -25 DL     
    ##  6  2013     1     1      554        558      -4     740     728      12 UA     
    ##  7  2013     1     1      555        600      -5     913     854      19 B6     
    ##  8  2013     1     1      557        600      -3     709     723     -14 EV     
    ##  9  2013     1     1      557        600      -3     838     846      -8 B6     
    ## 10  2013     1     1      558        600      -2     753     745       8 AA     
    ## # … with 336,766 more rows, 13 more variables: flight <int>, tailnum <chr>,
    ## #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
    ## #   minute <dbl>, time_hour <dttm>, lat.origin <dbl>, lon.origin <dbl>,
    ## #   lat.dest <dbl>, lon.dest <dbl>, and abbreviated variable names
    ## #   ¹​sched_dep_time, ²​dep_delay, ³​arr_time, ⁴​sched_arr_time, ⁵​arr_delay

``` r
#Q3) After grouping planes by age, and calculating the Arrival Delay for arrival and departure, it seems as though younger planes have a larger delay on average.
plane_age_arr_delay = flights %>% inner_join(planes, c("tailnum" = "tailnum")) %>% 
  filter(!is.na(arr_delay)) %>%
  mutate(plane_age = year.x-year.y) %>% 
  group_by(plane_age) %>% 
  mutate(mean_arr_delay = mean(arr_delay)) %>% 
  select(tailnum, year.x, year.y, plane_age, arr_delay, mean_arr_delay)
plane_age_arr_delay
```

    ## # A tibble: 279,017 × 6
    ## # Groups:   plane_age [47]
    ##    tailnum year.x year.y plane_age arr_delay mean_arr_delay
    ##    <chr>    <int>  <int>     <int>     <dbl>          <dbl>
    ##  1 N14228    2013   1999        14        11           7.12
    ##  2 N24211    2013   1998        15        20           6.71
    ##  3 N619AA    2013   1990        23        33           5.69
    ##  4 N804JB    2013   2012         1       -18           2.85
    ##  5 N668DN    2013   1991        22       -25           4.18
    ##  6 N39463    2013   2012         1        12           2.85
    ##  7 N516JB    2013   2000        13        19           7.26
    ##  8 N829AS    2013   1998        15       -14           6.71
    ##  9 N593JB    2013   2004         9        -8          10.2 
    ## 10 N793JB    2013   2011         2        -2           5.70
    ## # … with 279,007 more rows

``` r
plane_age_arr_delay %>% ggplot(aes(x=plane_age, y=mean_arr_delay)) + geom_point() + geom_smooth(method="lm") + xlab("Plane Age") + ylab("Mean Arrival Delay") + ggtitle("Plane Age vs. Mean Arrival Delay")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 5164 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 5164 rows containing missing values (geom_point).

![unnamed-chunk-3-1](https://user-images.githubusercontent.com/78119439/196497480-a8eff8af-d3ea-48c6-a152-ec8d9eee3b27.png)

``` r
plane_age_dep_delay = flights %>% inner_join(planes, c("tailnum" = "tailnum")) %>% 
  filter(!is.na(dep_delay)) %>%
  mutate(plane_age = year.x-year.y) %>% 
  group_by(plane_age) %>% 
  mutate(mean_dep_delay = mean(dep_delay)) %>% 
  select(tailnum, year.x, year.y, plane_age, arr_delay, mean_dep_delay)
plane_age_dep_delay
```

    ## # A tibble: 279,971 × 6
    ## # Groups:   plane_age [47]
    ##    tailnum year.x year.y plane_age arr_delay mean_dep_delay
    ##    <chr>    <int>  <int>     <int>     <dbl>          <dbl>
    ##  1 N14228    2013   1999        14        11          13.1 
    ##  2 N24211    2013   1998        15        20          13.4 
    ##  3 N619AA    2013   1990        23        33          11.5 
    ##  4 N804JB    2013   2012         1       -18           9.64
    ##  5 N668DN    2013   1991        22       -25          10.7 
    ##  6 N39463    2013   2012         1        12           9.64
    ##  7 N516JB    2013   2000        13        19          11.7 
    ##  8 N829AS    2013   1998        15       -14          13.4 
    ##  9 N593JB    2013   2004         9        -8          16.4 
    ## 10 N793JB    2013   2011         2        -2          11.8 
    ## # … with 279,961 more rows

``` r
plane_age_dep_delay %>% ggplot(aes(x=plane_age, y=mean_dep_delay)) + geom_point() + geom_smooth(method="lm") + xlab("Plane Age") + ylab("Mean Departure Delay") + ggtitle("Plane Age vs. Mean Departure Delay")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 5175 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 5175 rows containing missing values (geom_point).
    
![unnamed-chunk-4-1](https://user-images.githubusercontent.com/78119439/196497517-621fb1ab-13e8-4df1-94fd-86a75c2d46a5.png)
