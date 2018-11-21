Data Import
================

Data Import
-----------

CSV zip files (corresponding to each month) of Citi Bike data for 2017 was downloaded from [the company's system data website](https://s3.amazonaws.com/tripdata/index.html). Originally, we felt it was better to download directly from the data source for reproducibility purposes. However, when using read\_csv with the direct URL to the zip, there were several parsing errors and the import failed. This may be due to the size of the files (zips of 25-70 MB). We decided to download the zips of two months and add them to our project's data folder. The data is provided according to the [NYCBS Data Use Policy](https://www.citibikenyc.com/data-sharing-policy).

``` r
# removed col names type for now and went with automatic column parsing which seemed to work  well

# Need to add a variable/split date so we can identify if the trip is in the January or March dataset 

citibike_import = function(file) {
  
  df = read_csv(file, col_names = TRUE) %>%
    janitor::clean_names() %>% 
    separate(start_time, into = c("start_date", "start_time"), sep = " ") %>% 
    separate(stop_time, into = c("stop_date", "stop_time"), sep = " ") %>% 
    separate(start_date, into = c("start_year", "start_month", "start_day"), sep = "-") %>% 
    separate(stop_date, into = c("stop_year", "stop_month", "stop_day"), sep = "-") %>% 
    mutate(user_type = tolower(user_type),
           trip_minutes = (trip_duration / 60),
           rider_age = (2017 - birth_year))
    
  df
  
}

citibike_tidy = 
  bind_rows(citibike_import("./data/201701-citibike-tripdata.csv.zip"),
            citibike_import("./data/201703-citibike-tripdata.csv.zip"))
```

    ## Parsed with column specification:
    ## cols(
    ##   `Trip Duration` = col_integer(),
    ##   `Start Time` = col_datetime(format = ""),
    ##   `Stop Time` = col_datetime(format = ""),
    ##   `Start Station ID` = col_integer(),
    ##   `Start Station Name` = col_character(),
    ##   `Start Station Latitude` = col_double(),
    ##   `Start Station Longitude` = col_double(),
    ##   `End Station ID` = col_integer(),
    ##   `End Station Name` = col_character(),
    ##   `End Station Latitude` = col_double(),
    ##   `End Station Longitude` = col_double(),
    ##   `Bike ID` = col_integer(),
    ##   `User Type` = col_character(),
    ##   `Birth Year` = col_integer(),
    ##   Gender = col_integer()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   `Trip Duration` = col_integer(),
    ##   `Start Time` = col_datetime(format = ""),
    ##   `Stop Time` = col_datetime(format = ""),
    ##   `Start Station ID` = col_integer(),
    ##   `Start Station Name` = col_character(),
    ##   `Start Station Latitude` = col_double(),
    ##   `Start Station Longitude` = col_double(),
    ##   `End Station ID` = col_integer(),
    ##   `End Station Name` = col_character(),
    ##   `End Station Latitude` = col_double(),
    ##   `End Station Longitude` = col_double(),
    ##   `Bike ID` = col_integer(),
    ##   `User Type` = col_character(),
    ##   `Birth Year` = col_integer(),
    ##   Gender = col_integer()
    ## )

``` r
# Reading the data directly from website doesn't work - thousands of parsing failures, even with auto parsing and when specifing column parsing in function
# citibike_import("https://s3.amazonaws.com/tripdata/201701-citibike-tripdata.csv.zip"),
# citibike_import("https://s3.amazonaws.com/tripdata/201703-citibike-tripdata.csv.zip"))
```

Reserach Questions of Interest:

-   Most popular/used beginning station (and least)

``` r
# Top 10 used beginning stations in all dataset (Jan)

citibike_tidy %>% 
  filter(start_month == "01") %>% 
  group_by(start_station_id, start_station_name) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n > 3990) %>% 
  knitr::kable()
```

|  start\_station\_id| start\_station\_name  |     n|
|-------------------:|:----------------------|-----:|
|                 519| Pershing Square North |  8795|
|                 435| W 21 St & 6 Ave       |  5454|
|                 497| E 17 St & Broadway    |  5110|
|                 402| Broadway & E 22 St    |  4913|
|                 490| 8 Ave & W 33 St       |  4816|
|                 477| W 41 St & 8 Ave       |  4480|
|                 285| Broadway & E 14 St    |  4407|
|                 379| W 31 St & 7 Ave       |  4150|
|                 459| W 20 St & 11 Ave      |  4114|
|                3255| 8 Ave & W 31 St       |  3993|

``` r
# Top 10 used beginning stations in all dataset (March)

citibike_tidy %>% 
  filter(start_month == "03") %>% 
  group_by(start_station_id, start_station_name) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n > 4300) %>% 
  knitr::kable()
```

|  start\_station\_id| start\_station\_name  |     n|
|-------------------:|:----------------------|-----:|
|                 519| Pershing Square North |  9076|
|                 497| E 17 St & Broadway    |  5443|
|                 435| W 21 St & 6 Ave       |  5305|
|                 492| W 33 St & 7 Ave       |  5293|
|                 402| Broadway & E 22 St    |  5173|
|                 490| 8 Ave & W 33 St       |  4715|
|                 477| W 41 St & 8 Ave       |  4518|
|                 379| W 31 St & 7 Ave       |  4476|
|                 520| W 52 St & 5 Ave       |  4392|
|                3255| 8 Ave & W 31 St       |  4334|

``` r
# Bottom 10 used beginning stations in all dataset (Jan)
# Remove NYCBS Depots? 

citibike_tidy %>% 
  filter(start_month == "01") %>% 
  group_by(start_station_id, start_station_name) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  filter(n < 60) %>% 
  knitr::kable()
```

|  start\_station\_id| start\_station\_name           |    n|
|-------------------:|:-------------------------------|----:|
|                3036| 8D OPS 01                      |    1|
|                3040| SSP Tech Workshop              |    1|
|                3446| NYCBS Depot - STY - Valet Scan |    2|
|                3017| NYCBS Depot - FAR              |    4|
|                3240| NYCBS Depot BAL - DYR          |   19|
|                3219| NYCBS Depot - STY              |   25|
|                3245| Kiosk in a box Motivate        |   35|
|                3330| Henry St & Bay St              |   42|
|                3394| Columbia St & W 9 St           |   46|
|                3352| Sigourney St & Columbia St     |   57|

``` r
# Bottom 10 used beginning stations in all dataset (March)
# Remove NYCBS Depots? 

citibike_tidy %>% 
  filter(start_month == "03") %>% 
  group_by(start_station_id, start_station_name) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  filter(n < 40) %>% 
  knitr::kable()
```

|  start\_station\_id| start\_station\_name      |    n|
|-------------------:|:--------------------------|----:|
|                 298| 3 Ave & Schermerhorn St   |    1|
|                3040| SSP Tech Workshop         |    6|
|                3239| Bressler                  |    7|
|                3240| NYCBS Depot BAL - DYR     |   12|
|                3441| 10 Hudson Yards           |   21|
|                3394| Columbia St & W 9 St      |   27|
|                3219| NYCBS Depot - STY         |   28|
|                3456| Jackson St & Leonard St   |   30|
|                3337| Dwight St & Van Dyke St   |   34|
|                3333| Columbia St & Lorraine St |   36|

-   Most popular/used ending station (and least)

``` r
# Top 10 used end stations in all dataset (Jan)

citibike_tidy %>% 
  filter(start_month == "01") %>% 
  group_by(end_station_id, end_station_name) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n > 3990) %>% 
  knitr::kable()
```

|  end\_station\_id| end\_station\_name    |     n|
|-----------------:|:----------------------|-----:|
|               519| Pershing Square North |  8791|
|               402| Broadway & E 22 St    |  5915|
|               497| E 17 St & Broadway    |  5679|
|               435| W 21 St & 6 Ave       |  5507|
|               490| 8 Ave & W 33 St       |  4882|
|               520| W 52 St & 5 Ave       |  4823|
|               285| Broadway & E 14 St    |  4573|
|               477| W 41 St & 8 Ave       |  4532|
|               459| W 20 St & 11 Ave      |  4491|
|              3255| 8 Ave & W 31 St       |  4205|
|               379| W 31 St & 7 Ave       |  4131|
|               509| 9 Ave & W 22 St       |  4112|
|               523| W 38 St & 8 Ave       |  4023|

``` r
# Top 10 used end stations in all dataset (March)

citibike_tidy %>% 
  filter(start_month == "03") %>% 
  group_by(end_station_id, end_station_name) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n > 4300) %>% 
  knitr::kable()
```

|  end\_station\_id| end\_station\_name    |     n|
|-----------------:|:----------------------|-----:|
|               519| Pershing Square North |  9014|
|               492| W 33 St & 7 Ave       |  6312|
|               497| E 17 St & Broadway    |  5894|
|               402| Broadway & E 22 St    |  5608|
|               435| W 21 St & 6 Ave       |  5375|
|               520| W 52 St & 5 Ave       |  4693|
|               490| 8 Ave & W 33 St       |  4567|
|              3255| 8 Ave & W 31 St       |  4468|
|               477| W 41 St & 8 Ave       |  4450|
|               459| W 20 St & 11 Ave      |  4316|

``` r
# Bottom 10 used end stations in all dataset (Jan)

citibike_tidy %>% 
  filter(start_month == "01") %>% 
  group_by(end_station_id, end_station_name) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  filter(n < 60) %>% 
  knitr::kable()
```

|  end\_station\_id| end\_station\_name             |    n|
|-----------------:|:-------------------------------|----:|
|              3036| 8D OPS 01                      |    1|
|              3040| SSP Tech Workshop              |    1|
|              3183| Exchange Place                 |    1|
|              3446| NYCBS Depot - STY - Valet Scan |    2|
|              3447| E 71 St & 1 Ave                |    2|
|              3250| NYCBS Depot - PIT              |    3|
|              3240| NYCBS Depot BAL - DYR          |    9|
|              3017| NYCBS Depot - FAR              |   14|
|              3219| NYCBS Depot - STY              |   28|
|              3245| Kiosk in a box Motivate        |   35|
|              3302| Columbus Ave & W 103 St        |   45|
|              3394| Columbia St & W 9 St           |   45|
|              3330| Henry St & Bay St              |   47|
|              3395| Henry St & W 9 St              |   49|
|               255| NYCBS Depot - SSP              |   57|

``` r
# Bottom 10 used end stations in all dataset (March)

citibike_tidy %>% 
  filter(start_month == "03") %>% 
  group_by(end_station_id, end_station_name) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  filter(n < 40) %>% 
  knitr::kable()
```

|  end\_station\_id| end\_station\_name              |    n|
|-----------------:|:--------------------------------|----:|
|              3214| Essex Light Rail                |    1|
|              3247| SSP - Basement                  |    1|
|              3257| Adventures NYC                  |    1|
|               298| 3 Ave & Schermerhorn St         |    2|
|              3250| NYCBS Depot - PIT               |    5|
|              3267| Morris Canal                    |    6|
|              3239| Bressler                        |    7|
|              3040| SSP Tech Workshop               |    8|
|              3240| NYCBS Depot BAL - DYR           |    8|
|              3441| 10 Hudson Yards                 |   20|
|              3450| Penn Station Valet - Valet Scan |   20|
|              3456| Jackson St & Leonard St         |   24|
|              3394| Columbia St & W 9 St            |   33|
|              3337| Dwight St & Van Dyke St         |   39|

-   Mapping the above (like airbnb from class)

-   Men v. Women (1 = Male, 2 = Female)

-   Age rangers and average time spent on ride

-   Customers (24h & 3 day passes) vs. Subscribers (Annual Member)

-   Plots of amounts of rides over time of day

-   Most/least popular days of the week of travel

-   Jan. vs. March trips
