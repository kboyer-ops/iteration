writing\_functions
================
Kaila Boyer
11/4/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
set.seed(1)
```

## Z scores

``` r
x_vec <- rnorm(25, mean = 5, sd=4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

``` r
z_scores = function(x) {
  
  z = (x-mean(x)) / sd(x)
  
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

``` r
y_vec <- rnorm(40, mean = 12, sd=0.3)
z_scores(y_vec)
```

    ##  [1] -0.2022306 -0.3204049 -1.8795425 -0.7026191  0.3598721  1.4753003
    ##  [7] -0.2575539  0.3239812 -0.1994754 -1.7684514 -0.6277359 -0.6031866
    ## [13] -0.2060066  1.1686154  0.7692145 -0.3307538 -0.4360887  0.6907069
    ## [19]  0.5243537 -0.9523331 -0.9745524  0.2966039  0.7755665 -0.2688873
    ## [25]  0.9090459  0.3363531 -0.8613556  0.2687848 -1.4747590  1.5634498
    ## [31]  2.2124710 -0.5710916 -1.3737041  0.5398346 -0.2958126  2.7119068
    ## [37] -0.1822057  0.6821414 -0.1024770 -1.0169742

How great is this?

only kinda great

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores(c("my", "name", "is", "kaila"))
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

``` r
z_scores(mtcars)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

try again

``` r
z_scores = function(x) { 
  if(!is.numeric(x)) { 
    stop("x needs to be numeric")
  }
  
  if(length(x) < 3) { 
    stop("x should have at least 3 numbers")
  }
}

z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores = function(x) {
  
  z = (x-mean(x)) / sd(x)
  
  return(z)
}
```

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(mean = mean_x, 
       sd = sd_x)
}


mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  output_df = tibble(
    mean = mean_x, 
    sd = sd_x
  )
  return(output_df)
}
```

``` r
sim_data <- tibble( 
  x = rnorm(30, mean= 2, sd=3)
  )

sim_data %>% summarize(
  mean = mean(x), 
  sd = sd(x)
)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.51  2.82

Let’s write a function that simulates data, computes mean and sd

``` r
sim_mean_sd = function(n, mu, sigma) {
  
    sim_data = tibble( 
      x = rnorm(n, mean = mu, sd = sigma)
      )
    
    sim_data %>% summarize(
      mean = mean(x), 
      sd = sd(x)
      )

  
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.96  2.43

``` r
sim_mean_sd(n = 30, mu = 4, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.75  2.73

``` r
sim_data
```

    ## # A tibble: 30 × 1
    ##         x
    ##     <dbl>
    ##  1  2.57 
    ##  2 -3.41 
    ##  3  6.40 
    ##  4  2.46 
    ##  5  8.52 
    ##  6  3.43 
    ##  7 -0.130
    ##  8  3.83 
    ##  9 -0.802
    ## 10 -1.76 
    ## # … with 20 more rows

## Revisit Napoleon Dynamite

``` r
library(rvest)

url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

There are a lot of review pages

write a function that gets views based on page url

``` r
get_page_reviews <- function(page_url) { 
  
    html = read_html(page_url)
    
    review_titles = 
      html %>%
      html_elements(".a-text-bold span") %>%
      html_text()
    
    review_stars = 
      html %>%
      html_elements("#cm_cr-review_list .review-rating") %>%
      html_text()
    
    review_text = 
     html %>%
      html_elements(".review-text-content span") %>%
      html_text()
    
    reviews = tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
    return(reviews)
  }
  
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

get_page_reviews(url)  
```

    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  get_page_reviews(vec_urls[1]),
  get_page_reviews(vec_urls[2]),
  get_page_reviews(vec_urls[3]),
  get_page_reviews(vec_urls[4]),
  get_page_reviews(vec_urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…
    ## # … with 40 more rows
