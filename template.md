writing functions
================

## do something simple

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.83028909  0.72050302  0.41387697  2.73447822  0.60463795 -0.27006681
    ##  [7] -1.32006243  0.39773294 -0.11667216 -0.94740107 -0.24730846  1.22005905
    ## [13] -0.13204504  1.36305890  0.06888112 -0.64198263 -0.14045870 -1.14501929
    ## [19]  0.58116110 -0.14442668 -0.41494327  1.31840490 -1.01122974 -0.85039081
    ## [25] -0.21049800

function compute z score

``` r
z_scores = function(x) {

  z = (x - mean(x)) / sd(x)
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1] -1.83028909  0.72050302  0.41387697  2.73447822  0.60463795 -0.27006681
    ##  [7] -1.32006243  0.39773294 -0.11667216 -0.94740107 -0.24730846  1.22005905
    ## [13] -0.13204504  1.36305890  0.06888112 -0.64198263 -0.14045870 -1.14501929
    ## [19]  0.58116110 -0.14442668 -0.41494327  1.31840490 -1.01122974 -0.85039081
    ## [25] -0.21049800

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

## multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } 
  if (length(x) < 3) {
    stop("input must have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(mean = mean_x, 
       sd = sd_x)
}
```

check

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.64  3.35

## multiple inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 2, sd = 3)
)

sim_data %>% 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.28      3.44

``` r
sim_mean_sd = function(n, mu = 3, sigma = 4) {
  
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  sim_data %>% 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}
```

## review amazon example

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>% # get first digit \\d, at the beginning^
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_page1 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

what about next page of review

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_page2 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

turn it into function

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

let’s read a few page of review

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(vec_urls[1]),
  read_page_reviews(vec_urls[2]),
  read_page_reviews(vec_urls[3]),
  read_page_reviews(vec_urls[4]),
  read_page_reviews(vec_urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 I Just everyone to know this....                          5 VOTE FOR PEDRO !…
    ##  2 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein…
    ##  3 Best quirky movie ever                                    5 You all know the…
    ##  4 Classic Film                                              5 Had to order thi…
    ##  5 hehehehe                                                  5 goodjobboys      
    ##  6 Painful                                                   1 I think I sneeze…
    ##  7 GRAND                                                     5 GRAND            
    ##  8 Hello, 90s                                                5 So nostalgic mov…
    ##  9 Cult Classic                                              5 Watched it with …
    ## 10 Format was inaccurate                                     4 There was an opt…
    ## # … with 40 more rows

## mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x_vec, sd)
```

    ## [1] 0.955852

``` r
my_summary(x_vec, IQR)
```

    ## [1] 1.030544

``` r
my_summary(x_vec, var)
```

    ## [1] 0.913653
