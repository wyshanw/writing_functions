writing functions
================

## do something simple

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.37092890 -0.20365789  0.13522347 -1.18737728 -0.34864590 -0.22502749
    ##  [7] -1.67177725  0.06684568 -1.24446091 -0.18169144 -0.22589142 -0.60872994
    ## [13] -0.64461623  0.46902275  0.92972243  0.39947374  1.02751197  2.71935257
    ## [19]  0.05445105 -1.66673313 -0.32674801  0.28676480  1.33771319  1.43591139
    ## [25] -0.69756503

function compute z score

``` r
z_scores = function(x) {

  z = (x - mean(x)) / sd(x)
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  0.37092890 -0.20365789  0.13522347 -1.18737728 -0.34864590 -0.22502749
    ##  [7] -1.67177725  0.06684568 -1.24446091 -0.18169144 -0.22589142 -0.60872994
    ## [13] -0.64461623  0.46902275  0.92972243  0.39947374  1.02751197  2.71935257
    ## [19]  0.05445105 -1.66673313 -0.32674801  0.28676480  1.33771319  1.43591139
    ## [25] -0.69756503

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
    ## 1  5.16  2.58

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
    ## 1   2.97      2.36

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
