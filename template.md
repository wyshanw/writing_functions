writing functions
================

## do something simple

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.30836692  0.09063767  0.22098314 -0.27398802 -1.00361071  0.31536297
    ##  [7]  0.47645912  0.28915906  1.12279751 -0.30404691 -0.66773856  1.82185022
    ## [13] -2.00568166  0.47793237 -2.05150443 -0.35409226  0.42980591 -0.42062745
    ## [19]  1.18089703  0.87617664 -0.33065028 -0.28047715 -1.50079140  1.89485806
    ## [25] -0.31207776

function compute z score

``` r
z_scores = function(x) {

  z = (x - mean(x)) / sd(x)
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  0.30836692  0.09063767  0.22098314 -0.27398802 -1.00361071  0.31536297
    ##  [7]  0.47645912  0.28915906  1.12279751 -0.30404691 -0.66773856  1.82185022
    ## [13] -2.00568166  0.47793237 -2.05150443 -0.35409226  0.42980591 -0.42062745
    ## [19]  1.18089703  0.87617664 -0.33065028 -0.28047715 -1.50079140  1.89485806
    ## [25] -0.31207776

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
    ## 1  4.55  2.28

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
    ## 1   1.51      2.35

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
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```
