
# ncaascrapR

<!-- badges: start -->

[![Version-Number](https://img.shields.io/github/r-package/v/ehess/ncaascrapR?label=ncaascrapR&logo=R&style=for-the-badge)](https://github.com/ehess/ncaascrapR/)
[![R-CMD-check](https://img.shields.io/github/workflow/status/ehess/ncaascrapR/R-CMD-check?label=R-CMD-Check&logo=R&logoColor=white&style=for-the-badge)](https://github.com/ehess/ncaascrapR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg?style=for-the-badge&logo=github)](https://github.com/ehess/ncaascrapR/)
[![Contributors](https://img.shields.io/github/contributors/ehess/ncaascrapR?style=for-the-badge)](https://github.com/ehess/ncaascrapR/graphs/contributors)
[![Twitter
Follow](https://img.shields.io/twitter/follow/arbitanalytics?color=blue&label=%40arbitanalytics&logo=twitter&style=for-the-badge)](https://twitter.com/arbitanalytics)
[![Twitter
Follow](https://img.shields.io/twitter/follow/SportsDataverse?color=blue&label=%40SportsDataverse&logo=twitter&style=for-the-badge)](https://twitter.com/SportsDataverse)
<!-- badges: end -->

The goal of [**`ncaascrapR`**](https://ehess.github.io/ncaascrapR/) is
to provide the community with an R package for working with NCAA Sports
data. An R package for working with NCAA Data. It is a scraping and
aggregating interface for the Data and Stats portals of the NCAA
website.

## **Installation**

You can install the released version of
[**`ncaascrapR`**](https://github.com/ehess/ncaascrapR/) from
[GitHub](https://github.com/ehess/ncaascrapR) with:

``` r
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("ehess/ncaascrapR")
```

``` r
# if you would prefer devtools installation
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
# Alternatively, using the devtools package:
devtools::install_github(repo = "ehess/ncaascrapR")
```

## **Breaking Changes**

[**Full News on
Releases**](https://ehess.github.io/ncaascrapR/news/index.html)

## Follow the [SportsDataverse](https://twitter.com/SportsDataverse) on Twitter and star this repo

[![Twitter
Follow](https://img.shields.io/twitter/follow/SportsDataverse?color=blue&label=%40SportsDataverse&logo=twitter&style=for-the-badge)](https://twitter.com/SportsDataverse)

[![GitHub
stars](https://img.shields.io/github/stars/ehess/ncaascrapR.svg?color=eee&logo=github&style=for-the-badge&label=Star%20ncaascrapR&maxAge=2592000)](https://github.com/ehess/ncaascrapR/stargazers/)

## **Our Authors**

-   [Akshay Easwaran](https://twitter.com/akeaswaran)  
    <a href="https://twitter.com/akeaswaran" target="blank"><img src="https://img.shields.io/twitter/follow/akeaswaran?color=blue&label=%40akeaswaran&logo=twitter&style=for-the-badge" alt="@akeaswaran" /></a>
    <a href="https://github.com/akeaswaran" target="blank"><img src="https://img.shields.io/github/followers/akeaswaran?color=eee&logo=Github&style=for-the-badge" alt="@akeaswaran" /></a>

-   [Eric Hess](https://twitter.com/arbitanalytics) </br>
    <a href="https://twitter.com/arbitanalytics" target="blank"><img src="https://img.shields.io/twitter/follow/arbitanalytics?color=blue&label=%40arbitanalytics&logo=twitter&style=for-the-badge" alt="@arbitanalytics" /></a>
    <a href="https://github.com/ehess" target="blank"><img src="https://img.shields.io/github/followers/ehess?color=eee&logo=Github&style=for-the-badge" alt="@ehess" /></a>

-   [John Edwards](https://twitter.com/John_B_Edwards) </br>
    <a href="https://twitter.com/John_B_Edwards" target="blank"><img src="https://img.shields.io/twitter/follow/John_B_Edwards?color=blue&label=%40John_B_Edwards&logo=twitter&style=for-the-badge" alt="@John_B_Edwards" /></a>
    <a href="https://github.com/john-b-edwards" target="blank"><img src="https://img.shields.io/github/followers/john-b-edwards?color=eee&logo=Github&style=for-the-badge" alt="@john-b-edwards" /></a>

## **Our Contributors (they’re awesome)**

-   [Saiem Gilani](https://twitter.com/saiemgilani)  
    <a href="https://twitter.com/saiemgilani" target="blank"><img src="https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge" alt="@saiemgilani" /></a>
    <a href="https://github.com/saiemgilani" target="blank"><img src="https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>

## **Citations**

To cite the [**`ncaascrapR`**](https://ehess.github.io/ncaascrapR/) R
package in publications, use:

BibTex Citation

``` bibtex
@misc{easwaran_hess_edwards_2021,
  author = {Akshay Easwaran and Eric Hess and John B. Edwards},
  title = {ncaascrapR: The SportsDataverse's R Package for College Sports Data.},
  url = {https://ehess.github.io/ncaascrapR/},
  year = {2021}
}
```
