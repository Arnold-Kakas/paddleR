
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paddleR

<!-- badges: start -->

<!-- badges: end -->

The goal of paddleR is to …

## Installation

You can install the development version of regexcite from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Arnold-Kakas/paddleR")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo Arnold-Kakas/paddleR@HEAD
#> rlang   (1.1.5 -> 1.1.6) [CRAN]
#> cli     (3.6.4 -> 3.6.5) [CRAN]
#> openssl (2.3.2 -> 2.3.3) [CRAN]
#> curl    (6.2.2 -> 6.2.3) [CRAN]
#> Installing 4 packages: rlang, cli, openssl, curl
#> Installing packages into 'C:/Users/Arnold/AppData/Local/Temp/Rtmpg7iSAC/temp_libpath351704d3a2169'
#> (as 'lib' is unspecified)
#> package 'rlang' successfully unpacked and MD5 sums checked
#> package 'cli' successfully unpacked and MD5 sums checked
#> package 'openssl' successfully unpacked and MD5 sums checked
#> package 'curl' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\Arnold\AppData\Local\Temp\RtmpacqFQO\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>       ✔  checking for file 'C:\Users\Arnold\AppData\Local\Temp\RtmpacqFQO\remotes368e42dde738d\Arnold-Kakas-paddleR-7fcea2cb3e4abe1e3ff8592de8efb331581adefa/DESCRIPTION'
#>       ─  preparing 'paddleR':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>      Omitted 'LazyData' from DESCRIPTION
#>      NB: this package now depends on R (>=        NB: this package now depends on R (>= 4.1.0)
#>      WARNING: Added dependency on R >= 4.1.0 because package code uses the
#>      pipe |> or function shorthand \(...) syntax added in R 4.1.0.
#>      File(s) using such syntax:
#>        'ZZZ.r'
#>   ─  building 'paddleR_0.1.0.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/Arnold/AppData/Local/Temp/Rtmpg7iSAC/temp_libpath351704d3a2169'
#> (as 'lib' is unspecified)

library(paddleR)
#> 
#> Attaching package: 'paddleR'
#> The following object is masked from 'package:base':
#> 
#>     %||%
```

## 📦 About paddleR

This R package provides a robust interface to the Paddle API, enabling
developers, analysts, and product teams to interact with Paddle’s
billing and subscription platform directly from R.

The package allows you to:

- Create, retrieve, and update customers
- Generate authentication tokens for Paddle.js checkout flows
- Query customer lists with filters and pagination
- Retrieve subscription details, including status and history
- Manage products
- Manage invoices
- Many more

The package supports two modes of operation:

- Live mode (default): connects to the production Paddle API
- Sandbox mode: connects to the Paddle sandbox environment for testing

You can toggle modes using:

``` r
set_paddle_mode("sandbox")  # or "live"
```

All API endpoints automatically adjust their base URL and authentication
based on the selected mode.

## 🔐 API Key Configuration

To authenticate with the Paddle API, you must set your API key(s) as
environment variables. You have two options:

### Option 1: Use a single key

Set PADDLE_KEY to the appropriate key for your current mode.

``` r
Sys.setenv(PADDLE_KEY = "your-live-or-sandbox-key") # or use .Renviron file
```

## Option 2 (Recommended): Use separate keys for live and sandbox

Set both PADDLE_KEY_LIVE and PADDLE_KEY_SANDBOX, and the package will
automatically use the correct one based on the selected mode.

``` r
Sys.setenv(PADDLE_KEY_LIVE = "your-live-key")
Sys.setenv(PADDLE_KEY_SANDBOX = "your-sandbox-key")
```

API keys must have the appropriate permissions based on the Paddle
endpoint you are accessing. Refer to the official [Paddle API
docs](https://developer.paddle.com/api-reference/overview) for required
scopes.
