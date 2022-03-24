
# Halodotapi for R

*Wrapping the Halodotapi into R functions*

This package wraps four endpoints for R users, all related to player
stats:

-   `get_match_details()`
-   `get_player_csr()`
-   `get_player_match_history()`
-   `get_player_service_record()`

To begin you will need to obtain a Halodotapi access token. If you don’t
have one, you can obtain one by navigating to <https://autocode.com/>
and registering for an account. Go to your Profile \> Account \>
Identity Tokens \> General Use Identity Tokens and click “Create General
Use Identity”

The default Halodotapi version currently defaults to
`r halodotapi:::pkgenv$latest_supported_version`. You may set other
versions to be used – noting that they may not be supported – using:

``` r
options(halodotapi.default_version = 'x.x.x')
```

You can place this in your .Rprofile or at the beginning of your
scripts.

The package will throw a warning on startup if an invalid version is
set.
