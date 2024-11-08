# peekds

This is the "peek data standard" package, which facilitates writing scripts that convert arbitrary 2 alternative word recognition eye-tracking datasets to the `peekds` format, which allows import in [Peekbank](http://peekbank.stanford.edu). 

### Install `peekbands` from GitHub:

```
# install.packages("remotes")
remotes::install_github("langcog/peekbankr") # This dependency is not automatically installed
remotes::install_github("langcog/peekds")
```

### Local Installation from Source

When developing, you can run:

```
install.packages(".", repos = NULL, type="source", dependencies=TRUE)
```

If it fails to install the `RMariaDB` dependency automatically, you can manually trigger the installation using

```
install.packages("RMariaDB")
```

After making changes, be sure to run 

```
roxygen2::roxygenise()
```

to update exports and documentation.

# Schema Column Documentation

https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit?usp=sharing
