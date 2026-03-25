# STA380 Project 

## what is included now

The R package is located at the repository root.

## Updates
Presently we are using ARMA for fit. Due to time constraint regarding this checkpoint, there was no time to add MLE estimates, but it will be implemented by the final submission.

## accessing the website via RPosit link
https://019d0430-b246-4443-c144-9437cec4e883.share.connect.posit.cloud/

## run RShiny demo locally

When in the repository root, run the following:

```r
Rscript -e 'shiny::runApp("shiny-app")'
```
Then type the corresponding address in your web browser to access the RShiny website.

## run tests

In the repository root:

```r
devtools::test()
```
