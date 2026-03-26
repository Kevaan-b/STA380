# STA380 Project 

## what is included now

The R package is located at the repository root.

## Updates

Implemented features since March 18:

1. **R Package Restructuring** - Converted to proper R package structure with DESCRIPTION, NAMESPACE, LICENSE, .Rbuildignore

2. **CLS Estimation** - Implemented Conditional Least Squares estimation using mini-batch SGD with momentum optimization

3. **Shiny App Enhancements**:
   - Convergence status indicator (button showing if model converges)
   - PACF/ACF plots
   - Coefficient plots
   - UI layout improvements
   - About page
   - Parameter tuning and estimated params display

4. **Documentation** - Added manual pages for all exported functions

5. **Vignette** - Created comprehensive ARMA vignette

6. **Test Coverage** - Updated testthat tests

7. **Code Cleanup** - Removed redundant estimate_ARMA_errors function

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
