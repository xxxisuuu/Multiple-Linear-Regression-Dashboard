# Multiple Linear Regression Dashboard

An interactive Shiny app for teaching **Unit 3: Multiple Linear Regression** (STAT 462/862, Statistical Learning).

Built as a companion to a set of 1-on-1 tutoring notes, this app lets a student explore the core ideas hands-on instead of only seeing static slides:

- **Least Squares & Fit** — adjust sample size and noise, watch the fitted line and residuals update.
- **beta-hat Sampling Distribution (Unbiasedness)** — resample thousands of times and watch the average of the estimates converge to the true coefficient.
- **What Does a 95% CI Actually Mean** — a Monte Carlo simulation showing dozens of confidence intervals at once, so "95% confidence" can be seen rather than just defined.
- **Hypothesis Testing: F vs t** — toggle predictors on/off and watch summary(), anova(), and the identity F = t^2 update live.
- **Polynomial & Interaction Terms** — toggle a quadratic or interaction term and compare against a nested F-test.

## Try it live

This app is deployed with [shinylive](https://shiny.posit.co/py/docs/shinylive.html), which compiles the app to WebAssembly so it runs **entirely in your browser** — no server required. Once GitHub Pages finishes building (see the Actions tab), the live link appears under **Settings → Pages** on this repository.

## Running it locally instead

```r
install.packages("shiny")
shiny::runApp("app")
```

## How this site is built

Every push to `main` triggers `.github/workflows/deploy.yml`, which installs R and the `shinylive` package on a GitHub Actions runner, exports `app/app.R` to a static site in `_site/`, and publishes it to GitHub Pages.
