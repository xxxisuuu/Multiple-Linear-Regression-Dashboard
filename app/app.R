## =============================================================
## Unit 3 Interactive Teaching App -- Multiple Linear Regression
## STAT 462/862, companion to Unit3_LinearRegression_notes.Rmd
##
## Usage: open this file in RStudio and click "Run App" in the
## top-right corner -- no need to publish it anywhere, it runs
## fine locally on your own machine.
## Before running for the first time, make sure this package is installed:
##   install.packages(c("shiny"))
## =============================================================

library(shiny)

## -------------------------------------------------------------
## 0. Data setup: prefer the real Advertising dataset (needs internet).
##    If there's no network (e.g. no wifi in the classroom that day),
##    automatically fall back to simulated data with the same
##    structure -- the app keeps working either way.
## -------------------------------------------------------------
load_advertising <- function() {
  out <- tryCatch({
    df <- read.csv("https://www.statlearning.com/s/Advertising.csv", row.names = 1)
    attr(df, "source") <- "real"
    df
  }, error = function(e) NULL)

  if (is.null(out)) {
    set.seed(1)
    n <- 200
    df <- data.frame(
      TV        = runif(n, 0, 300),
      radio     = runif(n, 0, 50),
      newspaper = runif(n, 0, 100)
    )
    df$sales <- 3 + 0.045 * df$TV + 0.19 * df$radio + rnorm(n, sd = 1.5)
    attr(df, "source") <- "simulated"
    out <- df
  }
  out
}

Advertising <- load_advertising()
data(mtcars)

## =============================================================
## UI
## =============================================================
ui <- navbarPage(
  title = "Unit 3: Multiple Linear Regression -- Interactive Demo",
  theme = NULL,

  ## ---------- Tab 0: Overview ----------
  tabPanel(
    "Overview",
    fluidPage(
      h3("What this app is for"),
      p("This app is meant to be used alongside Unit3_LinearRegression_notes.Rmd. The formulas and
         derivations in the notes can all be explored here live -- adjust a parameter and immediately
         see how the result changes. It's more intuitive than working through it on a whiteboard, and
         students can also come back and explore it on their own after class."),
      tags$ul(
        tags$li(strong("Least squares & fit: "), "adjust the noise level and sample size, and watch the regression line and residuals change."),
        tags$li(strong("Sampling distribution of beta_hat: "), "resample many times to verify 'unbiasedness' -- the average of many estimates equals the true value."),
        tags$li(strong("What a 95% confidence interval actually means: "), "the single most commonly misunderstood
                 concept in this course, demonstrated directly with an animated Monte Carlo simulation of exactly
                 what '95%' refers to."),
        tags$li(strong("Hypothesis testing: F vs t: "), "toggle predictors in or out and watch the t-test, F-test,
                 and the identity F = t^2 update in real time."),
        tags$li(strong("Polynomial & interaction terms: "), "toggle a quadratic term / interaction term on or off and
                 compare against the nested F-test from anova().")
      ),
      p(em(paste0(
        "Current source of the Advertising data: ",
        if (identical(attr(Advertising, "source"), "real")) "downloaded successfully from statlearning.com (real data)."
        else "no internet connection available, so simulated data with the same structure is being used instead (the numbers won't exactly match the slides, but that's fine for demonstrating the concepts)."
      )))
    )
  ),

  ## ---------- Tab 1: Least squares & fit ----------
  tabPanel(
    "Least Squares & Fit",
    sidebarLayout(
      sidebarPanel(
        helpText("Simulated data: sales = 3 + 0.045*TV + noise"),
        sliderInput("ols_n", "Sample size n", min = 20, max = 400, value = 100, step = 10),
        sliderInput("ols_noise", "Noise standard deviation (sigma)", min = 0.1, max = 6, value = 1.5, step = 0.1),
        actionButton("ols_resample", "Draw a new sample"),
        hr(),
        verbatimTextOutput("ols_stats")
      ),
      mainPanel(
        plotOutput("ols_plot", height = "480px")
      )
    )
  ),

  ## ---------- Tab 2: beta_hat sampling distribution / unbiasedness ----------
  tabPanel(
    "β̂ Sampling Distribution (Unbiasedness)",
    sidebarLayout(
      sidebarPanel(
        helpText("Resample many times, re-estimating beta_hat each time, and check whether the average
                   of these estimates is exactly equal to the true value we set."),
        sliderInput("unb_nsim", "Number of resampling repetitions", min = 50, max = 5000, value = 1000, step = 50),
        sliderInput("unb_n", "Sample size n per repetition", min = 20, max = 300, value = 100, step = 10),
        numericInput("unb_true_beta1", "True coefficient beta_1 (effect of TV)", value = 0.045, step = 0.005),
        numericInput("unb_sigma", "Error standard deviation sigma", value = 1.5, step = 0.1),
        actionButton("unb_run", "Run simulation", class = "btn-primary")
      ),
      mainPanel(
        plotOutput("unb_plot", height = "420px"),
        verbatimTextOutput("unb_stats")
      )
    )
  ),

  ## ---------- Tab 3: What a 95% CI actually means ----------
  tabPanel(
    "What Does a 95% CI Mean",
    sidebarLayout(
      sidebarPanel(
        helpText(strong("The most commonly misunderstood concept: "),
                 "'The true beta has a 95% probability of falling in this interval' is WRONG!
                  The correct statement is: if we repeated the sampling many times and recomputed
                  the interval every time, about 95% of those intervals would contain the true value."),
        sliderInput("cov_nsim", "Number of simulation repetitions", min = 20, max = 1000, value = 200, step = 20),
        numericInput("cov_true_beta1", "True coefficient beta_1 (effect of TV)", value = 0.045, step = 0.005),
        actionButton("cov_run", "Run simulation and plot all intervals", class = "btn-primary"),
        hr(),
        h4("Coverage rate (should be ≈ 0.95):"),
        verbatimTextOutput("cov_rate")
      ),
      mainPanel(
        plotOutput("cov_plot", height = "560px"),
        helpText("Each horizontal line in the plot is a 95% confidence interval from one simulation run;
                   green = contains the true value (the dashed vertical line), red = does not.
                   '95%' refers to the proportion of green lines in this whole batch, not the
                   probability attached to any single line.")
      )
    )
  ),

  ## ---------- Tab 4: Hypothesis testing F vs t ----------
  tabPanel(
    "Hypothesis Testing: F vs t",
    sidebarLayout(
      sidebarPanel(
        helpText("Toggle each predictor in or out of the model and watch the t-test, the overall/partial
                   F-test, and the identity F = t^2 (when dropping exactly one variable) update live."),
        checkboxInput("ht_tv", "Include TV", value = TRUE),
        checkboxInput("ht_radio", "Include radio", value = TRUE),
        checkboxInput("ht_newspaper", "Include newspaper", value = TRUE),
        hr(),
        verbatimTextOutput("ht_f_eq_t2")
      ),
      mainPanel(
        h4("summary(fit)"),
        verbatimTextOutput("ht_summary"),
        h4("Partial F-test against the nested model with one variable dropped"),
        verbatimTextOutput("ht_anova")
      )
    )
  ),

  ## ---------- Tab 5: Polynomial & interaction terms ----------
  tabPanel(
    "Polynomial & Interaction Terms",
    sidebarLayout(
      sidebarPanel(
        selectInput("poly_dataset", "Choose a dataset / example",
                    choices = c("mtcars: mpg ~ wt (polynomial)" = "poly",
                                "Advertising: sales ~ TV*radio (interaction)" = "int")),
        conditionalPanel(
          condition = "input.poly_dataset == 'poly'",
          checkboxInput("poly_quad", "Add quadratic term I(wt^2)", value = FALSE)
        ),
        conditionalPanel(
          condition = "input.poly_dataset == 'int'",
          checkboxInput("poly_interact", "Add interaction term TV:radio", value = FALSE)
        )
      ),
      mainPanel(
        plotOutput("poly_plot", height = "420px"),
        h4("summary(fit)"),
        verbatimTextOutput("poly_summary"),
        h4("Nested-model F-test: does adding this term significantly improve the fit"),
        verbatimTextOutput("poly_anova")
      )
    )
  )
)

## =============================================================
## Server
## =============================================================
server <- function(input, output, session) {

  ## ---------- Tab 1: Least squares & fit ----------
  ols_data <- eventReactive(list(input$ols_resample, input$ols_n, input$ols_noise), {
    n <- input$ols_n
    sigma <- input$ols_noise
    TV <- runif(n, 0, 300)
    sales <- 3 + 0.045 * TV + rnorm(n, sd = sigma)
    data.frame(TV = TV, sales = sales)
  }, ignoreNULL = FALSE)

  output$ols_plot <- renderPlot({
    d <- ols_data()
    fit <- lm(sales ~ TV, data = d)
    plot(d$TV, d$sales, pch = 16, col = adjustcolor("steelblue", 0.6),
         xlab = "TV", ylab = "sales", main = "Least-squares fit + residuals")
    abline(fit, col = "firebrick", lwd = 2)
    segments(d$TV, d$sales, d$TV, fitted(fit), col = adjustcolor("grey40", 0.5))
  })

  output$ols_stats <- renderPrint({
    d <- ols_data()
    fit <- lm(sales ~ TV, data = d)
    cat("Fitted coefficients:\n"); print(coef(fit))
    cat(sprintf("\nRSS = %.2f\nR^2 = %.4f\n", sum(resid(fit)^2), summary(fit)$r.squared))
  })

  ## ---------- Tab 2: beta_hat sampling distribution ----------
  unb_result <- eventReactive(input$unb_run, {
    n <- input$unb_n
    n_sim <- input$unb_nsim
    true_beta1 <- input$unb_true_beta1
    sigma <- input$unb_sigma

    TV_fixed <- runif(n, 0, 300)   # keep the design matrix X fixed and only regenerate y (matches the notes' approach)
    beta1_sim <- numeric(n_sim)
    for (s in seq_len(n_sim)) {
      y_sim <- 3 + true_beta1 * TV_fixed + rnorm(n, sd = sigma)
      beta1_sim[s] <- coef(lm(y_sim ~ TV_fixed))[2]
    }
    list(beta1_sim = beta1_sim, true_beta1 = true_beta1)
  })

  output$unb_plot <- renderPlot({
    res <- unb_result()
    hist(res$beta1_sim, breaks = 40, col = "steelblue", border = "white",
         main = "Distribution of beta_hat_1 across repeated samples", xlab = expression(hat(beta)[1]))
    abline(v = res$true_beta1, col = "firebrick", lwd = 2, lty = 2)
    abline(v = mean(res$beta1_sim), col = "darkgreen", lwd = 2)
    legend("topright", legend = c("True value", "Mean of simulations"),
           col = c("firebrick", "darkgreen"), lty = c(2, 1), lwd = 2, bty = "n")
  })

  output$unb_stats <- renderPrint({
    res <- unb_result()
    cat(sprintf("True value beta_1                 = %.5f\n", res$true_beta1))
    cat(sprintf("Mean of %d simulated estimates     = %.5f\n", length(res$beta1_sim), mean(res$beta1_sim)))
    cat(sprintf("A single simulated estimate        = %.5f  <- one draw can be noticeably off from\n", res$beta1_sim[1]))
    cat("                                       the true value, but averaged over many draws it's exactly\n")
    cat("                                       right -- that's what 'unbiasedness' means.\n")
  })

  ## ---------- Tab 3: 95% CI coverage simulation ----------
  cov_result <- eventReactive(input$cov_run, {
    n <- nrow(Advertising)
    n_sim <- input$cov_nsim
    true_beta1 <- input$cov_true_beta1
    X <- cbind(1, as.matrix(Advertising[, c("TV", "radio", "newspaper")]))
    true_beta <- c(coef(lm(sales ~ TV + radio + newspaper, data = Advertising))[1],
                   true_beta1, 0.19, -0.001)

    lo <- numeric(n_sim); hi <- numeric(n_sim); covered <- logical(n_sim)
    for (s in seq_len(n_sim)) {
      eps <- rnorm(n, sd = 1.5)
      y_sim <- X %*% true_beta + eps
      fit_sim <- lm(y_sim ~ Advertising$TV + Advertising$radio + Advertising$newspaper)
      ci <- confint(fit_sim)["Advertising$TV", ]
      lo[s] <- ci[1]; hi[s] <- ci[2]
      covered[s] <- (ci[1] <= true_beta1) & (true_beta1 <= ci[2])
    }
    list(lo = lo, hi = hi, covered = covered, true_beta1 = true_beta1)
  })

  output$cov_plot <- renderPlot({
    res <- cov_result()
    n_show <- min(length(res$lo), 100)   # show at most 100 lines on the plot -- more than that gets unreadable
    idx <- seq_len(n_show)
    cols <- ifelse(res$covered[idx], "forestgreen", "firebrick")
    plot(NA, xlim = range(c(res$lo, res$hi)), ylim = c(1, n_show),
         xlab = expression(paste("95% confidence interval (each row is one independent repeated sample)")),
         ylab = "Simulation run #", yaxt = "n",
         main = sprintf("95%% CIs from the first %d simulations (green = contains true value, red = doesn't)", n_show))
    segments(res$lo[idx], idx, res$hi[idx], idx, col = cols, lwd = 2)
    abline(v = res$true_beta1, col = "black", lwd = 2, lty = 2)
    legend("topright", legend = "True value beta_1", lty = 2, lwd = 2, bty = "n")
  })

  output$cov_rate <- renderPrint({
    res <- cov_result()
    cat(sprintf("Proportion of the %d simulated intervals that contained the true value = %.3f\n", length(res$covered), mean(res$covered)))
    cat("(should be very close to 0.95 -- this is what '95% confidence interval' actually means)\n")
  })

  ## ---------- Tab 4: Hypothesis testing F vs t ----------
  ht_formula <- reactive({
    vars <- c(if (input$ht_tv) "TV", if (input$ht_radio) "radio", if (input$ht_newspaper) "newspaper")
    if (length(vars) == 0) vars <- "1"
    as.formula(paste("sales ~", paste(vars, collapse = " + ")))
  })

  ht_fit <- reactive({ lm(ht_formula(), data = Advertising) })

  output$ht_summary <- renderPrint({ summary(ht_fit()) })

  output$ht_anova <- renderPrint({
    vars <- c(input$ht_tv, input$ht_radio, input$ht_newspaper)
    names(vars) <- c("TV", "radio", "newspaper")
    included <- names(vars)[vars]
    if (length(included) < 1) {
      cat("The model has no predictors at all, so there's no nested model to compare against.\n"); return(invisible())
    }
    drop_var <- included[length(included)]
    reduced_vars <- setdiff(included, drop_var)
    reduced_formula <- if (length(reduced_vars) == 0) sales ~ 1 else
      as.formula(paste("sales ~", paste(reduced_vars, collapse = " + ")))
    fit_reduced <- lm(reduced_formula, data = Advertising)
    cat(sprintf("Partial F-test after dropping '%s':\n\n", drop_var))
    print(anova(fit_reduced, ht_fit()))
    if (length(included) - length(reduced_vars) == 1 && length(reduced_vars) >= 0) {
      full_smry <- summary(ht_fit())$coefficients
      if (drop_var %in% rownames(full_smry)) {
        t_val <- full_smry[drop_var, "t value"]
        cat(sprintf("\nVerifying F = t^2: t = %.4f, t^2 = %.4f\n", t_val, t_val^2))
      }
    }
  })

  output$ht_f_eq_t2 <- renderPrint({
    cat("Note: as long as you drop exactly 1 variable (q=1) at a time,\nthe F value from anova() above should be exactly equal to\nthe square of that variable's t value from summary().")
  })

  ## ---------- Tab 5: Polynomial & interaction terms ----------
  poly_fit <- reactive({
    if (input$poly_dataset == "poly") {
      if (isTRUE(input$poly_quad)) lm(mpg ~ wt + I(wt^2), data = mtcars)
      else lm(mpg ~ wt, data = mtcars)
    } else {
      if (isTRUE(input$poly_interact)) lm(sales ~ TV * radio, data = Advertising)
      else lm(sales ~ TV + radio, data = Advertising)
    }
  })

  poly_fit_reduced <- reactive({
    if (input$poly_dataset == "poly") lm(mpg ~ wt, data = mtcars)
    else lm(sales ~ TV + radio, data = Advertising)
  })

  output$poly_plot <- renderPlot({
    if (input$poly_dataset == "poly") {
      plot(mtcars$wt, mtcars$mpg, pch = 16, col = adjustcolor("steelblue", 0.6),
           xlab = "wt", ylab = "mpg", main = "mpg ~ wt")
      wt_seq <- seq(min(mtcars$wt), max(mtcars$wt), length.out = 100)
      lines(wt_seq, predict(poly_fit(), data.frame(wt = wt_seq)), col = "firebrick", lwd = 2)
    } else {
      plot(Advertising$TV, Advertising$sales, pch = 16, col = adjustcolor("steelblue", 0.6),
           xlab = "TV", ylab = "sales", main = "sales ~ TV (fitted lines at different radio levels, colored by radio level)")
      med_radio <- median(Advertising$radio)
      is_high <- Advertising$radio > med_radio
      points(Advertising$TV[is_high], Advertising$sales[is_high], col = adjustcolor("darkorange", 0.7), pch = 16)
      TV_seq <- seq(min(Advertising$TV), max(Advertising$TV), length.out = 100)
      lines(TV_seq, predict(poly_fit(), data.frame(TV = TV_seq, radio = quantile(Advertising$radio, 0.25))),
            col = "steelblue", lwd = 2)
      lines(TV_seq, predict(poly_fit(), data.frame(TV = TV_seq, radio = quantile(Advertising$radio, 0.75))),
            col = "darkorange", lwd = 2)
      legend("topleft", legend = c("Low radio (25th pct.)", "High radio (75th pct.)"),
             col = c("steelblue", "darkorange"), lwd = 2, bty = "n")
    }
  })

  output$poly_summary <- renderPrint({ summary(poly_fit()) })

  output$poly_anova <- renderPrint({ anova(poly_fit_reduced(), poly_fit()) })
}

shinyApp(ui = ui, server = server)
