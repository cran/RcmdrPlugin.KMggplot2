kmg2_plot_contour <- function() {

  plotContour <- kmg2_contour$new()
  plotContour$plotContour()

}

kmg2_plot_qq <- function() {

  plotQQ <- kmg2_qq$new()
  plotQQ$plotQQ()

}

kmg2_plot_discretebar <- function() {

  plotDiscretebar <- kmg2_discretebar$new()
  plotDiscretebar$plotDiscretebar()

}

kmg2_plot_pie <- function() {

  plotPie <- kmg2_pie$new()
  plotPie$plotPie()

}

kmg2_plot_scattermatrix <- function() {

  plotScattermatrix <- kmg2_scattermatrix$new()
  plotScattermatrix$plotScattermatrix()

}

kmg2_plot_line <- function() {

  plotLine <- kmg2_line$new()
  plotLine$plotLine()

}

kmg2_plot_scatter <- function() {

  plotScatter <- kmg2_scatter$new()
  plotScatter$plotScatter()

}

kmg2_plot_box <- function() {

  plotBox <- kmg2_box$new()
  plotBox$plotBox()

}

kmg2_plot_hist <- function() {

  plotHist <- kmg2_hist$new()
  plotHist$plotHist()

}

kmg2_plot_km <- function() {

  plotKM <- kmg2_km$new()
  plotKM$plotKM()

}

kmg2_dist_norm <- function() {

  distNorm <- kmg2_dist$new()
  distNorm$plotContinuous(
    distName   = "norm",
    parmInits  = list("0", "1"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Mean"),
                 kmg2_gettextRcmdr("S.D.")),
    title      = kmg2_gettextRcmdr("Plot normal distiribution")
  )

}

kmg2_dist_t <- function() {

  distT <- kmg2_dist$new()
  distT$plotContinuous(
    distName   = "t",
    parmInits  = list("1"),
    parmTitles = list(kmg2_gettextRcmdr("df")),
    title      = kmg2_gettextRcmdr("Plot t distiribution")
  )

}

kmg2_dist_chisq <- function() {

  distChisq <- kmg2_dist$new()
  distChisq$plotContinuous(
    distName   = "chisq",
    parmInits  = list("1"),
    parmTitles = list(kmg2_gettextRcmdr("df")),
    title      = kmg2_gettextRcmdr("Plot chi-square distiribution")
  )

}

kmg2_dist_f <- function() {

  distF <- kmg2_dist$new()
  distF$plotContinuous(
    distName   = "f",
    parmInits  = list("1", "1"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Numerator df"),
                 kmg2_gettextRcmdr("Denominator df")),
    title      = kmg2_gettextRcmdr("Plot F distiribution")
  )

}

kmg2_dist_exp <- function() {

  distExp <- kmg2_dist$new()
  distExp$plotContinuous(
    distName   = "exp",
    parmInits  = list("1"),
    parmTitles = list(kmg2_gettextRcmdr("rate")),
    title      = kmg2_gettextRcmdr("Plot exponential distiribution")
  )

}

kmg2_dist_unif <- function() {

  distUnif <- kmg2_dist$new()
  distUnif$plotContinuous(
    distName   = "unif",
    parmInits  = list("0", "1"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Minimum"),
                 kmg2_gettextRcmdr("Maximum")),
    title      = kmg2_gettextRcmdr("Plot uniform distiribution")
  )

}

kmg2_dist_beta <- function() {

  distBeta <- kmg2_dist$new()
  distBeta$plotContinuous(
    distName   = "beta",
    parmInits  = list("9", "3"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Shape 1"),
                 kmg2_gettextRcmdr("Shape 2")),
    title      = kmg2_gettextRcmdr("Plot beta distiribution")
  )

}

kmg2_dist_cauchy <- function() {

  distCauchy <- kmg2_dist$new()
  distCauchy$plotContinuous(
    distName   = "cauchy",
    parmInits  = list("0", "1"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Location"),
                 kmg2_gettextRcmdr("Scale")),
    title      = kmg2_gettextRcmdr("Plot cauchy distiribution")
  )

}

kmg2_dist_logis <- function() {

  distLogis <- kmg2_dist$new()
  distLogis$plotContinuous(
    distName   = "logis",
    parmInits  = list("0", "1"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Location"),
                 kmg2_gettextRcmdr("Scale")),
    title      = kmg2_gettextRcmdr("Plot logistic distiribution")
  )

}

kmg2_dist_lnorm <- function() {

  distLnorm <- kmg2_dist$new()
  distLnorm$plotContinuous(
    distName   = "lnorm",
    parmInits  = list("0", "1"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Mean (log scale)"),
                 kmg2_gettextRcmdr("S.D. (log scale)")),
    title      = kmg2_gettextRcmdr("Plot log-normal distiribution")
  )

}

kmg2_dist_gamma <- function() {

  distGamma <- kmg2_dist$new()
  distGamma$plotContinuous(
    distName   = "gamma",
    parmInits  = list("1", "1"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Shape"),
                 kmg2_gettextRcmdr("Rate (inverse scale)")),
    title      = kmg2_gettextRcmdr("Plot gamma distiribution")
  )

}

kmg2_dist_weibull <- function() {

  distWeibull <- kmg2_dist$new()
  distWeibull$plotContinuous(
    distName   = "weibull",
    parmInits  = list("1", "pi"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Shape"),
                 kmg2_gettextRcmdr("Scale")),
    title      = kmg2_gettextRcmdr("Plot weibull distiribution")
  )

}

kmg2_dist_binom <- function() {

  distBinom <- kmg2_dist$new()
  distBinom$plotDiscrete(
    distName   = "binom",
    parmInits  = list("20", "0.5"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Binomial trials"),
                 kmg2_gettextRcmdr("Probability of success")),
    title      = kmg2_gettextRcmdr("Plot binomial distiribution")
  )

}

kmg2_dist_pois <- function() {

  distPois <- kmg2_dist$new()
  distPois$plotDiscrete(
    distName   = "pois",
    parmInits  = list("10"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Mean")),
    title      = kmg2_gettextRcmdr("Plot poisson distiribution")
  )

}

kmg2_dist_geom <- function() {

  distGeom <- kmg2_dist$new()
  distGeom$plotDiscrete(
    distName   = "geom",
    parmInits  = list("0.25"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Probability of success")),
    title      = kmg2_gettextRcmdr("Plot geometric distiribution")
  )

}

kmg2_dist_hyper <- function() {

  distHyper <- kmg2_dist$new()
  distHyper$plotDiscrete(
    distName   = "hyper",
    parmInits  = list("25", "20", "8"),
    parmTitles = list(
                 kmg2_gettextRcmdr("m (number of white balls in the urn)"),
                 kmg2_gettextRcmdr("n (number of black balls in the urn)"),
                 kmg2_gettextRcmdr("k (number of balls drawn from the urn)")),
    title      = kmg2_gettextRcmdr("Plot hypergeometric distiribution")
  )

}

kmg2_dist_nbinom <- function() {

  distNbinom <- kmg2_dist$new()
  distNbinom$plotDiscrete(
    distName   = "nbinom",
    parmInits  = list("5", "0.5"),
    parmTitles = list(
                 kmg2_gettextRcmdr("Target number of success"),
                 kmg2_gettextRcmdr("Probability of success")),
    title      = kmg2_gettextRcmdr("Plot negative binomial distiribution")
  )

}
