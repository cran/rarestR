## ----eval=FALSE---------------------------------------------------------------
#  # Stable version
#  install.packages('rarestR')
#  # Development version
#  remotes::install_github('pzhaonet/rarestR')

## -----------------------------------------------------------------------------
library(rarestR)
data("share")

## -----------------------------------------------------------------------------
es(share, m = 100)
es(share, method = "b", m = 100)
# When the m is larger than the total sample size, "NA" will be filled:
es(share, m = 150)

## -----------------------------------------------------------------------------
ess(share)
ess(share, m = 100)
ess(share, m = 100, index = "ESS")

## -----------------------------------------------------------------------------
Output_tes <- tes(share[1,])
Output_tes

## ----eval=FALSE---------------------------------------------------------------
#  plot(Output_tes)

## -----------------------------------------------------------------------------
Output_tess <- tess(share[1:2,])
Output_tess

## ----eval=FALSE---------------------------------------------------------------
#  plot(Output_tess)

