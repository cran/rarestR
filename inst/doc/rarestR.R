## ----eval=FALSE---------------------------------------------------------------
# # Stable version
# install.packages('rarestR')
# # Development version
# remotes::install_github('pzhaonet/rarestR')

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
Output_tes <- tes(x = share[1,])
Output_tes

## ----fig.width=6, fig.height=3------------------------------------------------
plot(Output_tes)

## -----------------------------------------------------------------------------
Output_tess <- tess(share[1:2,])
Output_tess

## ----fig.width=3, fig.height=3------------------------------------------------
plot(Output_tess)

## -----------------------------------------------------------------------------
Output_tes
Output_tes$tbl

## ----eval=FALSE---------------------------------------------------------------
# Output_tes$TESa
# Output_tes$TESb

## -----------------------------------------------------------------------------
str(Output_tes$TESa)
str(Output_tes$TESb)

## ----eval=FALSE---------------------------------------------------------------
# Output_tes$TESa$result
# Output_tes$TESa$Predx
# Output_tes$TESa$Predy

## ----fig.width=3, fig.height=3------------------------------------------------
plot(Output_tes$TESa$Predx, Output_tes$TESa$Predy, type = 'l', las = 1, 
     xlab = 'ln(m)', ylab = 'ES', log = 'y')
points(Output_tes$TESa$result$Logm, Output_tes$TESa$result$value, col = 'blue')

## ----eval=FALSE---------------------------------------------------------------
# library(ggplot2)
# ggplot() +
#   geom_line(aes(Output_tes$TESa$Predx, Output_tes$TESa$Predy)) +
#   geom_point(aes(Logm, value), data = Output_tes$TESa$result, colour = 'red', shape = 1, size = 3) +
#   geom_hline(yintercept = as.numeric(Output_tes$TESa$par[1]), linetype = 2) +
#   lims(x = c(0, 20), y = c(0, 150)) +
#   labs(x = 'ln(m)', y = 'ES')

