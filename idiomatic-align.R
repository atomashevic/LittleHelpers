# install MIE package from Rudnev's github
remotes::install_github("MaksimRudnev/MIE.package", dependencies =T)
# install helpers package from Rudnev's github
remotes::install_github("MaksimRudnev/LittleHelpers", dependencies =T)

install.packages("kableExtra")

library(MIE)
library(LittleHelpers)
library(kableExtra)

spane_all <- extractAlignment("new.out")

df_to_viewer(spane_all$summary, colformat = c(2, 2, 0, 0, NA, NA))

# copy paste works from the html shown by RStudio

spane_nomac <- extractAlignment("spane-nomac.out")

df_to_viewer(spane_nomac$summary, colformat = c(2, 2, 0, 0, NA, NA))
