remotes::install_github("rstudio/bslib")
library(bslib)

install.packages("pkgbuild")
library(pkgbuild)
has_rtools(debug = TRUE)
check_rtools(debug = TRUE)
rtools_path()
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
install.packages("jsonlite", type = "source")

theme <- bs_theme(bg = "#6c757d", fg = "white", primary = "orange")
bs_theme_preview(theme)

bslib(theme = 4)

bs_theme_preview(
    bs(theme(bootswatch = "solar"))
)

bs_theme_preview()

bs_theme(
    bg = "grey",fg = "black",primary = "#88398a",
    base_font = font_google("Fraunces")
)

thematic - translate CSS to R plots

if (!require("remotes"))
    install.packages("remotes")
remotes::install_github("rstudio/shiny")

thematic::thematic_shiny()
fluidPage(
    theme = bs_theme(
        bg = "#002B36",fg = "#EEE8D5",primary = "#2AA198",
    )
)