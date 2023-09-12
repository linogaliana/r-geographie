source("renv/activate.R")
setHook("rstudio.sessionInit", function(newSession) {
 if (newSession)
  {
    rstudioapi::navigateToFile("README.md")
    rstudioapi::applyTheme("Vibrant Ink")
  }
}, action = "append")
