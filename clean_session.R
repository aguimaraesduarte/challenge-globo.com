# Clear all packages
pkgs <- names(sessionInfo()$otherPkgs)
if(!is.null(pkgs)) pkgs <- paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE, force = TRUE)

# Clean session
rm(list=ls(all = TRUE))
cat("\014")
