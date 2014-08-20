

pkgInstall <- function(packageToInstall)
{
  #Repository for package install
  repos='http://star-www.st-andrews.ac.uk/cran/'
  
  if (!require(packageToInstall,character.only = TRUE))
  {
    install.packages(packageToInstall,dep=TRUE, repos=repos)
    if(!require(packageToInstall, character.only = TRUE)) stop("Package not found")
  }
}