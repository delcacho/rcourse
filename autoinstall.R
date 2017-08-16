library <- function(somepackage) {
   pkgname <- as.character(substitute(somepackage))
   if(!require(pkgname, character.only = TRUE)){
      install.packages(pkgname,
          repos="http://cran.us.r-project.org",quiet=TRUE)
      require(pkgname,character.only=TRUE)
   }
}

