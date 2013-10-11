#!/usr/bin/env Rscript

files <- function() {
  # paste joins strings help.search("concatenate")
  # getwd returns working directory
  print(paste("cwd: ", getwd()))

  setwd("../") 

  cat("cwd: ", getwd(), "\n")
}

files()

