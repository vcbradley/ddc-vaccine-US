# Assuming both this repo and our private repo are available locally, this
# script moves files from the private version to the public version


library(fs)

dir_priv <- "~/Dropbox/ddi-covid-studies/"

# Assume using Rproject and the public project directory is the current working directory


file_copy(path(dir_priv, ".gitignore"), new_path = ".", overwrite = TRUE)
file_copy(dir_ls(dir_priv, regexp = "(08|09).*\\.R$"), new_path = ".", overwrite = TRUE)
