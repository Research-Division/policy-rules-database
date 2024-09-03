# This process will be defined in a function ----
# It is saved in the location: paste0(getwd(),"/","UpdateGitHub.R")
rm(list=ls())
user<-Sys.getenv("username")
setwd(paste0("C://Users//",user,"//Documents//policy-rules-database"))

UpdateGitHub <- function(repo=getwd(), untracked=TRUE, stage=TRUE, commit=TRUE, pull=TRUE, push=TRUE) {
  
  # Input: ----
  # - 'repo' must be an atomic string value which is a valid directory that contains the files for this repository. It will be validated by the rprojroot package.
  # - 'untracked' must be an atomic logical value which is used for determining whether or not to process the untracked files.
  # - 'stage' must be an atomic logical value which is used for determining whether or not to stage the files.
  # - 'commit' must be an atomic logical value which is used for determining whether or not to commit the files.
  # - 'pull' must be an atomic logical value which is used for determining whether or not to pull from the repo.
  # - 'push' must be an atomic logical value which is used for determining whether or not to push to the repo.
  
  # Output: ----
  # - Will print updates from the different stages.
  
  # Validate parameters: ----
  stopifnot(is.character(repo))
  stopifnot(dir.exists(repo))
  stopifnot(is.atomic(repo))
  stopifnot(is.atomic(untracked))
  stopifnot(is.atomic(stage))
  stopifnot(is.atomic(commit))
  stopifnot(is.atomic(pull))
  stopifnot(is.atomic(push))
  stopifnot(is.logical(untracked))
  stopifnot(is.logical(stage))
  stopifnot(is.logical(commit))
  stopifnot(is.logical(pull))
  stopifnot(is.logical(push))
  
  # Loop through the required packages. If not installed, then install it. If not loaded, then load it. ----
  packages <- c("git2r","rprojroot")
  for (package in packages) {
    if (!package %in% installed.packages()) {
      install.packages( package
                        , quiet = TRUE
                        , verbose = FALSE
                        , dependencies = TRUE
      )
    }
    if (!package %in% .packages()) {
      suppressPackageStartupMessages(
        suppressWarnings(
          suppressMessages(
            library( package
                     , character.only = TRUE
                     , quietly = TRUE
                     , warn.conflicts = FALSE
                     , verbose = FALSE
            )
          )
        )
      )
    }
  }
  
  # Check the Project Root directory. This is to ensure that the entire repo is captured. ----
  if (getwd() == find_rstudio_root_file()) {
    repo <- getwd()
  } else {
    repo <- find_rstudio_root_file()
  }
  
  # Check if there is anything to do. ####
  if (is.null(unlist(status()))) {
    return (writeLines(paste0("There is nothing to do.")))
  }
  
  # Set credentials. This will require input in the Console. ----
  username <- readline(prompt = "Please enter your GitHub Username: ") #ALWAYS BE CAREFUL ABOUT STORING YOUR CREDENTIALS ON GITHUB!!
  password <- readline(prompt = "Please enter your GitHub Password: ") #ALWAYS BE CAREFUL ABOUT STORING YOUR CREDENTIALS ON GITHUB!!
  credentials <- cred_user_pass(username = username, password = password)
  
  # NOTE: values returned from the status() command are as follows ----
  # 1. "untracked" means new files which have not yet been added to GitHub.
  # 2. "unstaged" means existing files which have been modified but not yet ready to be committed to GitHub.
  # 3. "staged" means files that are staged and ready to be committed.
  
  # Check the Untracked items. The intention is to just push these straight through. So this will Add, Commit, then Push them. ----
  if (untracked == TRUE) {
    num <- length(unlist(status()["untracked"]))
    if (num > 0) {
      writeLines(paste0("There are ", num, " Untracked items to be processed."))
      for (i in 1:num) {
        writeLines(paste0("    ", i, ": ",unlist(status()["untracked"])[i]))
      }
      add(repo, unlist(status()["untracked"]))
      writeLines(paste0("Items have been Staged."))
      commit(message = paste(Sys.time(), "Initial commit", sep = " - "))
      writeLines(paste0("Items have been Committed."))
      push(credentials = credentials)
      writeLines(paste0("Items have been Pushed."))
    }
  }
  
  # Process the Unstaged items. Add them. ----
  if (stage == TRUE) {
    num <- length(unlist(status()["unstaged"]))
    if (num > 0) {
      writeLines(paste0("There are ", num, " Tracked items to be processed."))
      for (i in 1:num) {
        writeLines(paste0("    ", i, ": ", unlist(status()["unstaged"])[i]))
      }
    }
    if (!is.null(unlist(status()["unstaged"]))) {
      add(repo, unlist(status()["unstaged"]))
      num2 <- length(unlist(status()["unstaged"]))
      if (num2 == 0) {
        writeLines(paste0("Items have been Staged."))
      } else if (num == num2) {
        stop ("Something went wrong with the Staging.")
      }
    }
  }
  
  # Process the Staged items. Commit them. ----
  if (commit == TRUE) {
    if (!is.null(unlist(status()["staged"]))) {
      commit(message = paste(Sys.time(), "Update", sep = " - ")) # Generic message, including timestamp.
      num2 <- length(unlist(status()["staged"]))
      if (num2 == 0) {
        writeLines(paste0("Items have been Committed."))
      } else if (num == num2) {
        stop ("Something went wrong with Committing.")
      }
    }
  }
  
  # Do the Pull step. ----
  if (pull == TRUE) {
    pull <- tryCatch ( #tryCatch is utilised because the error message when executing pull() or push() is not very helpful: "too many redirects or authentication replays". The main issue is usually that the credentials are incorrect or missing.
      expr = {
        pull(credentials = credentials)
      },
      error = function (err) {
        message (paste0("Error when Pulling from GitHub. Try checking your credentials and try again.","\n","Message thrown: "))
        stop (err)
      },
      warning = function (war) {
        message ("There was a Warning when Pulling from GitHub.")
        return (war)
      },
      finally = {
        # It was successful. Move on.
      }
    )
    if (unlist(pull["up_to_date"]) == TRUE) {
      writeLines(paste0("There are no discrepancies with the Master branch."))
    } else {
      stop ("Something went wrong with pulling the repo. Please manually check, merge the code, validate discrepancies, then re-try.")
    }
  }
  
  # Process the Committed items. Push them. ----
  if (push == TRUE) {
    if (num > 0) {
      tryCatch(
        expr = {
          push(credentials = credentials)
        },
        error = function(err) {
          message (paste0("Error when Pushing to GitHub. Try checking your credentials and try again.","\n","Message thrown: "))
          stop (err)
        },
        warning = function (war) {
          message ("There was a Warning when Pushing to GitHub.")
          return (war)
        },
        finally = {
          # It was successful. Move on.
        }
      )
      num2 <- length(unlist(status()))
      if (num2 == 0) {
        writeLines(paste0("Items have been Pushed."))
      } else if (num == num2) {
        stop ("Something went wrong with Pushing.")
      }
    }
  }
  
  return(writeLines(paste0("Successfully updated.")))
  
}

# Run the function.
UpdateGitHub()