#' Quarto renders the project to produce the website, checks for git, sends to GitHub. 
#' 
#' @param FolderName Default is InstrumentQC2, if folder/repository was named as
#'  something else, please specify
#' @param GitArchive Default FALSE, when set true will (create if needed) and back up
#' to GitHub
#' 
#' @importFrom quarto quarto_render
#' @importFrom usethis use_git
#' @importFrom usethis use_github
#' @importFrom git2r repository
#' @importFrom git2r pull
#' 
#' @return Rendered .html files containing the website to the docs folder, pushes changes
#' to GitHub
#' 
#' @export
#' 
#' @examples
#' 
#' A <- 2 + 2
#' 
RenderWebsite <- function(FolderName="InstrumentQC2", GitArchive=FALSE){
  FolderPattern <- paste0("^", FolderName, "$")
  DocumentsPath <- OperatingSystemCheck()
  InstrumentQC <- list.files(DocumentsPath, pattern=FolderPattern,
   full.names=TRUE)
  if (length(InstrumentQC) == 0){stop("Run FolderSetup step first!")}

  GitPresent <- list.files(InstrumentQC, all.files=TRUE, pattern="\\.git$")

  if (length(GitPresent) == 1){
    setwd(InstrumentQC)
    TheRepo <- git2r::repository(InstrumentQC)
    git2r::pull(TheRepo)
  }

  #quarto_render(input=InstrumentQC)

  setwd(InstrumentQC)
  ExecuteThis <- paste("quarto render")
  system(ExecuteThis)

  if (GitArchive == TRUE){

  if (length(GitPresent) == 0){
    setwd(InstrumentQC)
    use_git(message="Initial project setup")
    use_github(private=FALSE) # Needs pre-established credential setup
  } else {
    Today <- Sys.Date()
    Today <- as.Date(Today)

    git2r::add(TheRepo, "*")
    TheCommitMessage <- paste0("Updated on ", Today)
    git2r::commit(TheRepo, message = TheCommitMessage)
    # Same Git Credentialling Issues for Git2R vs alternative setups in play
    #cred <- git2r::cred_token(token = "GITHUB_PAT")
    #git2r::push(TheRepo, credentials=cred)
  }
  } else {message("Website rendered")}

}