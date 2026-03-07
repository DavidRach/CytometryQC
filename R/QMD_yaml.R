
#' Creates initial quarto yaml
#' 
#' @param outpath The location to save the file to, default is InstrumentQC folder
#' @param organization_name The name of the organization, ex. UMGCC FCSS
#' @param githubusername The githubusername, ex. umgccfcss
#' @param institution_name The name of the institution, ex. University of Maryland, Baltimore
#' @param FolderName Passed from FolderSetup
#' 
#' @return The .yaml file needed to generate the website
#' 
#' @noRd
QMD_yaml <- function(outpath, organization_name, githubusername,
   institution_name, FolderName){
  StorageLocation <- file.path(outpath, "_quarto.yml")

  GithubPage <- paste0("https://", githubusername, ".github.io/", FolderName, "/")

  SitePath <- paste0("/", FolderName, "/")

  content <- sprintf('project:
  type: website
  output-dir: docs/
  render:
    - "!index.qmd"
    - "*.qmd"
website:
  google-analytics: 
    tracking-id: "G-BYJ5XE4WD4"
  announcement: 
    icon: info-circle
    dismissable: true
    content: "Please see left-sidebar for definitions"
    type: info
    position: below-navbar  
  title: "%s"
  site-path: "%s"
  navbar:
    logo: images/hex.svg
    left:
    - text: "Home"
      href: index.qmd
    - text: "Levey-Jennings Plots"
      menu:
      - text: "Instrument"
        href: Instrument.qmd
    - text: "Historical"
      menu:
      - text: "Instrument"
        href: Historical.qmd
    right:
    - text: "Help"
      href: help.qmd
    - text: "Download Data"
      href: Data.qmd
    - text: "Other"
      menu:
      - text: "Miscellaneous"
        href: Miscellaneous.qmd
    - icon: github
      href: %s
      aria-label: GitHub          
  page-footer:
    background: light
    left: %s
    right: This dashboard was created with [Quarto](https://quarto.org/) using [CytometryQC](https://github.com/DavidRach/CytometryQC)
format:
  html:
    theme: styles.scss
', organization_name, SitePath, GithubPage, institution_name)
  
cat(content, file = StorageLocation)    
}