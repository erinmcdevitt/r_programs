
# Clean Session ------------------------------------------------------------

rm(list = ls())

#install and load libraries, styles, etc.
source(paste0())




# Current Date -------------------------------------------------------------

date <- 20231103


############################################################################
################## DO NOT UPDATE ANYTHING BELOW THIS LINE ##################
############################################################################

# Set Up -------------------------------------------------------------------

#file path
weekly_folder <- paste()

setwd(paste(weekly_folder))

# Outputs ------------------------------------------------------------------#

current_weekly <- paste0(weekly_folder, date, sep = '')

if (!dir.exists(current_weekly)) {
  dir.create(current_weekly)
  cat(paste("Folder for week created.\n"))
} else {
  cat(paste("Folder for week already exists.\n"))
}

figures_folder <- paste0(current_weekly, "/figures", sep = '')

if (!dir.exists(figures_folder)) {
  dir.create(figures_folder)
  cat(paste("Figures folder created.\n"))
} else {
  cat(paste("Figures folder already exists.\n"))
}

data_folder <- paste0(current_weekly, "/data", sep = '')

if (!dir.exists(data_folder)) {
  dir.create(data_folder)
  cat(paste("Data folder created.\n"))
} else {
  cat(paste("Data folder already exists.\n"))
}


# Create a new program ------------------------------------------------------#

#Set new program name and file path

new_program_name <- paste("weekly_charts_", date, ".R", sep="")
new_program_path <- paste(current_weekly, new_program_name, sep="/")

#Create program if it doesn't already exist and add relevant functions
if (!file.exists(new_program_path)) {
  file.create(new_program_path)
  sink(new_program_path, append=FALSE)
  cat(paste("#### Weekly Newsletter Visuals - ", date, "\n", sep = ''))
  cat(paste("#### Created By:", Sys.getenv("USERNAME"), "\n", sep = ''))
  cat(paste("#### Created: ", Sys.Date(), "\n\n\n", sep = ''))
  cat("#############################################################################\n")
  cat("# Set Up                                                                    #\n")
  cat("#############################################################################\n\n")
  
  cat("#Clean Session\n")
  cat("rm(list = ls())\n\n")
  cat("#Date of weekly (Friday)\n")
  cat(paste("date <- ", date, "\n\n", sep = ''))  
  cat("#Create link to Data Bank\n")
  cat("data <- paste()\n\n")
  cat("#Ready the environment\n")
  cat("source(paste0())\n\n")
  cat("source(paste0())\n\n")
  cat("source(paste0())\n\n")
  cat("source(paste0())\n\n")
  cat("#Create link to Weekly folder\n")
  cat("output_folder <- paste0()\n\n")
  cat("#Change directory to current weekly\n")
  cat("setwd(output_folder)\n\n")
  cat("#Create macro to connect to data and output folders\n")
  cat("figures_folder <- paste0(output_folder, '/figures', sep = '')\n\n")
  cat("data_folder <- paste0(output_folder, '/data', sep = '')\n\n")
  cat("#############################################################################\n")
  cat("# Data                                                                      #\n")
  cat("#############################################################################\n\n")
  cat("#Example: [Dataset 1] ------------------------------------------------------#\n\n")
  cat("#############################################################################\n")
  cat("# Charts                                                                    #\n")
  cat("#############################################################################\n\n")  
  cat("#Example: [Chart 1] --------------------------------------------------------#\n\n")
  cat("#Save output\n\n")
  cat("#ggsave(paste0(figures_folder, '/[file name].png', sep = ''), [plot name], width = 4, height = 6, unit = 'in')\n\n")
  cat("#ggsave(paste0(figures_folder, '/[file name].svg', sep = ''), [plot name], width = 4, height = 6, unit = 'in')\n\n")
  sink()
  cat(paste("Program for", date, "created.\n"))
} else {
  cat(paste("Program for", date, "already exists.\n"))
}

#Open file to edit

file.edit(new_program_path)


