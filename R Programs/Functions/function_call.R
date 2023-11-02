# R Function Call 


functions <- paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Research/R Programs/Functions', sep = '')

source(paste0(functions, 'install_packages.R', sep = ''))
source(paste0(functions, 'load_libraries.R', sep = ''))
source(paste0(functions, 'api_connections.R', sep = ''))
source(paste0(functions, 'rcg_style.R', sep = ''))

#To call this file in other programs, use:
#source(paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Research/R Programs/Functions/function_call.R', sep = ''))