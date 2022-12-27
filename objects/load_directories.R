# File containing directories to be used in project (should be loaded at
# beginning of all scripts)
#
# Code chunk to run:
#
# source(file.path('C:/Users/zacha/OneDrive/Desktop/nba_live_betting_model',
#                  'code',
#                  'utilities',
#                  'load_directories.r'),
#        local = knitr::knit_global())


base_path = repo_path

data_path = str_replace(repo_path,"ML-MADNESS","mbb-data-lake")

object_path = file.path(repo_path,'objects')

source(file.path(object_path,'helper_functions.r'))
