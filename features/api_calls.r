source_python(file.path(repo_path,'sagemaker','utilities','s3write.py'))

require(hoopR)

.load_box_score_data <- function(
    seasons = 2006:2023,
    save_to_s3 = FALSE
){
    boxes = list()
    for(season in seasons){
        progressr::with_progress({
            box = load_mbb_team_box(season=season) %>%
                select(-team_uid)
        })
        if(save_to_s3){
            write_csv(box,file.path(repo_path,'temp.csv'))
            write_to_s3(file.path(repo_path,'temp.csv'),bucket,'ML-MADNESS/data_in',paste0('bs_',season,'.csv'))
        }
    boxes[[season]]=box
    }

    
    if(save_to_s3){unlink(file.path(repo_path,'temp.csv'))}
    
    return(bind_rows(boxes))
}

load_box_score_data = memoise::memoise(.load_box_score_data)

.load_pbp_data <- function(

    seasons = 2006:2023,
    save_to_s3 = FALSE

){

    boxes = list()
    for(season in seasons){
    
        box = load_mbb_pbp(seasons=season)
        boxes[[season]] = box
    }
    
    return(bind_rows(boxes))
   
}

load_pbp_data = memoise::memoise(.load_pbp_data)