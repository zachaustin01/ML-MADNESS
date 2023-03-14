set.seed(2)

require(parallel)
##################################################################################################

get_model_info <- function(
    
    job_name,
    
    session,
    
    keep_inferior_models = False

){

    res = list()
    
    s = session$describe_tuning_job(job_name)
    completed = s$TrainingJobStatusCounters$Completed
    in_progress = s$TrainingJobStatusCounters$InProgress
    
    if(in_progress==0 & s[["HyperParameterTuningJobStatus"]]=="Completed"){
    
        tuning_job_results <- sagemaker$HyperparameterTuningJobAnalytics(job_name)
        res[['tuning_results_df']] <- tuning_job_results$dataframe()
        
        res[['Hyperparameter_plot']] <- ggplot(tuning_results_df, aes(TrainingEndTime, FinalObjectiveValue)) +
      geom_point() +
      xlab("Time") +
      ylab(tuning_job_results$description()$TrainingJobDefinition$StaticHyperParameters$`_tuning_objective_metric`) +
      ggtitle("Hyperparameter tuning objective metric",  
              "Progression over the period of all 30 training jobs") +
      theme_minimal()

        res[['best_tuned_model']] <- tuning_results_df %>%
          filter(FinalObjectiveValue == max(FinalObjectiveValue)) %>%
          pull(TrainingJobName)
        
        if(!keep_inferior_models){
        
            
        
        }
        
    }
    
}


##################################################################################################
train_test_upload_s3 <- function(
    session,
    sagemaker,
    model_dataset,

    non_predictors=c(),
    response="away_cover",
    
    split_type = 'normal',
    split_ratio = 0.8,

    bucket,
    key_prefix = 'data',
    model_name,
    project_name,
    repo_path = '/home/ec2-user/SageMaker/NFL-machine-learning'

){
    
    # Note: Sagemaker XGBoost requires response column to be the first variable when training
    model_dataset = model_dataset %>%
        relocate(response)
        
    if(split_type=="normal"){
        sample = sample.split(modeling[[response]], SplitRatio = split_ratio)
        train_data = subset(modeling, sample == TRUE)
        valid_data  = subset(modeling, sample == FALSE)
    }
    
    train_d_predictors = train_data %>% 
        select(-c(non_predictors)) %>%
        relocate(response)
    valid_d_predictors = valid_data %>%
        select(-c(non_predictors)) %>%
        relocate(response)
    
    write_csv(train_d_predictors, file.path(repo_path,paste0(model_name,"_train.csv")),col_names = F)
    write_csv(valid_d_predictors, file.path(repo_path,paste0(model_name,"_valid.csv")),col_names = F)
    
    s3_train <- session$upload_data(path = file.path(repo_path,paste0(model_name,"_train.csv")),
                                bucket = bucket,
                                key_prefix = paste0(file.path(project_name,model_name,key_prefix)))
    s3_valid <- session$upload_data(path = file.path(repo_path,paste0(model_name,"_valid.csv")),
                                    bucket = bucket,
                                key_prefix = paste0(file.path(project_name,model_name,key_prefix)))
    
    # Deletes files from local memory
    unlink(c(file.path(repo_path,paste0(model_name,"_train.csv")),file.path(repo_path,paste0(model_name,"_valid.csv"))))

    print('done uploading to S3')
    
    print('Getting XGB Docker container for Sagemaker')
    
    container <- sagemaker$image_uris$retrieve(framework='xgboost', region= session$boto_region_name, version='latest')
    cat('XGBoost Container Image URL: ', container)
    
    return(
    list(
       'container' = container,
       's3_train' = s3_train,
       's3_valid' = s3_valid
    )
    )
}
##################################################################################################

sagemaker_xgb <- function(
    pre_model_info,
    session,
    bucket,
    role_arn,
    sagemaker,

    model_name,
    project_name,
    model_prefix,
    
    instance_type = 'ml.m3.medium',
    
    models_to_try = 12L
    
    
){
    
    # Guide: https://alex23lemm.github.io/posts/2020-08-16-hyperparameter-tuning/
    
    estimator <- sagemaker$estimator$Estimator(image_uri = pre_model_info[['container']],
                                               role = role_arn,
                                               train_instance_count = 1L,
                                               train_instance_type = instance_type,
                                               train_volume_size = 30L,
                                               train_max_run = 3600L,
                                               input_mode = 'File',
                                               output_path = paste0("s3://", bucket, "/", project_name, "/", model_name, "/", model_prefix),
                                               output_kms_key = NULL,
                                               base_job_name = NULL,
                                               sagemaker_session = session)
    
    # Set Hyperparameters
    estimator$set_hyperparameters(eval_metric='auc',
                                  objective='binary:logistic',
                                  num_round=100L,
                                  rate_drop=0.3,
                                  tweedie_variance_power=1.4)
    
    # Set Hyperparameter Ranges
    hyperparameter_ranges = list('eta' = sagemaker$parameter$ContinuousParameter(0,0.1),
                            'min_child_weight'= sagemaker$parameter$ContinuousParameter(0,10),
                            'alpha'= sagemaker$parameter$ContinuousParameter(0,2),
                            'max_depth'= sagemaker$parameter$IntegerParameter(3L,10L))
    
    # Set the tuning objective to AUC
    objective_metric_name = 'validation:auc'
    
    # Create a hyperparamter tuner
    tuner <- sagemaker$tuner$HyperparameterTuner(
        estimator,
         objective_metric_name,
         hyperparameter_ranges,
         objective_type=
         ifelse(objective_metric_name == 'validation:auc', 'Maximize', 'Minimize'),
         max_jobs=models_to_try,
         max_parallel_jobs=4L)

    # Create a tuning job name
    job_name <- paste('tune-xgboost', format(Sys.time(), '%Y%m%d-%H-%M-%S'), sep = '-')

    s3_train_input <- sagemaker$inputs$TrainingInput(s3_data = pre_model_info[['s3_train']],
                                     content_type = 'csv')
    s3_valid_input <- sagemaker$inputs$TrainingInput(s3_data = pre_model_info[['s3_valid']],
                                     content_type = 'csv')
    
    s3_output <- paste0('s3://', bucket, '/output')
    
    # Define the data channels for train and validation datasets
    input_data <- list('train' = s3_train_input,
                       'validation' = s3_valid_input)
     
    # train the tuner
    tuner$fit(inputs = input_data, job_name = job_name, wait=FALSE)

    print(paste0('Started tuner for job: ',job_name))
    
    return(job_name)
}
    

##################################################################################################
get_best_xgb <- function(
    
    # Data Specific
    model_dataset,
    non_predictors,
    response,
    
    # Train / Test specific
    split_type = "normal", # Can be normal, or season to exclude as test
    split_ratio = 0.8, # Only used if split_type is normal
    
    # Model Specific
    nfold = c(5),
    etas = c(.1,.05,.025,.001,.0001),
    max.depth = c(7),
    min_child_weight = c(10),
    gamma = c(0),
    subsample = c(0.9),
    colsample_bytree = c(0.9),
    nrounds = c(1000,2000),
    early_stopping_rounds = c(50),
    
    nthread = parallel::detectCores(),
    verbose = 0,
    objective = "binary:logistic"
){

    # Prepare for grid search
    grid = expand.grid(
        nfold=nfold,
        etas=etas,
        max.depth=max.depth,
        min_child_weight=min_child_weight,
        gamma=gamma,
        subsample=subsample,
        colsample_bytree=colsample_bytree,
        nrounds=nrounds,
        early_stopping_rounds=early_stopping_rounds
        )
        
    model_res <- vector(mode = "list", length = nrow(grid))
    cm_res <- vector(mode = "list", length = nrow(grid))
    obs_res <- vector(mode = "list", length = nrow(grid))
    
    # Train test split
    if(split_type=="normal"){
    
        sample = sample.split(modeling$away_cover, SplitRatio = split_ratio)
        train_data = subset(modeling, sample == TRUE)
        test_data  = subset(modeling, sample == FALSE)
    
    }
    
    train_d_predictors <- train_data %>%
      select(-c(all_of(non_predictors),all_of(response)))

    tst_d_predictors<- test_data %>%
      select(-c(all_of(non_predictors),all_of(response)))
    
    dtrain <- xgb.DMatrix(data = as.matrix(train_d_predictors),
                      label = as.numeric(train_data[[response]]))
    # Create test matrix
    dtest <- xgb.DMatrix(data = as.matrix(tst_d_predictors),
                         label = as.numeric(test_data[[response]]))
    
    print(paste0('Testing ',nrow(grid),' models'))
    for(r in 1:nrow(grid)){
    
        row = grid[r,]
        temp_model <- xgboost(data = dtrain, # Set training data
                             
              eta = row$etas, # Set learning rate
              max.depth = row$max.depth, # Set max depth
              min_child_weight = row$min_child_weight, # Set minimum number of samples in node to split
              gamma = row$gamma, # Set minimum loss reduction for split
              subsample = row$subsample , # Set proportion of training data to use in tree
              colsample_bytree = row$colsample_bytree, # Set number of variables to use in each tree
               
              nrounds = row$nrounds, # Set number of rounds
              early_stopping_rounds = row$early_stopping_rounds, # Set number of rounds to stop at if there is no improvement
               
              verbose = verbose, # 1 - Prints out fit
              nthread = nthread, # Set number of parallel threads
              print_every_n = 20, # Prints out result every 20th iteration
               
              objective = objective, # Set objective
              eval_metric = 'error',
              eval_metric = 'auc'
              )
        model_res[[r]] = temp_model
        
        bst = temp_model
        
        boost_preds <- predict(bst, dtest) # Create predictions for XGBoost model

        res = data.frame(boost_preds,test_data)

        oc <- optimal.cutpoints(X = "boost_preds",
                       status = response,
                       tag.healthy = 0,
                       data = res,
                       methods = "MaxEfficiency")

        boost_pred_class <- rep(0, length(boost_preds))
        boost_pred_class[boost_preds >= 
                           oc$MaxEfficiency$Global$optimal.cutoff$cutoff[1]] <- 1

        t <- table(boost_pred_class, test_data$away_cover) # Create table

        cm <- confusionMatrix(t, positive = "1") # Produce confusion matrix

        cm_res[[r]] = t
        x = data.frame(boost_pred_class,test_data$away_cover,boost_preds)
        names(x) = c('yhat','y','conf')
        obs_res[[r]] = x
         
        
        acc = t %>% 
            group(yhat==1) %>%
            summarize(acc = mean(y==yhat),
                     pred = n())

        

    }
    
    return(list(
        'cm_res' = cm_res,
        'model_res' = model_res,
        'obs_res' = obs_res,
        'acc' = acc
    ))
    
    

}