import boto3

def delete_inferior_models(bucket_name,
                                      folder_prefix,
                                      best_model):
    """
    This function deletes all files in a folder from S3 bucket
    :return: None
    """
    s3 = boto3.resource('s3')
    bucket = s3.Bucket(bucket_name)
    
    # Move best model to new directory
    for obj in bucket.objects.filter(Prefix = folder_prefix + best_model + "/").all():
        print(obj.key)
        new_key = str(obj.key).replace('models','best_models')
        s3.Object(bucket_name,new_key).copy_from(CopySource={'Bucket' : bucket_name, 'Key' : obj.key})
    
    # Delete all inferior models
    bucket.objects.filter(Prefix=folder_prefix).all().delete()
    
    print('done')