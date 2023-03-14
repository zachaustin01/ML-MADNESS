import boto3
import pandas as pd
s3 = boto3.resource('s3')

def write_to_s3(local_filepath,bucket_name,object_prefix,s3_object_name):
    s3.meta.client.upload_file(local_filepath, bucket_name, object_prefix + '/' + s3_object_name)
        