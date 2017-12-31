### CANDIDATE

* If you're using OSX, Install docker on your local machine

#### preparing your code
- you will be running the mapreduce streaming job on an EMR cluster 
- EMR clusters run amazon linux so your binaries need to be compiled for linux
- If you're compiling on a macbook, you would need to use stacks docker compile
``` 
    docker build -t 'takt/stack-protoc-build:lts-8.22' docker
    stack --docker --docker-image=takt/stack-protoc-build:lts-8.22 build --ghc-options '-optl-static'
```

* install aws-cli http://docs.aws.amazon.com/cli/latest/userguide/installing.html

* Get the aws credentials from the interviewer

* add the "rohanmc" credentials to your ~/.aws/credentials file

* Get the cluster ID of the "interview cluster" by running 
 
```aws emr list-clusters --active --profile=rohanmc --region=us-west-2```

* upload your binaries and files using aws cli to s3 - http://docs.aws.amazon.com/cli/latest/reference/s3/
* use this path for the files s3://haskell-emr-test/<your name or identifier>/

* refer to the hadoop streaming documentation if you have any questions about how hadoop streaming works https://wiki.apache.org/hadoop/HadoopStreaming

#### example of how to run an emr streaming job

* ```aws emr add-steps --region=us-west-2 --profile=rohanmc --cluster-id j-1DA7HSBVGVQ5L --steps file://./hcap.json```

where hcap.json is

``` 
 [
   {
      "Name": "JSON Streaming Step",
      "Type": "STREAMING",
      "ActionOnFailure": "CONTINUE",
      "Args": [
          "-files",
          "s3://haskell-emr-test/code/mapperbin,s3://haskell-emr-test/code/reducerbin",
          "-mapper",
          "mapperbin",
          "-reducer",
          "reducerbin",
          "-input",
          "s3://haskell-emr-test/input/someinputfile",
          "-output",
          "s3://haskell-emr-test/input/someoutputfile"]
   }
 ]
 ```

### INTERVIEWER
* checkout the takt-tools repo

* make sure you have access to the rohanmc aws account

* generate  credentials for candidate using 

```python python/create_user.py -p rohanmc -u <username> --policy_list="['AdministratorAccess','IAMUserChangePassword']"```

* Get the cluster ID of the "haskell emr cluster" by running 
 
```aws emr list-clusters --active --profile=rohanmc --region=us-west-2```

* If the cluster isn't running already, you can launch it using

```
aws emr create-cluster --profile=rohanmc --auto-scaling-role EMR_AutoScaling_DefaultRole --applications Name=Hadoop Name=Hive Name=Pig Name=Hue Name=Ganglia --ec2-attributes '{"KeyName":"candidate","InstanceProfile":"EMR_EC2_DefaultRole","SubnetId":"subnet-02b32d5a","EmrManagedSlaveSecurityGroup":"sg-03be6f79","EmrManagedMasterSecurityGroup":"sg-d2a170a8"}' --service-role EMR_DefaultRole --enable-debugging --release-label emr-5.6.0 --log-uri 's3n://aws-logs-571270348016-us-west-2/elasticmapreduce/' --name 'interview cluster' --instance-groups '[{"InstanceCount":2,"InstanceGroupType":"CORE","InstanceType":"m3.xlarge","Name":"Core - 2"},{"InstanceCount":1,"InstanceGroupType":"MASTER","InstanceType":"m3.xlarge","Name":"Master - 1"}]' --configurations '[{"Classification":"yarn-site","Properties":{"yarn.nodemanager.vmem-check-enabled":"false"},"Configurations":[]}]' --tags cluster_name="candidate_cluster" --region us-west-2
```

* once interview is done

```python python/create_user.py -d```
