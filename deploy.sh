#!/bin/bash

dirname="LambdaCalculus"
base=`pwd`

echo -e "\n\n ------------------- Deploying $dirname from $base to $1 --------------\n\n"
echo -e "\n\n ******************* compiling $dirname *************************\n\n"

ghcjs -O2 $dirname

cd $dirname.jsexe
if [ -d "$dirname" ] 
    then rm -rf $dirname
    fi
    
mkdir $dirname

echo -e "\n\n *********************** compressing *************************** \n\n"

ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > $dirname/all.min.js 

cp $base/index.html.deploy $dirname/index.html > /dev/null

echo -e "\n\n *********************** uploading *************************** \n\n"

scp -r $dirname $1 



