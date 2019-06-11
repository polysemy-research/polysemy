#!/bin/sh

# run with: ./build-stack-template.sh ../polysemy-template.hsfiles template-build-test
template_file=$1
project_name=$2
curr_dir=$(pwd)

stack new --resolver=nightly $project_name $template_file &&
    cd $project_name &&
    stack build &&
    cd $curr_dir &&
    rm -r $project_name
