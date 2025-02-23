#!/bin/bash

errors=0
exe="$1"

# Allow output message to contain "Usage" or "Synopsis" in any case
$exe -h | grep -q -i -e 'Usage' -e 'Synopsis' || {
    echo "Test Failed: $exe -h. Expected 'Usage' or 'Synopsis' in output"
    let errors+=1
}


res1='test_data/one_sequence.fasta	1	237	237	237	237'
$exe test_data/one_sequence.fasta | grep -q "$res1" || {
    echo "Test Failed: $exe test_data/one_sequence.fasta. Expected '$res1'"
    let errors+=1
}

res2='test_data/two_sequence.fasta	2	357	120	178	237'
$exe test_data/two_sequence.fasta | grep -q "$res2" || {
    echo "Test Failed: $exe test_data/two_sequence.fasta. Expected '$res2'"
    let errors+=1
}

res3='test_data/two_sequence.fasta	1	237	237	237	237'
$exe --minlen 200 test_data/two_sequence.fasta | grep -q "$res3" || {
    echo "Test Failed: $exe --minlen 200 test_data/two_sequence.fasta. Expected '$res3'"
    let errors+=1
}

# Test exit status for a bad command line invocation
#$exe --this_is_not_a_valid_argument > /dev/null 2>&1
#[ $? -ne 2 ] && {
#    echo "Test Failed '$exe --this_is_not_a_valid_argument'. Exit status was $?. Expected 2"
#    let errors+=1
#}

# Test exit status for a non existent input FASTA file 
#$exe this_file_does_not_exist.fasta > /dev/null 2>&1
#[ $? -ne 1 ] && {
#    echo "Test Failed '$exe this_file_does_not_exist.fasta'. Exit status was $?. Expected 1"
#    let errors+=1
#}

[ "$errors" -gt 0 ] && {
    echo "There were $errors errors found"
    exit 1
}

echo "Ok : $exe"
