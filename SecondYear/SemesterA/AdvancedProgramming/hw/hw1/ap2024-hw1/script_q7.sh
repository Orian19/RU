#!/bin/bash
first="$(git rev-list --max-parents=0 @)" # The hash of the initial commit
# check for the input argument - is it inside the data file script
echo "if grep -wc $1 data; then exit 1; else exit 0; fi" > isWordInFile.sh
chmod +x isWordInFile.sh                        # adding executable premissions
git bisect start > /dev/null                    # starting bisect process
git bisect bad > /dev/null                      # setting the bad commit to be the current one
git bisect good $first > /dev/null              # setting the good commit
# run the bash script with bisect + display the selected version
git bisect run ./isWordInFile.sh $1 2> /dev/null | grep Version | tail -n1 | cut -d" " -f6
