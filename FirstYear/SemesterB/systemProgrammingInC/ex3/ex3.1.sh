cp /share/ex_data/ex3/date_of_birth.txt /share/ex_data/ex3/story.txt .
grep -E "[[:alpha:]]{5,}:.*[2-3][0-9]," date_of_birth.txt > long-names.txt
cat date_of_birth.txt | grep -E "[[:alpha:]][^r] [[:digit:]]*, (19)[4-5][0-9]" | sort -k1,1 | cut -f1 -d":" > boomers.txt
cat date_of_birth.txt | grep ": July " | sort -t" " -k4,4n -k3,3n | sed -r 's/(.*): July ([0-9]{,2}), [0-9]{2}([0-9]{2})/\1 was born on \2-7-\3/' > born-in-july.txt
cat story.txt |  tr "A-Z" "a-z" | tr -s "\n" | tr "[:punct:] " "\n" | grep -E "[[:alpha:]]{3,}" | sort | uniq -c |grep " [2-6] "| sort -k2,2 | tr -d " "| tail -1 |sed -r 's/([2-6])([[:alpha:]]*)/Word "\2" appears \1 times in story.txt/'
cat story.txt | sed -r 's/[0-9]*\.[0-9]*\.[0-9]*/\n&\n/g' | grep -E "^([1-9]|[1-2][0-9]|3[0-1])\.([1-9]|1[0-2])\.[0-9]{2}$" > story-dates.txt
