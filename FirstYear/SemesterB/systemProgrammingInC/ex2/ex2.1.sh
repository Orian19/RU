cp /share/ex_data/ex2/date_of_birth.txt .
sort -k4,4rn -k2,2 date_of_birth.txt > dob_sorted.txt
cat date_of_birth.txt | tr -d [:punct:] | cut -f1,2,4 -d" " | tr " " "\t" > dob_summary.txt
cat date_of_birth.txt | cut -f2 -d" " | sort | cut -c1-3 | tr a-z A-Z | uniq -c | sort -k1rn | tr -s " "  >> dob_summary.txt
cat date_of_birth.txt | cut -f4 -d" " | cut -c3-4 | paste -d" " date_of_birth.txt - | cut -f1-3,5 -d " " > dob_yr.txt
