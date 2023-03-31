cp /share/ex_data/ex2/story.txt .
cat story.txt | head -n120 | tail -n+101 | tr "wokeWOKE" "WOKEwoke" > story-case.txt
cat story.txt | tr -s "-" | tr -d [:space:] | tr -d [:alnum:] | tr "-" "\n" | wc -l
cat story.txt | tr "\n" " " | tr ".?!" "\n" | tail -n66 > story-sentences.txt
cat story.txt |  tr -d [:punct:] | tr -s "\n" | tr " " "\n" | sort | uniq -c |sort -k1,1nr | head -10 | tr -s " " | cut -f3 -d" " > most-freq-words.txt

