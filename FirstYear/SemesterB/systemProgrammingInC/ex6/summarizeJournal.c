#include <stdio.h>
#include <string.h>
#include "/share/ex_data/ex6/journalLog.h"
#define LINE_LEN 302

int main(int argc, char** argv) {
    FILE* file;
    char* file_name;
    if ((argc == 1) || ((argc = 2) && (strcmp(argv[1], "-") == 0))){ //checking if file is recieved by stdin
        file = stdin;
        file_name = "stdin";
    }else{ //file recieved by argv[1]
        file = fopen(argv[1], "r");
        file_name = argv[1];
        if(!file) { //checking if the file did no open
            fprintf(stderr, "Error: %s cannot be open for reading\n", argv[1]);
            return 1;
        }
    }
    char buffer[LINE_LEN];
    int countF = 0, bLine = 0, flagH = 0, flagF = 0;
    JournalLog j_break; //will contain entries
    while (fgets(buffer, LINE_LEN, file) != NULL) { //reading lines from file
        buffer[strlen(buffer)-1] = '\0'; //adding null terminator to end of line
        if (buffer[0] == '#') { //checking if we are at a break line
            if(bLine != 0 && flagH == 1){ //if we have entries we print them using prinJournalLog
                printJournalLog(j_break);
                if (j_break){ //if j_break contains entries we free it (so it will be empty for next iterations(entries))
                    freeJournalLog(j_break);
                    j_break = NULL;
                }
            }
            else if (bLine != 0) {
                printf("No journal entries to print\n");
            }
            printf("%s\n",buffer+1); //printing entry without #

            //updating flags
            bLine++; //number of lines read
            flagF = 0; //if j_break is empty
            flagH = 0; //if we have consective hashtags
        }
        else {
            JournalLog j1 = strToJournalEntry(buffer); //contains current line
            if (!j1) { //checking if entry is valid
                countF++;
                continue;
            }
            if (flagF == 1){  //checking if we have previous entries
                j_break = concatJournalLogs(j_break, j1); //concat entries
                freeJournalLog(j1); //free current entry
            }else {
                j_break = j1; //copying j1 to j_break
                flagF = 1; //updating flagF as now j_break is not empty
            }
            if(!j_break) { //checking if concatination worked
                fprintf(stderr,"Out of memory\n");
                fclose(file); //closing the file
                freeJournalLog(j1); //freeing current entry (j1)
                freeJournalLog(j_break); //freeing all entries(j_break)
                return 13;
            }
            flagH = 1; //updating flagH to be 1 as now we did not encounter #
        }
    }
    if(j_break) { //checking if j_break not empty
        printJournalLog(j_break); //printing the final result with all relevant entries
        freeJournalLog(j_break); //freeing j_break
    } else
        printf("No journal entries to print\n");

    if (!strcmp(file_name, "stdin")) //closing the file if input was not stdin
        fclose(file);

    if (countF == 0) //returning 0 if no invalid lines were found
        return 0;
    else {
        fprintf(stderr,"File %s contained %d invalid lines, which were ignored\n", file_name, countF);
        return 2;
    }
}