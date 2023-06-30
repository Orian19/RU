#include "/share/ex_data/ex6/journalLog.h"
#include <stdio.h>
#include <string.h>
#include "/share/ex_data/ex6/virtualHeap.h"
/**********************************************************
* Problem 1.1
* Structure type struct journal_log
* --------------------------------
* The strucutre should have fields hodling:
* - the list of entries in the log (char*)
* - the start date for the log (short array of length 8)
* - the end   date for the log (short array of length 8)
* The date arrays should be part of the struct, but the
* entries string itself is not
* Object type is a pointer to the struct type
**********************************************************/
struct journal_log {
        char* log_entries;
        short start_date[8];
        short end_date[8];
};
/**********************************************************
* Problem 1.2
* Function newJournalEntry
* ------------------------
* creates a new journal log object with a single entry
* params:
* - logEntry (char*)  - a string containing a journal log entry.
*                       the string should not contain newlines ('\n')
*                       to indicate that this is a single entry
* - date     (char*)  - pointer to an 8-char array holding date
*                       in format YYYYMMDD
* Parameters should satisfy the following:
* - logEntry should not contain a newline char ('\n')
* - year  in date should be in range 1970 - 2023
* - month in date should be in 01 - 12
* - day   in date should be in 01 - 31
*         (function does not check for actual match of day and month)
* If input is invalid, function returns NULL
* Otherwise, the function allocates space for the struct and description
* on the heap, sets all members according to parameter values,
* and returns a pointer to the allocated structure.
* If allocation fails, function returns NULL
**********************************************************/
JournalLog newJournalEntry (const char* logEntry, const char* date) {
        if (strchr(logEntry, '\n')) return NULL; //returning null if entry contains \n
        int year = 0, power = 1000,day = 0, month = 0, i, countM = 10, countD = 10;

        for (i = 0; i < 8; i++) { //converting year,mont,day to integers values
                if(*(date) < '0' || *(date) > '9') return NULL; //if date not valid
                if(i <= 3) {
                   year += (*(date) - '0') * power;
                   power /= 10;
                } else if (i <= 5) {
                  month += (*(date) - '0') * countM;
                  countM = 1;
                } else {
                   day += (*(date) - '0') * countD;
                   countD = 1;
                }
                date++; //advancing the pointer of date
        }
        date-=8; //returning the pointer of date to the first address (first value)
        if (year < 1970 || year > 2023 || month > 12 || month < 1 || day < 1 || day > 31) return NULL; //checking if year or month not valid
        JournalLog j;
        j = ourMalloc(sizeof(struct journal_log)); //allocating memory for a new journal
        if(!j) return NULL; //checking if allocation succeded
        j->log_entries = ourMalloc(1+strlen(logEntry)*sizeof(char)); //allocating memory for a new entry
        if(!j->log_entries){ //checking allocation
          ourFree(j); //freeing the new journal
          return NULL;
        }
        strcpy(j->log_entries,logEntry); //copying entries to the field of entries
        for (i=0; i < 8; i++){ //updating the start,end date fields
                j->start_date[i] = date[i] - '0';
                j->end_date[i] = date[i] - '0';
        }
        return j; //returning journal log
        }
/**********************************************************
* Problem 1.3
* Function freeJournalLog
* -----------------------
* frees all space allocated by a journal log object
* params:
* - journalLog  (JournalLog) - pointer to a journal log object
* Frees all allocated space (structure and log entries)
**********************************************************/
void freeJournalLog(JournalLog journalLog){
        ourFree(journalLog->log_entries); //freeing entries
        ourFree(journalLog); //freeing the log struct
     }
/**********************************************************
* Problem 1.4
* Function printJournalLog
* ------------------------
* prints a journal log
* params:
* - journalLog  (JournalLog) - pointer to a journal log object
* Prints a journal log in the following format:
*
* log entries between DD/MM/YYYY and DD/MM/YYYY:
* < ENTRY 1 >
* < ENTRY 2 >
* . . .
* < ENTRY N >
**********************************************************/
void printJournalLog(JournalLog journalLog){
        int i;
        printf("log entries between ");
        for (i=6;i<8;i++) printf("%d",journalLog->start_date[i]); //day (start)
        printf("/");
        for (i=4;i<6;i++) printf("%d",journalLog->start_date[i]); //month (start)
        printf("/");
        for (i=0;i<4;i++) printf("%d",journalLog->start_date[i]); //year (start)
        printf(" and ");
        for (i=6;i<8;i++) printf("%d",journalLog->end_date[i]); //day (end)
        printf("/");
        for (i=4;i<6;i++) printf("%d",journalLog->end_date[i]); //month (end)
        printf("/");
        for (i=0;i<4;i++) printf("%d",journalLog->end_date[i]); //year (end)
        printf(":\n%s\n",journalLog->log_entries);
        }

/**********************************************************
* Problem 1.5
* Function strToJournalEntry
* --------------------------
* creates a new journal log object based on an input string
* params:
* - str (char*)  - a string in the following format:
*      "<SPACE1><TOKEN1><SPACE2><TOKEN2>"
*      where <SPACE1> is a (possibly empty) sequence of spaces
*      and   <SPACE2> is a non-empty sequence of spaces
*      and   <TOKEN1> contains no spaces
*      and   <TOKEN2> does not contain newlines
* The function creates a new journal log object by using
* <TOKEN1> to determine the date, and using <TOKEN2> for
* the log entry.
* If the input string does not have the expected structure
* as defined above, the function returns NULL.
* Otherwise, it returns the new journal log object
**********************************************************/
JournalLog strToJournalEntry(const char* str){
        const char *token1;
        while (*str == ' ') str++; //advancing to token1
        token1 = str;
        if(!strchr(str, ' ')) return NULL; //checking if no space after token1
        while (*str != ' ') str++; //advancing to token 2
        while (*str == ' ') str++; //advancing to end of token2
        //now str contains token2
        return newJournalEntry(str,token1); //returing the complete string as new logEntry
}

/**********************************************************
* Problem 1.6
* Function numJournalLogEntries
* ------------------------
* returns the number of entries in a given journal log
* params:
* - journalLog  (JournalLog) - pointer to journal log object
* Returns the number of entries in a given journal log
* by counting the number of newlines in the entry list string
* and adding 1.
**********************************************************/
int numJournalLogEntries(JournalLog journalLog){
      int count = 1 , i;
      for (i = 0; i < strlen(journalLog->log_entries); i++) {
         if (journalLog->log_entries[i] == '\n') //checking if we ecnountred a new entry
                count++;
      }
      return count; //returning the enrtries count
}

/**********************************************************
* Problem 1.7
* Function concatJournalLogs
* --------------------------
* concatenates one journal log after the other
* params:
* - journalLog1  (JournalLog) - pointer to journal log object
* - journalLog2  (JournalLog) - pointer to journal log object
* The function modifies journalLog1 to include all the entries
* originally listed in it followed by the entries listed in
* journalLog2. The new date range should be the minimal range
* that covers both ranges.
* The contents of journalLog2 should remain unchanged.
* The function returns a pointer to the modified object journalLog1.
**********************************************************/
JournalLog concatJournalLogs(JournalLog journalLog1, JournalLog journalLog2){
        int i, minSDate = 2, maxEDate = 2;
        short *start1 = journalLog1->start_date, *start2 = journalLog2->start_date; //pointers to start dates of logs
        short *end1 = journalLog1->end_date, *end2 = journalLog2->end_date; //pointers to end date to logs
        int size = strlen(journalLog1->log_entries)*sizeof(char) + strlen(journalLog2->log_entries)*sizeof(char) + 2; //size of new entry (after concat)

        for (i = 0; i < 8; i++) {
             //checking which log has the minimal start date by comparing the digits from msb to lsb
             if(*(start1) < * (start2)) minSDate = 1;
             if(*(start1++) > * (start2++)) break;
        }

        for(i = 0; i < 8; i++) {
             //checking which log has the maximal end date (same methood as above)
             if(*(end1) > * (end2)) maxEDate = 1;
             if(*(end1++) < * (end2++)) break;
         }

        char* entries = ourRealloc(journalLog1->log_entries, size); //reallocating memort to hold both log1 and log2 entries

        if(!entries) return NULL; // checking allocation worked
        journalLog1->log_entries = entries; //updating log1 entries to hold log1 entries + empty space for log2 (=entries)
        strcat(entries,"\n");
        strcat(entries,journalLog2->log_entries); //concat entries with log2 entreis --> entries contains log1+log2 entries

        if (minSDate == 2) { //in this case we need to update to log2 start date
                for (i = 0; i < 8; i++) {
                        journalLog1->start_date[i] = journalLog2->start_date[i];
                }
        }

        if (maxEDate == 2){ //in this case we need to update to log2 end date
                for (i=0;i<8;i++){
                        journalLog1->end_date[i] = journalLog2->end_date[i];
                }
        }
        return journalLog1;
}