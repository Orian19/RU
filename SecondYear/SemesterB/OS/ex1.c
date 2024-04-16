/*
 * ex1.c
 */

#include <sys/types.h>
#include <sys/stat.h>

#include <fcntl.h>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAX_BUFFER_SIZE 65536
#define DESTINATION_FILE_MODE S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH

extern int opterr, optind;

void exit_with_usage(const char *message) {
	fprintf (stderr, "%s\n", message);
	fprintf (stderr, "Usage:\n\tex1 [-f] BUFFER_SIZE SOURCE DEST\n");
	exit(EXIT_FAILURE);
}

void copy_file(const char *source_file, const char *dest_file, int buffer_size, int force_flag) {
	/*
	 * Copy source_file content to dest_file, buffer_size bytes at a time.
	 * If force_flag is true, then also overwrite dest_file. Otherwise print error, and exit.
	 *
	 * TODO:
	 * 	1. Open source_file for reading
	 * 	2. Open dest_file for writing (Hint: is force_flag true?)
	 * 	3. Loop reading from source and writing to the destination buffer_size bytes each time
	 * 	4. Close source_file and dest_file
	 *
	 *  ALWAYS check the return values of syscalls for errors!
	 *  If an error was found, use perror(3) to print it with a message, and then exit(EXIT_FAILURE)
	 */

	// 1

	int sf = open(source_file, O_RDONLY, DESTINATION_FILE_MODE);  // opening the file with read only flag
	if (sf == -1) {  // checking if opened successfully (exiting and printing error if not)
		perror("Unable to open source file for reading");
		exit(EXIT_FAILURE);
	}

	// 2

	int df;
	char dErrMsg[] = "Unable to open destination file for writing";
	if (!force_flag) {  // checking if force should be used
		df = open(dest_file, O_WRONLY | O_CREAT | O_EXCL, DESTINATION_FILE_MODE);  // opening in write mode, creating the file if DNE and using CREAT together with EXCL to prevent override
		if (df == -1) {
			close(sf); // closing the source file
			perror(dErrMsg);
			exit(EXIT_FAILURE);
		}
	} else {  // here using force
		df = open(dest_file, O_WRONLY | O_CREAT | O_TRUNC, DESTINATION_FILE_MODE); // here using TRUNC to truncate file length to 0 (if exists) => override
		if (df == -1) {
			close(sf); // closing the source file
			perror(dErrMsg);
			exit(EXIT_FAILURE);
		}
	}

	// 3

	char buff[MAX_BUFFER_SIZE]; // init buffer (with max size)
	int bytes_read = 1; // init to 1 for the first loop
	int bytes_write;
	while(bytes_read > 0) {
		bytes_read = read(sf, buff, buffer_size); // reading buffer_size bytes to the buffer
		if (bytes_read == -1) {
			perror("Unable to read source file");
			exit(EXIT_FAILURE);
		} else {
			bytes_write = write(df, buff, buffer_size); // writing bytes from buffer to the destination file
			if (bytes_write == -1) {
				close(sf);
				close(df);
				perror("Unable to write to destination file");
				exit(EXIT_FAILURE);
			}
			else if (bytes_write < bytes_read) {
				close(sf);
				close(df);
				perror("Unable to write buffer content to destination file");
				exit(EXIT_FAILURE);
			}
		}
	}
	write(df, "\n", 1); // ending the file with a newline


	// 4

	// closing source file
	int closeSf = close(sf);
	if (closeSf == -1) {
		perror("Unable to close source file");
		exit(EXIT_FAILURE);
	}

	// closing destination file
	int closeDf = close(df);
	if (closeDf == -1) {
		perror("Unable to close destination file");
		exit(EXIT_FAILURE);
	}

	// printing success message
	printf("File %s was successfully copied to %s \n", source_file, dest_file);
	exit(EXIT_SUCCESS);

}

void parse_arguments(
		int argc, char **argv,
		char **source_file, char **dest_file, int *buffer_size, int *force_flag) {
	/*
	 * parses command line arguments and set the arguments required for copy_file
	 */
	int option_character;

	opterr = 0; /* Prevent getopt() from printing an error message to stderr */

	while ((option_character = getopt(argc, argv, "f")) != -1) {
		switch (option_character) {
		case 'f':
			*force_flag = 1;
			break;
		default:  /* '?' */
			exit_with_usage("Unknown option specified");
		}
	}

	if (argc - optind != 3) {
		exit_with_usage("Invalid number of arguments");
	} else {
		*source_file = argv[argc-2];
		*dest_file = argv[argc-1];
		*buffer_size = atoi(argv[argc-3]);

		if (strlen(*source_file) == 0 || strlen(*dest_file) == 0) {
			exit_with_usage("Invalid source / destination file name");
		} else if (*buffer_size < 1 || *buffer_size > MAX_BUFFER_SIZE) {
			exit_with_usage("Invalid buffer size");
		}
	}
}

int main(int argc, char **argv) {
	int force_flag = 0; /* force flag default: false */
	char *source_file = NULL;
	char *dest_file = NULL;
	int buffer_size = MAX_BUFFER_SIZE;

	parse_arguments(argc, argv, &source_file, &dest_file, &buffer_size, &force_flag);

	copy_file(source_file, dest_file, buffer_size, force_flag);

	return EXIT_SUCCESS;
}
