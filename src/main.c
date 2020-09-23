#include <stdio.h>
#include <stdlib.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

#define NAME    "oba"
#define VERSION "0.0"
#define PROMPT  ">> "

char* read(void) {

}

int eval(char* input) {}


void repl(void) {
	char *input;
	int status;

	do {
		printf(PROMPT);
		input = read(); 
		status = eval(input);
		free(input);
	} while (status);
}

void welcome(void) {
	printf("%s %s\n", NAME, VERSION);
}

int main(int argc, char **argv) {
	welcome();
	repl();
	return EXIT_SUCCESS;
}


