#include <stdio.h>
#include <stdlib.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

char* read_line(void) {

}

int eval_line(char* line) {}


void repl(void) {
	char *line;
	int status;

	do {
		printf("> ");
		line = read_line(); // Line may not be a complete expr?
		status = eval_line(line);
		free(line);
	} while (status);
}

int main(int argc, char **argv) {
	repl();
	return EXIT_SUCCESS;
}


