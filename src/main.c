#include <stdio.h>
#include <stdlib.h>

#include <oba.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

#define NAME    "oba"
#define VERSION "0.0"
#define PROMPT  ">> "

#define RL_BUFSIZE 1024

char* read(void) {
  char *line = NULL;
  ssize_t bufsize = 0; // have getline allocate a buffer for us

  if (getline(&line, &bufsize, stdin) == -1){
    if (feof(stdin)) {
      return NULL; 
    } else  {
      perror("read");
      exit(EXIT_FAILURE);
    }
  }

  return line;
}

// TODO(kendal): Support calling the compiler.
int eval(char* input) {}


void repl(void) {
	char *input;
	int status;

	do {
		printf(PROMPT);
		input = read(); 
		status = eval(input);
		free(input);
	} while (!feof(stdin));

	printf("exiting. \n");
}

void welcome(void) {
	printf("%s %s\n", NAME, VERSION);
}

int main(int argc, char **argv) {
	welcome();
	repl();
	return EXIT_SUCCESS;
}


