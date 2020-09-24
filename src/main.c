#include <stdio.h>
#include <stdlib.h>

#include <oba.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

#define PROMPT ">> "

char* read(void) {
  char* line = NULL;
  ssize_t bufsize = 0; // have getline allocate a buffer for us

  if (getline(&line, &bufsize, stdin) == -1) {
    if (feof(stdin)) {
      return NULL;
    } else {
      perror("read");
      exit(EXIT_FAILURE);
    }
  }

  return line;
}

ObaInterpretResult interpret(char* input) {
  // TODO(kendal): the repl should use the same VM each time.
  ObaVM* vm = obaNewVM();
  ObaInterpretResult result = obaInterpret(vm, input);
  obaFreeVM(vm);
  return result;
}

void repl(void) {
  char* input;
  ObaInterpretResult result;

  do {
    printf(PROMPT);
    input = read();
    result = interpret(input);
    if (input != NULL)
      free(input);
  } while (!feof(stdin));

  printf("exiting. \n");
}

void welcome(void) {
  printf("oba %s\n", OBA_VERSION_STRING);
  printf("Press ctrl+d to exit\n");
}

int main(int argc, char** argv) {
  welcome();
  repl();
  return EXIT_SUCCESS;
}
