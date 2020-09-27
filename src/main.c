#include <stdio.h>
#include <stdlib.h>

#include <oba.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#define EXIT_COMPILE_ERROR 65
#define EXIT_RUNTIME_ERROR 75

#define PROMPT ">> "

static char* read(void) {
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

static ObaInterpretResult interpret(char* input) {
  // TODO(kendal): the repl should use the same VM each time.
  ObaVM* vm = obaNewVM();
  ObaInterpretResult result = obaInterpret(vm, input);
  obaFreeVM(vm);
  return result;
}

static void repl(void) {
  char* input;
  ObaInterpretResult result;

  // Print banner.
  printf("oba %s\n", OBA_VERSION_STRING);
  printf("Press ctrl+d to exit\n");

  do {
    printf(PROMPT);
    input = read();
    result = interpret(input);
    if (input != NULL)
      free(input);
  } while (!feof(stdin));

  printf("exiting. \n");
}

static char* readFile(const char* filename) {
  FILE* file = fopen(filename, "rb");

  fseek(file, 0L, SEEK_END);
  size_t fileSize = ftell(file);
  rewind(file);

  char* buffer = (char*)malloc(fileSize + 1);
  size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
  buffer[bytesRead] = '\0';

  fclose(file);
  return buffer;
}

static void runFile(const char* filename) {
  char* source = readFile(filename);
  ObaInterpretResult result = interpret(source);
  free(source);

  if (result == OBA_RESULT_COMPILE_ERROR)
    exit(EXIT_COMPILE_ERROR);
  if (result == OBA_RESULT_RUNTIME_ERROR)
    exit(EXIT_RUNTIME_ERROR);
}

int main(int argc, char** argv) {
  if (argc == 1) {
    repl();
  } else if (argc == 2) {
    runFile(argv[1]);
  } else {
    fprintf(stderr, "Usage: oba [path]\n");
    exit(EXIT_FAILURE);
  }
  return EXIT_SUCCESS;
}
