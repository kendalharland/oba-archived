#ifndef oba_h
#define oba_h

#define OBA_VERSION_STRING "0.0.1"

// Public APIs for the Oba language -------------------------------------------

typedef enum {
	OBA_RESULT_SUCCESS,
	OBA_RESULT_COMPILE_ERROR,
	OBA_RESULT_RUNTIME_ERROR
} ObaInterpretResult;

// A single virtual machine for execute Oba code.
typedef struct ObaVM ObaVM;

// Creates a new Oba Virtual Machine.
ObaVM* obaNewVM();

// Disposes of all resources in use by the vm, which was previously created by
// a call to [obaVM].
void obaFreeVM(ObaVM*);

// Runs [source], a string of Oba source code.
ObaInterpretResult obaInterpret(ObaVM* vm, const char* source);

#endif
