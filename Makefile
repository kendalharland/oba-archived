ifndef config
  config=release
endif

ifeq ($(config),debug)
			 ALL_CFLAGS += -g -DDEBUG_TRACE_EXECUTION
else ifeq ($(config),optimize)
			 ALL_CFLAGS += -DOBA_COMPUTED_GOTO
else ifneq ($(config),release)
		$(error "invalid configuration $(config)")
endif

PROJECTS := oba
TARGET := oba

INCLUDES += -I ./src/include
ALL_CFLAGS += $(INCLUDES) -o $(TARGET)

.PHONY: all clean format run test help

all: $(PROJECTS)

clean:
	@echo "==== Removing oba ===="
	rm -rf $(TARGET)

format:
	@echo "==== Formatting oba source code ===="
	find . -regex '.*\.\(c\|h\)' -exec clang-format -style=file -i {} \;
	@echo "==== Formatting tools ===="
	black tools/

oba: clean
	@echo "==== Building oba ($(config)) ===="
	$(CC) $(ALL_CFLAGS) ./src/main.c ./src/vm/*.c

run: oba
	@echo "==== Running oba ($(config)) ===="
	./oba 

test: oba
	@echo "==== Testing oba ($(config)) ===="
	python3 tools/test.py

help:
	@echo "Usage: make [target]"
	@echo ""
	@echo "TARGETS:"
	@echo "   all (default)"
	@echo "   clean"
	@echo "   format"
	@echo "   oba"
	@echo "   run"
	@echo "   test"
	@echo ""
	@echo "For more information, see https://github.com/premake/premake-core/wiki"

