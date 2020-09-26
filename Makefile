ifndef config
  config=release
endif

ifeq ($(config),debug)
			 ALL_CFLAGS += -g
else ifneq ($(config),release)
		$(error "invalid configuration $(config)")
endif

PROJECTS := oba
TARGET := oba

INCLUDES += -I ./src/include
ALL_CFLAGS += $(INCLUDES) -o $(TARGET)

all: $(PROJECTS)

oba:
	@echo "==== Building oba ($(config)) ===="
	$(CC) $(ALL_CFLAGS) ./src/main.c ./src/vm/*

format:
	@echo "==== Formatting oba source code ===="
	find . -regex '.*\.\(c\|h\)' -exec clang-format -style=file -i {} \;

clean:
	@echo "=== Removing oba ===="
	rm -rf $(TARGET)

run: clean oba
	./oba

help:
	@echo "Usage: make [target]"
	@echo ""
	@echo "TARGETS:"
	@echo "   all (default)"
	@echo "   oba"
	@echo "   run"
	@echo "   format"
	@echo "   clean"
	@echo ""
	@echo "For more information, see https://github.com/premake/premake-core/wiki"

