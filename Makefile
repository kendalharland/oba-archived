
PROJECTS := oba
TARGET := oba

INCLUDES += -I ./src/include
ALL_CFLAGS += $(INCLUDES) -o $(TARGET)

all: $(PROJECTS)

oba:
	@echo "==== Building oba ===="
	$(CC) $(ALL_CFLAGS) ./src/main.c ./src/vm/*

format:
	@echo "==== Formatting oba source code ===="
	find . -regex '.*\.\(c\|h\)' -exec clang-format -style=file -i {} \;

clean:
	rm -rf $(TARGET)

help:
	@echo "Usage: make [target]"
	@echo ""
	@echo "TARGETS:"
	@echo "   all (default)"
	@echo "   oba"
	@echo ""
	@echo "For more information, see https://github.com/premake/premake-core/wiki"

