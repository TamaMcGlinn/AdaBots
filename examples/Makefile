MAIN_SOURCES=$(shell echo src/*.adb | xargs -n 1 basename | tr '\n' ,)

all:
	alr build -Xexecutables=$(MAIN_SOURCES)

%: src/%.adb
	alr build -Xexecutables=$^

