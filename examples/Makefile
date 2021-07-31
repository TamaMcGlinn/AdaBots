MAIN_SOURCES=$(shell echo src/*.adb | xargs -n 1 basename | tr '\n' ,)

all:
	gprbuild -Xexecutables=$(MAIN_SOURCES)

%: src/%.adb
	gprbuild -Xexecutables=$^

