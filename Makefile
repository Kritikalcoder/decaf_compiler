FLAGS=-w -Wno-deprecated -g -std=c++11
CFLAGS = -lfl `llvm-config --cxxflags --cppflags mcjit native --ldflags --libs core` -ltinfo -lpthread -ldl
parser: parser.ypp scanner.l
		bison -v -d parser.ypp
		flex scanner.l
		g++ $(FLAGS) -o parser lex.yy.c parser.tab.cpp ast.cpp constructs.cpp $(CFLAGS)

clean:
		rm parser.tab.cpp lex.yy.c parser.tab.hpp parser.output parser 