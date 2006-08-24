################################################################
#
# Preccsコンパイラ Makefile
#
#	make		-- 実行ファイルを生成
#	make clean	-- 中間ファイルやオブジェクトファイルを削除
#	make depend	-- 依存関係を生成
# ---
# $Id: Makefile,v 1.5 2006/06/06 04:39:15 hattori Exp $
#

TARGET = prcc
OBJS =	symbol.cmo	\
	error.cmo	\
	label.cmo	\
	prop.cmo	\
	cset.cmo	\
	pos.cmo	\
	cond.cmo	\
	regex.cmo	\
	nfa.cmo	\
	subset.cmo	\
	types.cmo	\
	env.cmo	\
	lexer.cmo	\
	parser.cmo	\
	syntax.cmo	\
	check.cmo	\
	dfa.cmo	\
	pi.cmo		\
	cps.cmo	\
	closure.cmo	\
	rmap.cmo	\
	emit.cmo	\
	trans.cmo	\
	semant.cmo	\
	main.cmo

DEPEND += lexer.ml parser.ml

all: $(DEPEND) $(OBJS) $(TARGET)

include .depend

# Build an executable typechecker
$(TARGET):	$(OBJS)
	@echo Linking $@
	ocamlc -o $@ $(COMMONOBJS) $(OBJS)

# Compile an ML module interface
%.cmi : %.mli
	ocamlc -c $<

# Compile an ML module implementation
%.cmo : %.ml
	ocamlc -c $<

# Generate ML files from a parser definition file
parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	ocamlyacc -v parser.mly
	@chmod -w parser.ml parser.mli

# Generate ML files from a lexer definition file
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

# Clean up the directory
clean::
	rm -rf lexer.ml parser.ml parser.mli *.o *.cmo *.cmi parser.output \
		$(TARGET) *~

# Rebuild intermodule dependencies
depend:: $(DEPEND)
	ocamldep $(INCLUDE) *.mli *.ml > .depend
