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
RESULT = prcc
SOURCES = \
	exception.ml	\
	symbol.ml	\
	label.ml	\
	cset.ml	\
	pos.ml	\
	regex.ml	\
	prop.ml	\
	cond.ml	\
	nfa.ml	\
	subset.ml	\
	types.ml	\
	error.ml	\
	env.ml	\
	syntax.ml	\
	check.ml	\
	tcond.ml	\
	dstate.ml	\
	dtrans.ml	\
	dtable.ml	\
	dfa.ml	\
	pi.ml		\
	match.ml	\
	ptrans.ml	\
	cps.ml	\
	closure.ml	\
	rmap.ml	\
	emit.ml	\
	trans.ml	\
	semant.ml	\
	version.ml	\
	parser.mli	\
	parser.mly	\
	lexer.mll \
	main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
