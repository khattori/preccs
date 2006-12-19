#!/bin/sh
################################################################
#
# Preccsパッケージビルド
#
# ---
# $Id$
#

prc_version=0.2.1
prc_blddir=build
prc_pkglist=pkglist

prc_pkgname="preccs-$prc_version"
prc_pkgdir="$prc_blddir/$prc_pkgname"

# パッケージビルド
if test -d "$prc_blddir"; then
    echo "build directory clean up..."
    rm -rf "$prc_blddir"
fi
mkdir "$prc_blddir"
mkdir "$prc_pkgdir"

# サブディレクトリをクリーンアップしておく
for i in sample/*; do
    (cd $i; make clean-all)
done
(cd test; make clean-all)

# ファイルをコピー
echo "copy files..."
mkdir $prc_pkgdir/src
mkdir $prc_pkgdir/src/compiler
mkdir $prc_pkgdir/src/runtime
mkdir $prc_pkgdir/sample
for i in sample/*; do
    mkdir $prc_pkgdir/$i
done
mkdir $prc_pkgdir/test

cp README   $prc_pkgdir/README
cp Makefile $prc_pkgdir/Makefile

for i in \
    src/compiler/Makefile \
    src/compiler/*.ml src/compiler/*.mly \
    src/compiler/*.mll \
    src/runtime/Makefile \
    src/runtime/*.c \
    src/runtime/*.h \
    src/runtime/*.def \
    test/Makefile \
    test/*.prc \
    test/*.c \
    test/*.h
  do
  if test -f $i; then
      cp $i $prc_pkgdir/$i
  fi
done

sed -e "s/x.x.x/$prc_version/" src/compiler/version.ml \
    > $prc_pkgdir/src/compiler/version.ml

for i in sample/*; do
    for j in $i/Makefile $i/*.prc $i/*.c $i/*.h; do
	if test -f $j; then
	    cp $j $prc_pkgdir/$j
	fi
    done
done

# パッケージ作成
echo "create package..."
(cd $prc_blddir; tar zcvf $prc_pkgname.tar.gz $prc_pkgname)

echo "done."
