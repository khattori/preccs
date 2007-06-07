#!/bin/sh
################################################################
#
# Preccsパッケージビルド
#
# ---
# $Id$
#

prc_version=0.2.3c
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
  if test -d $i; then
    (cd $i; make clean-all)
  fi
done
(cd test; make clean-all)

# ファイルをコピー
echo "copy files..."
mkdir $prc_pkgdir/src
mkdir $prc_pkgdir/src/compiler
mkdir $prc_pkgdir/src/runtime
mkdir $prc_pkgdir/src/runtime/unix
mkdir $prc_pkgdir/src/runtime/win32
mkdir $prc_pkgdir/sample
for i in sample/*; do
  if test -d $i; then
    mkdir $prc_pkgdir/$i
  fi
done
mkdir $prc_pkgdir/test
mkdir $prc_pkgdir/test/error
mkdir $prc_pkgdir/test/io

cp README    $prc_pkgdir/README
cp Makefile  $prc_pkgdir/Makefile
cp ChangeLog $prc_pkgdir/ChangeLog
cp TODO      $prc_pkgdir/TODO

for i in \
    sample/sample.mk \
    src/compiler/Makefile \
    src/compiler/*.ml src/compiler/*.mly \
    src/compiler/*.mll \
    src/runtime/Makefile \
    src/runtime/*.c \
    src/runtime/*.h \
    src/runtime/*.def \
    src/runtime/unix/*.c \
    src/runtime/win32/*.c \
    test/Makefile \
    test/*.prc \
    test/*.c \
    test/*.h \
    test/error/*.prc \
    test/io/Makefile \
    test/io/*.exp \
    test/io/*.prc \
    test/io/*.c \
    test/io/*.h
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
