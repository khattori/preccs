#!/bin/sh
################################################################
#
# Preccs�p�b�P�[�W�r���h
#
# ---
# $Id$
#

prc_version=0.2.0a1
prc_blddir=build
prc_pkglist=pkglist

prc_pkgname="preccs-$prc_version"
prc_pkgdir="$prc_blddir/$prc_pkgname"

# �p�b�P�[�W�r���h
if test -d "$prc_blddir"; then
    echo "build directory clean up..."
    rm -rf "$prc_blddir"
fi
mkdir "$prc_blddir"
mkdir "$prc_pkgdir"

# �T�u�f�B���N�g�����N���[���A�b�v���Ă���
for i in sample/*; do
    (cd $i; make clean-all)
done
(cd test; make clean-all)

# �p�b�P�[�W���X�g���쐬
echo "create package list..."
cat<<EOF|grep -v '*' >"$prc_blddir/$prc_pkglist"
$prc_pkgname/README
$prc_pkgname/Makefile
$prc_pkgname/src/compiler/Makefile
`for i in src/compiler/*.ml src/compiler/*.mly src/compiler/*.mll; do
   echo $prc_pkgname/$i
 done`
$prc_pkgname/src/runtime/Makefile
`for i in src/runtime/*.c src/runtime/*.h src/runtime/*.def; do
   echo $prc_pkgname/$i
 done`
$prc_pkgname/test/Makefile
`for i in test/*.prc test/*.c test/*.h; do
   echo $prc_pkgname/$i
 done`
`for i in sample/*; do
   echo $prc_pkgname/$i/Makefile
   for j in $i/*.prc $i/*.c $i/*.h; do
     echo "$prc_pkgname/$j"
   done
done`
EOF

# �p�b�P�[�W�쐬�p�̃V���{���b�N�����N�𐶐�
echo "create symbolic link..."
ln -s src $prc_pkgdir/src
ln -s sample $prc_pkgdir/sample
ln -s test $prc_pkgdir/test
ln -s README $prc_pkgdir/README
ln -s Makefile $prc_pkgdir/Makefile

# �p�b�P�[�W�쐬
echo "create package..."
(cd $prc_blddir; tar zcvf $prc_pkgname.tar.gz -T $prc_pkglist)
echo "done."
