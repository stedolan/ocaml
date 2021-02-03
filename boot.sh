#!/bin/bash

cd "$(dirname "$0")"
set -e

PAR="-j $((`nproc` * 2))"

cd ocaml
[ -e config.status ] || ./configure --prefix `pwd`/_install --disable-instrumented-runtime --disable-debugger --disable-ocamldoc --disable-shared

echo

# This bit could be sped up by using make -q to check for a null build
# In the null build case, we can skip 'make install'

echo -n "Building bytecode OCaml..."
make -s $PAR world
echo "done"

echo -n "Installing bytecode OCaml..."
rm -f ocamlopt ocamlopt.opt
make -s install
echo "done"

cd ..

# Something in asmcomp's dune file expects this to be here
# Just copy it here for now
cp ocaml/Makefile.config .

export PATH=`pwd`/ocaml/_install/bin:$PATH

echo "Building Flambda compiler..."
# This builds a bytecode optmain if only bytecode is available
dune b optmain.exe
echo done

cp _build/default/optmain.exe ocaml/ocamlopt.opt
chmod 755 ocaml/ocamlopt.opt
touch ocaml/ocamlopt
cd ocaml
rm -f $(find . -name '*.cmx*')

echo -n "Building native runtime..."
make -s $PAR runtimeopt
echo done

# I think these bits would be faster as a dune build,
# although it doesn't need to be the same dune invocation as optmain above

echo -n "Building native OCaml stdlib..."
make -s $PAR --old-file ocamlopt CAMLOPT='$(ROOTDIR)/ocamlopt.opt -g -nostdlib -I stdlib -I otherlibs/dynlink' libraryopt
echo done
echo -n "Building native OCaml compilerlibs..."
make -s $PAR --old-file ocamlopt CAMLOPT='$(ROOTDIR)/ocamlopt.opt -g -nostdlib -I stdlib -I otherlibs/dynlink' compilerlibs/{ocamlcommon,ocamlbytecomp}.cmxa
echo done
echo -n "Building native OCaml otherlibs..."
make -s $PAR otherlibrariesopt
echo done

echo -n "Installing..."
cp ocamlopt.opt _install/bin/
make -s -C runtime installopt
make -s -C stdlib installopt
cp \
  utils/*.cmx parsing/*.cmx typing/*.cmx bytecomp/*.cmx \
  file_formats/*.cmx \
  lambda/*.cmx \
  driver/*.cmx \
  compilerlibs/*.{cmxa,a} \
  _install/lib/ocaml/compiler-libs/
echo done
cd ..
