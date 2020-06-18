#!/bin/bash

# #########
# Variables
# #########
# number of errors lines to show from the log file
N="${N:-15}"
# tmpfile for logigng command output
TMPFILE="${TMPFILE:-$(mktemp /tmp/flambda-build.XXXXXX)}"
# Local install directory
INSTALL_DIR="$1"
# Local Flambda repo
REPO="$2"


# ##################
# Auxiliary function
# ##################
# Run a command in a directory and print some output message if it fails
# $1 : messgae to announce the beginiing
# $2 : directory to cd into before running
# $3 : command to run inside of $2
wrap () {
  echo -ne "$1 ..."
  cd "$2" || (echo "cannot cd into $2"; exit 1)
  if $3 "${@:4}" &> "$TMPFILE"; then
    echo " done"
  else
    echo " failed !"
    echo -e "\e[31mERROR !\e[0m see full log file ${TMPFILE}, but here's an excerpt:"
    echo -e "\e[1m$2 \$ $3 " "${@:4}" "\e[0m"
    tail -n "${N}" "${TMPFILE}"
    exit 1
  fi
}

# ##################
# Cleaning functions
# ##################

clean_git () {
  wrap "cleaning repo" "$1" "git" "clean" "-dfx"
}

clean_dune () {
  wrap "cleaning build artefacts" "$1" "dune" "clean"
}

clean_tmpfiles () {
  rm -rf /tmp/flambda-build.*
}

# #################
# Helpers functions
# #################

configure () {
  wrap "configuring repo with option $1" "${REPO}" "./configure" "$1"
}

build () {
  wrap "building ${1} stage (${2})" "${REPO}" "dune" "build" "${2}" "${@:3}"
}

patch () {
  wrap "applying patch" "${REPO}" "git" "apply" "$1"
}

dcp () {
  wrap "copying ${1}" "${REPO}" "cp" "_build/default/${2}" "${3}"
}

cpd () {
  wrap "copying ${1}" "${REPO}" "cp" "-r" "_build/default/${2}"* "${3}"
}

# ###########
# Main script
# ###########

main () {
  export PATH=${INSTALL_DIR}/bin/:$PATH
  export OCAMLPARAM="_,flambda-invariants=0"

  # print some info
  echo "install dir = ${INSTALL_DIR}"
  echo "repository  = ${REPO}"

  # First stage compilation
  configure "--enable-flambda"
  build "first" "@world"

  # Override vanilla ocamlopt
  wrap "override vanilla ocamlopt" "${REPO}" "cp" "_build/default/ocamlopt.opt" "${INSTALL_DIR}/bin/ocamlopt.opt"

  # Cleanup repo
  clean_dune "${REPO}"

  # Second stage compilation
  build "second" "@world"
  build "second" "@testing"
  build "second" "testsuite/tools/expect_test.exe"

  # Copy build artefacts back into the repo
  wrap "copying ocamltest" "${REPO}" "cp" "${INSTALL_DIR}/bin/ocamltest"   "ocamltest/"
  dcp "ocamlc"        "ocamlc"                                "./"
  dcp "ocmalopt"      "ocamlopt.opt"                          "./"
  dcp "expect_test"   "testsuite/tools/expect_test.exe"       "./testsuite/tools/expect_test"
  cpd "runtime"       "runtime/"                              "./runtime/"
  cpd "stdlib"        "stdlib/"                               "./stdlib/"
  cpd "stdlib.byte"   "stdlib/.stdlib.objs/byte/"             "./stdlib/"
  cpd "stdlib.opt"    "stdlib/.stdlib.objs/native/"           "./stdlib/"
  cpd "testlib"       "testsuite/lib/testing."                "./testsuite/lib/"
  cpd "testlib.byte"  "testsuite/lib/.testing.objs/byte/"     "./testsuite/lib/"
  cpd "testlib.opt"   "testsuite/lib/.testing.objs/native/"   "./testsuite/lib/"
  # create a dummy ocamlrun script
  echo -ne "creating dummy ocamlrun script ..."
  echo "#!/bin/bash" > "${REPO}/runtime/ocamlrun"
  echo "\"\$@\"" >> "${REPO}/runtime/ocamlrun"
  chmod +x "${REPO}/runtime/ocamlrun"
  echo "done"

  # Setup the env for the testsuite
  export OCAMLSRCDIR=${REPO}

  # Run the testsuite
  wrap "running the testsuite" "${REPO}/testsuite" "make" "list-parallel" "FILE=../.github/workflows/dune.list"

}

# #################
# Main Control Flow
# #################
main
rm -rf "${TMPFILE}"


