#!/usr/bin/env bash

set -e

if [[ $# < 6 ]]
then
  echo "Usage: ./prepare-transitive-load.sh GHC GHC_OPTS GHC_PKG DLL_EXT NUM_PKGS NUM_FUNCS NUM_REFS"
  exit 1
fi

ghc_cmd="$1"
ghc_opts="$2"
ghc_pkg_cmd="$3"
dll_ext="$4"
num_pkgs="$5"
num_funcs="$6"
num_refs="$7"

base="$PWD"
src="$base/src"
build="$base/build"
db="$base/db"

ghc_pkg()
{
  eval "${ghc_pkg_cmd@Q} --no-user-package-db --package-db=${db@Q} $@"
}

ghc()
{
  eval "${ghc_cmd@Q} $ghc_opts $@"
}

version_suffix=$(ghc --numeric-version)

range()
{
  eval echo "{1..$1}"
}

append()
{
  echo -e "$*" >> $file_name
}

create_sources()
{
  local num="$1"
  local prev="$(($num - 1))"
  local name="load${num}"
  local module_name="Load${num}"
  local fun expr
  file_name="${module_name}.hs"
  cd "$base"
  mkdir -p "$src/$name" "$build/$name/lib" "$build/$name/hi"
  cd "$src"
  cd "$name"
  append "module $module_name where"
  if [[ $prev != 0 ]]
  then
    append "\nimport Load${prev}"
  fi
  for j in $(range $num_funcs)
  do
    fun="num${num}_${j}"
    append "\n$fun :: Int"
    if [[ $prev == 0 ]]
    then
      expr="$j"
    else
      expr="num${prev}_${j}"
    fi
    append "$fun = $expr"
  done
  cd "$build"
  file_name="${name}/${name}.conf"
  append "name: $name
version: 1.0
id: ${name}-1.0
key: ${name}-1.0
exposed: True
exposed-modules: ${module_name}
import-dirs: \${pkgroot}/build/${name}/hi
dynamic-library-dirs: \${pkgroot}/build/${name}/lib
hs-libraries: HS${name}-1.0"
  if [[ $prev != 0 ]]
  then
    append "depends: load${prev}-1.0"
  fi
}

create_package()
{
  local num="$1"
  local prev="$(($num - 1))"
  local name="load${num}"
  local module_name="Load${num}"
  local pkg_build="$build/$name"
  if [[ $prev != 0 ]]
  then
    extra="-package load${prev}"
  fi
  local opts="-package-db ${db@Q} -hidir ${pkg_build@Q}/hi -O0 -this-unit-id ${name}-1.0 $extra"
  # dynamic-too produces .dyn_o and .dyn_hi
  # eval "${ghc@Q} $opts -dynamic-too -c ${src@Q}/${name}/${module_name}.hs"
  ghc "$opts -dynamic-too -c ${src@Q}/${name}/${module_name}.hs"
  # -dynamic instructs GHC to link against shared objects of dependencies
  ghc "$opts -dynamic -shared -fPIC -o ${pkg_build@Q}/lib/libHS${name}-1.0-ghc${version_suffix}${dll_ext} ${src@Q}/${name}/${module_name}.dyn_o"
  ghc_pkg register "${build@Q}/${name}/${name}.conf"
}

mkdir "$src" "$build" "$db"

for i in $(range $num_pkgs)
do
  create_sources $i
done

cd "$base"
ghc_pkg recache

for i in $(range $num_pkgs)
do
  create_package $i
done

cd "$base"

file_name="Main.hs"

append "module Main where"

# for i in $(range $num_pkgs)
# do
#   append "import Load${i}"
# done

append "import Load${num_pkgs}"

for i in $(range $num_refs)
do
  append "
ref${i} :: Int
ref${i} = sum ["
  for j in $(range $num_funcs)
  do
    append "  num${num_pkgs}_${j},"
  done
  append "  0]"
done

append '
main :: IO ()
main = do'

append '  putStrLn $ show $ sum ['

for i in $(range $num_refs)
do
  append "    ref${i},"
done

append "    0]"

file_name=T23415.script

append 'import Data.Time.Clock.System
import System.IO
import Data.Int
start <- systemSeconds <$> getSystemTime
writeFile "start-time" (show start)
hPutStrLn stderr "--------------------- START"
'

for i in $(eval echo "{1..${num_pkgs}}")
# for i in $(eval echo "{${num_pkgs}..1}")
do
  append "import Load${i}
putStrLn (show num${i}_1)"
done

append '
endLoad1 <- systemSeconds <$> getSystemTime
start :: Int64 <- readIO =<< readFile "start-time"
writeFile "start-time" (show endLoad1)
hPutStrLn stderr ("--------------------- LOADED 1: " ++ show (endLoad1 - start) ++ " seconds")
:load Main
endLoad2 <- systemSeconds <$> getSystemTime
endLoad1 :: Int64 <- readIO =<< readFile "start-time"
hPutStrLn stderr ("--------------------- LOADED 2: " ++ show (endLoad2 - endLoad1) ++ " seconds")
main
endExec1 <- systemSeconds <$> getSystemTime
hPutStrLn stderr ("--------------------- EXECUTED 1: " ++ show (endExec1 - endLoad2) ++ " seconds")
main
endExec2 <- systemSeconds <$> getSystemTime
writeFile "start-time" (show endExec2)
hPutStrLn stderr ("--------------------- EXECUTED 2: " ++ show (endExec2 - endExec1) ++ " seconds")
:reload
main
endExec3 <- systemSeconds <$> getSystemTime
endExec2 :: Int64 <- readIO =<< readFile "start-time"
hPutStrLn stderr ("--------------------- EXECUTED 3: " ++ show (endExec3 - endExec2) ++ " seconds")
'
