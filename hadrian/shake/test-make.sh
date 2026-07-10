DEFAULT_TESTS=" \
    features/comments \
    misc/general1 \
    misc/general2 \
    targets/clean \
    targets/FORCE \
    targets/PHONY \
    targets/SILENT \
    variables/flavors \
    "

mkdir --parents .hpc/shake
ghc -hide-package=hashmap -package transformers --make Main.hs Paths.hs -w -odir .hpc/shake -hidir .hpc/shake -o .hpc/shake/shake
SHAKE=`pwd`/.hpc/shake/shake
TESTS=$*
if [ "$TESTS" == "" ]
then
    TESTS=$DEFAULT_TESTS
fi
cd ../gnumake/tests; ./run_make_tests.pl -make_path $SHAKE $TESTS
echo Total: 83 Tests in 39 Categories
