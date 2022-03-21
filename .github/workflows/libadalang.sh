FILE=libadalang-22.1-x86_64-linux-bin.tar.gz
PREFIX=/tmp/ADALIB_DIR
mkdir -p $PREFIX
tar xzf $FILE -C $PREFIX
rm -f -v $FILE
